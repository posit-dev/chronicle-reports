# Posit Usage Report
# Monthly usage over the last 12 months for each licensed product, showing
# active users, content hits, content deployments and sessions started.
#
# The report draws from a list of small monthly tables. By default they are
# built from Chronicle data at CHRONICLE_BASE_PATH. To rebuild the report from a
# file previously exported with the Download button, set CHRONICLE_USAGE_FILE.

library(shiny)
library(bslib)
library(chronicle.reports)
library(rlang)

# Common application configuration
APP_CONFIG <- list(
  # Default Chronicle data path
  DEFAULT_BASE_PATH = "/var/lib/posit-chronicle/data"
)

# Brand colors
BRAND_COLORS <- list(
  BLUE = "#447099",
  GREEN = "#72994E",
  BURGUNDY = "#9A4665",
  GRAY = "#404041"
)

# Series colors for stacked charts: brand colors first, then lighter tints
SERIES_COLORS <- c(
  "#447099", "#72994E", "#9A4665", "#7a7a7b",
  "#8fb0cf", "#a9c58b", "#c795a9", "#b5b5b6"
)

# Product id the data layer gives usage recorded without one
# (USAGE_UNSPECIFIED_PRODUCT in R/usage_data.R, which is not exported)
UNSPECIFIED_PRODUCT <- "(unspecified)"

NO_DATA_MESSAGE <-"No data in the last 12 months."

# Height of one product's panel in a stacked-bar chart, in pixels
PANEL_HEIGHT <- 220

# ==============================================
# Data loading
# ==============================================

# Returns the list of monthly tables the report draws from.
load_usage_tables <- function() {
  usage_file <- Sys.getenv("CHRONICLE_USAGE_FILE", "")
  if (nzchar(usage_file)) {
    return(chronicle_usage_from_json(usage_file))
  }

  base_path <- Sys.getenv("CHRONICLE_BASE_PATH", APP_CONFIG$DEFAULT_BASE_PATH)
  chronicle_usage_data(base_path)
}

# ==============================================
# Chart data helpers
# ==============================================

# First day of each month in the reporting window.
usage_window_months <- function(tables) {
  window <- attr(tables, "window")
  if (is.null(window)) {
    all_months <- do.call(c, lapply(tables, function(tbl) tbl$month))
    if (length(all_months) == 0) {
      return(as.Date(character()))
    }
    window <- range(all_months)
  }
  seq(window[1], window[2], by = "month")
}

# Sum `value` per month and product id (and per `group`, if given), with a zero
# for every month, so that empty months still appear on the chart. Every product
# id gets every group, so that each product's panel has the same series.
usage_series <- function(data, value, months, group = NULL) {
  by <- c("month", "product_id", group)

  totals <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(value = sum(.data[[value]]), .groups = "drop")

  levels <- list(
    month = months,
    product_id = sort(unique(data$product_id))
  )
  if (!is.null(group)) {
    levels[[group]] <- sort(unique(data[[group]]))
  }

  do.call(expand.grid, c(levels, stringsAsFactors = FALSE)) |>
    dplyr::left_join(totals, by = by) |>
    dplyr::mutate(value = dplyr::coalesce(.data$value, 0)) |>
    dplyr::arrange(.data$product_id, .data$month) |>
    dplyr::as_tibble()
}

# TRUE when there is anything other than zeros to draw.
usage_has_data <- function(series) {
  nrow(series) > 0 && any(series$value > 0)
}

# Axis labels, flagging the current month because it is still in progress.
usage_month_labels <- function(months, today = Sys.Date()) {
  labels <- format(months, "%b %Y")
  partial <- months == as.Date(format(today, "%Y-%m-01"))
  ifelse(partial, paste0(labels, "*"), labels)
}

# A color for each name, in order. Names are sorted first, so a name keeps its
# color however the chart is filtered.
usage_colors <- function(names) {
  names <- sort(unique(names))
  colors <- rep_len(SERIES_COLORS, length(names))
  stats::setNames(colors, names)
}

# Pixel height of a chart with one panel per product id.
usage_panel_height <- function(series, minimum = 340) {
  max(minimum, length(unique(series$product_id)) * PANEL_HEIGHT)
}

# ==============================================
# Chart helpers
# ==============================================

usage_axis_layout <- function(p, labels, y_title) {
  plotly::layout(
    p,
    xaxis = list(title = "", categoryorder = "array", categoryarray = labels, fixedrange = TRUE),
    yaxis = list(title = y_title, rangemode = "tozero", fixedrange = TRUE),
    legend = list(orientation = "h", y = -0.15)
  ) |>
    plotly::config(displayModeBar = FALSE)
}

# One line per product id.
usage_line_chart <- function(series, months, y_title = NULL) {
  series$label <- usage_month_labels(series$month)

  plotly::plot_ly(
    series,
    x = ~label,
    y = ~value,
    color = ~product_id,
    colors = usage_colors(series$product_id),
    type = "scatter",
    mode = "lines+markers",
    hovertemplate = "%{x}: %{y:,}<extra>%{fullData.name}</extra>"
  ) |>
    usage_axis_layout(usage_month_labels(months), y_title %||% "")
}

# Y-axis title of one product's panel: the product id, then `y_title`. When no
# product has an id (every id is the "(unspecified)" placeholder) there is
# nothing to tell panels apart by, so the title is just `y_title`. Alongside
# real product ids, "(unspecified)" is shown like any other.
usage_panel_y_title <- function(product_id, product_ids, y_title = NULL) {
  y_title <- y_title %||% ""
  if (identical(as.character(product_ids), UNSPECIFIED_PRODUCT)) {
    return(y_title)
  }
  paste0(product_id, "<br>", y_title)
}

# One panel per product id, each a stacked bar chart split by `group`.
usage_stacked_chart <- function(series, months, group, y_title = NULL) {
  series$label <- usage_month_labels(series$month)
  colors <- usage_colors(series[[group]])
  product_ids <- sort(unique(series$product_id))

  panels <- lapply(seq_along(product_ids), function(i) {
    panel <- series[series$product_id == product_ids[i], ]
    plotly::plot_ly(
      panel,
      x = ~label,
      y = ~value,
      color = panel[[group]],
      colors = colors,
      legendgroup = panel[[group]],
      showlegend = i == 1,
      type = "bar",
      hovertemplate = "%{x}: %{y:,}<extra>%{fullData.name}</extra>"
    ) |>
      usage_axis_layout(
        usage_month_labels(months),
        usage_panel_y_title(product_ids[i], product_ids, y_title)
      )
  })

  plotly::subplot(panels, nrows = length(panels), shareX = TRUE, titleY = TRUE, margin = 0.04) |>
    plotly::layout(barmode = "stack", legend = list(orientation = "h", y = -0.15))
}

# ==============================================
# UI
# ==============================================

# A chart in a card. The plot is rendered through a UI output so that the
# height can follow the number of product ids.
chart_card <- function(title, output_id, controls = NULL) {
  bslib::card(
    bslib::card_header(title),
    controls,
    shinycssloaders::withSpinner(
      uiOutput(paste0(output_id, "_plot")),
      color = BRAND_COLORS$BLUE
    ),
    full_screen = TRUE,
    fill = FALSE
  )
}

# The container for a plot, `height` pixels tall
plot_container <- function(output_id, height) {
  plotly::plotlyOutput(output_id, height = paste0(height, "px"))
}

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5),
  div(
    class = "d-flex justify-content-between align-items-start flex-wrap gap-3 my-3",
    div(
      h3("Posit Usage Report", class = "mb-1"),
      p(
        class = "text-muted mb-0",
        "Monthly usage for each licensed product. Each product ID is one series. ",
        "Counts are calendar months, and * marks the month still in progress."
      )
    ),
    div(
      downloadButton("download_data", "Download data (JSON)", class = "btn-outline-primary"),
      p(
        class = "text-muted small mt-1 mb-0",
        "Monthly counts only, with no user or content details."
      )
    )
  ),
  h4("Posit Connect"),
  bslib::layout_columns(
    col_widths = c(6, 6),
    chart_card("Active users", "connect_active_users"),
    chart_card(
      "Content hits",
      "content_hits",
      controls = radioButtons(
        "hits_group",
        NULL,
        choices = c("Authenticated vs anonymous" = "authenticated", "Content type" = "app_mode"),
        inline = TRUE
      )
    )
  ),
  bslib::layout_columns(
    col_widths = 6,
    chart_card(
      "Content deployments",
      "content_deployments",
      controls = radioButtons(
        "deployments_metric",
        NULL,
        choices = c(
          "All deployments, including redeploys" = "deployments",
          "Unique content items deployed" = "unique_content"
        ),
        inline = TRUE
      )
    )
  ),
  h4("Posit Workbench"),
  bslib::layout_columns(
    col_widths = c(6, 6),
    chart_card("Active users", "workbench_active_users"),
    chart_card("Sessions started", "sessions")
  )
)

# ==============================================
# Server
# ==============================================

server <- function(input, output, session) {
  tables <- reactive(load_usage_tables())
  months <- reactive(usage_window_months(tables()))

  # Define a chart and its sized container. `series_fn` returns the chart data,
  # and `chart_fn(series)` draws it. Both are re-run when their inputs change.
  define_chart <- function(output_id, series_fn, chart_fn, stacked) {
    series <- reactive(series_fn())

    output[[paste0(output_id, "_plot")]] <- renderUI({
      height <- if (stacked) usage_panel_height(series()) else 340
      plot_container(output_id, height)
    })

    output[[output_id]] <- plotly::renderPlotly({
      validate(need(usage_has_data(series()), NO_DATA_MESSAGE))
      chart_fn(series())
    })
  }

  active_users_series <- function(product) {
    function() {
      data <- tables()$active_users
      usage_series(data[data$product == product, ], "users", months())
    }
  }

  define_chart(
    "connect_active_users",
    active_users_series("connect"),
    function(series) usage_line_chart(series, months(), "Active users"),
    stacked = FALSE
  )

  define_chart(
    "workbench_active_users",
    active_users_series("workbench"),
    function(series) usage_line_chart(series, months(), "Active users"),
    stacked = FALSE
  )

  hits_group <- reactive(input$hits_group %||% "authenticated")
  define_chart(
    "content_hits",
    function() {
      data <- tables()$content_hits
      data$authenticated <- ifelse(data$authenticated, "Authenticated", "Anonymous")
      usage_series(data, "hits", months(), group = hits_group())
    },
    function(series) usage_stacked_chart(series, months(), hits_group(), "Hits"),
    stacked = TRUE
  )

  deployments_metric <- reactive(input$deployments_metric %||% "deployments")
  define_chart(
    "content_deployments",
    function() {
      usage_series(tables()$content_deployments, deployments_metric(), months(), group = "app_mode")
    },
    function(series) {
      y_title <- if (deployments_metric() == "deployments") "Deployments" else "Content items"
      usage_stacked_chart(series, months(), "app_mode", y_title)
    },
    stacked = TRUE
  )

  define_chart(
    "sessions",
    function() usage_series(tables()$sessions, "sessions", months(), group = "session_type"),
    function(series) usage_stacked_chart(series, months(), "session_type", "Sessions started"),
    stacked = TRUE
  )

  # The export is the same list of tables the charts draw from.
  output$download_data <- downloadHandler(
    filename = function() paste0("chronicle-usage-", format(Sys.Date(), "%Y-%m-%d"), ".json"),
    content = function(file) chronicle_usage_to_json(tables(), file),
    contentType = "application/json"
  )
}

shinyApp(ui, server)
