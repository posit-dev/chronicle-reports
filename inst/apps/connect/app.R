# Posit Connect Dashboard
# Comprehensive dashboard providing analytics for Posit Connect across Users, Content, and Usage

library(shiny)
library(bslib)
library(chronicle.reports)
library(rlang)

# Common application configuration
APP_CONFIG <- list(
  # Default Chronicle data path
  DEFAULT_BASE_PATH = "/var/lib/posit-chronicle/data"
)

# Get base path from environment variable
base_path <- Sys.getenv(
  "CHRONICLE_BASE_PATH",
  APP_CONFIG$DEFAULT_BASE_PATH
)

# Optional data window: when set, only load the last N days of data on startup.
# Value should be a positive integer (number of days). When unset, all data is
# loaded. Date selectors can expand the loaded range beyond the initial window.
data_window_days <- Sys.getenv("CHRONICLE_DATA_WINDOW", "")
data_window_int <- suppressWarnings(as.integer(data_window_days))
if (
  nzchar(data_window_days) && (is.na(data_window_int) || data_window_int <= 0)
) {
  warning(
    "CHRONICLE_DATA_WINDOW must be a positive integer. ",
    "Got '",
    data_window_days,
    "'. Loading all available data.",
    call. = FALSE
  )
}
data_window_cutoff <- if (!is.na(data_window_int) && data_window_int > 0) {
  Sys.Date() - data_window_int
} else {
  NULL
}

# Pick the later of data_window_cutoff and the actual data minimum so the
# date-range selector starts at a sensible value.
initial_date_start <- function(min_date) {
  if (!is.null(data_window_cutoff)) {
    max(min_date, data_window_cutoff)
  } else {
    min_date
  }
}

# Show a popup warning when a curated dataset directory does not exist yet,
# so users get actionable guidance. All missing datasets share one persistent
# notification -- re-shown with the same id, it updates in place with the
# growing list rather than stacking a popup per dataset.
notify_missing_dataset <- function(metric) {
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) {
    return(invisible(NULL))
  }
  missing <- session$userData$missing_datasets
  if (metric %in% missing) {
    return(invisible(NULL))
  }
  missing <- c(missing, metric)
  session$userData$missing_datasets <- missing
  shiny::showNotification(
    # overflow-wrap lets long unbroken data paths wrap instead of
    # overflowing the notification
    shiny::div(
      style = "overflow-wrap: anywhere;",
      paste0(
        "No curated data found for: ",
        paste0("'", missing, "'", collapse = ", "),
        ". Confirm that Chronicle data collection is enabled for Posit ",
        "Connect, that at least 30 hours have passed since collection ",
        "began, and that the data path ('",
        base_path,
        "') is correct."
      )
    ),
    duration = NULL,
    id = "chronicle-missing-datasets",
    type = "warning",
    session = session
  )
  invisible(NULL)
}

# Brand colors
BRAND_COLORS <- list(
  BLUE = "#447099",
  GREEN = "#72994E",
  BURGUNDY = "#9A4665",
  GRAY = "#404041"
)

# ==============================================
# Download UI Helper Functions
# ==============================================

download_icon <- bsicons::bs_icon("download")

# Card header with a single download button (for tables)
card_header_with_download <- function(
  title,
  download_id,
  subtitle_output = NULL
) {
  title_el <- if (is.null(subtitle_output)) {
    shiny::span(title)
  } else {
    shiny::div(
      shiny::span(title),
      shiny::span(
        style = "font-weight: normal; font-size: 0.9em; color: #555;",
        subtitle_output
      )
    )
  }

  bslib::card_header(
    shiny::div(
      style = "display: flex; justify-content: space-between; align-items: center; gap: 16px;", # nolint: line_length
      title_el,
      shiny::downloadLink(
        download_id,
        label = shiny::tagList(
          download_icon,
          shiny::span("Download CSV", class = "visually-hidden")
        ),
        style = "text-decoration: none; color: #555; font-size: 1.1em;"
      )
    )
  )
}

# Card header with a popover dropdown offering chart data + raw data downloads
card_header_with_chart_downloads <- function(
  title,
  chart_download_id,
  raw_download_id
) {
  bslib::card_header(
    shiny::div(
      style = "display: flex; justify-content: space-between; align-items: center; gap: 16px;", # nolint: line_length
      shiny::span(title),
      bslib::popover(
        shiny::actionLink(
          paste0(chart_download_id, "_trigger"),
          label = shiny::tagList(
            download_icon,
            shiny::span("Download CSV", class = "visually-hidden")
          ),
          style = "text-decoration: none; color: #555; font-size: 1.1em;"
        ),
        title = "Download CSV",
        shiny::div(
          shiny::downloadLink(
            chart_download_id,
            "Chart data (aggregated)",
            style = "display: block; margin-bottom: 8px;"
          ),
          shiny::downloadLink(
            raw_download_id,
            "Raw data (filtered)",
            style = "display: block;"
          )
        )
      )
    )
  )
}

# Build a downloadHandler that writes data_fn() as CSV. NULL data (no data
# source) becomes a fully empty file; a 0-row result keeps its columns so
# the CSV still has headers. Shared by the chart/table download links.
csv_download_handler <- function(data_fn, suffix) {
  shiny::downloadHandler(
    filename = function() {
      paste0("chronicle_connect_", suffix, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- data_fn()
      if (is.null(data)) {
        data <- data.frame()
      }
      utils::write.csv(data, file, row.names = FALSE)
    }
  )
}

# ==============================================
# Users - Overview UI/Server
# ==============================================

users_overview_ui <- bslib::card(
  bslib::card_header("Filters"),
  shiny::dateRangeInput(
    "users_overview_date_range",
    "Date Range:",
    start = NULL,
    end = NULL,
    format = "yyyy-mm-dd"
  ),
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    bslib::value_box(
      title = "Licensed Users",
      max_height = "120px",
      value = shiny::textOutput("users_licensed_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BLUE)
    ),
    bslib::value_box(
      title = "Daily Users",
      max_height = "120px",
      value = shiny::textOutput("users_daily_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$GREEN)
    ),
    bslib::value_box(
      title = "Publishers",
      max_height = "120px",
      value = shiny::textOutput("users_publishers_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BURGUNDY)
    )
  ),
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      card_header_with_chart_downloads(
        "User Trends Over Time",
        "download_user_trends_chart",
        "download_user_trends_raw"
      ),
      shinycssloaders::withSpinner(plotly::plotlyOutput("users_trend_plot"))
    ),
    bslib::card(
      card_header_with_chart_downloads(
        "Average Users by Day of Week",
        "download_user_dow_chart",
        "download_user_dow_raw"
      ),
      shinycssloaders::withSpinner(plotly::plotlyOutput("users_dow_plot"))
    )
  )
)

users_overview_server <- function(input, output, session, user_totals) {
  # Use shared user_totals data (error handling in main server)
  users_data <- user_totals

  date_range <- shiny::reactive({
    input$users_overview_date_range
  })

  # Set default date range on first data load only (skip on range expansion
  # reloads to preserve the user's current selection).
  date_init_done <- shiny::reactiveVal(FALSE)
  shiny::observe({
    shiny::req(users_data())
    if (date_init_done()) {
      return()
    }

    date_summary <- users_data() |>
      dplyr::filter(!is.na(date)) |>
      dplyr::summarise(
        min_date = min(date, na.rm = TRUE),
        max_date = max(date, na.rm = TRUE)
      )

    initial_start <- initial_date_start(date_summary$min_date)

    shiny::updateDateRangeInput(
      session,
      "users_overview_date_range",
      start = initial_start,
      end = date_summary$max_date,
      max = date_summary$max_date
    )
    date_init_done(TRUE)
  })

  # Get latest data (for value boxes - always max_date)
  latest_users_data <- shiny::reactive({
    data <- users_data()
    if (is.null(data)) {
      return(NULL)
    }

    max_date <- max(data$date, na.rm = TRUE)
    data |>
      dplyr::filter(date == max_date) |>
      dplyr::slice(1)
  })

  # Filter data by date range (for charts only)
  filtered_users_data <- shiny::reactive({
    data <- users_data()
    if (is.null(data)) {
      return(NULL)
    }

    shiny::req(date_range())

    data |>
      dplyr::filter(
        date >= date_range()[1],
        date <= date_range()[2]
      )
  })

  # Aggregated data for User Trends chart
  user_trends_chart_data <- shiny::reactive({
    data <- filtered_users_data()
    if (is.null(data)) {
      return(NULL)
    }

    data |>
      dplyr::select("date", "named_users", "active_users_1day", "publishers") |>
      dplyr::filter(!is.na(date)) |>
      tidyr::pivot_longer(-date, names_to = "metric", values_to = "value") |>
      dplyr::filter(!is.na(.data$value), is.finite(.data$value)) |>
      dplyr::arrange(date) |>
      dplyr::mutate(
        metric = factor(
          .data$metric,
          levels = c("named_users", "active_users_1day", "publishers"),
          labels = c("Licensed Users", "Daily Users", "Publishers")
        )
      )
  })

  # Aggregated data for Day of Week chart
  user_dow_chart_data <- shiny::reactive({
    data <- filtered_users_data()
    if (is.null(data)) {
      return(NULL)
    }

    data |>
      dplyr::mutate(
        day_of_week = lubridate::wday(date, label = TRUE, abbr = FALSE)
      ) |>
      dplyr::group_by(.data$day_of_week) |>
      dplyr::summarise(
        avg_active_users = mean(.data$active_users_1day, na.rm = TRUE),
        .groups = "drop"
      )
  })

  # Value boxes (always latest data)
  output$users_licensed_value <- shiny::renderText({
    data <- latest_users_data()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    prettyNum(data$named_users, big.mark = ",")
  })

  output$users_daily_value <- shiny::renderText({
    data <- latest_users_data()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    prettyNum(data$active_users_1day, big.mark = ",")
  })

  output$users_publishers_value <- shiny::renderText({
    data <- latest_users_data()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    prettyNum(data$publishers, big.mark = ",")
  })

  # Trend chart (filtered data)
  output$users_trend_plot <- plotly::renderPlotly({
    plot_data <- user_trends_chart_data()

    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE),
            annotations = list(
              list(
                text = "<b>No data available for selected date range</b>",
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 18, color = "#666666")
              )
            )
          )
      )
    }

    p <- suppressWarnings(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = date, y = .data$value, color = .data$metric)
      ) +
        ggplot2::geom_line(linewidth = 0.5) +
        ggplot2::geom_point(
          ggplot2::aes(
            text = paste0(
              format(date, "%B %d, %Y"),
              "<br>",
              prettyNum(.data$value, big.mark = ","),
              " ",
              .data$metric
            )
          ),
          size = 0.5
        ) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "", y = "Number of Users", color = "") +
        ggplot2::scale_color_manual(
          values = c(
            "Licensed Users" = BRAND_COLORS$BLUE,
            "Daily Users" = BRAND_COLORS$GREEN,
            "Publishers" = BRAND_COLORS$BURGUNDY
          )
        )
    )

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE),
        legend = list(orientation = "h", x = 0.5, xanchor = "center")
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  # Day of week chart (filtered data)
  output$users_dow_plot <- plotly::renderPlotly({
    day_summary <- user_dow_chart_data()

    if (is.null(day_summary) || nrow(day_summary) == 0) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE),
            annotations = list(
              list(
                text = "<b>No data available for selected date range</b>",
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 18, color = "#666666")
              )
            )
          )
      )
    }

    p <- ggplot2::ggplot(
      day_summary,
      ggplot2::aes(x = .data$day_of_week, y = .data$avg_active_users)
    ) +
      ggplot2::geom_col(fill = BRAND_COLORS$BLUE) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "", y = "Average Number of Users")

    plotly::ggplotly(p) |>
      plotly::layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  # Download handlers for User Trends and Day of Week charts
  output$download_user_trends_chart <- csv_download_handler(
    user_trends_chart_data,
    "user_trends_chart"
  )
  output$download_user_trends_raw <- csv_download_handler(
    filtered_users_data,
    "user_trends_raw"
  )
  output$download_user_dow_chart <- csv_download_handler(
    user_dow_chart_data,
    "user_dow_chart"
  )
  output$download_user_dow_raw <- csv_download_handler(
    filtered_users_data,
    "user_dow_raw"
  )
}

# ==============================================
# Users - User List UI/Server
# ==============================================

users_list_ui <- bslib::card(
  card_header_with_download(
    "Filters",
    "download_users_list",
    shiny::textOutput("users_list_as_of", inline = TRUE)
  ),
  bslib::layout_columns(
    col_widths = c(6, 6),
    shiny::selectInput(
      "users_list_environment",
      "Environment:",
      choices = c("All")
    ),
    shiny::selectInput(
      "users_list_role",
      "Role:",
      choices = c("All", "publisher", "viewer", "administrator")
    )
  ),
  shinycssloaders::withSpinner(
    DT::dataTableOutput("users_list_table")
  )
)

users_list_server <- function(input, output, session, user_list) {
  # Use shared user_list data (already latest snapshot from main server)
  users_list_data <- shiny::reactive({
    data <- user_list()
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    data
  })

  # Populate environment filter dynamically
  shiny::observe({
    data <- users_list_data()
    if (is.null(data) || nrow(data) == 0) {
      return()
    }

    # Get unique environment values
    env_values <- data |>
      dplyr::pull(environment) |>
      unique()

    # Check if there are any NAs or empty strings
    has_na <- any(is.na(env_values) | env_values == "" | env_values == " ")

    # Remove NAs, empty strings, and sort
    env_values <- env_values[
      !is.na(env_values) & env_values != "" & env_values != " "
    ] |>
      sort()

    # Add "(Not Set)" if there were any NAs or empty strings
    if (has_na) {
      env_values <- c(env_values, "(Not Set)")
    }

    # Update selectInput with "All" followed by sorted environment values
    shiny::updateSelectInput(
      session,
      "users_list_environment",
      choices = c("All", env_values)
    )
  })

  # Apply filters
  filtered_users_list <- shiny::reactive({
    data <- users_list_data()
    if (is.null(data)) {
      return(NULL)
    }

    # Environment filter
    if (input$users_list_environment != "All") {
      if (input$users_list_environment == "(Not Set)") {
        data <- data |>
          dplyr::filter(
            is.na(environment) |
              environment == "" |
              environment == " "
          )
      } else {
        data <- data |>
          dplyr::filter(environment == input$users_list_environment)
      }
    }

    # Role filter
    if (input$users_list_role != "All") {
      data <- data |> dplyr::filter(.data$user_role == input$users_list_role)
    }

    data
  })

  # "As of" label showing the latest snapshot date
  output$users_list_as_of <- shiny::renderText({
    data <- users_list_data()
    if (is.null(data) || nrow(data) == 0 || !"date" %in% names(data)) {
      return("")
    }

    latest_date <- max(data$date, na.rm = TRUE)
    paste0("Users as of ", format(latest_date, "%Y-%m-%d"))
  })

  # Render table
  output$users_list_table <- DT::renderDataTable({
    data <- filtered_users_list()

    if (is.null(data) || nrow(data) == 0) {
      # Return empty table with message
      return(
        DT::datatable(
          data.frame(
            " " = "Data not available - Check that Chronicle data exists at the configured path."
          ),
          options = list(
            dom = "t",
            ordering = FALSE,
            columnDefs = list(
              list(className = "dt-center", targets = "_all")
            )
          ),
          rownames = FALSE,
          colnames = ""
        )
      )
    }

    data |>
      dplyr::mutate(
        environment = ifelse(
          is.na(environment) |
            environment == "" |
            environment == " ",
          "(Not Set)",
          environment
        )
      ) |>
      dplyr::select(
        "username",
        "email",
        "first_name",
        "last_name",
        "environment",
        "user_role",
        "last_active_at"
      ) |>
      DT::datatable(
        options = list(
          pageLength = 25,
          autoWidth = TRUE,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
  })

  # Download handler for users list — same display columns as the table
  users_list_download_data <- shiny::reactive({
    data <- filtered_users_list()
    if (is.null(data)) {
      return(NULL)
    }
    data |>
      dplyr::mutate(
        environment = ifelse(
          is.na(environment) |
            environment == "" |
            environment == " ",
          "(Not Set)",
          environment
        )
      ) |>
      dplyr::select(
        "username",
        "email",
        "first_name",
        "last_name",
        "environment",
        "user_role",
        "last_active_at"
      )
  })
  output$download_users_list <- csv_download_handler(
    users_list_download_data,
    "users_list"
  )
}

# ==============================================
# Content - Overview UI/Server
# ==============================================

content_overview_ui <- bslib::card(
  bslib::card_header("Filters"),
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    shiny::selectInput(
      "content_overview_environment",
      "Environment:",
      choices = c("All")
    ),
    shiny::selectInput(
      "content_overview_type",
      "Type:",
      choices = c("All")
    ),
    shiny::dateRangeInput(
      "content_overview_date_range",
      "Date Range:",
      start = NULL,
      end = NULL,
      format = "yyyy-mm-dd"
    )
  ),
  bslib::card(
    card_header_with_chart_downloads(
      "Content Trends Over Time",
      "download_content_trends_chart",
      "download_content_trends_raw"
    ),
    shinycssloaders::withSpinner(plotly::plotlyOutput("content_trend_plot"))
  ),
  bslib::card(
    card_header_with_chart_downloads(
      "Content by Type",
      "download_content_type_chart",
      "download_content_type_raw"
    ),
    shinycssloaders::withSpinner(plotly::plotlyOutput("content_type_bar_plot"))
  )
)

content_overview_server <- function(input, output, session, content_totals) {
  # Use shared content_totals data (error handling in main server)
  contents_data <- content_totals

  date_range <- shiny::reactive({
    input$content_overview_date_range
  })

  # Populate environment filter dynamically based on curated data
  shiny::observe({
    data <- contents_data()
    if (is.null(data)) {
      return()
    }
    df <- data
    # Environment column is always `environment`
    env_values <- df |>
      dplyr::pull(environment) |>
      unique()

    has_na <- any(is.na(env_values) | env_values == "" | env_values == " ")
    env_values <- env_values[
      !is.na(env_values) & env_values != "" & env_values != " "
    ] |>
      sort()
    if (has_na) {
      env_values <- c(env_values, "(Not Set)")
    }

    shiny::updateSelectInput(
      session,
      "content_overview_environment",
      choices = c("All", env_values),
      selected = "All"
    )

    type_values <- df |>
      dplyr::pull("type") |>
      unique()

    has_type_na <- any(
      is.na(type_values) | type_values == "" | type_values == " "
    )
    type_values <- type_values[
      !is.na(type_values) & type_values != "" & type_values != " "
    ] |>
      sort()
    if (has_type_na) {
      type_values <- c(type_values, "(Not Set)")
    }

    shiny::updateSelectInput(
      session,
      "content_overview_type",
      choices = c("All", type_values),
      selected = "All"
    )
  })

  # Set default date range on first data load only (skip on range expansion
  # reloads to preserve the user's current selection).
  date_init_done <- shiny::reactiveVal(FALSE)
  shiny::observe({
    shiny::req(contents_data())
    if (date_init_done()) {
      return()
    }

    date_summary <- contents_data() |>
      dplyr::filter(!is.na(date)) |>
      dplyr::summarise(
        min_date = min(date, na.rm = TRUE),
        max_date = max(date, na.rm = TRUE)
      )

    initial_start <- initial_date_start(date_summary$min_date)

    shiny::updateDateRangeInput(
      session,
      "content_overview_date_range",
      start = initial_start,
      end = date_summary$max_date,
      max = date_summary$max_date
    )
    date_init_done(TRUE)
  })

  filtered_contents <- shiny::reactive({
    data <- contents_data()
    if (is.null(data)) {
      return(NULL)
    }

    df <- data |> dplyr::collect()

    # Environment filter
    if (input$content_overview_environment != "All") {
      if (input$content_overview_environment == "(Not Set)") {
        df <- df |>
          dplyr::filter(
            is.na(environment) |
              environment == "" |
              environment == " "
          )
      } else {
        df <- df |>
          dplyr::filter(environment == input$content_overview_environment)
      }
    }

    # Content Type filter
    if (input$content_overview_type != "All") {
      if (input$content_overview_type == "(Not Set)") {
        df <- df |>
          dplyr::filter(
            is.na(.data$type) | .data$type == "" | .data$type == " "
          )
      } else {
        df <- df |>
          dplyr::filter(.data$type == input$content_overview_type)
      }
    }

    df
  })

  filtered_contents_in_range <- shiny::reactive({
    df <- filtered_contents()
    if (is.null(df)) {
      return(NULL)
    }

    shiny::req(date_range())

    df |>
      dplyr::filter(
        date >= date_range()[1],
        date <= date_range()[2]
      )
  })

  # Aggregated data for Content Trends chart
  content_trends_chart_data <- shiny::reactive({
    df <- filtered_contents_in_range()
    if (is.null(df)) {
      return(NULL)
    }
    df |>
      dplyr::group_by(date) |>
      dplyr::summarise(
        total_content = sum(.data$count, na.rm = TRUE),
        .groups = "drop"
      )
  })

  # Aggregated data for Content by Type chart
  content_type_chart_data <- shiny::reactive({
    df <- filtered_contents_in_range()
    if (is.null(df)) {
      return(NULL)
    }
    latest_date <- suppressWarnings(max(df$date, na.rm = TRUE))
    df |>
      dplyr::filter(date == latest_date) |>
      dplyr::group_by(.data$type) |>
      dplyr::summarise(
        total = sum(.data$count, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::rename(content_type = "type") |>
      dplyr::arrange(.data$total) |>
      dplyr::mutate(
        content_type = factor(.data$content_type, levels = .data$content_type)
      )
  })

  # Trend chart (filtered by date range)
  output$content_trend_plot <- plotly::renderPlotly({
    total_by_date <- content_trends_chart_data()

    if (is.null(total_by_date) || nrow(total_by_date) == 0) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE),
            annotations = list(
              list(
                text = "<b>Data not available</b>",
                x = 0.5,
                y = 0.55,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 18, color = "#666666")
              ),
              list(
                text = "Check that Chronicle data exists at the configured path",
                x = 0.5,
                y = 0.45,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 14, color = "#666666")
              )
            )
          )
      )
    }

    # Only plot Total Content over time
    plot_data <- total_by_date |>
      dplyr::mutate(
        metric = factor("Total Content", levels = "Total Content")
      ) |>
      dplyr::rename(value = "total_content")

    if (nrow(plot_data) == 0) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE),
            annotations = list(
              list(
                text = "<b>No data available for selected date range</b>",
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 18, color = "#666666")
              )
            )
          )
      )
    }

    p <- suppressWarnings(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data$date, y = .data$value, color = .data$metric)
      ) +
        ggplot2::geom_line(linewidth = 0.5) +
        ggplot2::geom_point(
          ggplot2::aes(
            text = paste0(
              format(date, "%B %d, %Y"),
              "<br>",
              prettyNum(.data$value, big.mark = ","),
              " ",
              .data$metric
            )
          ),
          size = 0.5
        ) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "", y = "Content Items", color = "") +
        ggplot2::scale_color_manual(
          values = c("Total Content" = BRAND_COLORS$BLUE)
        )
    )

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE),
        legend = list(orientation = "h", x = 0.5, xanchor = "center")
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  # Bar chart: content counts by type over selected date range
  output$content_type_bar_plot <- plotly::renderPlotly({
    type_summary <- content_type_chart_data()

    if (is.null(type_summary) || nrow(type_summary) == 0) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE),
            annotations = list(
              list(
                text = "<b>Data not available</b>",
                x = 0.5,
                y = 0.55,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 18, color = "#666666")
              ),
              list(
                text = "Check that Chronicle data exists at the configured path",
                x = 0.5,
                y = 0.45,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 14, color = "#666666")
              )
            )
          )
      )
    }

    p <- ggplot2::ggplot(
      type_summary,
      ggplot2::aes(
        x = .data$content_type,
        y = .data$total
      )
    ) +
      ggplot2::geom_col(fill = BRAND_COLORS$GREEN) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "Content Type", y = "Count")

    plotly::ggplotly(p) |>
      plotly::layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  # Download handlers for Content Trends and Content by Type charts
  output$download_content_trends_chart <- csv_download_handler(
    content_trends_chart_data,
    "content_trends_chart"
  )
  output$download_content_trends_raw <- csv_download_handler(
    filtered_contents_in_range,
    "content_trends_raw"
  )
  output$download_content_type_chart <- csv_download_handler(
    content_type_chart_data,
    "content_type_chart"
  )
  output$download_content_type_raw <- csv_download_handler(
    filtered_contents_in_range,
    "content_type_raw"
  )
}

# ==============================================
# Content - Content List UI/Server
# ==============================================

content_list_ui <- bslib::card(
  card_header_with_download("Content List", "download_content_list"),
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    shiny::selectInput(
      "content_list_environment",
      "Environment:",
      choices = c("All")
    ),
    shiny::selectInput(
      "content_list_owner",
      "Owner:",
      choices = c("All")
    ),
    shiny::selectInput(
      "content_list_type",
      "Type:",
      choices = c("All")
    )
  ),
  shinycssloaders::withSpinner(
    DT::dataTableOutput("content_list_table")
  )
)

content_list_server <- function(
  input,
  output,
  session,
  user_list,
  content_list
) {
  # Use shared content_list data (already latest snapshot from main server)
  content_list_data <- shiny::reactive({
    df <- content_list()
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    df
  })

  # Use shared user_list (already latest snapshot from main server)
  latest_user_list <- shiny::reactive({
    udf <- user_list()
    if (is.null(udf) || nrow(udf) == 0) {
      return(NULL)
    }
    udf
  })

  # Populate owner and type filters dynamically
  shiny::observe({
    data <- content_list_data()
    if (is.null(data) || nrow(data) == 0) {
      shiny::updateSelectInput(
        session,
        "content_list_environment",
        choices = c("All"),
        selected = "All"
      )
      shiny::updateSelectInput(
        session,
        "content_list_owner",
        choices = c("All"),
        selected = "All"
      )
      shiny::updateSelectInput(
        session,
        "content_list_type",
        choices = c("All"),
        selected = "All"
      )
      return()
    }

    df <- data

    # Environment choices (environment column is guaranteed)
    env_values <- df |>
      dplyr::pull(environment) |>
      unique()

    has_env_na <- any(is.na(env_values) | env_values == "" | env_values == " ")
    env_values <- env_values[
      !is.na(env_values) & env_values != "" & env_values != " "
    ] |>
      sort()
    if (has_env_na) {
      env_values <- c(env_values, "(Not Set)")
    }

    shiny::updateSelectInput(
      session,
      "content_list_environment",
      choices = c("All", env_values),
      selected = "All"
    )

    # Resolve owner names by joining latest user list on owner id
    owners_choices <- c("All")
    ulist <- latest_user_list()
    if (!is.null(ulist) && nrow(ulist) > 0 && "owner_guid" %in% names(df)) {
      owners <- df |>
        dplyr::left_join(
          ulist |>
            dplyr::select("id", "username") |>
            dplyr::rename(owner_guid = "id", owner = "username"),
          by = "owner_guid"
        ) |>
        dplyr::pull(.data$owner) |>
        unique()

      has_na <- any(is.na(owners) | owners == "" | owners == " ")
      owners <- owners[!is.na(owners) & owners != "" & owners != " "] |>
        sort()
      if (has_na) {
        owners <- c(owners, "(Not Set)")
      }
      owners_choices <- c("All", owners)
    }
    shiny::updateSelectInput(
      session,
      "content_list_owner",
      choices = owners_choices,
      selected = "All"
    )

    # Populate type choices
    types <- df |>
      dplyr::pull("type") |>
      unique()
    has_na <- any(is.na(types) | types == "" | types == " ")
    types <- types[!is.na(types) & types != "" & types != " "] |> sort()
    if (has_na) {
      types <- c(types, "(Not Set)")
    }
    shiny::updateSelectInput(
      session,
      "content_list_type",
      choices = c("All", types),
      selected = "All"
    )
  })

  # Apply filters
  filtered_content_list <- shiny::reactive({
    data <- content_list_data()
    if (is.null(data)) {
      return(NULL)
    }

    df <- data

    # Environment filter (environment column is guaranteed)
    if (input$content_list_environment != "All") {
      if (input$content_list_environment == "(Not Set)") {
        df <- df |>
          dplyr::filter(
            is.na(environment) |
              environment == "" |
              environment == " "
          )
      } else {
        df <- df |>
          dplyr::filter(environment == input$content_list_environment)
      }
    }

    # Join owner display for filtering, using latest user list
    ulist <- latest_user_list()
    if (!is.null(ulist) && nrow(ulist) > 0 && "owner_guid" %in% names(df)) {
      owner_lookup <- ulist |>
        dplyr::select("id", "username") |>
        dplyr::rename(owner_guid = "id", owner = "username")

      df <- df |>
        dplyr::left_join(owner_lookup, by = "owner_guid")
    }

    # Owner filter
    if ("owner" %in% names(df) && input$content_list_owner != "All") {
      if (input$content_list_owner == "(Not Set)") {
        df <- df |>
          dplyr::filter(
            is.na(.data$owner) | .data$owner == "" | .data$owner == " "
          )
      } else {
        df <- df |> dplyr::filter(.data$owner == input$content_list_owner)
      }
    }

    # Type filter
    if (input$content_list_type != "All") {
      if (input$content_list_type == "(Not Set)") {
        df <- df |>
          dplyr::filter(
            is.na(.data$type) |
              .data$type == "" |
              .data$type == " "
          )
      } else {
        df <- df |>
          dplyr::filter(.data$type == input$content_list_type)
      }
    }

    df
  })

  # Render table
  output$content_list_table <- DT::renderDataTable({
    data <- filtered_content_list()
    if (is.null(data) || nrow(data) == 0) {
      return(
        DT::datatable(
          data.frame(
            " " = "Data not available - Check that Chronicle data exists at the configured path."
          ),
          options = list(
            dom = "t",
            ordering = FALSE,
            columnDefs = list(list(className = "dt-center", targets = "_all"))
          ),
          rownames = FALSE,
          colnames = ""
        )
      )
    }

    df <- data
    cols <- c(
      "title",
      "owner",
      "type",
      "environment",
      "py_version",
      "r_version",
      "quarto_version",
      "last_deployed_time"
    )

    DT::datatable(
      df[, cols, drop = FALSE],
      options = list(
        pageLength = 25,
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })

  # Download handler for content list — same display columns as the table
  content_list_download_data <- shiny::reactive({
    data <- filtered_content_list()
    if (is.null(data)) {
      return(NULL)
    }
    cols <- c(
      "title",
      "owner",
      "type",
      "environment",
      "py_version",
      "r_version",
      "quarto_version",
      "last_deployed_time"
    )
    data[, cols, drop = FALSE]
  })
  output$download_content_list <- csv_download_handler(
    content_list_download_data,
    "content_list"
  )
}

# ==============================================
# Usage - Content Hits Overview UI/Server
# ==============================================

content_hits_overview_ui <- bslib::card(
  bslib::card_header("Content Hits"),
  bslib::layout_columns(
    col_widths = c(4, 8),
    shiny::selectInput(
      "content_hits_overview_environment",
      "Environment:",
      choices = c("All")
    ),
    shiny::dateRangeInput(
      "content_hits_overview_date_range",
      "Date Range:",
      start = Sys.Date() - 90,
      end = Sys.Date(),
      format = "yyyy-mm-dd"
    )
  ),
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    bslib::value_box(
      title = "Total Hits",
      max_height = "120px",
      value = shiny::textOutput("content_hits_total_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$GREEN)
    ),
    bslib::value_box(
      title = "Unique Users",
      max_height = "120px",
      value = shiny::textOutput("content_hits_unique_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BLUE)
    ),
    bslib::value_box(
      title = "Avg Daily Hits",
      max_height = "120px",
      value = shiny::textOutput("content_hits_avg_daily_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BURGUNDY)
    )
  ),
  bslib::card(
    card_header_with_chart_downloads(
      "Total Hits by Day",
      "download_content_hits_chart",
      "download_content_hits_raw"
    ),
    shinycssloaders::withSpinner(plotly::plotlyOutput(
      "content_hits_total_plot"
    ))
  ),
  bslib::card(
    card_header_with_chart_downloads(
      "Unique Users by Day",
      "download_content_hits_unique_chart",
      "download_content_hits_unique_raw"
    ),
    shinycssloaders::withSpinner(plotly::plotlyOutput(
      "content_hits_unique_plot"
    ))
  )
)

content_hits_overview_server <- function(
  input,
  output,
  session,
  content_hits_by_user
) {
  hits_data <- content_hits_by_user

  date_range <- shiny::reactive({
    input$content_hits_overview_date_range
  })

  # Populate environment filter dynamically
  shiny::observe({
    data <- hits_data()
    if (is.null(data)) {
      shiny::updateSelectInput(
        session,
        "content_hits_overview_environment",
        choices = c("All"),
        selected = "All"
      )
      return()
    }

    df <- data |> dplyr::collect()
    if (!"environment" %in% names(df) || nrow(df) == 0) {
      shiny::updateSelectInput(
        session,
        "content_hits_overview_environment",
        choices = c("All"),
        selected = "All"
      )
      return()
    }

    env_values <- df |>
      dplyr::pull(environment) |>
      unique()

    has_env_na <- any(is.na(env_values) | env_values == "" | env_values == " ")
    env_values <- env_values[
      !is.na(env_values) & env_values != "" & env_values != " "
    ] |>
      sort()
    if (has_env_na) {
      env_values <- c(env_values, "(Not Set)")
    }

    shiny::updateSelectInput(
      session,
      "content_hits_overview_environment",
      choices = c("All", env_values),
      selected = "All"
    )
  })

  # Set default date range on first data load only (skip on range expansion
  # reloads to preserve the user's current selection).
  date_init_done <- shiny::reactiveVal(FALSE)
  shiny::observe({
    data <- hits_data()
    if (is.null(data)) {
      return()
    }
    if (date_init_done()) {
      return()
    }

    date_summary <- data |>
      dplyr::filter(!is.na(date)) |>
      dplyr::summarise(
        min_date = min(date, na.rm = TRUE),
        max_date = max(date, na.rm = TRUE)
      )

    if (nrow(date_summary) == 0) {
      return()
    }

    initial_start <- initial_date_start(date_summary$min_date)

    shiny::updateDateRangeInput(
      session,
      "content_hits_overview_date_range",
      start = initial_start,
      end = date_summary$max_date,
      max = date_summary$max_date
    )
    date_init_done(TRUE)
  })

  hits_filtered <- shiny::reactive({
    data <- hits_data()
    if (is.null(data)) {
      return(NULL)
    }

    df <- data |> dplyr::collect()

    # Environment filter
    if (
      "environment" %in%
        names(df) &&
        input$content_hits_overview_environment != "All"
    ) {
      if (input$content_hits_overview_environment == "(Not Set)") {
        df <- df |>
          dplyr::filter(
            is.na(environment) |
              environment == "" |
              environment == " "
          )
      } else {
        df <- df |>
          dplyr::filter(
            environment == input$content_hits_overview_environment
          )
      }
    }

    shiny::req(date_range())

    df |>
      dplyr::filter(
        date >= date_range()[1],
        date <= date_range()[2]
      ) |>
      dplyr::collect()
  })

  # Aggregated data for Total Hits chart
  hits_chart_data <- shiny::reactive({
    df <- hits_filtered()
    if (is.null(df) || !"hits" %in% names(df)) {
      return(NULL)
    }
    df |>
      dplyr::group_by(date) |>
      dplyr::summarise(
        total_hits = sum(.data$hits, na.rm = TRUE),
        .groups = "drop"
      )
  })

  # Aggregated data for Unique Users chart
  unique_users_chart_data <- shiny::reactive({
    df <- hits_filtered()
    if (is.null(df) || !"user_guid" %in% names(df)) {
      return(NULL)
    }
    df |>
      dplyr::group_by(date) |>
      dplyr::summarise(
        unique_users = dplyr::n_distinct(.data$user_guid, na.rm = TRUE),
        .groups = "drop"
      )
  })

  output$content_hits_total_value <- shiny::renderText({
    df <- hits_filtered()

    if (is.null(df) || nrow(df) == 0 || !"hits" %in% names(df)) {
      return("0")
    }

    total_hits <- sum(df$hits, na.rm = TRUE)
    prettyNum(total_hits, big.mark = ",")
  })

  output$content_hits_unique_value <- shiny::renderText({
    df <- hits_filtered()

    if (is.null(df) || nrow(df) == 0 || !"user_guid" %in% names(df)) {
      return("0")
    }

    unique_users <- dplyr::n_distinct(df$user_guid, na.rm = TRUE)
    prettyNum(unique_users, big.mark = ",")
  })

  output$content_hits_avg_daily_value <- shiny::renderText({
    df <- hits_filtered()

    if (is.null(df) || nrow(df) == 0 || !"hits" %in% names(df)) {
      return("0")
    }

    shiny::req(date_range())

    total_hits <- sum(df$hits, na.rm = TRUE)
    num_days <- as.numeric(
      date_range()[2] - date_range()[1]
    ) +
      1

    if (num_days <= 0) {
      return("0")
    }

    avg_daily <- total_hits / num_days
    prettyNum(round(avg_daily), big.mark = ",")
  })

  output$content_hits_total_plot <- plotly::renderPlotly({
    daily <- hits_chart_data()

    if (is.null(daily) || nrow(daily) == 0) {
      return(plotly::plotly_empty(type = "scatter", mode = "markers"))
    }

    p <- ggplot2::ggplot(
      daily,
      ggplot2::aes(x = date, y = total_hits)
    ) +
      ggplot2::geom_line(linewidth = 0.5, color = BRAND_COLORS$GREEN) +
      ggplot2::geom_point(size = 0.5, color = BRAND_COLORS$GREEN) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "", y = "Total Hits")

    plotly::ggplotly(p) |>
      plotly::layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  output$content_hits_unique_plot <- plotly::renderPlotly({
    daily <- unique_users_chart_data()

    if (is.null(daily) || nrow(daily) == 0) {
      return(plotly::plotly_empty(type = "scatter", mode = "markers"))
    }

    p <- ggplot2::ggplot(
      daily,
      ggplot2::aes(x = date, y = unique_users)
    ) +
      ggplot2::geom_line(linewidth = 0.5, color = BRAND_COLORS$BLUE) +
      ggplot2::geom_point(size = 0.5, color = BRAND_COLORS$BLUE) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "", y = "Unique Users")

    plotly::ggplotly(p) |>
      plotly::layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  # Download handlers for Total Hits and Unique Users charts
  output$download_content_hits_chart <- csv_download_handler(
    hits_chart_data,
    "content_hits_chart"
  )
  output$download_content_hits_raw <- csv_download_handler(
    hits_filtered,
    "content_hits_raw"
  )
  output$download_content_hits_unique_chart <- csv_download_handler(
    unique_users_chart_data,
    "content_hits_unique_chart"
  )
  output$download_content_hits_unique_raw <- csv_download_handler(
    hits_filtered,
    "content_hits_unique_raw"
  )
}

# ==============================================
# Usage - Content Hits by User UI/Server
# ==============================================

content_hits_by_user_ui <- bslib::card(
  card_header_with_download(
    "Content Hits by User",
    "download_content_hits_by_user"
  ),
  bslib::layout_columns(
    col_widths = c(4, 8),
    shiny::selectInput(
      "content_hits_by_user_environment",
      "Environment:",
      choices = c("All")
    ),
    shiny::dateRangeInput(
      "content_hits_by_user_date_range",
      "Date Range:",
      start = Sys.Date() - 90,
      end = Sys.Date(),
      format = "yyyy-mm-dd"
    )
  ),
  shinycssloaders::withSpinner(
    DT::dataTableOutput("content_hits_by_user_table")
  )
)

content_hits_by_user_server <- function(
  input,
  output,
  session,
  content_hits_by_user,
  content_list,
  user_list
) {
  hits_data <- content_hits_by_user

  date_range <- shiny::reactive({
    input$content_hits_by_user_date_range
  })

  # Use shared content_list (already latest snapshot from main server)
  content_list_latest <- shiny::reactive({
    df <- content_list()
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    df
  })

  # Use shared user_list (already latest snapshot from main server)
  user_list_latest <- shiny::reactive({
    df <- user_list()
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    df
  })

  # Set default date range on first data load only (skip on range expansion
  # reloads to preserve the user's current selection).
  date_init_done <- shiny::reactiveVal(FALSE)
  shiny::observe({
    data <- hits_data()
    if (is.null(data)) {
      shiny::updateSelectInput(
        session,
        "content_hits_by_user_environment",
        choices = c("All"),
        selected = "All"
      )
      return()
    }

    df <- data |> dplyr::collect()
    if (!"date" %in% names(df) || nrow(df) == 0) {
      shiny::updateSelectInput(
        session,
        "content_hits_by_user_environment",
        choices = c("All"),
        selected = "All"
      )
      return()
    }

    # Environment choices
    env_values <- df |>
      dplyr::pull(environment) |>
      unique()

    has_env_na <- any(is.na(env_values) | env_values == "" | env_values == " ")
    env_values <- env_values[
      !is.na(env_values) & env_values != "" & env_values != " "
    ] |>
      sort()
    if (has_env_na) {
      env_values <- c(env_values, "(Not Set)")
    }

    shiny::updateSelectInput(
      session,
      "content_hits_by_user_environment",
      choices = c("All", env_values),
      selected = "All"
    )

    # Date range — only set on first load
    if (!date_init_done()) {
      date_summary <- df |>
        dplyr::filter(!is.na(date)) |>
        dplyr::summarise(
          min_date = min(date, na.rm = TRUE),
          max_date = max(date, na.rm = TRUE)
        )

      if (nrow(date_summary) > 0) {
        initial_start <- initial_date_start(date_summary$min_date)

        shiny::updateDateRangeInput(
          session,
          "content_hits_by_user_date_range",
          start = initial_start,
          end = date_summary$max_date,
          max = date_summary$max_date
        )
        date_init_done(TRUE)
      }
    }
  })

  hits_filtered <- shiny::reactive({
    data <- hits_data()
    if (is.null(data)) {
      return(NULL)
    }

    df <- data |> dplyr::collect()

    # Environment filter
    if (
      "environment" %in%
        names(df) &&
        input$content_hits_by_user_environment != "All"
    ) {
      if (input$content_hits_by_user_environment == "(Not Set)") {
        df <- df |>
          dplyr::filter(
            is.na(environment) |
              environment == "" |
              environment == " "
          )
      } else {
        df <- df |>
          dplyr::filter(
            environment == input$content_hits_by_user_environment
          )
      }
    }

    shiny::req(date_range())

    df |>
      dplyr::filter(
        date >= date_range()[1],
        date <= date_range()[2]
      )
  })

  # Aggregated data for Content Hits by User table
  content_hits_by_user_table_data <- shiny::reactive({
    df <- hits_filtered()
    if (
      is.null(df) ||
        nrow(df) == 0 ||
        !"content_guid" %in% names(df) ||
        !"user_guid" %in% names(df) ||
        !"hits" %in% names(df)
    ) {
      return(NULL)
    }

    summary_df <- df |>
      dplyr::group_by(.data$environment, .data$user_guid, .data$content_guid) |>
      dplyr::summarise(
        total_hits = sum(.data$hits, na.rm = TRUE),
        .groups = "drop"
      )

    # Join usernames
    u_df <- user_list_latest()
    if (!is.null(u_df) && all(c("id", "username") %in% names(u_df))) {
      user_join <- u_df |> dplyr::select("id", "username")
      summary_df <- summary_df |>
        dplyr::left_join(user_join, by = c("user_guid" = "id"))
    }

    # Join content titles
    c_df <- content_list_latest()
    if (
      !is.null(c_df) && all(c("id", "environment", "title") %in% names(c_df))
    ) {
      content_join <- c_df |>
        dplyr::select("id", "environment", "title")
      summary_df <- summary_df |>
        dplyr::left_join(
          content_join,
          by = c("content_guid" = "id", "environment" = "environment")
        )
    }

    summary_df |>
      dplyr::mutate(
        username = ifelse(
          is.na(.data$user_guid) | is.na(.data$username),
          "(anonymous)",
          .data$username
        ),
        environment = ifelse(
          is.na(environment) |
            environment == "" |
            environment == " ",
          "(Not Set)",
          environment
        )
      )
  })

  output$content_hits_by_user_table <- DT::renderDataTable({
    display_df <- content_hits_by_user_table_data()

    if (is.null(display_df) || nrow(display_df) == 0) {
      return(
        DT::datatable(
          data.frame(
            " " = "Data not available - Check that Chronicle data exists at the configured path." # nolint: line_length
          ),
          options = list(
            dom = "t",
            ordering = FALSE,
            columnDefs = list(list(className = "dt-center", targets = "_all"))
          ),
          rownames = FALSE,
          colnames = ""
        )
      )
    }

    cols <- c("username", "title", "environment", "total_hits")

    DT::datatable(
      display_df[, cols, drop = FALSE],
      options = list(
        pageLength = 25,
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })

  # Download handler for Content Hits by User table — same display columns
  # as the table
  content_hits_by_user_download_data <- shiny::reactive({
    data <- content_hits_by_user_table_data()
    if (is.null(data)) {
      return(NULL)
    }
    cols <- c("username", "title", "environment", "total_hits")
    data[, cols, drop = FALSE]
  })
  output$download_content_hits_by_user <- csv_download_handler(
    content_hits_by_user_download_data,
    "content_hits_by_user"
  )
}

# ==============================================
# Main UI (page_navbar with three dropdowns)
# ==============================================

ui <- bslib::page_navbar(
  id = "main_nav",
  title = "Posit Connect Dashboard",
  theme = bslib::bs_theme(preset = "shiny"),
  fillable = FALSE,

  # Users dropdown
  bslib::nav_menu(
    "Users",
    bslib::nav_panel("Overview", users_overview_ui, value = "users_overview"),
    bslib::nav_panel("User List", users_list_ui, value = "users_list")
  ),

  # Content dropdown
  bslib::nav_menu(
    "Content",
    bslib::nav_panel(
      "Overview",
      content_overview_ui,
      value = "content_overview"
    ),
    bslib::nav_panel("Content List", content_list_ui, value = "content_list")
  ),

  # Usage dropdown
  bslib::nav_menu(
    "Usage",
    bslib::nav_panel(
      "Content Hits",
      content_hits_overview_ui,
      value = "content_hits_overview"
    ),
    bslib::nav_panel(
      "Content Hits by User",
      content_hits_by_user_ui,
      value = "content_hits_by_user"
    )
  )
)

# ==============================================
# Main Server
# ==============================================

server <- function(input, output, session) {
  # ============================================
  # Tab tracking for deferred data loading
  # ============================================
  # Data only loads when the user first visits a tab that needs it.
  # Once loaded, Shiny's reactive() caches the result — no re-fetching
  # on subsequent tab switches.
  visited_tabs <- shiny::reactiveValues()
  shiny::observeEvent(input$main_nav, {
    visited_tabs[[input$main_nav]] <- TRUE
  })

  # ============================================
  # Connect Data - load each dataset on demand
  # ============================================
  # Datasets are collected eagerly (as data frames) so sub-servers never
  # touch Arrow directly. When CHRONICLE_DATA_WINDOW is set, only the
  # last N days are loaded initially. When a date selector extends beyond
  # the loaded range, the range expands to cover the new dates (Arrow
  # partition pruning means only relevant partitions are read).

  # Helper: initial loaded range based on the data window env var.
  # NULL means no restriction (load everything).
  initial_range <- if (!is.null(data_window_cutoff)) {
    list(min = data_window_cutoff, max = Sys.Date())
  }

  # --- user_totals: Always loaded (default visible tab) ---
  user_totals_range <- shiny::reactiveVal(initial_range)
  all_user_totals <- shiny::reactive({
    range <- user_totals_range()
    tryCatch(
      {
        ds <- chronicle_data("connect/user_totals", base_path)
        if (is.null(ds)) {
          notify_missing_dataset("connect/user_totals")
          return(NULL)
        }
        if (!is.null(range)) {
          range_min <- range$min
          range_max <- range$max
          ds <- ds |> dplyr::filter(date >= range_min, date <= range_max)
        }
        ds |> dplyr::collect()
      },
      error = function(e) {
        message("Error loading user totals: ", e$message)
        NULL
      }
    )
  })

  # user_list: Deferred until a tab that needs it is visited.
  # Collects latest snapshot only (one date partition, not all dates).
  # Use a sticky flag so the reactive only loads once, even when
  # additional tabs in the list are visited later.
  should_load_user_list <- shiny::reactiveVal(FALSE)
  shiny::observe({
    if (
      !should_load_user_list() &&
        (isTRUE(visited_tabs[["users_list"]]) ||
          isTRUE(visited_tabs[["content_list"]]) ||
          isTRUE(visited_tabs[["content_hits_by_user"]]))
    ) {
      should_load_user_list(TRUE)
    }
  })
  all_user_list <- shiny::reactive({
    shiny::req(should_load_user_list())
    tryCatch(
      {
        ds <- chronicle_data("connect/user_list", base_path)
        if (is.null(ds)) {
          notify_missing_dataset("connect/user_list")
          return(NULL)
        }
        max_date <- ds |>
          dplyr::summarise(max_date = max(date, na.rm = TRUE)) |>
          dplyr::collect() |>
          dplyr::pull(max_date)
        ds |>
          dplyr::filter(date == max_date) |>
          dplyr::collect()
      },
      error = function(e) {
        message("Error loading user list: ", e$message)
        NULL
      }
    )
  })

  # --- content_totals: Deferred until Content Overview visited ---
  content_totals_range <- shiny::reactiveVal(initial_range)
  all_content_totals <- shiny::reactive({
    shiny::req(isTRUE(visited_tabs[["content_overview"]]))
    range <- content_totals_range()
    tryCatch(
      {
        ds <- chronicle_data("connect/content_totals", base_path)
        if (is.null(ds)) {
          notify_missing_dataset("connect/content_totals")
          return(NULL)
        }
        if (!is.null(range)) {
          range_min <- range$min
          range_max <- range$max
          ds <- ds |> dplyr::filter(date >= range_min, date <= range_max)
        }
        ds |> dplyr::collect()
      },
      error = function(e) {
        message("Error loading content totals: ", e$message)
        NULL
      }
    )
  })

  # content_list: Deferred until tabs that need it are visited.
  # Collects latest snapshot only (one date partition, not all dates).
  should_load_content_list <- shiny::reactiveVal(FALSE)
  shiny::observe({
    if (
      !should_load_content_list() &&
        (isTRUE(visited_tabs[["content_list"]]) ||
          isTRUE(visited_tabs[["content_hits_by_user"]]))
    ) {
      should_load_content_list(TRUE)
    }
  })
  all_content_list <- shiny::reactive({
    shiny::req(should_load_content_list())
    tryCatch(
      {
        ds <- chronicle_data("connect/content_list", base_path)
        if (is.null(ds)) {
          notify_missing_dataset("connect/content_list")
          return(NULL)
        }
        max_date <- ds |>
          dplyr::summarise(max_date = max(date, na.rm = TRUE)) |>
          dplyr::collect() |>
          dplyr::pull(max_date)
        ds |>
          dplyr::filter(date == max_date) |>
          dplyr::collect()
      },
      error = function(e) {
        message("Error loading content list: ", e$message)
        NULL
      }
    )
  })

  # --- content_hits: Deferred — LARGE dataset ---
  should_load_content_hits <- shiny::reactiveVal(FALSE)
  shiny::observe({
    if (
      !should_load_content_hits() &&
        (isTRUE(visited_tabs[["content_hits_overview"]]) ||
          isTRUE(visited_tabs[["content_hits_by_user"]]))
    ) {
      should_load_content_hits(TRUE)
    }
  })
  content_hits_range <- shiny::reactiveVal(initial_range)
  all_content_hits_by_user <- shiny::reactive({
    shiny::req(should_load_content_hits())
    range <- content_hits_range()
    tryCatch(
      {
        ds <- chronicle_data(
          "connect/content_hits_totals_by_user",
          base_path
        )
        if (is.null(ds)) {
          notify_missing_dataset("connect/content_hits_totals_by_user")
          return(NULL)
        }
        if (!is.null(range)) {
          range_min <- range$min
          range_max <- range$max
          ds <- ds |> dplyr::filter(date >= range_min, date <= range_max)
        }
        ds |> dplyr::collect()
      },
      error = function(e) {
        message("Error loading content hits: ", e$message)
        NULL
      }
    )
  })

  # ============================================
  # Load-more observers: when a date selector extends beyond the loaded
  # range, expand the range to cover the new dates. Only the additional
  # data is fetched (Arrow partition pruning), not the full dataset.
  # ============================================
  load_more_observer <- function(range_val, input_id) {
    shiny::observe({
      date_val <- input[[input_id]]
      shiny::req(date_val)
      range <- range_val()
      if (is.null(range)) {
        return()
      }
      new_min <- min(range$min, date_val[1])
      new_max <- max(range$max, date_val[2])
      if (new_min < range$min || new_max > range$max) {
        range_val(list(min = new_min, max = new_max))
      }
    })
  }
  load_more_observer(user_totals_range, "users_overview_date_range")
  load_more_observer(content_totals_range, "content_overview_date_range")
  load_more_observer(content_hits_range, "content_hits_overview_date_range")
  load_more_observer(content_hits_range, "content_hits_by_user_date_range")

  # ============================================
  # Call sub-servers with data
  # ============================================
  users_overview_server(input, output, session, all_user_totals)
  users_list_server(input, output, session, all_user_list)
  content_overview_server(input, output, session, all_content_totals)
  content_list_server(input, output, session, all_user_list, all_content_list)
  content_hits_overview_server(
    input,
    output,
    session,
    all_content_hits_by_user
  )
  content_hits_by_user_server(
    input,
    output,
    session,
    all_content_hits_by_user,
    all_content_list,
    all_user_list
  )
}

shinyApp(ui, server)
