# Posit Workbench Dashboard
# Comprehensive dashboard providing analytics for Posit Workbench Users

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

# Brand colors
BRAND_COLORS <- list(
  BLUE = "#447099",
  GREEN = "#72994E",
  BURGUNDY = "#9A4665",
  GRAY = "#404041"
)

# Ordered palette used for session-type series in Sessions charts.
SESSION_PALETTE <- c(
  BRAND_COLORS$BLUE,
  BRAND_COLORS$GREEN,
  BRAND_COLORS$BURGUNDY,
  BRAND_COLORS$GRAY
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

# ==============================================
# Users → Overview UI/Server
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
    col_widths = c(3, 3, 3, 3),
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
      title = "Admins",
      max_height = "120px",
      value = shiny::textOutput("users_admins_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BURGUNDY)
    ),
    bslib::value_box(
      title = "Super Admins",
      max_height = "120px",
      value = shiny::textOutput("users_super_admins_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$GRAY)
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

users_overview_server <- function(input, output, session) {
  # Load user_totals data — collected eagerly with optional window filter.
  # When a date selector extends beyond the loaded range, the range expands
  # to cover the new dates (only the additional data is fetched via Arrow
  # partition pruning, not the full dataset).
  # NULL range means no restriction (load everything).
  initial_range <- if (!is.null(data_window_cutoff)) {
    list(min = data_window_cutoff, max = Sys.Date())
  }
  user_totals_range <- shiny::reactiveVal(initial_range)

  users_data <- shiny::reactive({
    range <- user_totals_range()
    tryCatch(
      {
        ds <- chronicle_data("workbench/user_totals", base_path)
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

  # Expand loaded range when date selector extends beyond the current range
  shiny::observe({
    date_val <- input$users_overview_date_range
    shiny::req(date_val)
    range <- user_totals_range()
    if (is.null(range)) {
      return()
    }
    new_min <- min(range$min, date_val[1])
    new_max <- max(range$max, date_val[2])
    if (new_min < range$min || new_max > range$max) {
      user_totals_range(list(min = new_min, max = new_max))
    }
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

    shiny::req(input$users_overview_date_range)

    data |>
      dplyr::filter(
        date >= input$users_overview_date_range[1],
        date <= input$users_overview_date_range[2]
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

  output$users_admins_value <- shiny::renderText({
    data <- latest_users_data()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    prettyNum(data$administrators, big.mark = ",")
  })

  output$users_super_admins_value <- shiny::renderText({
    data <- latest_users_data()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    prettyNum(data$super_administrators, big.mark = ",")
  })

  # Aggregated data for User Trends chart
  user_trends_chart_data <- shiny::reactive({
    data <- filtered_users_data()
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }

    data |>
      dplyr::select(
        "date",
        "named_users",
        "active_users_1day",
        "administrators",
        "super_administrators"
      ) |>
      dplyr::filter(!is.na(date)) |>
      tidyr::pivot_longer(-date, names_to = "metric", values_to = "value") |>
      dplyr::filter(!is.na(.data$value), is.finite(.data$value)) |>
      dplyr::arrange(date) |>
      dplyr::mutate(
        metric = factor(
          .data$metric,
          levels = c(
            "named_users",
            "active_users_1day",
            "administrators",
            "super_administrators"
          ),
          labels = c("Licensed Users", "Daily Users", "Admins", "Super Admins")
        )
      )
  })

  # Aggregated data for Day of Week chart
  user_dow_chart_data <- shiny::reactive({
    data <- filtered_users_data()
    if (is.null(data) || nrow(data) == 0) {
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
            "Admins" = BRAND_COLORS$BURGUNDY,
            "Super Admins" = BRAND_COLORS$GRAY
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

  # Download handlers for User Trends chart
  output$download_user_trends_chart <- shiny::downloadHandler(
    filename = function() {
      paste0("chronicle_workbench_user_trends_chart_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- user_trends_chart_data()
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame()
      }
      utils::write.csv(data, file, row.names = FALSE)
    }
  )

  output$download_user_trends_raw <- shiny::downloadHandler(
    filename = function() {
      paste0("chronicle_workbench_user_trends_raw_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- filtered_users_data()
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame()
      }
      utils::write.csv(data, file, row.names = FALSE)
    }
  )

  # Download handlers for Day of Week chart
  output$download_user_dow_chart <- shiny::downloadHandler(
    filename = function() {
      paste0("chronicle_workbench_user_dow_chart_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- user_dow_chart_data()
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame()
      }
      utils::write.csv(data, file, row.names = FALSE)
    }
  )

  output$download_user_dow_raw <- shiny::downloadHandler(
    filename = function() {
      paste0("chronicle_workbench_user_dow_raw_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- filtered_users_data()
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame()
      }
      utils::write.csv(data, file, row.names = FALSE)
    }
  )
}

# ==============================================
# Users → User List UI/Server
# ==============================================

user_list_ui <- bslib::card(
  card_header_with_download("Filters", "download_user_list"),
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    shiny::selectInput(
      "user_list_environment",
      "Environment:",
      choices = c("All")
    ),
    shiny::selectInput(
      "user_list_role",
      "Role:",
      choices = c("All", "user", "administrator", "super_administrator")
    ),
    shiny::textInput(
      "user_list_search",
      "Search:",
      placeholder = "Username"
    )
  ),
  shinycssloaders::withSpinner(
    DT::dataTableOutput("user_list_table")
  )
)

user_list_server <- function(input, output, session, should_load) {
  # Load user_list data (snapshot at max_date), deferred until tab is visited
  user_list_data <- shiny::reactive({
    shiny::req(should_load())
    tryCatch(
      {
        data <- chronicle_data("workbench/user_list", base_path)

        # Find max_date in Arrow (reads only parquet metadata), then collect
        # just that partition instead of all historical snapshots
        max_date_result <- data |>
          dplyr::summarise(max_date = max(date, na.rm = TRUE)) |>
          dplyr::collect()
        if (nrow(max_date_result) == 0) {
          return(data |> dplyr::head(0) |> dplyr::collect())
        }
        max_date <- max_date_result$max_date

        data |>
          dplyr::filter(date == max_date) |>
          dplyr::collect()
      },
      error = function(e) {
        message("Error loading user list: ", e$message)
        NULL
      }
    )
  })

  # Populate environment filter dynamically
  shiny::observe({
    data <- user_list_data()
    if (is.null(data) || nrow(data) == 0) {
      return()
    }

    env_values <- data |>
      dplyr::pull(.data$environment) |>
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
      "user_list_environment",
      choices = c("All", env_values)
    )
  })

  # Apply filters
  filtered_user_list <- shiny::reactive({
    data <- user_list_data()
    if (is.null(data)) {
      return(NULL)
    }

    # Environment filter
    if (input$user_list_environment != "All") {
      if (input$user_list_environment == "(Not Set)") {
        data <- data |>
          dplyr::filter(
            is.na(environment) |
              environment == "" |
              environment == " "
          )
      } else {
        data <- data |>
          dplyr::filter(environment == input$user_list_environment)
      }
    }

    # Role filter
    if (input$user_list_role != "All") {
      data <- data |> dplyr::filter(.data$user_role == input$user_list_role)
    }

    # Search filter
    if (nzchar(input$user_list_search)) {
      search_term <- tolower(input$user_list_search)
      data <- data |>
        dplyr::filter(
          grepl(search_term, tolower(.data$username))
        )
    }

    data
  })

  # Render table
  output$user_list_table <- DT::renderDataTable({
    data <- filtered_user_list()

    if (is.null(data) || nrow(data) == 0) {
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
          is.na(.data$environment) |
            .data$environment == "" |
            .data$environment == " ",
          "(Not Set)",
          .data$environment
        )
      ) |>
      dplyr::select(
        "username",
        "user_role",
        "environment",
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

  # Download handler for User List table
  output$download_user_list <- shiny::downloadHandler(
    filename = function() {
      paste0("chronicle_workbench_user_list_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- filtered_user_list()
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame()
      } else {
        data <- data |>
          dplyr::mutate(
            environment = ifelse(
              is.na(.data$environment) |
                .data$environment == "" |
                .data$environment == " ",
              "(Not Set)",
              .data$environment
            )
          ) |>
          dplyr::select(
            "username",
            "user_role",
            "environment",
            "last_active_at"
          )
      }
      utils::write.csv(data, file, row.names = FALSE)
    }
  )
}

# ==============================================
# Sessions → Overview UI/Server
# ==============================================

sessions_overview_ui <- bslib::card(
  bslib::card_header("Filters"),
  bslib::layout_columns(
    col_widths = c(8, 4),
    shiny::dateRangeInput(
      "sessions_overview_date_range",
      "Date Range:",
      start = NULL,
      end = NULL,
      format = "yyyy-mm-dd"
    ),
    shiny::selectInput(
      "sessions_overview_environment",
      "Environment:",
      choices = c("All")
    )
  ),
  bslib::layout_columns(
    col_widths = c(3, 3, 3, 3),
    bslib::value_box(
      title = "Sessions Started (latest day)",
      max_height = "120px",
      value = shiny::textOutput("sessions_latest_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BLUE)
    ),
    bslib::value_box(
      title = "Total Sessions (range)",
      max_height = "120px",
      value = shiny::textOutput("sessions_total_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$GREEN)
    ),
    bslib::value_box(
      title = "Median Startup (range avg)",
      max_height = "120px",
      value = shiny::textOutput("sessions_median_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BURGUNDY)
    ),
    bslib::value_box(
      title = "P95 Startup (range avg)",
      max_height = "120px",
      value = shiny::textOutput("sessions_p95_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$GRAY)
    )
  ),
  bslib::card(
    bslib::card_header("Sessions Started Over Time"),
    shinycssloaders::withSpinner(
      plotly::plotlyOutput("sessions_trend_plot")
    )
  ),
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header("Startup Duration Over Time (Median)"),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("sessions_duration_median_plot")
      )
    ),
    bslib::card(
      bslib::card_header("Startup Duration Over Time (P95)"),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("sessions_duration_p95_plot")
      )
    )
  )
)

# Empty-plot placeholder shared by the Sessions charts.
sessions_empty_plot <- function(
  title = "Data not available",
  subtitle = "Check that Chronicle data exists at the configured path"
) {
  annotations <- list(
    list(
      text = paste0("<b>", title, "</b>"),
      x = 0.5,
      y = if (nzchar(subtitle)) 0.55 else 0.5,
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      font = list(size = 18, color = "#666666")
    )
  )
  if (nzchar(subtitle)) {
    annotations <- c(
      annotations,
      list(list(
        text = subtitle,
        x = 0.5,
        y = 0.45,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 14, color = "#666666")
      ))
    )
  }
  plotly::plotly_empty(type = "scatter", mode = "markers") |>
    plotly::layout(
      xaxis = list(showgrid = FALSE, zeroline = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE),
      annotations = annotations
    )
}

# Sessions-weighted mean of a per-group duration column (ms). The dataset stores
# a daily median/p95 per (environment, session type); those can't be re-medianed
# exactly, so each group's value is weighted by the sessions it represents —
# a sound "typical startup" proxy across the selected range and environment.
sessions_weighted_ms <- function(data, col) {
  value <- data[[col]]
  weight <- data$sessions_started
  ok <- !is.na(value) & is.finite(value) & !is.na(weight) & weight > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  sum(value[ok] * weight[ok]) / sum(weight[ok])
}

# Format a duration in milliseconds for a value box (e.g. "2.4s", "850 ms").
format_startup_ms <- function(ms) {
  if (is.na(ms) || !is.finite(ms)) {
    return("-")
  }
  if (ms >= 1000) {
    paste0(formatC(ms / 1000, format = "f", digits = 1), "s")
  } else {
    paste0(round(ms), " ms")
  }
}

# Build a startup-duration plotly for one metric column (median or p95).
# Collapses multiple environments into one line per session type via a
# sessions-weighted average, consistent with the value box calculation.
render_startup_duration_plot <- function(data, metric_col, metric_label) {
  if (is.null(data) || nrow(data) == 0) {
    return(sessions_empty_plot())
  }

  plot_data <- data |>
    dplyr::filter(!is.na(date)) |>
    dplyr::transmute(
      date = date,
      session_type = .data$session_type,
      value = .data[[metric_col]],
      weight = .data$sessions_started
    ) |>
    dplyr::filter(!is.na(.data$value), is.finite(.data$value)) |>
    dplyr::group_by(date, .data$session_type) |>
    dplyr::summarise(
      value = stats::weighted.mean(
        .data$value,
        w = dplyr::coalesce(.data$weight, 0)
      ),
      .groups = "drop"
    ) |>
    dplyr::filter(is.finite(.data$value)) |>
    dplyr::arrange(date)

  if (nrow(plot_data) == 0) {
    return(sessions_empty_plot("No data available for selected date range", ""))
  }

  types <- sort(unique(plot_data$session_type))
  pal <- stats::setNames(
    rep(SESSION_PALETTE, length.out = length(types)),
    types
  )

  p <- suppressWarnings(
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = date, y = .data$value, color = .data$session_type)
    ) +
      ggplot2::geom_line(linewidth = 0.5) +
      ggplot2::geom_point(
        ggplot2::aes(
          text = paste0(
            format(date, "%B %d, %Y"),
            "<br>",
            .data$session_type,
            "<br>",
            metric_label,
            ": ",
            prettyNum(round(.data$value), big.mark = ","),
            " ms"
          )
        ),
        size = 0.5
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        x = "",
        y = paste0(metric_label, " Startup Duration (ms)"),
        color = ""
      ) +
      ggplot2::scale_color_manual(values = pal)
  )
  # across environments stay exact (they are additive). Startup-duration
  # percentiles are combined across environments via a sessions-weighted mean
  # when "All" is selected; filter to a single environment to view exact values.
    plotly::layout(
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE),
      legend = list(orientation = "h", x = 0.5, xanchor = "center")
    ) |>
    plotly::config(displayModeBar = FALSE)
}

sessions_overview_server <- function(input, output, session, sessions_data) {
  # Set default date range on first data load only (skip on reloads to
  # preserve the user's current selection).
  date_init_done <- shiny::reactiveVal(FALSE)
  shiny::observe({
    shiny::req(sessions_data())
    if (date_init_done()) {
      return()
    }

    dated_sessions <- sessions_data() |>
      dplyr::filter(!is.na(date))

    # Nothing to initialize if no dated rows remain (e.g. CHRONICLE_DATA_WINDOW
    # filtered them all out); min()/max() would otherwise yield Inf/-Inf and
    # feed invalid dates to updateDateRangeInput().
    if (nrow(dated_sessions) == 0) {
      return()
    }

    date_summary <- dated_sessions |>
      dplyr::summarise(
        min_date = min(date, na.rm = TRUE),
        max_date = max(date, na.rm = TRUE)
      )

    initial_start <- initial_date_start(date_summary$min_date)

    shiny::updateDateRangeInput(
      session,
      "sessions_overview_date_range",
      start = initial_start,
      end = date_summary$max_date,
      max = date_summary$max_date
    )
    date_init_done(TRUE)
  })

  # Populate the environment filter from the loaded data.
  shiny::observe({
    data <- sessions_data()
    if (is.null(data) || nrow(data) == 0) {
      return()
    }

    env_values <- data |>
      dplyr::pull(.data$environment) |>
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
      "sessions_overview_environment",
      choices = c("All", env_values)
    )
  })

  # Apply the environment filter. "All" keeps every environment; counts summed
  # across environments stay exact (they are additive). Percentiles are never
  # combined across environments — they are plotted per environment instead.
  apply_env_filter <- function(data) {
    env <- input$sessions_overview_environment
    if (is.null(env) || env == "All") {
      return(data)
    }
    if (env == "(Not Set)") {
      data |>
        dplyr::filter(
          is.na(environment) | environment == "" | environment == " "
        )
    } else {
      data |> dplyr::filter(environment == env)
    }
  }

  # Latest-day rows (one per environment/session_type), environment-scoped.
  latest_sessions_data <- shiny::reactive({
    data <- sessions_data()
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    max_date <- max(data$date, na.rm = TRUE)
    data |> dplyr::filter(date == max_date) |> apply_env_filter()
  })

  # Date-range- and environment-scoped rows for the charts and range metrics.
  filtered_sessions_data <- shiny::reactive({
    data <- sessions_data()
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    shiny::req(input$sessions_overview_date_range)
    data |>
      dplyr::filter(
        date >= input$sessions_overview_date_range[1],
        date <= input$sessions_overview_date_range[2]
      ) |>
      apply_env_filter()
  })

  # Value boxes (counts only — additive and therefore exact across groups).
  output$sessions_latest_value <- shiny::renderText({
    data <- latest_sessions_data()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    prettyNum(sum(data$sessions_started, na.rm = TRUE), big.mark = ",")
  })

  output$sessions_total_value <- shiny::renderText({
    data <- filtered_sessions_data()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    prettyNum(sum(data$sessions_started, na.rm = TRUE), big.mark = ",")
  })

  # Startup-duration boxes: sessions-weighted across the selected range and
  # environment (daily medians/p95s can't be re-medianed exactly).
  output$sessions_median_value <- shiny::renderText({
    data <- filtered_sessions_data()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    format_startup_ms(sessions_weighted_ms(data, "median_startup_duration_ms"))
  })

  output$sessions_p95_value <- shiny::renderText({
    data <- filtered_sessions_data()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    format_startup_ms(sessions_weighted_ms(data, "p95_startup_duration_ms"))
  })

  # Sessions started over time, one line per session type. Counts are summed
  # across environments only when "All" is selected (sums are exact).
  output$sessions_trend_plot <- plotly::renderPlotly({
    data <- filtered_sessions_data()
    if (is.null(data) || nrow(data) == 0) {
      return(sessions_empty_plot())
    }

    plot_data <- data |>
      dplyr::filter(!is.na(date)) |>
      dplyr::group_by(date, .data$session_type) |>
      dplyr::summarise(
        sessions_started = sum(.data$sessions_started, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(date)

    if (nrow(plot_data) == 0) {
      return(sessions_empty_plot(
        "No data available for selected date range",
        ""
      ))
    }

    types <- sort(unique(plot_data$session_type))
    pal <- stats::setNames(
      rep(SESSION_PALETTE, length.out = length(types)),
      types
    )

    p <- suppressWarnings(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = date,
          y = .data$sessions_started,
          color = .data$session_type
        )
      ) +
        ggplot2::geom_line(linewidth = 0.5) +
        ggplot2::geom_point(
          ggplot2::aes(
            text = paste0(
              format(date, "%B %d, %Y"),
              "<br>",
              prettyNum(.data$sessions_started, big.mark = ","),
              " ",
              .data$session_type,
              " sessions"
            )
          ),
          size = 0.5
        ) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "", y = "Sessions Started", color = "") +
        ggplot2::scale_color_manual(values = pal)
    )

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE),
        legend = list(orientation = "h", x = 0.5, xanchor = "center")
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  output$sessions_duration_median_plot <- plotly::renderPlotly({
    render_startup_duration_plot(
      filtered_sessions_data(),
      "median_startup_duration_ms",
      "Median"
    )
  })

  output$sessions_duration_p95_plot <- plotly::renderPlotly({
    render_startup_duration_plot(
      filtered_sessions_data(),
      "p95_startup_duration_ms",
      "P95"
    )
  })
}

# ==============================================
# Sessions → By User UI/Server
# ==============================================

sessions_by_user_ui <- bslib::card(
  bslib::card_header("Filters"),
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    shiny::selectInput(
      "sessions_by_user_environment",
      "Environment:",
      choices = c("All")
    ),
    shiny::selectInput(
      "sessions_by_user_type",
      "Session Type:",
      choices = c("All")
    ),
    shiny::textInput(
      "sessions_by_user_search",
      "Search:",
      placeholder = "User"
    )
  ),
  shinycssloaders::withSpinner(
    DT::dataTableOutput("sessions_by_user_table")
  )
)

sessions_by_user_server <- function(input, output, session, should_load) {
  # Load by-user session data (latest snapshot), deferred until tab is visited.
  sessions_by_user_data <- shiny::reactive({
    shiny::req(should_load())
    tryCatch(
      {
        data <- chronicle_data(
          "workbench/session_start_totals_by_user",
          base_path
        )

        # Find max_date via Arrow metadata, then collect only that partition.
        max_date_result <- data |>
          dplyr::summarise(max_date = max(date, na.rm = TRUE)) |>
          dplyr::collect()
        if (nrow(max_date_result) == 0) {
          return(data |> dplyr::head(0) |> dplyr::collect())
        }
        max_date <- max_date_result$max_date

        data |>
          dplyr::filter(date == max_date) |>
          dplyr::collect()
      },
      error = function(e) {
        message("Error loading session totals by user: ", e$message)
        NULL
      }
    )
  })

  # Populate environment filter dynamically
  shiny::observe({
    data <- sessions_by_user_data()
    if (is.null(data) || nrow(data) == 0) {
      return()
    }

    env_values <- data |>
      dplyr::pull(.data$environment) |>
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
      "sessions_by_user_environment",
      choices = c("All", env_values)
    )
  })

  # Populate session type filter dynamically
  shiny::observe({
    data <- sessions_by_user_data()
    if (is.null(data) || nrow(data) == 0) {
      return()
    }

    type_values <- data |>
      dplyr::pull(.data$session_type) |>
      unique()
    type_values <- sort(type_values[!is.na(type_values)])

    shiny::updateSelectInput(
      session,
      "sessions_by_user_type",
      choices = c("All", type_values)
    )
  })

  # Apply filters
  filtered_sessions_by_user <- shiny::reactive({
    data <- sessions_by_user_data()
    if (is.null(data)) {
      return(NULL)
    }

    # Environment filter
    if (input$sessions_by_user_environment != "All") {
      if (input$sessions_by_user_environment == "(Not Set)") {
        data <- data |>
          dplyr::filter(
            is.na(environment) |
              environment == "" |
              environment == " "
          )
      } else {
        data <- data |>
          dplyr::filter(environment == input$sessions_by_user_environment)
      }
    }

    # Session type filter
    if (input$sessions_by_user_type != "All") {
      data <- data |>
        dplyr::filter(.data$session_type == input$sessions_by_user_type)
    }

    # Search filter (on user_guid)
    if (nzchar(input$sessions_by_user_search)) {
      search_term <- tolower(input$sessions_by_user_search)
      data <- data |>
        dplyr::filter(grepl(search_term, tolower(.data$user_guid)))
    }

    data
  })

  # Render table
  output$sessions_by_user_table <- DT::renderDataTable({
    data <- filtered_sessions_by_user()

    if (is.null(data) || nrow(data) == 0) {
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
          is.na(.data$environment) |
            .data$environment == "" |
            .data$environment == " ",
          "(Not Set)",
          .data$environment
        )
      ) |>
      dplyr::arrange(dplyr::desc(.data$sessions_started)) |>
      dplyr::select(
        "user_guid",
        "session_type",
        "environment",
        "sessions_started",
        "median_startup_duration_ms",
        "p95_startup_duration_ms"
      ) |>
      DT::datatable(
        colnames = c(
          "User" = "user_guid",
          "Session Type" = "session_type",
          "Environment" = "environment",
          "Sessions Started" = "sessions_started",
          "Median Startup (ms)" = "median_startup_duration_ms",
          "P95 Startup (ms)" = "p95_startup_duration_ms"
        ),
        options = list(
          pageLength = 25,
          autoWidth = TRUE,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
  })
}

# ==============================================
# Main UI (page_navbar with one dropdown)
# ==============================================

ui <- bslib::page_navbar(
  id = "main_nav",
  title = "Posit Workbench Dashboard",
  theme = bslib::bs_theme(preset = "shiny"),
  fillable = FALSE,

  # Users dropdown
  bslib::nav_menu(
    "Users",
    bslib::nav_panel("Overview", users_overview_ui, value = "users_overview"),
    bslib::nav_panel("User List", user_list_ui, value = "user_list")
  ),

  # Sessions dropdown
  bslib::nav_menu(
    "Sessions",
    bslib::nav_panel(
      "Overview",
      sessions_overview_ui,
      value = "sessions_overview"
    ),
    bslib::nav_panel("By User", sessions_by_user_ui, value = "sessions_by_user")
  )
)

# ==============================================
# Main Server
# ==============================================

server <- function(input, output, session) {
  # Deferred loading: only load user list data when the tab is first visited
  should_load_user_list <- shiny::reactiveVal(FALSE)
  shiny::observe({
    if (!should_load_user_list() && input$main_nav == "user_list") {
      should_load_user_list(TRUE)
    }
  })

  # Users → Overview
  users_overview_server(input, output, session)

  # Users → User List
  user_list_server(input, output, session, should_load_user_list)

  # Sessions → Overview: load session totals deferred until the tab is visited.
  # The loaded range expands when the date selector extends beyond it (only the
  # additional partitions are fetched, not the full dataset).
  should_load_sessions <- shiny::reactiveVal(FALSE)
  shiny::observe({
    if (!should_load_sessions() && input$main_nav == "sessions_overview") {
      should_load_sessions(TRUE)
    }
  })

  sessions_initial_range <- if (!is.null(data_window_cutoff)) {
    list(min = data_window_cutoff, max = Sys.Date())
  }
  sessions_totals_range <- shiny::reactiveVal(sessions_initial_range)

  sessions_data <- shiny::reactive({
    shiny::req(should_load_sessions())
    range <- sessions_totals_range()
    tryCatch(
      {
        ds <- chronicle_data("workbench/session_start_totals", base_path)
        if (!is.null(range)) {
          range_min <- range$min
          range_max <- range$max
          ds <- ds |> dplyr::filter(date >= range_min, date <= range_max)
        }
        ds |> dplyr::collect()
      },
      error = function(e) {
        message("Error loading session totals: ", e$message)
        NULL
      }
    )
  })

  shiny::observe({
    date_val <- input$sessions_overview_date_range
    shiny::req(date_val)
    range <- sessions_totals_range()
    if (is.null(range)) {
      return()
    }
    new_min <- min(range$min, date_val[1])
    new_max <- max(range$max, date_val[2])
    if (new_min < range$min || new_max > range$max) {
      sessions_totals_range(list(min = new_min, max = new_max))
    }
  })

  sessions_overview_server(input, output, session, sessions_data)

  # Sessions → By User: load the latest by-user snapshot deferred until visited.
  should_load_sessions_by_user <- shiny::reactiveVal(FALSE)
  shiny::observe({
    if (
      !should_load_sessions_by_user() &&
        input$main_nav == "sessions_by_user"
    ) {
      should_load_sessions_by_user(TRUE)
    }
  })

  sessions_by_user_server(
    input,
    output,
    session,
    should_load_sessions_by_user
  )
}

shinyApp(ui, server)
