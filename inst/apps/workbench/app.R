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

# Wider palette for exit reasons (up to 7 distinct values). Green leads so the
# most common reason — normally NormalExit — reads as healthy.
EXIT_PALETTE <- c(
  BRAND_COLORS$GREEN,
  BRAND_COLORS$BLUE,
  BRAND_COLORS$BURGUNDY,
  "#EE6331",
  BRAND_COLORS$GRAY,
  "#419599",
  "#C0A33E"
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

user_list_server <- function(input, output, session, user_list_data) {
  # Uses the shared user_list reactive (latest snapshot, loaded in the main
  # server and shared with the Sessions user tables for username lookups).

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
          scrollX = TRUE,
          # No built-in DataTables search box — each page provides at most
          # one search/select control of its own.
          dom = "lrtip"
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
    col_widths = c(4, 4, 4),
    bslib::value_box(
      title = "Total Sessions Started",
      max_height = "120px",
      value = shiny::textOutput("sessions_started_total_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BLUE)
    ),
    bslib::value_box(
      title = "Total Sessions Ended",
      max_height = "120px",
      value = shiny::textOutput("sessions_ended_total_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$GREEN)
    ),
    bslib::value_box(
      title = "Total Session Hours",
      max_height = "120px",
      value = shiny::textOutput("sessions_total_hours_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BURGUNDY)
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

# Format a duration in seconds for a value box (e.g. "1.4h", "32m", "45s").
# Sub-hour values truncate (integer division) rather than round so labels
# never cross their unit boundary (e.g. 3599s is "59m", never "60m").
format_duration_secs <- function(secs) {
  if (is.na(secs) || !is.finite(secs)) {
    return("-")
  }
  if (secs >= 3600) {
    paste0(formatC(secs / 3600, format = "f", digits = 1), "h")
  } else if (secs >= 60) {
    paste0(secs %/% 60, "m")
  } else {
    paste0(floor(secs), "s")
  }
}

# Format total session time (seconds) as hours for a value box.
format_total_hours <- function(secs) {
  if (is.na(secs) || !is.finite(secs)) {
    return("-")
  }
  hours <- secs / 3600
  if (hours >= 100) {
    prettyNum(round(hours), big.mark = ",")
  } else {
    formatC(hours, format = "f", digits = 1)
  }
}

# Map user GUIDs to usernames using the latest user list snapshot. Returns a
# two-column data frame (user_guid, username), or NULL when the user list is
# unavailable. Mirrors how the Connect app labels content visits.
username_lookup <- function(user_list) {
  if (
    is.null(user_list) ||
      nrow(user_list) == 0 ||
      !all(c("id", "username") %in% names(user_list))
  ) {
    return(NULL)
  }
  user_list |>
    dplyr::distinct(.data$id, .data$username) |>
    dplyr::rename(user_guid = "id")
}

# Collect an Arrow query, returning NULL (with a message) on failure. Used
# wherever a lazy duration query is materialized, since collect-time errors
# (e.g. missing data path) surface here rather than when the query is built.
collect_or_null <- function(query, context) {
  if (is.null(query)) {
    return(NULL)
  }
  tryCatch(
    dplyr::collect(query),
    error = function(e) {
      message("Error collecting ", context, ": ", e$message)
      NULL
    }
  )
}

# Filter session-level duration rows by an environment selection ("All",
# "(Not Set)", or a specific environment). Works on both data frames and
# lazy Arrow queries. Shared by Overview and Duration.
filter_duration_env <- function(data, env) {
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

# Build a startup-duration plotly for one metric column (median or p95).
# Collapses multiple environments into one line per session type via a
# sessions-weighted average (daily percentiles can't be re-medianed exactly).
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
  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE),
      legend = list(orientation = "h", x = 0.5, xanchor = "center")
    ) |>
    plotly::config(displayModeBar = FALSE)
}

sessions_overview_server <- function(
  input,
  output,
  session,
  sessions_data,
  duration_data
) {
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
  # across environments stay exact (they are additive). When "All" is selected,
  # startup-duration percentiles are approximated via a sessions-weighted average.
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

  # Total sessions started over the selected range/environment (sums are
  # additive across groups, so this stays exact).
  output$sessions_started_total_value <- shiny::renderText({
    data <- filtered_sessions_data()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    prettyNum(sum(data$sessions_started, na.rm = TRUE), big.mark = ",")
  })

  # Total sessions ended over the selected range/environment. Each
  # session_duration row is one ended session (any exit_reason), so the count
  # from overview_duration_summary() is the ended-session total.
  output$sessions_ended_total_value <- shiny::renderText({
    summary <- overview_duration_summary()
    if (is.null(summary) || nrow(summary) == 0 || summary$sessions == 0) {
      return("-")
    }
    prettyNum(summary$sessions, big.mark = ",")
  })

  # Total session time over the selected range and environment, aggregated in
  # Arrow — only a single summary row is collected.
  overview_duration_summary <- shiny::reactive({
    ds <- duration_data()
    if (is.null(ds)) {
      return(NULL)
    }
    shiny::req(input$sessions_overview_date_range)
    query <- ds |>
      dplyr::filter(
        date >= input$sessions_overview_date_range[1],
        date <= input$sessions_overview_date_range[2]
      ) |>
      filter_duration_env(input$sessions_overview_environment) |>
      dplyr::summarise(
        sessions = dplyr::n(),
        total_duration_seconds = sum(.data$duration_seconds, na.rm = TRUE)
      )
    collect_or_null(query, "session duration summary")
  })

  output$sessions_total_hours_value <- shiny::renderText({
    summary <- overview_duration_summary()
    if (is.null(summary) || nrow(summary) == 0 || summary$sessions == 0) {
      return("-")
    }
    format_total_hours(summary$total_duration_seconds)
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
# Sessions → Duration UI/Server
# ==============================================

sessions_duration_ui <- bslib::card(
  bslib::card_header("Filters"),
  bslib::layout_columns(
    col_widths = c(6, 3, 3),
    shiny::dateRangeInput(
      "sessions_duration_date_range",
      "Date Range:",
      start = NULL,
      end = NULL,
      format = "yyyy-mm-dd"
    ),
    shiny::selectInput(
      "sessions_duration_environment",
      "Environment:",
      choices = c("All")
    ),
    shiny::selectInput(
      "sessions_duration_type",
      "Session Type:",
      choices = c("All")
    )
  ),
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    bslib::value_box(
      title = "Sessions Completed",
      max_height = "120px",
      value = shiny::textOutput("duration_sessions_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BLUE)
    ),
    bslib::value_box(
      title = "Median Duration",
      max_height = "120px",
      value = shiny::textOutput("duration_median_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$GREEN)
    ),
    bslib::value_box(
      title = "Total Session Hours",
      max_height = "120px",
      value = shiny::textOutput("duration_hours_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BURGUNDY)
    )
  ),
  bslib::card(
    card_header_with_chart_downloads(
      "Session Duration Over Time (Median)",
      "download_duration_median_chart",
      "download_duration_median_raw"
    ),
    shinycssloaders::withSpinner(
      plotly::plotlyOutput("duration_trend_median_plot")
    )
  ),
  bslib::card(
    card_header_with_chart_downloads(
      "Sessions by Exit Reason",
      "download_duration_exit_chart",
      "download_duration_exit_raw"
    ),
    shinycssloaders::withSpinner(
      plotly::plotlyOutput("duration_exit_plot")
    )
  )
)

# Build a session-duration trend plotly for one statistic (median or p95).
# Computed exactly from session-level rows, one line per session type,
# plotted in minutes for readability.
render_session_duration_plot <- function(plot_data, metric_label) {
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    return(sessions_empty_plot())
  }

  types <- sort(unique(plot_data$session_type))
  pal <- stats::setNames(
    rep(SESSION_PALETTE, length.out = length(types)),
    types
  )

  # format_duration_secs() is scalar; precompute the tooltip labels.
  plot_data$duration_label <- vapply(
    plot_data$value * 60,
    format_duration_secs,
    character(1)
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
            .data$duration_label
          )
        ),
        size = 0.5
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        x = "",
        y = paste0(metric_label, " Session Duration (minutes)"),
        color = ""
      ) +
      ggplot2::scale_color_manual(values = pal)
  )
  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE),
      legend = list(orientation = "h", x = 0.5, xanchor = "center")
    ) |>
    plotly::config(displayModeBar = FALSE)
}

sessions_duration_server <- function(input, output, session, duration_data) {
  # Set default date range on first load: min/max dates aggregated in Arrow.
  date_init_done <- shiny::reactiveVal(FALSE)
  shiny::observe({
    ds <- duration_data()
    if (date_init_done() || is.null(ds)) {
      return()
    }

    date_summary <- collect_or_null(
      ds |>
        dplyr::filter(!is.na(date)) |>
        dplyr::summarise(
          min_date = min(date, na.rm = TRUE),
          max_date = max(date, na.rm = TRUE)
        ),
      "session duration date bounds"
    )
    if (
      is.null(date_summary) ||
        nrow(date_summary) == 0 ||
        is.na(date_summary$min_date)
    ) {
      return()
    }

    initial_start <- initial_date_start(date_summary$min_date)

    shiny::updateDateRangeInput(
      session,
      "sessions_duration_date_range",
      start = initial_start,
      end = date_summary$max_date,
      max = date_summary$max_date
    )
    date_init_done(TRUE)
  })

  # Populate the environment filter (distinct values computed in Arrow).
  shiny::observe({
    ds <- duration_data()
    if (is.null(ds)) {
      return()
    }

    env_df <- collect_or_null(
      ds |> dplyr::distinct(.data$environment),
      "session duration environments"
    )
    if (is.null(env_df) || nrow(env_df) == 0) {
      return()
    }
    env_values <- env_df$environment

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
      "sessions_duration_environment",
      choices = c("All", env_values)
    )
  })

  # Populate the session type filter (distinct values computed in Arrow).
  shiny::observe({
    ds <- duration_data()
    if (is.null(ds)) {
      return()
    }

    type_df <- collect_or_null(
      ds |> dplyr::distinct(.data$session_type),
      "session duration types"
    )
    if (is.null(type_df) || nrow(type_df) == 0) {
      return()
    }
    type_values <- type_df$session_type
    type_values <- sort(type_values[!is.na(type_values)])

    shiny::updateSelectInput(
      session,
      "sessions_duration_type",
      choices = c("All", type_values)
    )
  })

  # Date-range-, environment-, and type-scoped LAZY query (not collected).
  filtered_duration_query <- shiny::reactive({
    ds <- duration_data()
    if (is.null(ds)) {
      return(NULL)
    }
    shiny::req(input$sessions_duration_date_range)
    query <- ds |>
      dplyr::filter(
        date >= input$sessions_duration_date_range[1],
        date <= input$sessions_duration_date_range[2]
      ) |>
      filter_duration_env(input$sessions_duration_environment)

    if (
      !is.null(input$sessions_duration_type) &&
        input$sessions_duration_type != "All"
    ) {
      query <- query |>
        dplyr::filter(.data$session_type == input$sessions_duration_type)
    }

    query
  })

  # Collected rows for the median trend chart and value boxes. Exact medians
  # can't be pushed down to Arrow (only approximate t-digest versions), so
  # this is the one place the filtered range is materialized.
  duration_rows <- shiny::reactive({
    collect_or_null(filtered_duration_query(), "session duration rows")
  })

  # Value boxes — exact statistics over session-level rows.
  output$duration_sessions_value <- shiny::renderText({
    data <- duration_rows()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    prettyNum(nrow(data), big.mark = ",")
  })

  output$duration_median_value <- shiny::renderText({
    data <- duration_rows()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    format_duration_secs(stats::median(data$duration_seconds, na.rm = TRUE))
  })

  output$duration_hours_value <- shiny::renderText({
    data <- duration_rows()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    format_total_hours(sum(data$duration_seconds, na.rm = TRUE))
  })

  # Daily duration statistics (minutes) per session type for the trend chart.
  duration_trend_data <- shiny::reactive({
    data <- duration_rows()
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }

    data |>
      dplyr::filter(
        !is.na(date),
        !is.na(.data$duration_seconds),
        is.finite(.data$duration_seconds)
      ) |>
      dplyr::group_by(date, .data$session_type) |>
      dplyr::summarise(
        sessions = dplyr::n(),
        median_duration_minutes = stats::median(.data$duration_seconds) / 60,
        .groups = "drop"
      ) |>
      dplyr::arrange(date)
  })

  # Exit reason breakdown — counts aggregated in Arrow; only the small
  # (exit_reason x session_type) summary is collected.
  duration_exit_data <- shiny::reactive({
    query <- filtered_duration_query()
    if (is.null(query)) {
      return(NULL)
    }

    counts <- collect_or_null(
      query |>
        dplyr::group_by(.data$exit_reason, .data$session_type) |>
        dplyr::summarise(sessions = dplyr::n(), .groups = "drop"),
      "exit reason summary"
    )
    if (is.null(counts) || nrow(counts) == 0) {
      return(NULL)
    }

    counts |>
      dplyr::mutate(
        exit_reason = dplyr::coalesce(.data$exit_reason, "(Unknown)")
      ) |>
      dplyr::arrange(dplyr::desc(.data$sessions))
  })

  output$duration_trend_median_plot <- plotly::renderPlotly({
    trend <- duration_trend_data()
    if (is.null(trend) || nrow(trend) == 0) {
      return(sessions_empty_plot())
    }
    render_session_duration_plot(
      trend |>
        dplyr::transmute(
          date = date,
          session_type = .data$session_type,
          value = .data$median_duration_minutes
        ),
      "Median"
    )
  })

  output$duration_exit_plot <- plotly::renderPlotly({
    exit_summary <- duration_exit_data()
    if (is.null(exit_summary) || nrow(exit_summary) == 0) {
      return(sessions_empty_plot())
    }

    # Color by exit reason, most common reason first in the legend.
    reason_order <- exit_summary |>
      dplyr::group_by(.data$exit_reason) |>
      dplyr::summarise(total = sum(.data$sessions), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$total)) |>
      dplyr::pull(.data$exit_reason)

    plot_data <- exit_summary |>
      dplyr::mutate(
        exit_reason = factor(.data$exit_reason, levels = reason_order)
      )

    pal <- stats::setNames(
      rep(EXIT_PALETTE, length.out = length(reason_order)),
      reason_order
    )

    p <- suppressWarnings(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = .data$session_type,
          y = .data$sessions,
          fill = .data$exit_reason,
          text = paste0(
            .data$session_type,
            "<br>",
            .data$exit_reason,
            "<br>",
            prettyNum(.data$sessions, big.mark = ","),
            " sessions"
          )
        )
      ) +
        ggplot2::geom_col() +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "", y = "Sessions", fill = "") +
        ggplot2::scale_fill_manual(values = pal)
    )

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        barmode = "stack",
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE),
        legend = list(orientation = "h", x = 0.5, xanchor = "center")
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  # Download handlers
  duration_download <- function(data_fn, suffix) {
    shiny::downloadHandler(
      filename = function() {
        paste0("chronicle_workbench_", suffix, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data <- data_fn()
        if (is.null(data) || nrow(data) == 0) {
          data <- data.frame()
        }
        utils::write.csv(data, file, row.names = FALSE)
      }
    )
  }

  output$download_duration_median_chart <- duration_download(
    duration_trend_data,
    "session_duration_trend_chart"
  )
  output$download_duration_median_raw <- duration_download(
    duration_rows,
    "session_duration_raw"
  )
  output$download_duration_exit_chart <- duration_download(
    duration_exit_data,
    "session_exit_reason_chart"
  )
  output$download_duration_exit_raw <- duration_download(
    duration_rows,
    "session_exit_reason_raw"
  )
}

# ==============================================
# Sessions → User Summary UI/Server
# ==============================================

sessions_by_user_ui <- bslib::card(
  card_header_with_download("Filters", "download_user_summary"),
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    shiny::dateRangeInput(
      "sessions_by_user_date_range",
      "Date Range:",
      start = NULL,
      end = NULL,
      format = "yyyy-mm-dd"
    ),
    shiny::selectInput(
      "sessions_by_user_environment",
      "Environment:",
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

sessions_by_user_server <- function(
  input,
  output,
  session,
  should_load,
  user_list_data
) {
  # Daily by-user rows, deferred until the tab is first visited. The loaded
  # range expands when the date selector extends beyond it (only the
  # additional partitions are fetched via Arrow partition pruning).
  by_user_initial_range <- if (!is.null(data_window_cutoff)) {
    list(min = data_window_cutoff, max = Sys.Date())
  }
  by_user_range <- shiny::reactiveVal(by_user_initial_range)

  sessions_by_user_data <- shiny::reactive({
    shiny::req(should_load())
    range <- by_user_range()
    tryCatch(
      {
        ds <- chronicle_data(
          "workbench/session_start_totals_by_user",
          base_path
        )
        if (!is.null(range)) {
          range_min <- range$min
          range_max <- range$max
          ds <- ds |> dplyr::filter(date >= range_min, date <= range_max)
        }
        ds |> dplyr::collect()
      },
      error = function(e) {
        message("Error loading session totals by user: ", e$message)
        NULL
      }
    )
  })

  shiny::observe({
    date_val <- input$sessions_by_user_date_range
    shiny::req(date_val)
    range <- by_user_range()
    if (is.null(range)) {
      return()
    }
    date_val <- date_val[!is.na(date_val)]
    if (length(date_val) == 0) {
      return()
    }
    new_min <- min(range$min, date_val)
    new_max <- max(range$max, date_val)
    if (new_min < range$min || new_max > range$max) {
      by_user_range(list(min = new_min, max = new_max))
    }
  })

  # Set default date range on first data load only (skip on range expansion
  # reloads to preserve the user's current selection).
  date_init_done <- shiny::reactiveVal(FALSE)
  shiny::observe({
    data <- sessions_by_user_data()
    if (date_init_done() || is.null(data) || nrow(data) == 0) {
      return()
    }

    dated_rows <- data |> dplyr::filter(!is.na(date))
    if (nrow(dated_rows) == 0) {
      return()
    }

    max_date <- max(dated_rows$date, na.rm = TRUE)
    min_date <- min(dated_rows$date, na.rm = TRUE)

    shiny::updateDateRangeInput(
      session,
      "sessions_by_user_date_range",
      start = initial_date_start(min_date),
      end = max_date,
      max = max_date
    )
    date_init_done(TRUE)
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

  # Pivoted summary: one row per user, one column per session type, plus a
  # Total column. The environment filter scopes the counts but environment is
  # intentionally not shown as a column.
  user_summary_data <- shiny::reactive({
    data <- sessions_by_user_data()
    if (is.null(data)) {
      return(NULL)
    }

    # Date range filter
    shiny::req(input$sessions_by_user_date_range)
    data <- data |>
      dplyr::filter(
        date >= input$sessions_by_user_date_range[1],
        date <= input$sessions_by_user_date_range[2]
      )

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

    # Attach usernames from the latest user list snapshot (guid -> username).
    # Any username column already present is dropped so the user list is the
    # single source of display names.
    data <- data |> dplyr::select(-dplyr::any_of("username"))
    lookup <- username_lookup(user_list_data())
    if (!is.null(lookup)) {
      data <- data |> dplyr::left_join(lookup, by = "user_guid")
    } else {
      data$username <- NA_character_
    }
    data <- data |>
      dplyr::mutate(
        username = ifelse(is.na(.data$username), "(unknown)", .data$username)
      )

    # Search filter (matches username or GUID)
    if (nzchar(input$sessions_by_user_search)) {
      search_term <- tolower(input$sessions_by_user_search)
      data <- data |>
        dplyr::filter(
          grepl(search_term, tolower(.data$user_guid)) |
            grepl(search_term, tolower(.data$username))
        )
    }

    if (nrow(data) == 0) {
      return(data.frame())
    }

    wide <- data |>
      dplyr::group_by(.data$username, .data$user_guid, .data$session_type) |>
      dplyr::summarise(
        sessions = sum(.data$sessions_started, na.rm = TRUE),
        .groups = "drop"
      ) |>
      tidyr::pivot_wider(
        names_from = "session_type",
        values_from = "sessions",
        values_fill = 0L
      )

    type_cols <- sort(setdiff(names(wide), c("username", "user_guid")))
    wide$Total <- rowSums(wide[type_cols])
    wide[, c("username", "user_guid", type_cols, "Total")] |>
      dplyr::arrange(dplyr::desc(.data$Total))
  })

  # Render table
  output$sessions_by_user_table <- DT::renderDataTable({
    data <- user_summary_data()

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
      # GUID stays in the underlying data (and CSV download) but is not shown.
      dplyr::select(-"user_guid") |>
      DT::datatable(
        colnames = c("Username" = "username"),
        # Fill the card width (autoWidth would shrink the table to fit its
        # content, leaving the card mostly empty with few columns).
        width = "100%",
        options = list(
          pageLength = 25,
          autoWidth = FALSE,
          scrollX = TRUE,
          # No built-in DataTables search box — each page provides at most
          # one search/select control of its own.
          dom = "lrtip"
        ),
        rownames = FALSE
      )
  })

  # Download handler for the pivoted summary table
  output$download_user_summary <- shiny::downloadHandler(
    filename = function() {
      paste0("chronicle_workbench_user_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- user_summary_data()
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame()
      }
      utils::write.csv(data, file, row.names = FALSE)
    }
  )
}

# ==============================================
# Sessions → User Detail UI/Server
# ==============================================

sessions_user_detail_ui <- bslib::card(
  bslib::card_header("Filters"),
  shiny::selectizeInput(
    "user_detail_user",
    "User:",
    choices = NULL,
    # Labels show the username only, but the option value is the user GUID, so
    # searching both fields keeps users findable by GUID as well as name.
    options = list(
      placeholder = "Select a user",
      searchField = c("label", "value")
    )
  ),
  bslib::layout_columns(
    col_widths = c(3, 3, 3, 3),
    bslib::value_box(
      title = "Total Sessions",
      max_height = "120px",
      value = shiny::textOutput("user_detail_sessions_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BLUE)
    ),
    bslib::value_box(
      title = "Total Session Hours",
      max_height = "120px",
      value = shiny::textOutput("user_detail_hours_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$GREEN)
    ),
    bslib::value_box(
      title = "Median Duration",
      max_height = "120px",
      value = shiny::textOutput("user_detail_median_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BURGUNDY)
    ),
    bslib::value_box(
      title = "Last Active",
      max_height = "120px",
      value = shiny::textOutput("user_detail_last_active_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$GRAY)
    )
  ),
  bslib::card(
    bslib::card_header("Session Timeline"),
    shinycssloaders::withSpinner(
      plotly::plotlyOutput("user_detail_timeline_plot")
    )
  ),
  bslib::card(
    card_header_with_download("Sessions", "download_user_detail_sessions"),
    shinycssloaders::withSpinner(
      DT::dataTableOutput("user_detail_sessions_table")
    )
  )
)

sessions_user_detail_server <- function(
  input,
  output,
  session,
  duration_data,
  user_list_data
) {
  # Populate the searchable user dropdown (single user at a time, no "All").
  # Distinct users are computed in Arrow; options are labeled with usernames
  # from the latest user list snapshot. The option value is the GUID and the
  # input searches both fields, so users can be found by name or GUID even
  # though only the username is shown. Users with no matching username fall
  # back to showing their GUID as the label.
  shiny::observe({
    ds <- duration_data()
    if (is.null(ds)) {
      return()
    }

    users_df <- collect_or_null(
      ds |> dplyr::distinct(.data$user_guid),
      "session users"
    )
    if (is.null(users_df) || nrow(users_df) == 0) {
      return()
    }

    users <- sort(users_df$user_guid)
    labels <- users
    lookup <- username_lookup(user_list_data())
    if (!is.null(lookup)) {
      idx <- match(users, lookup$user_guid)
      found <- !is.na(idx)
      labels[found] <- lookup$username[idx[found]]
    }

    current <- shiny::isolate(input$user_detail_user)
    selected <- if (!is.null(current) && current %in% users) {
      current
    } else {
      users[1]
    }

    shiny::updateSelectizeInput(
      session,
      "user_detail_user",
      choices = stats::setNames(users, labels),
      selected = selected,
      server = TRUE
    )
  })

  # Session rows for the selected user — the user_guid filter is pushed down
  # to Arrow, so only that user's rows are ever collected. Scopes every
  # output on this page: value boxes, timeline, and the sessions table.
  user_sessions <- shiny::reactive({
    ds <- duration_data()
    if (is.null(ds)) {
      return(NULL)
    }
    shiny::req(input$user_detail_user)
    selected_user <- input$user_detail_user
    data <- collect_or_null(
      ds |> dplyr::filter(.data$user_guid == selected_user),
      "sessions for selected user"
    )
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }

    data <- data |> dplyr::select(-dplyr::any_of("username"))
    lookup <- username_lookup(user_list_data())
    if (!is.null(lookup)) {
      data <- data |> dplyr::left_join(lookup, by = "user_guid")
    } else {
      data$username <- NA_character_
    }
    data |>
      dplyr::mutate(
        username = ifelse(is.na(.data$username), "(unknown)", .data$username)
      )
  })

  # Totals for the selected user
  output$user_detail_sessions_value <- shiny::renderText({
    data <- user_sessions()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    prettyNum(nrow(data), big.mark = ",")
  })

  output$user_detail_hours_value <- shiny::renderText({
    data <- user_sessions()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    format_total_hours(sum(data$duration_seconds, na.rm = TRUE))
  })

  output$user_detail_median_value <- shiny::renderText({
    data <- user_sessions()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    format_duration_secs(stats::median(data$duration_seconds, na.rm = TRUE))
  })

  output$user_detail_last_active_value <- shiny::renderText({
    data <- user_sessions()
    if (is.null(data) || nrow(data) == 0) {
      return("-")
    }
    last_seen <- suppressWarnings(max(data$session_ended_at, na.rm = TRUE))
    if (!is.finite(as.numeric(last_seen))) {
      return("-")
    }
    format(last_seen, "%b %d, %Y")
  })

  # Gantt-style timeline: one horizontal segment per session, ordered by start
  # time, colored by session type.
  output$user_detail_timeline_plot <- plotly::renderPlotly({
    data <- user_sessions()
    if (is.null(data) || nrow(data) == 0) {
      return(sessions_empty_plot("No sessions for selected user", ""))
    }

    plot_data <- data |>
      dplyr::filter(
        !is.na(.data$session_started_at),
        !is.na(.data$session_ended_at)
      ) |>
      dplyr::arrange(.data$session_started_at) |>
      dplyr::mutate(lane = dplyr::row_number())

    if (nrow(plot_data) == 0) {
      return(sessions_empty_plot("No sessions for selected user", ""))
    }

    # format_duration_secs() is scalar; precompute the tooltip labels.
    plot_data$duration_label <- vapply(
      as.numeric(plot_data$duration_seconds),
      format_duration_secs,
      character(1)
    )

    types <- sort(unique(plot_data$session_type))
    pal <- stats::setNames(
      rep(SESSION_PALETTE, length.out = length(types)),
      types
    )

    p <- suppressWarnings(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = .data$session_started_at,
          xend = .data$session_ended_at,
          y = .data$lane,
          yend = .data$lane,
          color = .data$session_type,
          text = paste0(
            format(.data$session_started_at, "%B %d, %Y %H:%M"),
            "<br>",
            .data$session_type,
            "<br>Duration: ",
            .data$duration_label,
            "<br>Exit: ",
            .data$exit_reason
          )
        )
      ) +
        ggplot2::geom_segment(linewidth = 2) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "", y = "", color = "") +
        ggplot2::scale_color_manual(values = pal) +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank()
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

  # Session-level table for the selected user.
  user_detail_table_data <- shiny::reactive({
    data <- user_sessions()
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }

    data |>
      dplyr::arrange(dplyr::desc(.data$session_started_at)) |>
      dplyr::mutate(
        duration = vapply(
          as.numeric(.data$duration_seconds),
          format_duration_secs,
          character(1)
        ),
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
        "session_started_at",
        "session_ended_at",
        "session_type",
        "duration",
        "environment",
        "exit_reason"
      )
  })

  output$user_detail_sessions_table <- DT::renderDataTable({
    data <- user_detail_table_data()

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
      DT::datatable(
        colnames = c(
          "Username" = "username",
          "Started" = "session_started_at",
          "Ended" = "session_ended_at",
          "Session Type" = "session_type",
          "Duration" = "duration",
          "Environment" = "environment",
          "Exit Reason" = "exit_reason"
        ),
        # Fill the card width (autoWidth would shrink the table to fit its
        # content, leaving the card mostly empty).
        width = "100%",
        options = list(
          pageLength = 25,
          autoWidth = FALSE,
          scrollX = TRUE,
          # No built-in DataTables search box — each page provides at most
          # one search/select control of its own.
          dom = "lrtip"
        ),
        rownames = FALSE
      )
  })

  # Download handler for the user's sessions
  output$download_user_detail_sessions <- shiny::downloadHandler(
    filename = function() {
      paste0("chronicle_workbench_user_detail_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- user_sessions()
      if (is.null(data) || nrow(data) == 0) {
        data <- data.frame()
      }
      utils::write.csv(data, file, row.names = FALSE)
    }
  )
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
    bslib::nav_panel(
      "Duration",
      sessions_duration_ui,
      value = "sessions_duration"
    ),
    bslib::nav_panel(
      "User Summary",
      sessions_by_user_ui,
      value = "sessions_by_user"
    ),
    bslib::nav_panel(
      "User Detail",
      sessions_user_detail_ui,
      value = "sessions_user_detail"
    )
  )
)

# ==============================================
# Main Server
# ==============================================

server <- function(input, output, session) {
  # Latest user list snapshot (one date partition), shared by Users → User
  # List and the Sessions → User Summary / User Detail username lookups.
  # Deferred until a tab that needs it is first visited.
  should_load_user_list <- shiny::reactiveVal(FALSE)
  shiny::observe({
    user_list_tabs <- c(
      "user_list",
      "sessions_by_user",
      "sessions_user_detail"
    )
    if (!should_load_user_list() && input$main_nav %in% user_list_tabs) {
      should_load_user_list(TRUE)
    }
  })

  user_list_data <- shiny::reactive({
    shiny::req(should_load_user_list())
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

  # Users → Overview
  users_overview_server(input, output, session)

  # Users → User List
  user_list_server(input, output, session, user_list_data)

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

  # Session-level duration data as a LAZY Arrow query (date-range filtered,
  # never collected here) shared by Sessions → Overview, Duration, and User
  # Detail. The dataset can be very large in real deployments, so each
  # consumer pushes the narrowest possible filters/aggregations down to Arrow
  # and collects only what it needs (e.g. one user's rows, or a grouped
  # summary). Creation is deferred until one of those tabs is first visited;
  # the date range expands when a selector extends beyond it.
  should_load_duration <- shiny::reactiveVal(FALSE)
  shiny::observe({
    duration_tabs <- c(
      "sessions_overview",
      "sessions_duration",
      "sessions_user_detail"
    )
    if (!should_load_duration() && input$main_nav %in% duration_tabs) {
      should_load_duration(TRUE)
    }
  })

  duration_initial_range <- if (!is.null(data_window_cutoff)) {
    list(min = data_window_cutoff, max = Sys.Date())
  }
  duration_range <- shiny::reactiveVal(duration_initial_range)

  duration_data <- shiny::reactive({
    shiny::req(should_load_duration())
    range <- duration_range()
    tryCatch(
      {
        ds <- chronicle_data("workbench/session_duration", base_path)
        if (!is.null(range)) {
          range_min <- range$min
          range_max <- range$max
          ds <- ds |> dplyr::filter(date >= range_min, date <= range_max)
        }
        ds
      },
      error = function(e) {
        message("Error loading session duration: ", e$message)
        NULL
      }
    )
  })

  shiny::observe({
    overview_val <- input$sessions_overview_date_range
    duration_val <- input$sessions_duration_date_range
    date_vals <- c(overview_val, duration_val)
    date_vals <- date_vals[!is.na(date_vals)]
    shiny::req(length(date_vals) > 0)
    range <- duration_range()
    if (is.null(range)) {
      return()
    }
    new_min <- min(range$min, date_vals)
    new_max <- max(range$max, date_vals)
    if (new_min < range$min || new_max > range$max) {
      duration_range(list(min = new_min, max = new_max))
    }
  })

  sessions_overview_server(
    input,
    output,
    session,
    sessions_data,
    duration_data
  )

  # Sessions → Duration
  sessions_duration_server(input, output, session, duration_data)

  # Sessions → User Detail
  sessions_user_detail_server(
    input,
    output,
    session,
    duration_data,
    user_list_data
  )

  # Sessions → User Summary: load the by-user totals deferred until visited.
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
    should_load_sessions_by_user,
    user_list_data
  )
}

shinyApp(ui, server)
