# Posit Workbench Users Dashboard
# This app analyzes user activity on Posit Workbench

library(shiny)
library(bslib)
library(chronicle.reports)
library(rlang)

# Brand colors
BRAND_COLORS <- list(
  BLUE = "#447099",
  GREEN = "#72994E",
  BURGUNDY = "#9A4665",
  GRAY = "#404041"
)

# Load app constants
source("../constants.R")

# Get base path from environment variable
base_path <- Sys.getenv(
  "CHRONICLE_BASE_PATH",
  APP_CONFIG$DEFAULT_BASE_PATH
)

# Workbench-specific color mappings
COLORS <- list(
  # Semantic mappings
  LICENSED_USERS = BRAND_COLORS$BLUE,
  DAILY_USERS = BRAND_COLORS$GREEN,
  ADMIN_USERS = BRAND_COLORS$BURGUNDY
)

#' Process the pwb_users data to compute daily metrics used for historical trends
#' @noRd
#'
#' @importFrom rlang .data
#' @param data A data frame or tibble containing the raw pwb_users data.
calculate_workbench_daily_user_counts <- function(data) {
  daily_user_counts <- data |>
    # First get latest state per user per date
    arrow::to_duckdb() |>
    dplyr::group_by(.data$date, .data$username) |>
    dplyr::slice_max(timestamp, n = 1) |>
    dplyr::ungroup() |>
    arrow::to_arrow() |>
    dplyr::collect() |>
    # Then calculate daily user counts for each date
    dplyr::group_by(date) |>
    dplyr::summarise(
      # Any "Active" user is counted as a licensed user
      licensed_users = dplyr::n_distinct(.data$username[
        !is.na(.data$created_at) &
          as.Date(.data$created_at) <= date &
          .data$status == "Active"
      ]),
      # Daily users are those active on a given date
      daily_users = dplyr::n_distinct(
        .data$username[
          !is.na(.data$last_active_at) &
            as.Date(.data$last_active_at) == date &
            .data$status == "Active"
        ]
      ),
      # Admin or super admin users
      admin_users = dplyr::n_distinct(
        .data$username[
          .data$status == "Active" &
            (.data$is_admin | .data$is_super_admin)
        ]
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(date)

  # Implicit return
  daily_user_counts
}

# ==============================================
# Define the UI layout
# ==============================================
ui <- bslib::page_sidebar(
  title = "Posit Workbench Users Dashboard",
  theme = bslib::bs_theme(preset = "shiny"),
  fillable = FALSE, # helps with vertical spacing

  # Sidebar with date range input
  sidebar = bslib::sidebar(
    title = "Filters",
    width = "270px",
    shiny::dateRangeInput(
      "date_range",
      "Select Date Range:",
      start = NULL, # Will be set dynamically based on data
      end = NULL, # Will be set dynamically based on data
      format = "yyyy-mm-dd"
    )
  ),

  # Summary row with current values for each user metric
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    bslib::value_box(
      title = "Licensed Users",
      max_height = "120px",
      value = shiny::textOutput("licensed_users_value"),
      theme = bslib::value_box_theme(bg = COLORS$LICENSED_USERS)
    ),
    bslib::value_box(
      title = "Daily Users",
      max_height = "120px",
      value = shiny::textOutput("daily_users_value"),
      theme = bslib::value_box_theme(bg = COLORS$DAILY_USERS)
    ),
    bslib::value_box(
      title = "Admin Users",
      max_height = "120px",
      value = shiny::textOutput("admin_users_value"),
      theme = bslib::value_box_theme(bg = COLORS$ADMIN_USERS)
    )
  ),

  # Charts row for historical trends
  bslib::layout_columns(
    col_widths = c(6, 6),

    # User trends chart
    bslib::card(
      bslib::card_header("User Trends Over Time"),
      shinycssloaders::withSpinner(plotly::plotlyOutput("user_trend_plot"))
    ),

    # Users by days of the week chart
    bslib::card(
      bslib::card_header("Average Users by Day of Week"),
      shinycssloaders::withSpinner(shiny::plotOutput("activity_pattern_plot"))
    )
  )
)

# ==============================================
# Define the server logic
# ==============================================
server <- function(input, output, session) {
  # Read data once at startup with error handling
  raw_data <- shiny::reactive({
    tryCatch(
      {
        data <- chronicle_raw_data("pwb_users", base_path, "daily") |>
          dplyr::mutate(date = as.Date(timestamp))
        return(data)
      },
      error = function(e) {
        shiny::showNotification(
          e$message,
          type = "error",
          duration = NULL
        )
        # Implicit return
        NULL
      }
    )
  })

  # Process data for metrics
  data <- shiny::reactive({
    shiny::req(raw_data())
    calculate_workbench_daily_user_counts(raw_data())
  })

  # Set default date range when data is loaded
  shiny::observe({
    shiny::req(data())
    date_range <- range(data()$date, na.rm = TRUE)

    shiny::updateDateRangeInput(
      session,
      "date_range",
      start = date_range[1],
      end = date_range[2],
      min = date_range[1],
      max = date_range[2]
    )
  })

  # Filter data based on selected date range
  filtered_data <- shiny::reactive({
    shiny::req(data(), input$date_range)

    data() |>
      dplyr::filter(
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
  })

  # Get most recent day's data
  latest_data <- shiny::reactive({
    shiny::req(filtered_data())
    filtered_data() |>
      dplyr::slice_max(date, n = 1)
  })

  # Get the latest values for each metric
  output$licensed_users_value <- shiny::renderText({
    shiny::req(latest_data())
    prettyNum(latest_data()$licensed_users, big.mark = ",")
  })
  output$daily_users_value <- shiny::renderText({
    shiny::req(latest_data())
    prettyNum(latest_data()$daily_users, big.mark = ",")
  })
  output$admin_users_value <- shiny::renderText({
    shiny::req(latest_data())
    prettyNum(latest_data()$admin_users, big.mark = ",")
  })

  # Plot for user trends over time
  output$user_trend_plot <- plotly::renderPlotly({
    shiny::req(filtered_data())

    plot_data <- filtered_data() |>
      dplyr::select("date", "licensed_users", "daily_users", "admin_users") |>
      tidyr::pivot_longer(-date, names_to = "metric", values_to = "value") |>
      dplyr::mutate(
        metric = factor(
          .data$metric,
          levels = c("licensed_users", "daily_users", "admin_users"),
          labels = c("Licensed Users", "Daily Users", "Admin Users")
        )
      )

    # Validate we have data to plot
    if (nrow(plot_data) == 0) {
      return(
        plotly::plotly_empty() |>
          plotly::layout(title = "No data available for selected date range")
      )
    }

    p <- ggplot2::ggplot(
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
      ggplot2::labs(
        x = "",
        y = "Number of Users",
        color = ""
      ) +
      ggplot2::scale_color_manual(
        values = c(
          "Licensed Users" = COLORS$LICENSED_USERS,
          "Daily Users" = COLORS$DAILY_USERS,
          "Admin Users" = COLORS$ADMIN_USERS
        )
      )

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        xaxis = list(fixedrange = TRUE), # Disable x-axis zoom/pan
        yaxis = list(fixedrange = TRUE), # Disable y-axis zoom/pan
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center"
        )
      ) |>
      plotly::config(
        displayModeBar = FALSE # Hide the toolbar completely
      )
  })

  # Plot the average daily activity pattern by day of week
  output$activity_pattern_plot <- shiny::renderPlot({
    shiny::req(filtered_data())

    # Calculate average users active by day of week
    day_summary <- filtered_data() |>
      dplyr::mutate(
        day_of_week = lubridate::wday(date, label = TRUE, abbr = FALSE)
      ) |>
      dplyr::group_by(.data$day_of_week) |>
      dplyr::summarise(
        avg_active_users = mean(.data$daily_users, na.rm = TRUE),
        .groups = "drop"
      )

    ggplot2::ggplot(
      day_summary,
      ggplot2::aes(x = .data$day_of_week, y = .data$avg_active_users)
    ) +
      ggplot2::geom_col(fill = BRAND_COLORS$BLUE) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        x = "",
        y = "Average Number of Users"
      )
  })
}

shinyApp(ui, server)
