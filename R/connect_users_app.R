#' Process the connect_users data to compute daily metrics used for historical trends
#'
#' @importFrom rlang .data
#' @param data A data frame or tibble containing the raw connect_users data.
calculate_connect_daily_user_counts <- function(data) {
  daily_user_counts <- data |>
    # First get latest state per user per date
    arrow::to_duckdb() |>
    dplyr::group_by(.data$date, .data$id) |>
    dplyr::slice_max(timestamp, n = 1) |>
    dplyr::ungroup() |>
    # Filter out users inactive for more than a year
    # Here, `date` is the date on which the metric was collected.
    arrow::to_arrow() |>
    dplyr::collect() |>
    dplyr::filter(
      is.na(.data$last_active_at) |
        as.Date(.data$last_active_at) >=
          date - APP_CONFIG$INACTIVE_USER_THRESHOLD
    ) |>
    # Then calculate daily user counts for each date
    dplyr::group_by(date) |>
    dplyr::summarise(
      # Any unlocked user active in the last year (computed above) is counted
      # as a licensed user
      licensed_users = dplyr::n_distinct(.data$id[
        !is.na(.data$created_at) &
          as.Date(.data$created_at) <= date &
          !.data$locked
      ]),
      # Daily users are those active on a given date
      daily_users = dplyr::n_distinct(
        .data$id[
          !is.na(.data$last_active_at) &
            as.Date(.data$last_active_at) == date &
            !.data$locked
        ]
      ),
      # Publishers are those with role publisher or admin
      publishers = dplyr::n_distinct(
        .data$id[
          !is.na(.data$created_at) &
            as.Date(.data$created_at) <= date &
            .data$user_role %in%
              c("publisher", "admin") &
            !.data$locked
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
  title = "Posit Connect Users Dashboard",
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
      theme = bslib::value_box_theme(bg = CONNECT_COLORS$LICENSED_USERS)
    ),
    bslib::value_box(
      title = "Daily Users",
      max_height = "120px",
      value = shiny::textOutput("daily_users_value"),
      theme = bslib::value_box_theme(bg = CONNECT_COLORS$DAILY_USERS)
    ),
    bslib::value_box(
      title = "Publishers",
      max_height = "120px",
      value = shiny::textOutput("publishers_value"),
      theme = bslib::value_box_theme(bg = CONNECT_COLORS$PUBLISHERS)
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

    # Daily activity pattern chart
    bslib::card(
      bslib::card_header("Average Daily Users by Day of Week"),
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
        base_path <- shiny::getShinyOption("base_path")

        data <- chr_get_metric_data("connect_users", base_path, "daily") |>
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
    calculate_connect_daily_user_counts(raw_data())
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

  # Get most recent day's data from filtered data
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
  output$publishers_value <- shiny::renderText({
    shiny::req(latest_data())
    prettyNum(latest_data()$publishers, big.mark = ",")
  })

  # Plot for user trends over time
  output$user_trend_plot <- plotly::renderPlotly({
    shiny::req(filtered_data())

    # Reshape data for easier plotting
    plot_data <- filtered_data() |>
      dplyr::select("date", "licensed_users", "daily_users", "publishers") |>
      dplyr::filter(
        !is.na(date),
        !is.na(.data$licensed_users) |
          !is.na(.data$daily_users) |
          !is.na(.data$publishers)
      ) |>
      tidyr::pivot_longer(-date, names_to = "metric", values_to = "value") |>
      dplyr::filter(!is.na(.data$value), is.finite(.data$value)) |>
      dplyr::arrange(date) |>
      dplyr::mutate(
        metric = factor(
          .data$metric,
          levels = c("licensed_users", "daily_users", "publishers"),
          labels = c("Licensed Users", "Daily Users", "Publishers")
        )
      )

    # Validate we have data to plot
    if (nrow(plot_data) == 0) {
      return(
        plotly::plotly_empty() |>
          plotly::layout(title = "No data available for selected date range")
      )
    }

    # Plot the trend of user counts over time
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = date, y = .data$value, color = .data$metric)
    ) +
      ggplot2::geom_line(linewidth = 0.5) +
      # add points with custom hover text
      ggplot2::geom_point(
        ggplot2::aes(
          # This code produces a warning about `text` being an unknown aesthetic.
          # This is normal when using ggplotly(). The text aesthetic is not a
          # standard ggplot2 aesthetic, but ggplotly() recognizes and uses it
          # for hover tooltips.
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
        color = "",
      ) +
      # The line colors should match the value box colors
      ggplot2::scale_color_manual(
        values = c(
          "Licensed Users" = CONNECT_COLORS$LICENSED_USERS,
          "Daily Users" = CONNECT_COLORS$DAILY_USERS,
          "Publishers" = CONNECT_COLORS$PUBLISHERS
        )
      )

    # Convert to plotly with custom hover
    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        # make the legend look pretty
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
        y = "Average Number of Users",
      )
  })
}


#' Run the Connect Users Dashboard Shiny App
#'
#' @param base_path the base path where Chronicle data files are stored.
#'   Defaults to the value of the `CHRONICLE_BASE_PATH` environment variable,
#'   or `"/var/lib/posit-chronicle/data"` if the environment variable is not set.
#'   This path should be accessible by the Shiny app.
#'   If you deploy this app to Posit Connect, you can set this environment variable
#'   in the Connect UI. See
#'   https://docs.posit.co/connect/user/content-settings/#content-vars
#'   for more information.
#'
#' @export
#'
#' @examples
#' connect_users_app()
connect_users_app <- function(
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", APP_CONFIG$DEFAULT_BASE_PATH)
) {
  # The base path where Chronicle data files are stored. If you deploy this app
  # to Posit Connect, you can set this environment variable in the Connect
  # UI. See https://docs.posit.co/connect/user/content-settings/#content-vars
  # for more information.
  shiny::shinyOptions(base_path = base_path)

  shiny::shinyApp(ui, server)
}
