# Connect-specific color mappings
COLORS <- list(
  LICENSED_USERS = BRAND_COLORS$BLUE,
  DAILY_USERS = BRAND_COLORS$GREEN,
  PUBLISHERS = BRAND_COLORS$BURGUNDY
)

#' Process the connect_users data to compute daily metrics used for historical trends
#'
#' @importFrom rlang .data
#' @param data A data frame or tibble containing the raw connect_users data.
#' @noRd
calculate_connect_totals_user_counts <- function(data) {
  daily_user_counts <- data |>
    dplyr::collect()

  # Implicit return
  daily_user_counts
}


# ==============================================
# Define the UI layout
# ==============================================
connect_user_totals_ui <- bslib::page_sidebar(
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
      theme = bslib::value_box_theme(bg = COLORS$LICENSED_USERS)
    ),
    bslib::value_box(
      title = "Daily Users",
      max_height = "120px",
      value = shiny::textOutput("daily_users_value"),
      theme = bslib::value_box_theme(bg = COLORS$DAILY_USERS)
    ),
    bslib::value_box(
      title = "Publishers",
      max_height = "120px",
      value = shiny::textOutput("publishers_value"),
      theme = bslib::value_box_theme(bg = COLORS$PUBLISHERS)
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
      bslib::card_header("Average Users by Day of Week"),
      shinycssloaders::withSpinner(shiny::plotOutput("activity_pattern_plot"))
    )
  )
)

# ==============================================
# Define the server logic
# ==============================================
connect_user_totals_server <- function(input, output, session) {
  # Read data once at startup with error handling
  raw_data <- shiny::reactive({
    tryCatch(
      {
        base_path <- shiny::getShinyOption("base_path")

        data <- chr_get_curated_metric_data(
          "connect/user_totals",
          base_path
        ) |>
          dplyr::mutate(date = as.Date(date))

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
    calculate_connect_totals_user_counts(raw_data())
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
    prettyNum(latest_data()$named_users, big.mark = ",")
  })
  output$daily_users_value <- shiny::renderText({
    shiny::req(latest_data())
    prettyNum(latest_data()$active_users_1day, big.mark = ",")
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
      dplyr::select("date", "named_users", "active_users_1day", "publishers") |>
      dplyr::filter(
        !is.na(date),
        !is.na(.data$named_users) |
          !is.na(.data$active_users_1day) |
          !is.na(.data$publishers)
      ) |>
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
          "Licensed Users" = COLORS$LICENSED_USERS,
          "Daily Users" = COLORS$DAILY_USERS,
          "Publishers" = COLORS$PUBLISHERS
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
        avg_active_users = mean(.data$active_users_1day, na.rm = TRUE),
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
#' @return A Shiny app object that can be run or deployed.
#'
#' @export
#'
#' @examples
#' connect_users_app(base_path = "/path/to/chronicle/data")
connect_users_totals_app <- function(
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", APP_CONFIG$DEFAULT_BASE_PATH)
) {
  # The base path where Chronicle data files are stored. If you deploy this app
  # to Posit Connect, you can set this environment variable in the Connect
  # UI. See https://docs.posit.co/connect/user/content-settings/#content-vars
  # for more information.
  shiny::shinyOptions(base_path = base_path)

  shiny::shinyApp(connect_user_totals_ui, connect_user_totals_server)
}
