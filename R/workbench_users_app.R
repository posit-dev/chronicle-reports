chr_path <- function(
  base_path,
  metric = NULL,
  frequency = c("daily", "hourly")
) {
  frequency <- match.arg(frequency)
  glue::glue("{base_path}/{frequency}/v2/{metric}/")
}

chr_get_metric_data <- function(
  metric,
  base_path,
  frequency = c("daily", "hourly"),
  ymd = NULL,
  schema = NULL
) {
  frequency <- match.arg(frequency)
  path <- chr_path(base_path, metric, frequency)

  if (!is.null(ymd)) {
    path <- glue::glue("{path}{ymd[['year']]}/{ymd[['month']]}/{ymd[['day']]}/")
    partitioning <- NULL
  } else {
    partitioning <- c("Year", "Month", "Day")
  }

  arrow::open_dataset(
    path,
    hive_style = FALSE,
    schema = schema,
    format = "parquet",
    partitioning = partitioning
  )
}

# Color constants
BRAND_COLORS <- list(
  # Brand colors
  BLUE = "#447099",
  GREEN = "#72994E",
  BURGUNDY = "#9A4665"
)

COLORS <- list(
  # Semantic mappings
  LICENSED_USERS = BRAND_COLORS$BLUE,
  DAILY_USERS = BRAND_COLORS$GREEN,
  ADMIN_USERS = BRAND_COLORS$BURGUNDY
)

#' Process the pwb_users data to compute daily metrics used for historical trends
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
      # Admin users are admin or super admin users
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
workbench_users_ui <- bslib::page_fluid(
  title = "Posit Workbench Users",
  theme = bslib::bs_theme(
    preset = "shiny",
    primary = BRAND_COLORS$BLUE,
    "card-cap-bg" = BRAND_COLORS$BLUE,
    "card-cap-color" = "#fff"
  ),

  # Title card to display the report name
  bslib::layout_columns(
    col_widths = 12,
    bslib::card(
      bslib::card_header("Posit Workbench Users")
    )
  ),

  # Summary row with current values for each user metric
  bslib::layout_columns(
    col_widths = c(4, 4),
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
workbench_users_server <- function(input, output, session) {
  # Read data once at startup with error handling
  raw_data <- shiny::reactive({
    tryCatch(
      {
        base_path <- shiny::getShinyOption("base_path")

        data <- chr_get_metric_data("pwb_users", base_path, "daily") |>
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

  # Get most recent day's data
  latest_data <- shiny::reactive({
    shiny::req(data())
    data() |>
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
    shiny::req(data())

    plot_data <- data() |>
      dplyr::select(date, licensed_users, daily_users, admin_users) |>
      tidyr::pivot_longer(-date, names_to = "metric", values_to = "value") |>
      dplyr::mutate(
        metric = factor(
          .data$metric,
          levels = c("licensed_users", "daily_users", "admin_users"),
          labels = c("Licensed Users", "Daily Users", "Admin Users")
        )
      )

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
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center"
        )
      )
  })

  # Plot the average daily activity pattern by day of week
  output$activity_pattern_plot <- shiny::renderPlot({
    shiny::req(data())

    # Calculate average users active by day of week
    day_summary <- data() |>
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

#' Run the Workbench Users Dashboard Shiny App
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
#' workbench_users_app()
workbench_users_app <- function(
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", "/var/lib/posit-chronicle/data")
) {
  # The base path where Chronicle data files are stored. If you deploy this app
  # to Posit Connect, you can set this environment variable in the Connect
  # UI. See https://docs.posit.co/connect/user/content-settings/#content-vars
  # for more information.
  shiny::shinyOptions(base_path = base_path)

  shiny::shinyApp(workbench_users_ui, workbench_users_server)
}
