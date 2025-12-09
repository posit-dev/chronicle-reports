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
      bslib::card_header("User Trends Over Time"),
      shinycssloaders::withSpinner(plotly::plotlyOutput("users_trend_plot"))
    ),
    bslib::card(
      bslib::card_header("Average Users by Day of Week"),
      shinycssloaders::withSpinner(shiny::plotOutput("users_dow_plot"))
    )
  )
)

users_overview_server <- function(input, output, session) {
  # Load user_totals data
  users_data <- shiny::reactive({
    tryCatch(
      {
        base_path <- shiny::getShinyOption("base_path")
        chr_get_curated_metric_data("workbench/user_totals", base_path)
      },
      error = function(e) {
        shiny::showNotification(
          paste("Error loading user totals:", e$message),
          type = "error",
          duration = NULL
        )
        NULL
      }
    )
  })

  # Set default date range when data loads
  shiny::observe({
    shiny::req(users_data())

    date_summary <- users_data() |>
      dplyr::filter(!is.na(date)) |>
      dplyr::summarise(
        min_date = min(date, na.rm = TRUE),
        max_date = max(date, na.rm = TRUE)
      ) |>
      dplyr::collect()

    shiny::updateDateRangeInput(
      session,
      "users_overview_date_range",
      start = date_summary$min_date,
      end = date_summary$max_date,
      min = date_summary$min_date,
      max = date_summary$max_date
    )
  })

  # Get latest data (for value boxes - always max_date)
  latest_users_data <- shiny::reactive({
    shiny::req(users_data())
    users_data() |>
      dplyr::arrange(dplyr::desc(date)) |>
      dplyr::collect() |>
      dplyr::slice(1)
  })

  # Filter data by date range (for charts only)
  filtered_users_data <- shiny::reactive({
    shiny::req(users_data(), input$users_overview_date_range)

    users_data() |>
      dplyr::filter(
        date >= input$users_overview_date_range[1],
        date <= input$users_overview_date_range[2]
      ) |>
      dplyr::collect()
  })

  # Value boxes (always latest data)
  output$users_licensed_value <- shiny::renderText({
    shiny::req(latest_users_data())
    prettyNum(latest_users_data()$named_users, big.mark = ",")
  })

  output$users_daily_value <- shiny::renderText({
    shiny::req(latest_users_data())
    prettyNum(latest_users_data()$active_users_1day, big.mark = ",")
  })

  output$users_admins_value <- shiny::renderText({
    shiny::req(latest_users_data())
    prettyNum(latest_users_data()$administrators, big.mark = ",")
  })

  output$users_super_admins_value <- shiny::renderText({
    shiny::req(latest_users_data())
    prettyNum(latest_users_data()$super_administrators, big.mark = ",")
  })

  # Trend chart (filtered data)
  output$users_trend_plot <- plotly::renderPlotly({
    shiny::req(filtered_users_data())

    plot_data <- filtered_users_data() |>
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
      ggplot2::labs(x = "", y = "Number of Users", color = "") +
      ggplot2::scale_color_manual(
        values = c(
          "Licensed Users" = BRAND_COLORS$BLUE,
          "Daily Users" = BRAND_COLORS$GREEN,
          "Admins" = BRAND_COLORS$BURGUNDY,
          "Super Admins" = BRAND_COLORS$GRAY
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
  output$users_dow_plot <- shiny::renderPlot({
    shiny::req(filtered_users_data())

    day_summary <- filtered_users_data() |>
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
      ggplot2::labs(x = "", y = "Average Number of Users")
  })
}

# ==============================================
# Users → User List UI/Server
# ==============================================

users_list_ui <- bslib::card(
  bslib::card_header("Filters"),
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    shiny::selectInput(
      "users_list_role",
      "Role:",
      choices = c("All", "user", "administrator", "super_administrator")
    ),
    shiny::selectInput(
      "users_list_active",
      "Active Today:",
      choices = c("All", "Yes", "No")
    ),
    shiny::textInput(
      "users_list_search",
      "Search:",
      placeholder = "Username or email"
    )
  ),
  shinycssloaders::withSpinner(
    DT::dataTableOutput("users_list_table")
  )
)

users_list_server <- function(input, output, session) {
  # Load user_list data (snapshot at max_date)
  users_list_data <- shiny::reactive({
    tryCatch(
      {
        base_path <- shiny::getShinyOption("base_path")
        data <- chr_get_curated_metric_data("workbench/user_list", base_path)

        # Get max_date snapshot
        data |>
          dplyr::arrange(dplyr::desc(date)) |>
          dplyr::collect() |>
          dplyr::filter(date == max(date, na.rm = TRUE))
      },
      error = function(e) {
        shiny::showNotification(
          paste("Error loading user list:", e$message),
          type = "error",
          duration = NULL
        )
        NULL
      }
    )
  })

  # Apply filters
  filtered_users_list <- shiny::reactive({
    shiny::req(users_list_data())

    data <- users_list_data()

    # Role filter
    if (input$users_list_role != "All") {
      data <- data |> dplyr::filter(.data$user_role == input$users_list_role)
    }

    # Active today filter
    if (input$users_list_active != "All") {
      active_val <- input$users_list_active == "Yes"
      data <- data |> dplyr::filter(.data$active_today == active_val)
    }

    # Search filter
    if (nzchar(input$users_list_search)) {
      search_term <- tolower(input$users_list_search)
      data <- data |>
        dplyr::filter(
          grepl(search_term, tolower(.data$username)) |
            grepl(search_term, tolower(.data$email))
        )
    }

    data
  })

  # Render table
  output$users_list_table <- DT::renderDataTable({
    shiny::req(filtered_users_list())

    data <- filtered_users_list()
    data |>
      dplyr::select(
        "username",
        "user_role",
        "last_active_at",
        "active_today"
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
}

# ==============================================
# Main UI (page_navbar with one dropdown)
# ==============================================

workbench_app_ui <- bslib::page_navbar(
  title = "Posit Workbench Dashboard",
  theme = bslib::bs_theme(preset = "shiny"),
  fillable = FALSE,

  # Users dropdown
  bslib::nav_menu(
    "Users",
    bslib::nav_panel("Overview", users_overview_ui),
    bslib::nav_panel("User List", users_list_ui)
  )
)

# ==============================================
# Main Server
# ==============================================

workbench_app_server <- function(input, output, session) {
  # Users → Overview
  users_overview_server(input, output, session)

  # Users → User List
  users_list_server(input, output, session)
}

# ==============================================
# Exported App Function
# ==============================================

#' Run the Posit Workbench Dashboard Shiny App
#'
#' This comprehensive dashboard provides analytics for Posit Workbench Users
#'
#' **Current Implementation Status:**
#' - **Users** section: Fully functional with real Chronicle data
#'   - Overview: User totals and trends from `workbench/user_totals`
#'   - WIP User List: Detailed user list from `workbench/user_list`
#'
#' **Navigation Structure:**
#' - Users → Overview, User List
#'
#' **Filter Behavior:**
#' - Date range filters apply ONLY to trend charts
#' - Value boxes always show latest (max_date) values
#' - List tables always show max_date snapshot (ignore date filters)
#' - Attribute filters (role, type, etc.) apply only to their specific tables
#'
#' @param base_path The base path where Chronicle data files are stored.
#'   Defaults to the value of the `CHRONICLE_BASE_PATH` environment variable,
#'   or `"/var/lib/posit-chronicle/data"` if not set.
#'   This path should be accessible by the Shiny app.
#'
#' @return A Shiny app object that can be run or deployed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run locally with filesystem path
#' workbench_app("/path/to/chronicle/data")
#'
#' # Run with S3 path
#' workbench_app("s3://chronicle-bucket/data")
#' }
workbench_app <- function(
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", APP_CONFIG$DEFAULT_BASE_PATH)
) {
  shiny::shinyOptions(base_path = base_path)
  shiny::shinyApp(workbench_app_ui, workbench_app_server)
}
