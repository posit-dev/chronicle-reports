# Posit Workbench Dashboard
# Comprehensive dashboard providing analytics for Posit Workbench Users

library(shiny)
library(bslib)
library(chronicle.reports)
library(rlang)
library(arrow)

# Configure Arrow for optimal S3 performance
arrow::set_io_thread_count(6) # Parallel S3 downloads
arrow::set_cpu_count(4) # Parallel data processing

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

# Brand colors
BRAND_COLORS <- list(
  BLUE = "#447099",
  GREEN = "#72994E",
  BURGUNDY = "#9A4665",
  GRAY = "#404041"
)


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
      shinycssloaders::withSpinner(plotly::plotlyOutput("users_dow_plot"))
    )
  )
)

users_overview_server <- function(input, output, session, user_totals) {
  # Use shared user_totals data (error handling in main server)
  users_data <- user_totals

  # Set default date range when data loads
  shiny::observe({
    shiny::req(users_data())

    date_summary <- users_data() |>
      dplyr::filter(!is.na(date)) |>
      dplyr::summarise(
        min_date = min(date, na.rm = TRUE),
        max_date = max(date, na.rm = TRUE)
      )

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
    data <- users_data()
    if (is.null(data)) {
      return(NULL)
    }

    data |>
      dplyr::arrange(dplyr::desc(date)) |>
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

  # Trend chart (filtered data)
  output$users_trend_plot <- plotly::renderPlotly({
    data <- filtered_users_data()

    if (is.null(data) || nrow(data) == 0) {
      return(
        plotly::plotly_empty() |>
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

    plot_data <- data |>
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
  output$users_dow_plot <- plotly::renderPlotly({
    data <- filtered_users_data()

    if (is.null(data) || nrow(data) == 0) {
      return(
        plotly::plotly_empty() |>
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

    day_summary <- data |>
      dplyr::mutate(
        day_of_week = lubridate::wday(date, label = TRUE, abbr = FALSE)
      ) |>
      dplyr::group_by(.data$day_of_week) |>
      dplyr::summarise(
        avg_active_users = mean(.data$active_users_1day, na.rm = TRUE),
        .groups = "drop"
      )

    if (nrow(day_summary) == 0) {
      return(
        plotly::plotly_empty() |>
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
}

# ==============================================
# Users → User List UI/Server
# ==============================================

user_list_ui <- bslib::card(
  bslib::card_header("Filters"),
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

user_list_server <- function(input, output, session, user_list) {
  # Use shared user_list data (error handling in main server)
  user_list_data <- shiny::reactive({
    data <- user_list()
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }

    # Get max_date snapshot
    max_date <- max(data$date, na.rm = TRUE)

    data |>
      dplyr::filter(date == max_date)
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
}

# ==============================================
# Main UI (page_navbar with one dropdown)
# ==============================================

ui <- bslib::page_navbar(
  title = "Posit Workbench Dashboard",
  theme = bslib::bs_theme(preset = "shiny"),
  fillable = FALSE,

  # Users dropdown
  bslib::nav_menu(
    "Users",
    bslib::nav_panel("Overview", users_overview_ui),
    bslib::nav_panel("User List", user_list_ui)
  )
)

# ==============================================
# Main Server
# ==============================================

# Helper function to safely open a dataset (lazy - no data transfer)
safe_open_dataset <- function(metric, base_path) {
  tryCatch(
    {
      chronicle_data(metric, base_path)
    },
    error = function(e) {
      message("Error opening dataset ", metric, ": ", e$message)
      NULL
    }
  )
}

server <- function(input, output, session) {
  # ============================================
  # Workbench Data - open datasets lazily (no S3 transfer yet)
  # ============================================

  # Open datasets once at startup (lazy - only lists files, no data transfer)
  user_totals_ds <- safe_open_dataset("workbench/user_totals", base_path)
  user_list_ds <- safe_open_dataset("workbench/user_list", base_path)

  # ============================================
  # Reactive wrappers that collect data
  # ============================================

  # User totals - collected once
  all_user_totals <- shiny::reactive({
    if (is.null(user_totals_ds)) return(NULL)
    tryCatch(
      user_totals_ds |> dplyr::collect(),
      error = function(e) {
        message("Error collecting user totals: ", e$message)
        NULL
      }
    )
  }) |> shiny::bindCache("user_totals")

  # User list - collected once
  all_user_list <- shiny::reactive({
    if (is.null(user_list_ds)) return(NULL)
    tryCatch(
      user_list_ds |> dplyr::collect(),
      error = function(e) {
        message("Error collecting user list: ", e$message)
        NULL
      }
    )
  }) |> shiny::bindCache("user_list")

  # ============================================
  # Call sub-servers with data
  # ============================================
  users_overview_server(input, output, session, all_user_totals)
  user_list_server(input, output, session, all_user_list)
}

shinyApp(ui, server)
