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
        chr_get_curated_metric_data("connect/user_totals", base_path)
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

  output$users_publishers_value <- shiny::renderText({
    shiny::req(latest_users_data())
    prettyNum(latest_users_data()$publishers, big.mark = ",")
  })

  # Trend chart (filtered data)
  output$users_trend_plot <- plotly::renderPlotly({
    shiny::req(filtered_users_data())

    plot_data <- filtered_users_data() |>
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
          "Publishers" = BRAND_COLORS$BURGUNDY
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
    col_widths = c(3, 3, 3, 3),
    shiny::selectInput(
      "users_list_environment",
      "Environment:",
      choices = c("All")
    ),
    shiny::selectInput(
      "users_list_role",
      "Role:",
      choices = c("All", "publisher", "viewer", "administrator")
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
        data <- chr_get_curated_metric_data("connect/user_list", base_path)

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

  # Populate environment filter dynamically
  shiny::observe({
    shiny::req(users_list_data())

    # Get unique environment values
    env_values <- users_list_data() |>
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
    shiny::req(users_list_data())

    data <- users_list_data()

    # Environment filter
    if (input$users_list_environment != "All") {
      if (input$users_list_environment == "(Not Set)") {
        data <- data |>
          dplyr::filter(
            is.na(.data$environment) |
              .data$environment == "" |
              .data$environment == " "
          )
      } else {
        data <- data |>
          dplyr::filter(.data$environment == input$users_list_environment)
      }
    }

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
        "email",
        "first_name",
        "last_name",
        "environment",
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
# Content → Overview UI/Server (PLACEHOLDER)
# ==============================================

content_overview_ui <- bslib::card(
  bslib::card_header(
    shiny::tags$div(
      shiny::tags$span("PLACEHOLDER DATA - Content Overview"),
      style = "color: #9A4665; font-weight: bold;"
    )
  ),
  shiny::dateRangeInput(
    "content_overview_date_range",
    "Date Range:",
    start = Sys.Date() - 90,
    end = Sys.Date(),
    format = "yyyy-mm-dd"
  ),
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    bslib::value_box(
      title = "Total Content",
      max_height = "120px",
      value = shiny::textOutput("content_total_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BLUE)
    ),
    bslib::value_box(
      title = "New Content",
      max_height = "120px",
      value = shiny::textOutput("content_new_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$GREEN)
    ),
    bslib::value_box(
      title = "Updated Content",
      max_height = "120px",
      value = shiny::textOutput("content_updated_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BURGUNDY)
    )
  ),
  bslib::card(
    bslib::card_header("Content Trends Over Time (PLACEHOLDER)"),
    shinycssloaders::withSpinner(plotly::plotlyOutput("content_trend_plot"))
  )
)

content_overview_server <- function(input, output, session) {
  # Generate placeholder data
  content_data <- shiny::reactive({
    data.frame(
      date = seq.Date(Sys.Date() - 90, Sys.Date(), by = "day"),
      total_content = 450 + cumsum(sample(-2:5, 91, replace = TRUE)),
      new_content = sample(0:5, 91, replace = TRUE),
      updated_content = sample(0:10, 91, replace = TRUE)
    )
  })

  # Value boxes (latest values)
  output$content_total_value <- shiny::renderText({
    data <- content_data()
    prettyNum(tail(data$total_content, 1), big.mark = ",")
  })

  output$content_new_value <- shiny::renderText({
    data <- content_data()
    prettyNum(tail(data$new_content, 1), big.mark = ",")
  })

  output$content_updated_value <- shiny::renderText({
    data <- content_data()
    prettyNum(tail(data$updated_content, 1), big.mark = ",")
  })

  # Trend chart
  output$content_trend_plot <- plotly::renderPlotly({
    data <- content_data()

    # Filter by date range
    data <- data |>
      dplyr::filter(
        date >= input$content_overview_date_range[1],
        date <= input$content_overview_date_range[2]
      )

    plot_data <- data |>
      tidyr::pivot_longer(-date, names_to = "metric", values_to = "value") |>
      dplyr::mutate(
        metric = factor(
          .data$metric,
          levels = c("total_content", "new_content", "updated_content"),
          labels = c("Total Content", "New Content", "Updated Content")
        )
      )

    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = date, y = .data$value, color = .data$metric)
    ) +
      ggplot2::geom_line(linewidth = 0.5) +
      ggplot2::geom_point(size = 0.5) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "", y = "Content Items", color = "") +
      ggplot2::scale_color_manual(
        values = c(
          "Total Content" = BRAND_COLORS$BLUE,
          "New Content" = BRAND_COLORS$GREEN,
          "Updated Content" = BRAND_COLORS$BURGUNDY
        )
      )

    plotly::ggplotly(p) |>
      plotly::layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE),
        legend = list(orientation = "h", x = 0.5, xanchor = "center")
      ) |>
      plotly::config(displayModeBar = FALSE)
  })
}

# ==============================================
# Content → Content List UI/Server (PLACEHOLDER)
# ==============================================

content_list_ui <- bslib::card(
  bslib::card_header(
    shiny::tags$div(
      shiny::tags$span("PLACEHOLDER DATA - Content List"),
      style = "color: #9A4665; font-weight: bold;"
    )
  ),
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    shiny::selectInput(
      "content_list_owner",
      "Owner:",
      choices = c("All", "user1", "user2", "user3")
    ),
    shiny::selectInput(
      "content_list_type",
      "Type:",
      choices = c("All", "shiny", "rmd", "jupyter", "quarto")
    ),
    shiny::textInput(
      "content_list_search",
      "Search:",
      placeholder = "Title"
    )
  ),
  shinycssloaders::withSpinner(
    DT::dataTableOutput("content_list_table")
  )
)

content_list_server <- function(input, output, session) {
  # Generate placeholder content list
  content_list_data <- shiny::reactive({
    data.frame(
      title = paste("Content Item", 1:50),
      owner = sample(c("user1", "user2", "user3"), 50, replace = TRUE),
      type = sample(c("shiny", "rmd", "jupyter", "quarto"), 50, replace = TRUE),
      python_version = sample(c("3.9", "3.10", "3.11", NA), 50, replace = TRUE),
      r_version = sample(c("4.1", "4.2", "4.3", NA), 50, replace = TRUE),
      last_updated = Sys.Date() - sample(0:365, 50, replace = TRUE)
    )
  })

  # Apply filters
  filtered_content_list <- shiny::reactive({
    data <- content_list_data()

    if (input$content_list_owner != "All") {
      data <- data |> dplyr::filter(.data$owner == input$content_list_owner)
    }

    if (input$content_list_type != "All") {
      data <- data |> dplyr::filter(.data$type == input$content_list_type)
    }

    if (nzchar(input$content_list_search)) {
      search_term <- tolower(input$content_list_search)
      data <- data |> dplyr::filter(grepl(search_term, tolower(title)))
    }

    data
  })

  # Render table
  output$content_list_table <- DT::renderDataTable({
    filtered_content_list() |>
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
# Usage → Overview UI/Server (PLACEHOLDER)
# ==============================================

usage_overview_ui <- bslib::card(
  bslib::card_header(
    shiny::tags$div(
      shiny::tags$span("PLACEHOLDER DATA - Usage Overview"),
      style = "color: #9A4665; font-weight: bold;"
    )
  ),
  shiny::dateRangeInput(
    "usage_overview_date_range",
    "Date Range:",
    start = Sys.Date() - 90,
    end = Sys.Date(),
    format = "yyyy-mm-dd"
  ),
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::value_box(
      title = "Total Visits",
      max_height = "120px",
      value = shiny::textOutput("usage_visits_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$GREEN)
    ),
    bslib::value_box(
      title = "Unique Visitors",
      max_height = "120px",
      value = shiny::textOutput("usage_unique_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BLUE)
    )
  ),
  bslib::card(
    bslib::card_header("Visit Trends Over Time (PLACEHOLDER)"),
    shinycssloaders::withSpinner(plotly::plotlyOutput("usage_trend_plot"))
  )
)

usage_overview_server <- function(input, output, session) {
  # Generate placeholder data
  usage_data <- shiny::reactive({
    data.frame(
      date = seq.Date(Sys.Date() - 90, Sys.Date(), by = "day"),
      total_visits = 1200 + cumsum(sample(-50:50, 91, replace = TRUE)),
      unique_visitors = 150 + cumsum(sample(-5:5, 91, replace = TRUE))
    )
  })

  # Value boxes
  output$usage_visits_value <- shiny::renderText({
    data <- usage_data()
    prettyNum(tail(data$total_visits, 1), big.mark = ",")
  })

  output$usage_unique_value <- shiny::renderText({
    data <- usage_data()
    prettyNum(tail(data$unique_visitors, 1), big.mark = ",")
  })

  # Trend chart
  output$usage_trend_plot <- plotly::renderPlotly({
    data <- usage_data()

    # Filter by date range
    data <- data |>
      dplyr::filter(
        date >= input$usage_overview_date_range[1],
        date <= input$usage_overview_date_range[2]
      )

    plot_data <- data |>
      tidyr::pivot_longer(-date, names_to = "metric", values_to = "value") |>
      dplyr::mutate(
        metric = factor(
          .data$metric,
          levels = c("total_visits", "unique_visitors"),
          labels = c("Total Visits", "Unique Visitors")
        )
      )

    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = date, y = .data$value, color = .data$metric)
    ) +
      ggplot2::geom_line(linewidth = 0.5) +
      ggplot2::geom_point(size = 0.5) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "", y = "Visits", color = "") +
      ggplot2::scale_color_manual(
        values = c(
          "Total Visits" = BRAND_COLORS$GREEN,
          "Unique Visitors" = BRAND_COLORS$BLUE
        )
      )

    plotly::ggplotly(p) |>
      plotly::layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE),
        legend = list(orientation = "h", x = 0.5, xanchor = "center")
      ) |>
      plotly::config(displayModeBar = FALSE)
  })
}

# ==============================================
# Usage → Shiny Apps UI/Server (PLACEHOLDER)
# ==============================================

shiny_apps_ui <- bslib::card(
  bslib::card_header(
    shiny::tags$div(
      shiny::tags$span("PLACEHOLDER DATA - Shiny App Usage"),
      style = "color: #9A4665; font-weight: bold;"
    )
  ),
  shiny::dateRangeInput(
    "shiny_apps_date_range",
    "Date Range:",
    start = Sys.Date() - 90,
    end = Sys.Date(),
    format = "yyyy-mm-dd"
  ),
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    bslib::value_box(
      title = "Total Sessions",
      max_height = "120px",
      value = shiny::textOutput("shiny_sessions_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BURGUNDY)
    ),
    bslib::value_box(
      title = "Avg Duration (min)",
      max_height = "120px",
      value = shiny::textOutput("shiny_duration_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$GREEN)
    ),
    bslib::value_box(
      title = "Peak Concurrent",
      max_height = "120px",
      value = shiny::textOutput("shiny_concurrent_value"),
      theme = bslib::value_box_theme(bg = BRAND_COLORS$BLUE)
    )
  ),
  bslib::card(
    bslib::card_header("Shiny Session Trends (PLACEHOLDER)"),
    shinycssloaders::withSpinner(plotly::plotlyOutput("shiny_trend_plot"))
  ),
  bslib::card(
    bslib::card_header("Per-App Breakdown (PLACEHOLDER)"),
    shinycssloaders::withSpinner(DT::dataTableOutput("shiny_apps_table"))
  )
)

shiny_apps_server <- function(input, output, session) {
  # Generate placeholder data
  shiny_data <- shiny::reactive({
    data.frame(
      date = seq.Date(Sys.Date() - 90, Sys.Date(), by = "day"),
      total_sessions = 250 + cumsum(sample(-10:10, 91, replace = TRUE)),
      avg_duration = 8 + rnorm(91, 0, 2),
      peak_concurrent = 15 + sample(-3:3, 91, replace = TRUE)
    )
  })

  # Per-app data
  shiny_apps_data <- shiny::reactive({
    data.frame(
      app_name = paste("Shiny App", 1:20),
      total_sessions = sample(50:500, 20),
      avg_duration = round(runif(20, 5, 15), 1),
      unique_users = sample(10:100, 20)
    )
  })

  # Value boxes
  output$shiny_sessions_value <- shiny::renderText({
    data <- shiny_data()
    prettyNum(tail(data$total_sessions, 1), big.mark = ",")
  })

  output$shiny_duration_value <- shiny::renderText({
    data <- shiny_data()
    round(tail(data$avg_duration, 1), 1)
  })

  output$shiny_concurrent_value <- shiny::renderText({
    data <- shiny_data()
    prettyNum(tail(data$peak_concurrent, 1), big.mark = ",")
  })

  # Trend chart
  output$shiny_trend_plot <- plotly::renderPlotly({
    data <- shiny_data()

    # Filter by date range
    data <- data |>
      dplyr::filter(
        date >= input$shiny_apps_date_range[1],
        date <= input$shiny_apps_date_range[2]
      )

    plot_data <- data |>
      dplyr::select(date, .data$total_sessions, .data$peak_concurrent) |>
      tidyr::pivot_longer(-date, names_to = "metric", values_to = "value") |>
      dplyr::mutate(
        metric = factor(
          .data$metric,
          levels = c("total_sessions", "peak_concurrent"),
          labels = c("Total Sessions", "Peak Concurrent Users")
        )
      )

    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = date, y = .data$value, color = .data$metric)
    ) +
      ggplot2::geom_line(linewidth = 0.5) +
      ggplot2::geom_point(size = 0.5) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "", y = "Count", color = "") +
      ggplot2::scale_color_manual(
        values = c(
          "Total Sessions" = BRAND_COLORS$BURGUNDY,
          "Peak Concurrent Users" = BRAND_COLORS$BLUE
        )
      )

    plotly::ggplotly(p) |>
      plotly::layout(
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE),
        legend = list(orientation = "h", x = 0.5, xanchor = "center")
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  # Per-app table
  output$shiny_apps_table <- DT::renderDataTable({
    shiny_apps_data() |>
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
# Main UI (page_navbar with three dropdowns)
# ==============================================

connect_app_ui <- bslib::page_navbar(
  title = "Posit Connect Dashboard",
  theme = bslib::bs_theme(preset = "shiny"),
  fillable = FALSE,

  # Users dropdown
  bslib::nav_menu(
    "Users",
    bslib::nav_panel("Overview", users_overview_ui),
    bslib::nav_panel("User List", users_list_ui)
  ),

  # Content dropdown
  bslib::nav_menu(
    "Content",
    bslib::nav_panel("Overview", content_overview_ui),
    bslib::nav_panel("Content List", content_list_ui)
  ),

  # Usage dropdown
  bslib::nav_menu(
    "Usage",
    bslib::nav_panel("Overview", usage_overview_ui),
    bslib::nav_panel("Shiny Apps", shiny_apps_ui)
  )
)

# ==============================================
# Main Server
# ==============================================

connect_app_server <- function(input, output, session) {
  # Users → Overview
  users_overview_server(input, output, session)

  # Users → User List
  users_list_server(input, output, session)

  # Content → Overview (placeholder)
  content_overview_server(input, output, session)

  # Content → Content List (placeholder)
  content_list_server(input, output, session)

  # Usage → Overview (placeholder)
  usage_overview_server(input, output, session)

  # Usage → Shiny Apps (placeholder)
  shiny_apps_server(input, output, session)
}

# ==============================================
# Exported App Function
# ==============================================

#' Run the Posit Connect Dashboard Shiny App
#'
#' This comprehensive dashboard provides analytics for Posit Connect across
#' three main areas: Users, Content, and Usage.
#'
#' **Current Implementation Status:**
#' - **Users** section: Fully functional with real Chronicle data
#'   - Overview: User totals and trends from `connect/user_totals`
#'   - User List: Detailed user list from `connect/user_list`
#' - **Content** section: Placeholder data (awaiting curated datasets)
#' - **Usage** section: Placeholder data (awaiting curated datasets)
#'
#' **Navigation Structure:**
#' - Users → Overview, User List
#' - Content → Overview, Content List
#' - Usage → Overview, Shiny Apps
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
#' connect_app("/path/to/chronicle/data")
#'
#' # Run with S3 path
#' connect_app("s3://chronicle-bucket/data")
#' }
connect_app <- function(
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", APP_CONFIG$DEFAULT_BASE_PATH)
) {
  shiny::shinyOptions(base_path = base_path)
  shiny::shinyApp(connect_app_ui, connect_app_server)
}
