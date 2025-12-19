# Posit Connect Dashboard
# Comprehensive dashboard providing analytics for Posit Connect across Users, Content, and Usage

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
      bslib::card_header("User Trends Over Time"),
      shinycssloaders::withSpinner(plotly::plotlyOutput("users_trend_plot"))
    ),
    bslib::card(
      bslib::card_header("Average Users by Day of Week"),
      shinycssloaders::withSpinner(plotly::plotlyOutput("users_dow_plot"))
    )
  )
)

users_overview_server <- function(input, output, session) {
  # Load user_totals data
  users_data <- shiny::reactive({
    tryCatch(
      {
        chronicle_data("connect/user_totals", base_path)
      },
      error = function(e) {
        message("Error loading user totals: ", e$message)
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
    data <- users_data()
    if (is.null(data)) {
      return(NULL)
    }

    # Collect first, then get the latest row
    data |>
      dplyr::collect() |>
      dplyr::arrange(dplyr::desc(date)) |>
      dplyr::slice(1)
  })

  # Filter data by date range (for charts only)
  filtered_users_data <- shiny::reactive({
    data <- users_data()
    if (is.null(data)) {
      return(NULL)
    }

    # Still need the date range input
    shiny::req(input$users_overview_date_range)

    data |>
      dplyr::filter(
        date >= input$users_overview_date_range[1],
        date <= input$users_overview_date_range[2]
      ) |>
      dplyr::collect()
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
# Users - User List UI/Server
# ==============================================

users_list_ui <- bslib::card(
  bslib::card_header(
    shiny::div(
      style = "display: flex; justify-content: space-between; align-items: baseline;",
      shiny::span("Filters"),
      shiny::span(
        style = "font-weight: normal; font-size: 0.9em; color: #555;",
        shiny::textOutput("users_list_as_of", inline = TRUE)
      )
    )
  ),
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
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
        data <- chronicle_data("connect/user_list", base_path)

        # Get max_date snapshot - collect first, then filter to all users from max date
        collected_data <- data |> dplyr::collect()
        max_date <- max(collected_data$date, na.rm = TRUE)

        collected_data |>
          dplyr::filter(date == max_date)
      },
      error = function(e) {
        message("Error loading user list: ", e$message)
        NULL
      }
    )
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
    bslib::card_header("Content Trends Over Time"),
    shinycssloaders::withSpinner(plotly::plotlyOutput("content_trend_plot"))
  ),
  bslib::card(
    bslib::card_header("Content by Type"),
    shinycssloaders::withSpinner(plotly::plotlyOutput("content_type_bar_plot"))
  )
)

content_overview_server <- function(input, output, session) {
  # Load curated content totals data
  contents_data <- shiny::reactive({
    tryCatch(
      {
        # Curated daily totals for content metrics
        chronicle_data("connect/content_totals", base_path)
      },
      error = function(e) {
        message("Error loading contents: ", e$message)
        NULL
      }
    )
  })

  # Populate environment filter dynamically based on curated data
  shiny::observe({
    data <- contents_data()
    if (is.null(data)) {
      return()
    }
    df <- data |> dplyr::collect()
    # Environment column is always `environment`
    env_values <- df |>
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

  # Set default date range when data loads
  shiny::observe({
    shiny::req(contents_data())

    date_summary <- contents_data() |>
      dplyr::filter(!is.na(.data$date)) |>
      dplyr::summarise(
        min_date = min(.data$date, na.rm = TRUE),
        max_date = max(.data$date, na.rm = TRUE)
      ) |>
      dplyr::collect()

    shiny::updateDateRangeInput(
      session,
      "content_overview_date_range",
      start = date_summary$min_date,
      end = date_summary$max_date,
      min = date_summary$min_date,
      max = date_summary$max_date
    )
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
            is.na(.data$environment) |
              .data$environment == "" |
              .data$environment == " "
          )
      } else {
        df <- df |>
          dplyr::filter(.data$environment == input$content_overview_environment)
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

    shiny::req(input$content_overview_date_range)

    df |>
      dplyr::filter(
        .data$date >= input$content_overview_date_range[1],
        .data$date <= input$content_overview_date_range[2]
      )
  })

  # Trend chart (filtered by date range)
  output$content_trend_plot <- plotly::renderPlotly({
    data <- contents_data()

    if (is.null(data)) {
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

    df <- filtered_contents_in_range()

    if (is.null(df) || nrow(df) == 0) {
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

    # Aggregate by date using canonical `count` column
    total_by_date <- df |>
      dplyr::group_by(.data$date) |>
      dplyr::summarise(
        total_content = sum(.data$count, na.rm = TRUE),
        .groups = "drop"
      )

    # Only plot Total Content over time (remove unique content types)
    plot_data <- total_by_date |>
      dplyr::mutate(
        metric = factor("Total Content", levels = "Total Content")
      ) |>
      dplyr::rename(value = total_content)

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
      ggplot2::labs(x = "", y = "Content Items", color = "") +
      ggplot2::scale_color_manual(
        values = c("Total Content" = BRAND_COLORS$BLUE)
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
    data <- contents_data()

    if (is.null(data)) {
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

    df <- filtered_contents_in_range()

    if (is.null(df)) {
      return(plotly::plotly_empty())
    }

    if (nrow(df) == 0) {
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

    latest_date <- suppressWarnings(max(df$date, na.rm = TRUE))
    df <- df |>
      dplyr::filter(.data$date == latest_date)

    # Latest-day totals are not cumulative; just use that day's counts
    type_summary <- df |>
      dplyr::group_by(.data$type) |>
      dplyr::summarise(
        total = sum(.data$count, na.rm = TRUE),
        .groups = "drop"
      )
    names(type_summary)[1] <- "content_type"

    if (nrow(type_summary) == 0) {
      return(plotly::plotly_empty())
    }

    # Order by total desc for nicer bars
    type_summary <- type_summary |>
      dplyr::arrange(dplyr::desc(.data$total))

    p <- ggplot2::ggplot(
      type_summary,
      ggplot2::aes(
        x = stats::reorder(.data$content_type, .data$total),
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
}

# ==============================================
# Content - Content List UI/Server
# ==============================================

content_list_ui <- bslib::card(
  bslib::card_header("Content List"),
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

content_list_server <- function(input, output, session) {
  # Load real content list data (snapshot at latest day)
  content_list_data <- shiny::reactive({
    tryCatch(
      {
        data <- chronicle_data("connect/content_list", base_path)

        df <- data |> dplyr::collect()
        if (!"date" %in% names(df) || nrow(df) == 0) {
          return(NULL)
        }

        latest_date <- suppressWarnings(max(df$date, na.rm = TRUE))
        df |> dplyr::filter(.data$date == latest_date)
      },
      error = function(e) {
        message("Error loading content list: ", e$message)
        NULL
      }
    )
  })

  # Load latest user list for owner name resolution
  latest_user_list <- shiny::reactive({
    tryCatch(
      {
        udata <- chronicle_data("connect/user_list", base_path)
        udf <- udata |> dplyr::collect()
        if (!"date" %in% names(udf) || nrow(udf) == 0) {
          return(NULL)
        }
        latest_date <- suppressWarnings(max(udf$date, na.rm = TRUE))
        udf |> dplyr::filter(.data$date == latest_date)
      },
      error = function(e) {
        message("Error loading user list for owner resolution: ", e$message)
        NULL
      }
    )
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
      dplyr::pull(.data$environment) |>
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
            dplyr::select(id, username) |>
            dplyr::rename(owner_guid = .data$id, owner = .data$username),
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
            is.na(.data$environment) |
              .data$environment == "" |
              .data$environment == " "
          )
      } else {
        df <- df |>
          dplyr::filter(.data$environment == input$content_list_environment)
      }
    }

    # Join owner display for filtering, using latest user list
    ulist <- latest_user_list()
    if (!is.null(ulist) && nrow(ulist) > 0 && "owner_guid" %in% names(df)) {
      owner_lookup <- ulist |>
        dplyr::select(id, username) |>
        dplyr::rename(owner_guid = .data$id, owner = .data$username)

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
}

# ==============================================
# Usage - Overview UI/Server
# ==============================================

usage_overview_ui <- bslib::card(
  bslib::card_header("Usage Overview"),
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
    bslib::card_header("Visit Trends Over Time"),
    shinycssloaders::withSpinner(plotly::plotlyOutput("usage_trend_plot"))
  )
)

usage_overview_server <- function(input, output, session) {
  usage_data <- shiny::reactive({
    tryCatch(
      {
        chronicle_data("connect/content_visits_totals_by_user", base_path)
      },
      error = function(e) {
        message("Error loading content visits totals by user: ", e$message)
        NULL
      }
    )
  })

  shiny::observe({
    data <- usage_data()
    if (is.null(data)) {
      return()
    }

    date_summary <- data |>
      dplyr::filter(!is.na(.data$date)) |>
      dplyr::summarise(
        min_date = min(.data$date, na.rm = TRUE),
        max_date = max(.data$date, na.rm = TRUE)
      ) |>
      dplyr::collect()

    if (nrow(date_summary) == 0) {
      return()
    }

    shiny::updateDateRangeInput(
      session,
      "usage_overview_date_range",
      start = date_summary$min_date,
      end = date_summary$max_date,
      min = date_summary$min_date,
      max = date_summary$max_date
    )
  })

  usage_filtered <- shiny::reactive({
    data <- usage_data()
    if (is.null(data)) {
      return(NULL)
    }

    shiny::req(input$usage_overview_date_range)

    data |>
      dplyr::filter(
        .data$date >= input$usage_overview_date_range[1],
        .data$date <= input$usage_overview_date_range[2]
      ) |>
      dplyr::collect()
  })

  output$usage_visits_value <- shiny::renderText({
    df <- usage_filtered()

    if (is.null(df) || nrow(df) == 0 || !"visits" %in% names(df)) {
      return("0")
    }

    total_visits <- sum(df$visits, na.rm = TRUE)
    prettyNum(total_visits, big.mark = ",")
  })

  output$usage_unique_value <- shiny::renderText({
    df <- usage_filtered()

    if (is.null(df) || nrow(df) == 0 || !"username" %in% names(df)) {
      return("0")
    }

    unique_visitors <- dplyr::n_distinct(df$username)
    prettyNum(unique_visitors, big.mark = ",")
  })

  output$usage_trend_plot <- plotly::renderPlotly({
    df <- usage_filtered()

    if (is.null(df) || nrow(df) == 0 ||
      !"visits" %in% names(df) || !"username" %in% names(df)) {
      return(plotly::plotly_empty())
    }

    daily <- df |>
      dplyr::group_by(.data$date) |>
      dplyr::summarise(
        total_visits = sum(.data$visits, na.rm = TRUE),
        unique_visitors = dplyr::n_distinct(.data$username),
        .groups = "drop"
      )

    if (nrow(daily) == 0) {
      return(plotly::plotly_empty())
    }

    plot_data <- daily |>
      tidyr::pivot_longer(
        c("total_visits", "unique_visitors"),
        names_to = "metric",
        values_to = "value"
      ) |>
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
# Usage - Shiny Apps UI/Server
# ==============================================

shiny_apps_ui <- bslib::card(
  bslib::card_header("Shiny App Usage"),
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
    bslib::card_header("Shiny Session Trends"),
    shinycssloaders::withSpinner(plotly::plotlyOutput("shiny_trend_plot"))
  ),
  bslib::card(
    bslib::card_header("Per-App Breakdown"),
    shinycssloaders::withSpinner(DT::dataTableOutput("shiny_apps_table"))
  )
)

shiny_apps_server <- function(input, output, session) {
  shiny_usage_data <- shiny::reactive({
    tryCatch(
      {
        chronicle_data("connect/shiny_usage_totals_by_user", base_path)
      },
      error = function(e) {
        message("Error loading shiny usage totals by user: ", e$message)
        NULL
      }
    )
  })

  shiny::observe({
    data <- shiny_usage_data()
    if (is.null(data)) {
      return()
    }

    date_summary <- data |>
      dplyr::filter(!is.na(.data$date)) |>
      dplyr::summarise(
        min_date = min(.data$date, na.rm = TRUE),
        max_date = max(.data$date, na.rm = TRUE)
      ) |>
      dplyr::collect()

    if (nrow(date_summary) == 0) {
      return()
    }

    shiny::updateDateRangeInput(
      session,
      "shiny_apps_date_range",
      start = date_summary$min_date,
      end = date_summary$max_date,
      min = date_summary$min_date,
      max = date_summary$max_date
    )
  })

  shiny_usage_filtered <- shiny::reactive({
    data <- shiny_usage_data()
    if (is.null(data)) {
      return(NULL)
    }

    shiny::req(input$shiny_apps_date_range)

    data |>
      dplyr::filter(
        .data$date >= input$shiny_apps_date_range[1],
        .data$date <= input$shiny_apps_date_range[2]
      ) |>
      dplyr::collect()
  })

  output$shiny_sessions_value <- shiny::renderText({
    df <- shiny_usage_filtered()

    if (is.null(df) || nrow(df) == 0 || !"sessions" %in% names(df)) {
      return("0")
    }

    total_sessions <- sum(df$sessions, na.rm = TRUE)
    prettyNum(total_sessions, big.mark = ",")
  })

  output$shiny_duration_value <- shiny::renderText({
    df <- shiny_usage_filtered()

    if (is.null(df) || nrow(df) == 0) {
      return("")
    }

    if ("avg_duration_minutes" %in% names(df) && "sessions" %in% names(df)) {
      total_sessions <- sum(df$sessions, na.rm = TRUE)
      if (total_sessions == 0) {
        return("0")
      }

      weighted_avg <- stats::weighted.mean(
        df$avg_duration_minutes,
        df$sessions,
        na.rm = TRUE
      )
      return(round(weighted_avg, 1))
    }

    ""
  })

  output$shiny_concurrent_value <- shiny::renderText({
    df <- shiny_usage_filtered()

    if (is.null(df) || nrow(df) == 0 || !"peak_concurrent" %in% names(df)) {
      return("0")
    }

    peak_val <- suppressWarnings(max(df$peak_concurrent, na.rm = TRUE))
    if (!is.finite(peak_val)) {
      peak_val <- 0
    }

    prettyNum(peak_val, big.mark = ",")
  })

  output$shiny_trend_plot <- plotly::renderPlotly({
    df <- shiny_usage_filtered()

    if (is.null(df) || nrow(df) == 0 || !"sessions" %in% names(df)) {
      return(plotly::plotly_empty())
    }

    daily <- df |>
      dplyr::group_by(.data$date) |>
      dplyr::summarise(
        total_sessions = sum(.data$sessions, na.rm = TRUE),
        peak_concurrent_daily = if ("peak_concurrent" %in% names(df)) {
          suppressWarnings(max(.data$peak_concurrent, na.rm = TRUE))
        } else {
          NA_real_
        },
        .groups = "drop"
      )

    if (nrow(daily) == 0) {
      return(plotly::plotly_empty())
    }

    metrics <- c("total_sessions")
    if ("peak_concurrent" %in% names(df)) {
      metrics <- c(metrics, "peak_concurrent_daily")
    }

    plot_data <- daily |>
      tidyr::pivot_longer(
        dplyr::all_of(metrics),
        names_to = "metric",
        values_to = "value"
      ) |>
      dplyr::mutate(
        metric = factor(
          .data$metric,
          levels = c("total_sessions", "peak_concurrent_daily"),
          labels = c("Total Sessions", "Peak Concurrent Users")
        )
      )

    plot_data <- plot_data[stats::complete.cases(plot_data$value), , drop = FALSE]

    if (nrow(plot_data) == 0) {
      return(plotly::plotly_empty())
    }

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

  output$shiny_apps_table <- DT::renderDataTable({
    df <- shiny_usage_filtered()

    if (is.null(df) || nrow(df) == 0 ||
      !"app_name" %in% names(df) ||
      !"sessions" %in% names(df) ||
      !"username" %in% names(df)) {
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

    app_summary <- df |>
      dplyr::group_by(.data$app_name) |>
      dplyr::summarise(
        total_sessions = sum(.data$sessions, na.rm = TRUE),
        unique_users = dplyr::n_distinct(.data$username),
        avg_duration_minutes = if ("avg_duration_minutes" %in% names(df)) {
          stats::weighted.mean(.data$avg_duration_minutes, .data$sessions, na.rm = TRUE)
        } else {
          NA_real_
        },
        .groups = "drop"
      )

    DT::datatable(
      app_summary,
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

ui <- bslib::page_navbar(
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

server <- function(input, output, session) {
  users_overview_server(input, output, session)
  users_list_server(input, output, session)
  content_overview_server(input, output, session)
  content_list_server(input, output, session)
  usage_overview_server(input, output, session)
  shiny_apps_server(input, output, session)
}

shinyApp(ui, server)
