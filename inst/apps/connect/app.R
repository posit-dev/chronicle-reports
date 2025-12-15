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

    # Search filter
    if (nzchar(input$users_list_search)) {
      search_term <- tolower(input$users_list_search)
      data <- data |>
        dplyr::filter(
          grepl(search_term, tolower(.data$username), fixed = TRUE) |
            grepl(search_term, tolower(.data$email), fixed = TRUE)
        )
    }

    data
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
    # Detect environment column if present
    env_col <- intersect(c("environment", "env"), names(df))[1]
    if (is.na(env_col)) {
      env_col <- NULL
    }
    if (is.null(env_col)) {
      # No environment column; keep only "All"
      shiny::updateSelectInput(
        session,
        "content_overview_environment",
        choices = c("All"),
        selected = "All"
      )
    } else {
      env_values <- df |>
        dplyr::pull(env_col) |>
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
    }

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

  # (Removed) value boxes and latest metrics; charts remain

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

    shiny::req(input$content_overview_date_range)

    df <- data |> dplyr::collect()
    # Apply environment filter if available and selected
    env_col <- intersect(c("environment", "env"), names(df))[1]
    if (is.na(env_col)) {
      env_col <- NULL
    }
    if (!is.null(env_col) && input$content_overview_environment != "All") {
      if (input$content_overview_environment == "(Not Set)") {
        df <- df |>
          dplyr::filter(
            is.na(.data[[env_col]]) |
              .data[[env_col]] == "" |
              .data[[env_col]] == " "
          )
      } else {
        df <- df |>
          dplyr::filter(.data[[env_col]] == input$content_overview_environment)
      }
    }

    # Apply type filter using fixed 'type' column
    if ("type" %in% names(df) && input$content_overview_type != "All") {
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

    if (!"date" %in% names(df) || nrow(df) == 0) {
      return(
        plotly::plotly_empty() |>
          plotly::layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE)
          )
      )
    }

    df <- df |>
      dplyr::filter(
        .data$date >= input$content_overview_date_range[1],
        .data$date <= input$content_overview_date_range[2]
      )

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

    # Aggregate by date according to spec
    count_cols <- intersect(c("total_content", "count", "value"), names(df))
    total_by_date <- if (length(count_cols) > 0) {
      df |>
        dplyr::group_by(.data$date) |>
        dplyr::summarise(
          total_content = sum(.data[[count_cols[1]]], na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      num_cols <- names(df)[sapply(df, is.numeric)]
      num_cols <- setdiff(num_cols, c("date"))
      df |>
        dplyr::group_by(.data$date) |>
        dplyr::summarise(
          total_content = if (length(num_cols) == 0) {
            0L
          } else {
            sum(
              rowSums(dplyr::across(dplyr::all_of(num_cols)), na.rm = TRUE),
              na.rm = TRUE
            )
          },
          .groups = "drop"
        )
    }

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

    shiny::req(input$content_overview_date_range)

    df <- data |> dplyr::collect()
    # Apply environment filter if available and selected
    env_col <- intersect(c("environment", "env"), names(df))[1]
    if (is.na(env_col)) {
      env_col <- NULL
    }
    if (!is.null(env_col) && input$content_overview_environment != "All") {
      if (input$content_overview_environment == "(Not Set)") {
        df <- df |>
          dplyr::filter(
            is.na(.data[[env_col]]) |
              .data[[env_col]] == "" |
              .data[[env_col]] == " "
          )
      } else {
        df <- df |>
          dplyr::filter(.data[[env_col]] == input$content_overview_environment)
      }
    }

    if (!"date" %in% names(df) || nrow(df) == 0) {
      return(plotly::plotly_empty())
    }

    # Filter to selected range and select ONLY the latest day in range
    df <- df |>
      dplyr::filter(
        .data$date >= input$content_overview_date_range[1],
        .data$date <= input$content_overview_date_range[2]
      )

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

    # Apply type filter using fixed 'type' column
    if ("type" %in% names(df) && input$content_overview_type != "All") {
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

    # Compute counts per type
    count_cols <- intersect(c("total_content", "count", "value"), names(df))

    if ("type" %in% names(df)) {
      if (length(count_cols) > 0) {
        # Latest-day totals are not cumulative; just use that day's counts
        type_summary <- df |>
          dplyr::group_by(.data$type) |>
          dplyr::summarise(
            total = sum(.data[[count_cols[1]]], na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        type_summary <- df |>
          dplyr::group_by(.data$type) |>
          dplyr::summarise(total = dplyr::n(), .groups = "drop")
      }
      names(type_summary)[1] <- "content_type"
    } else {
      # Fallback: no explicit type column. Treat each row as one item category "Unknown"
      total_val <- if (length(count_cols) > 0) {
        sum(df[[count_cols[1]]], na.rm = TRUE)
      } else {
        nrow(df)
      }
      type_summary <- tibble::tibble(
        content_type = "Unknown",
        total = total_val
      )
    }

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

    # Detect type column
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

    # Type column on this file is app_mode; UI label remains "Type"
    type_col <- "app_mode"

    # Resolve owner names by joining latest user list on owner id
    owners_choices <- c("All")
    ulist <- latest_user_list()
    if (!is.null(ulist) && nrow(ulist) > 0) {
      # Identify join keys heuristically
      content_owner_id_col <- intersect(
        c(
          "owner_id",
          "owner_guid",
          "content_owner_guid",
          "user_id",
          "owner",
          "user_guid",
          "guid"
        ),
        names(df)
      )[1]
      if (is.na(content_owner_id_col)) {
        content_owner_id_col <- NULL
      }

      user_id_col <- intersect(
        c("id", "user_id", "user_guid", "guid"),
        names(ulist)
      )[1]
      if (is.na(user_id_col)) {
        user_id_col <- NULL
      }
      # Identify display name columns in user list
      user_display_cols <- intersect(
        c("username", "email", "first_name", "last_name"),
        names(ulist)
      )
      if (
        !is.null(content_owner_id_col) &&
          !is.null(user_id_col) &&
          length(user_display_cols) > 0
      ) {
        joined <- tryCatch(
          {
            df |>
              dplyr::left_join(
                ulist |>
                  dplyr::mutate(
                    owner = dplyr::coalesce(.data$username, .data$email)
                  ),
                by = setNames(user_id_col, content_owner_id_col)
              )
          },
          error = function(e) {
            NULL
          }
        )
        if (!is.null(joined)) {
          owners <- joined |>
            dplyr::pull("owner") |>
            unique()
          has_na <- any(is.na(owners) | owners == "" | owners == " ")
          owners <- owners[!is.na(owners) & owners != "" & owners != " "] |>
            sort()
          if (has_na) {
            owners <- c(owners, "(Not Set)")
          }
          owners_choices <- c("All", owners)
        }
      }
    }
    # Fallback: if join didn't produce owner choices, derive from content list
    if (length(owners_choices) == 1) {
      owner_fallback_col <- intersect(
        c("owner", "username", "user", "email"),
        names(df)
      )[1]
      if (is.na(owner_fallback_col)) {
        owner_fallback_col <- NULL
      }
      if (!is.null(owner_fallback_col)) {
        owners <- df |>
          dplyr::pull(owner_fallback_col) |>
          unique()
        has_na <- any(is.na(owners) | owners == "" | owners == " ")
        owners <- owners[!is.na(owners) & owners != "" & owners != " "] |>
          sort()
        if (has_na) {
          owners <- c(owners, "(Not Set)")
        }
        owners_choices <- c("All", owners)
      }
    }
    shiny::updateSelectInput(
      session,
      "content_list_owner",
      choices = owners_choices,
      selected = "All"
    )

    # Types
    if (!is.null(type_col)) {
      types <- df |>
        dplyr::pull(type_col) |>
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
    } else {
      shiny::updateSelectInput(
        session,
        "content_list_type",
        choices = c("All"),
        selected = "All"
      )
    }
  })

  # Apply filters
  filtered_content_list <- shiny::reactive({
    data <- content_list_data()
    if (is.null(data)) {
      return(NULL)
    }

    df <- data

    # Type column is app_mode; UI label remains "Type"
    type_col <- "app_mode"

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
    if (!is.null(ulist) && nrow(ulist) > 0) {
      content_owner_id_col <- intersect(
        c(
          "owner_id",
          "owner_guid",
          "content_owner_guid",
          "user_id",
          "owner",
          "user_guid",
          "guid"
        ),
        names(df)
      )[1]
      if (is.na(content_owner_id_col)) {
        content_owner_id_col <- NULL
      }

      user_id_col <- intersect(
        c("id", "user_id", "user_guid", "guid"),
        names(ulist)
      )[1]
      if (is.na(user_id_col)) {
        user_id_col <- NULL
      }
      if (!is.null(content_owner_id_col) && !is.null(user_id_col)) {
        ulist2 <- ulist |>
          dplyr::mutate(owner = dplyr::coalesce(.data$username, .data$email)) |>
          dplyr::select(dplyr::all_of(c(user_id_col, "owner")))
        df <- tryCatch(
          {
            df |>
              dplyr::left_join(
                ulist2,
                by = setNames(user_id_col, content_owner_id_col)
              )
          },
          error = function(e) {
            df
          }
        )
      }
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
            is.na(.data[[type_col]]) |
              .data[[type_col]] == "" |
              .data[[type_col]] == " "
          )
      } else {
        df <- df |>
          dplyr::filter(.data[[type_col]] == input$content_list_type)
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
    # Choose columns to display if present
    owner_col <- intersect(c("owner", "username", "user"), names(df))[1]
    # Underlying column is app_mode; display header should read "Type"
    type_col <- "app_mode"
    title_col <- intersect(c("title", "name"), names(df))[1]
    env_col <- "environment"
    py_col <- intersect(c("python_version", "py_version", "python"), names(df))[
      1
    ]
    r_col <- intersect(c("r_version", "R_version", "r"), names(df))[1]
    quarto_col <- intersect(c("quarto_version", "quarto"), names(df))[1]
    updated_col <- intersect(
      c("last_updated", "last_deployed_at", "updated_at"),
      names(df)
    )[1]

    # Fallbacks
    cols <- c(
      title_col,
      owner_col,
      type_col,
      env_col,
      py_col,
      r_col,
      quarto_col,
      updated_col
    )
    cols <- cols[!is.na(cols)]
    if (length(cols) == 0) {
      # Show everything except internal columns like date
      cols <- setdiff(names(df), c("date"))
    }

    # Standardize column names for display
    display <- df[, cols, drop = FALSE]
    if (!is.na(type_col) && type_col %in% names(display)) {
      names(display)[names(display) == type_col] <- "type"
    }

    DT::datatable(
      display,
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
# Usage - Overview UI/Server (PLACEHOLDER)
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
# Usage - Shiny Apps UI/Server (PLACEHOLDER)
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
  # Users - Overview
  users_overview_server(input, output, session)

  # Users - User List
  users_list_server(input, output, session)

  # Content - Overview (placeholder)
  content_overview_server(input, output, session)

  # Content - Content List (placeholder)
  content_list_server(input, output, session)

  # Usage - Overview (placeholder)
  usage_overview_server(input, output, session)

  # Usage - Shiny Apps (placeholder)
  shiny_apps_server(input, output, session)
}

shinyApp(ui, server)
