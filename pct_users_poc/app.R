# Load required packages
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(lubridate)

source("./chronicle-reader.R")

# Color constants
COLORS <- list(
  # Brand colors
  DARK_GRAY = "#404041",
  BLUE = "#447099",
  GREEN = "#72994E",
  BURGUNDY = "#9A4665",

  # Semantic mappings
  LICENSED_USERS = "#447099", # Blue
  DAILY_USERS = "#72994E", # Green
  PUBLISHERS = "#9A4665" # Burgundy
)

# Example: CHRONICLE_BASE_PATH=s3://posit-dsp-chronicle

# Function to read Connect users dataset
read_connect_users_data <- function(
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", "/var/lib/posit-chronicle/data")
) {
  tryCatch(
    {
      data <- chr_get_metric_data("connect_users", base_path, "daily") |>
        mutate(date = as.Date(timestamp)) |>
        collect()

      return(data)
    },
    error = function(e) {
      stop("Error reading parquet files from S3: ", e$message)
    }
  )
}

# Function to process daily metrics
process_daily_metrics <- function(data) {
  # Calculate daily metrics including cumulative totals
  daily_metrics <- data |>
    # First get latest state per user per date
    group_by(date, id) |>
    slice_max(timestamp, n = 1) |>
    ungroup() |>
    # Filter out users inactive for more than a year from the reference date
    filter(
      is.na(last_active_at) | as.Date(last_active_at) >= date - 365
    ) |>
    # Then calculate metrics only using latest states
    group_by(date) |>
    summarise(
      # Get list of ids for debugging
      ids = list(sort(unique(id[as.Date(created_at) <= date & !locked]))),
      total_users = n_distinct(id[as.Date(created_at) <= date & !locked]),
      active_users = n_distinct(
        id[
          !is.na(last_active_at) &
            as.Date(last_active_at) >= date - 30 &
            !locked
        ]
      ),
      users_active_today = n_distinct(
        id[as.Date(last_active_at) == date & !locked]
      ),
      publishers = n_distinct(
        id[
          user_role %in%
            c("publisher", "admin") &
            as.Date(created_at) <= date &
            !locked
        ]
      ),
      .groups = "drop"
    ) |>
    arrange(date)

  return(daily_metrics)
}

# Function to process user list for a specific date
process_user_list <- function(data, target_date) {
  message("Processing user list for date: ", target_date)

  # Get state of users on the target date
  users <- data |>
    # Get the latest state for each user
    group_by(id) |>
    slice_max(timestamp, n = 1) |>
    ungroup() |>
    # Filter locked users after getting latest state
    filter(!locked) |>
    # Filter out users inactive for more than a year
    filter(
      is.na(last_active_at) | as.Date(last_active_at) >= target_date - dyears(1)
    ) |>
    # Sort by most recently active
    arrange(desc(last_active_at))

  message("User list count: ", nrow(users))
  return(users)
}

# UI
ui <- page_fluid(
  title = "Posit Connect Users Dashboard",
  theme = bs_theme(preset = "minty"),

  # Title bar
  div(
    class = "navbar navbar-expand-lg navbar-dark mb-4",
    style = paste0("background-color: ", COLORS$DARK_GRAY, ";"),
    div(
      class = "container-fluid",
      h4(
        "Posit Connect Users Dashboard",
        class = "navbar-brand text-white mb-0"
      )
    )
  ),

  # Value boxes row
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(
      title = "Licensed Users",
      max_height = "120px",
      value = textOutput("total_users_value"),
      theme = value_box_theme(bg = COLORS$LICENSED_USERS)
    ),
    value_box(
      title = "Daily Users",
      max_height = "120px",
      value = textOutput("active_users_value"),
      theme = value_box_theme(bg = COLORS$DAILY_USERS)
    ),
    value_box(
      title = "Publishers",
      max_height = "120px",
      value = textOutput("publishers_value"),
      theme = value_box_theme(bg = COLORS$PUBLISHERS)
    )
  ),

  # Charts row
  layout_columns(
    col_widths = c(6, 6),

    # User Trends chart
    card(
      card_header("User Trends Over Time"),
      plotOutput("user_trend_plot", height = "400px")
    ),

    # Activity Pattern chart
    card(
      card_header("Average Daily Activity by Day of Week"),
      plotOutput("activity_pattern_plot", height = "400px")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Read data once at startup with error handling
  raw_data <- reactive({
    withProgress(message = "Loading data...", {
      tryCatch(
        {
          read_connect_users_data()
        },
        error = function(e) {
          showNotification(
            e$message,
            type = "error",
            duration = NULL
          )
          return(NULL)
        }
      )
    })
  })

  # Process data for metrics
  data <- reactive({
    req(raw_data())
    process_daily_metrics(raw_data())
  })

  # Process data for user list using the latest date
  user_list_data <- reactive({
    req(raw_data(), data())
    latest_date <- max(data()$date)
    process_user_list(raw_data(), latest_date)
  })

  # Use all available data (no filtering)
  filtered_data <- reactive({
    req(data())
    data()
  })

  # Summary statistics
  # Get most recent day's data
  latest_data <- reactive({
    req(filtered_data())
    filtered_data() |>
      slice_max(date, n = 1)
  })

  output$total_users_value <- renderText({
    req(latest_data())
    formatC(
      latest_data()$total_users,
      format = "d",
      big.mark = ","
    )
  })

  output$active_users_value <- renderText({
    req(latest_data())
    formatC(
      latest_data()$active_users,
      format = "d",
      big.mark = ","
    )
  })

  output$publishers_value <- renderText({
    req(latest_data())
    formatC(
      latest_data()$publishers,
      format = "d",
      big.mark = ","
    )
  })

  # Trend plot
  output$user_trend_plot <- renderPlot({
    req(filtered_data())

    plot_data <- filtered_data() |>
      select(date, total_users, active_users, publishers) |>
      pivot_longer(-date, names_to = "metric", values_to = "value") |>
      mutate(
        metric = factor(
          metric,
          levels = c("total_users", "active_users", "publishers"),
          labels = c("Licensed Users", "Daily Users", "Publishers")
        )
      )

    # Define colors to match the value boxes
    metric_colors <- c(
      "Licensed Users" = COLORS$LICENSED_USERS,
      "Daily Users" = COLORS$DAILY_USERS,
      "Publishers" = COLORS$PUBLISHERS
    )

    ggplot(plot_data, aes(x = date, y = value, color = metric)) +
      geom_line(linewidth = 1) +
      theme_minimal() +
      labs(
        x = "Date",
        y = "Count",
        color = "Metric",
        title = "User Trends Over Time"
      ) +
      scale_color_manual(values = metric_colors) +
      scale_y_continuous(limits = c(0, NA)) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)
      )
  })

  # Activity pattern plot
  output$activity_pattern_plot <- renderPlot({
    req(filtered_data())

    # Calculate average users active by day of week
    day_summary <- filtered_data() |>
      mutate(
        day_of_week = wday(date, label = TRUE, abbr = FALSE)
      ) |>
      group_by(day_of_week) |>
      summarise(
        avg_active_users = mean(users_active_today, na.rm = TRUE),
        .groups = "drop"
      )

    ggplot(
      day_summary,
      aes(x = day_of_week, y = avg_active_users)
    ) +
      geom_col(fill = COLORS$BLUE) +
      theme_minimal() +
      labs(
        x = "Day of Week",
        y = "Average Users",
        title = "Average Daily Users by Day of Week"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
  })

  # Detailed table
  output$user_table <- renderDT({
    req(user_list_data())
    user_list_data() |>
      select(
        username,
        first_name,
        last_name,
        email,
        user_role,
        created_at,
        last_active_at
      ) |>
      arrange(desc(last_active_at)) |>
      mutate(
        created_at = format(as.POSIXct(created_at), "%Y-%m-%d"),
        last_active_at = format(as.POSIXct(last_active_at), "%Y-%m-%d"),
        full_name = paste(first_name, last_name)
      ) |>
      select(
        username,
        full_name,
        email,
        user_role,
        created_at,
        last_active_at
      ) |>
      datatable(
        colnames = c(
          "Username",
          "Full Name",
          "Email",
          "Role",
          "Created",
          "Last Active"
        ),
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          order = list(list(5, 'desc'))
        )
      )
  })
}

# Run the app
shinyApp(ui, server)
