# Load required packages
library(shiny)
library(arrow)
library(dplyr)
library(bslib)
library(ggplot2)
library(DT)
library(tidyr)
library(lubridate)


# Function to read parquet dataset
read_parquet_data <- function(
  # s3_path = "s3://posit-dsp-chronicle/daily/v2/connect_users"
  s3_path = "/Users/marktucker/work/chronicle-data/posit-it/2025-08/daily/v2/connect_users"
) {
  tryCatch(
    {
      data <- open_dataset(
        s3_path,
        format = "parquet",
        partitioning = c("Year", "Month", "Day"),
        hive_style = FALSE
      ) %>%
        filter(!locked) %>%
        mutate(date = as.Date(timestamp)) %>%
        collect()

      return(data)
    },
    error = function(e) {
      stop("Error reading parquet files from S3: ", e$message)
    }
  )
}

# Function to get latest state of all users
get_latest_user_state <- function(data) {
  data %>%
    group_by(username) %>%
    slice_max(timestamp, n = 1) %>%
    ungroup()
}

# Function to process daily metrics
process_daily_metrics <- function(data) {
  # Calculate daily metrics including cumulative totals
  daily_metrics <- data %>%
    group_by(date) %>%
    summarise(
      # Get list of usernames for debugging
      usernames = list(sort(unique(username[as.Date(created_at) <= date]))),
      total_users = n_distinct(username[as.Date(created_at) <= date]),
      active_users = n_distinct(
        username[
          !is.na(last_active_at) &
            as.Date(last_active_at) >= date - 30
        ]
      ),
      users_active_today = n_distinct(
        username[as.Date(last_active_at) == date]
      ),
      publishers = n_distinct(
        username[
          user_role %in% c("publisher", "admin") & as.Date(created_at) <= date
        ]
      ),
      .groups = "drop"
    ) %>%
    arrange(date)

  # Print debug info for latest date
  latest <- daily_metrics %>% slice_max(date, n = 1)
  message("Daily metrics latest date: ", latest$date)
  message("Total users count: ", latest$total_users)
  message("Usernames: ", length(latest$usernames[[1]]))

  return(daily_metrics)
}

# Function to process user list for a specific date
process_user_list <- function(data, target_date) {
  message("Processing user list for date: ", target_date)

  # Get state of users on the target date
  users <- data %>%
    # Only look at records for the target date
    filter(date == target_date) %>%
    filter(!locked) %>%
    # Only include users created by target date
    filter(as.Date(created_at) <= target_date) %>%
    # Get the latest state for each user on that date
    group_by(username) %>%
    slice_max(timestamp, n = 1) %>%
    ungroup() %>%
    # Sort by most recently active
    arrange(desc(last_active_at))

  message("User list count: ", nrow(users))
  return(users)
}






  # Read data once at startup with error handling
  raw_data <- function()
  {

      tryCatch(
        {
          read_parquet_data()
        },
        error = function(e) {
          print(e)

          return(NULL)
        }
      )
    }


rawdata <- raw_data()

  # Process data for metrics
  data <- process_daily_metrics(raw_data())


  # Process data for user list using the end date from date range
  user_list_data <- process_user_list(raw_data(), as.Date("2025-08-31"))


  # Filtered data based on date range
  filtered_data <- reactive({
    req(data(), input$date_range)

    data() %>%
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
  })

  # Update date range based on available data
  observe({
    req(data())
    date_range <- range(data()$date)
    updateDateRangeInput(
      session,
      "date_range",
      start = date_range[1],
      end = date_range[2],
      min = date_range[1],
      max = date_range[2]
    )
  })

  # Summary statistics
  # Get most recent day's data
  latest_data <- reactive({
    req(filtered_data())
    filtered_data() %>%
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
    req(filtered_data(), input$metrics, length(input$metrics) > 0)

    plot_data <- filtered_data() %>%
      select(date, all_of(input$metrics)) %>%
      pivot_longer(-date, names_to = "metric", values_to = "value") %>%
      mutate(
        metric = factor(
          metric,
          levels = c("total_users", "active_users", "publishers"),
          labels = c("Total Users", "Active Users", "Publishers")
        )
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
      scale_color_brewer(palette = "Set2") +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)
      )
  })

  # Activity pattern plot
  output$activity_pattern_plot <- renderPlot({
    req(filtered_data())

    # Calculate average users active by day of week
    day_summary <- filtered_data() %>%
      mutate(
        day_of_week = wday(date, label = TRUE, abbr = FALSE)
      ) %>%
      group_by(day_of_week) %>%
      summarise(
        avg_active_users = mean(users_active_today, na.rm = TRUE),
        .groups = "drop"
      )

    # Define calm/cool colors for each day
    day_colors <- c(
      "Monday" = "#E7B10A", # Posit yellow
      "Tuesday" = "#447099", # Posit blue
      "Wednesday" = "#72994E", # Posit green
      "Thursday" = "#D44000", # Posit red
      "Friday" = "#EE6331", # Posit orange
      "Saturday" = "#9A4665", # Posit burgundy
      "Sunday" = "#419599" # Posit teal
    )

    ggplot(
      day_summary,
      aes(x = day_of_week, y = avg_active_users, fill = day_of_week)
    ) +
      geom_col() +
      scale_fill_manual(values = day_colors) +
      guides(fill = "none") + # Remove the legend since colors are just for visual interest
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
    user_list_data() %>%
      select(
        username,
        first_name,
        last_name,
        email,
        user_role,
        created_at,
        last_active_at
      ) %>%
      arrange(desc(last_active_at)) %>%
      mutate(
        created_at = format(as.POSIXct(created_at), "%Y-%m-%d"),
        last_active_at = format(as.POSIXct(last_active_at), "%Y-%m-%d"),
        full_name = paste(first_name, last_name)
      ) %>%
      select(
        username,
        full_name,
        email,
        user_role,
        created_at,
        last_active_at
      ) %>%
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
