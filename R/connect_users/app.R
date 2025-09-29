library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(lubridate)
library(plotly)


# The base path where Chronicle data files are stored. If you deploy this app
# to Posit Connect, you can set this environment variable in the Connect
# UI. See https://docs.posit.co/connect/user/content-settings/#content-vars
# for more information.
BASE_PATH <- Sys.getenv("CHRONICLE_BASE_PATH", "/var/lib/posit-chronicle/data")

source("./chronicle-reader.R")


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
  PUBLISHERS = BRAND_COLORS$BURGUNDY
)

# Process the raw data to compute daily metrics used for historical trends
process_daily_metrics <- function(data) {
  daily_metrics <- data |>
    # First get latest state per user per date
    group_by(date, id) |>
    slice_max(timestamp, n = 1) |>
    ungroup() |>
    # Filter out users inactive for more than a year
    # Here, `date` is the date on which the metric was collected.
    filter(
      is.na(last_active_at) | as.Date(last_active_at) >= date - dyears(1)
    ) |>
    # Then calculate daily user counts for each date
    group_by(date) |>
    summarise(
      # Any unlocked user active in the last year (computed above) is counted
      # as a licensed user
      licensed_users = n_distinct(id[
        !is.na(created_at) &
          as.Date(created_at) <= date &
          !locked
      ]),
      # Daily users are those active on a given date
      daily_users = n_distinct(
        id[
          !is.na(last_active_at) &
            as.Date(last_active_at) == date &
            !locked
        ]
      ),
      # Publishers are those with role publisher or admin
      publishers = n_distinct(
        id[
          !is.na(created_at) &
            as.Date(created_at) <= date &
            user_role %in%
              c("publisher", "admin") &
            !locked
        ]
      ),
      users_active_today = n_distinct(
        id[as.Date(last_active_at) == date & !locked]
      ),
      .groups = "drop"
    ) |>
    arrange(date)

  return(daily_metrics)
}


# ==============================================
# Define the UI layout
# ==============================================
ui <- page_fluid(
  tags$head(tags$style(HTML('* {font-family: "Open Sans"};'))),
  title = "Posit Connect Users Dashboard",
  theme = bs_theme(preset = "shiny"),

  # add space at top of viewport for prettier layout
  layout_columns(),

  # Summary row with current values for each user metric
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(
      title = "Licensed Users",
      max_height = "120px",
      value = textOutput("licensed_users_value"),
      theme = value_box_theme(bg = COLORS$LICENSED_USERS)
    ),
    value_box(
      title = "Daily Users",
      max_height = "120px",
      value = textOutput("daily_users_value"),
      theme = value_box_theme(bg = COLORS$DAILY_USERS)
    ),
    value_box(
      title = "Publishers",
      max_height = "120px",
      value = textOutput("publishers_value"),
      theme = value_box_theme(bg = COLORS$PUBLISHERS)
    )
  ),

  # Charts row for historical trends
  layout_columns(
    col_widths = c(6, 6),

    # User trends chart
    card(
      card_header("User Trends Over Time"),
      plotlyOutput("user_trend_plot")
    ),

    # Daily activity pattern chart
    card(
      card_header("Average Daily Users by Day of Week"),
      plotOutput("activity_pattern_plot")
    )
  )
)

# ==============================================
# Define the server logic
# ==============================================
server <- function(input, output, session) {
  # Read data once at startup with error handling
  raw_data <- reactive({
    withProgress(message = "Loading data...", {
      tryCatch(
        {
          data <- chr_get_metric_data("connect_users", BASE_PATH, "daily") |>
            collect()
          return(data)
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

  # Get most recent day's data
  latest_data <- reactive({
    req(data())
    data() |>
      slice_max(date, n = 1)
  })

  # Get the latest values for each metric
  output$licensed_users_value <- renderText({
    req(latest_data())
    prettyNum(latest_data()$licensed_users, big.mark = ",")
  })
  output$daily_users_value <- renderText({
    req(latest_data())
    prettyNum(latest_data()$daily_users, big.mark = ",")
  })
  output$publishers_value <- renderText({
    req(latest_data())
    prettyNum(latest_data()$publishers, big.mark = ",")
  })

  # Plot for user trends over time
  output$user_trend_plot <- renderPlotly({
    req(data())

    # Reshape data for easier plotting
    plot_data <- data() |>
      select(date, licensed_users, daily_users, publishers) |>
      pivot_longer(-date, names_to = "metric", values_to = "value") |>
      mutate(
        metric = factor(
          metric,
          levels = c("licensed_users", "daily_users", "publishers"),
          labels = c("Licensed Users", "Daily Users", "Publishers")
        )
      )

    # Plot the trend of user counts over time
    p <- ggplot(plot_data, aes(x = date, y = value, color = metric)) +
      geom_line(linewidth = 1) +
      # add points with custom hover text
      geom_point(
        aes(
          # This code produces a warning about `text` being an unknown aesthetic.
          # This is normal when using ggplotly(). The text aesthetic is not a
          # standard ggplot2 aesthetic, but ggplotly() recognizes and uses it
          # for hover tooltips.
          text = paste0(
            format(date, "%B %d, %Y"),
            "<br>",
            prettyNum(value, big.mark = ","),
            " ",
            metric
          )
        ),
        size = 1
      ) +
      theme_minimal() +
      labs(
        x = "",
        y = "Number of Users",
        color = "Metric",
      ) +
      # The line colors should match the value box colors
      scale_color_manual(
        values = c(
          "Licensed Users" = COLORS$LICENSED_USERS,
          "Daily Users" = COLORS$DAILY_USERS,
          "Publishers" = COLORS$PUBLISHERS
        )
      )

    # Convert to plotly with custom hover
    ggplotly(p, tooltip = "text") |>
      layout(
        # make the legend look pretty
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center"
        )
      )
  })

  # Plot the average daily activity pattern by day of week
  output$activity_pattern_plot <- renderPlot({
    req(data())

    # Calculate average users active by day of week
    day_summary <- data() |>
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
      geom_col(fill = BRAND_COLORS$BLUE) +
      theme_minimal() +
      labs(
        x = "",
        y = "Average Number of Users",
      )
  })
}

# Run the app
shinyApp(ui, server)
