# Chronicle Connect Content Visits Dashboard
# Interactive analysis of Posit Connect usage data

library(shiny)
library(bslib)
library(DT)
library(plotly)
library(arrow)
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyr)

source("./chronicle-reader.R")

load_data <- function(
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", "/var/lib/posit-chronicle/data")
) {
  tryCatch(
    {
      # Load visits data
      visits_data <- chr_get_metric_data(
        "connect_content_visits",
        base_path,
        "daily"
      ) |>
        mutate(date = as.Date(timestamp)) |>
        distinct(timestamp, content_guid, user_guid, .keep_all = TRUE) |>
        collect()

      # Load content metadata
      content_data <- chr_get_metric_data(
        "connect_contents",
        base_path,
        "daily"
      ) |>
        collect() |>
        distinct()

      # Join visits with content metadata to get titles
      if (!is.null(content_data) && nrow(content_data) > 0) {
        tryCatch(
          {
            # Get unique content metadata, taking the most recent entry for each guid
            content_metadata <- content_data |>
              select(all_of(c("guid", "title"))) |>
              filter(!is.na(.data[["guid"]])) |>
              distinct(guid, .keep_all = TRUE)

            # Join visits with content metadata
            visits_data <- visits_data |>
              left_join(
                content_metadata,
                by = c("content_guid" = "guid"),
                relationship = "many-to-one"
              )
          },
          error = function(e) {
            # If joining fails, continue with just visits data
            message("Failed to join content metadata: ", e$message)
          }
        )
      }

      return(visits_data)
    },
    error = function(e) {
      stop("Error reading parquet files from S3: ", e$message)
    }
  )
}

# Initialize as loading state
data <- NULL
has_data <- FALSE

# Define UI
ui <- page_sidebar(
  title = "Chronicle Connect Content Visits Dashboard",
  sidebar = sidebar(
    width = 300,
    h4("Filters"),
    shinycssloaders::withSpinner(uiOutput("sidebar_content"))
  ),
  shinycssloaders::withSpinner(uiOutput("main_content"))
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values for data loading state
  values <- reactiveValues(
    data = NULL
  )

  # Render sidebar content based on loading state
  output$sidebar_content <- renderUI({
    tagList(
      uiOutput("date_range_ui"),

      selectInput(
        "top_n_content",
        "Show Top N Content:",
        choices = c(5, 10, 20, 50, 100),
        selected = 5
      ),

      hr(),

      h5("Data Summary"),
      div(
        class = "small text-muted",
        htmlOutput("data_summary")
      )
    )
  })

  # Render main content based on loading state
  output$main_content <- renderUI({
    tagList(
      card(
        card_header("Content Visits Over Time"),
        shinycssloaders::withSpinner(plotlyOutput("content_distribution_plot"))
      ),

      card(
        card_header("Top Content by Visits"),
        shinycssloaders::withSpinner(DT::dataTableOutput("top_content_table"))
      )
    )
  })

  # Use a timer to delay data loading and allow loading state to be shown
  timer <- reactiveTimer(500) # 500ms delay
  data_loaded <- reactiveVal(FALSE)

  observe({
    timer() # Depend on the timer

    if (!data_loaded()) {
      data_loaded(TRUE)

      # Load data after the timer fires
      loaded_data <- load_data()
      values$data <- loaded_data
    }
  })

  # Dynamic UI for date range (only created after data loads)
  output$date_range_ui <- renderUI({
    req(values$data)

    # Get the date range from data
    max_date <- max(values$data$date, na.rm = TRUE)
    min_date <- min(values$data$date, na.rm = TRUE)

    # Default to 30 days before max date, but not earlier than min date
    default_start <- max(max_date - 30, min_date)

    dateRangeInput(
      "date_range",
      "Date Range:",
      start = default_start,
      end = max_date,
      min = min_date,
      max = max_date
    )
  })

  # Reactive filtered data
  filtered_data <- reactive({
    req(values$data)
    df <- values$data

    if (!is.null(input$date_range)) {
      df <- df |>
        filter(date >= input$date_range[1] & date <= input$date_range[2])
    }

    return(df)
  })

  # Data summary
  output$data_summary <- renderUI({
    req(values$data)
    df <- filtered_data()

    tagList(
      div(
        style = "display: flex; justify-content: space-between; margin-bottom: 8px;",
        span(strong("Visits:")),
        span(format(nrow(df), big.mark = ","))
      ),
      div(
        style = "display: flex; justify-content: space-between; margin-bottom: 8px;",
        span(strong("Users:")),
        span(format(n_distinct(df$user_guid), big.mark = ","))
      ),
      div(
        style = "display: flex; justify-content: space-between; margin-bottom: 8px;",
        span(strong("Content:")),
        span(format(n_distinct(df$content_guid), big.mark = ","))
      ),
      div(
        style = "display: flex; justify-content: space-between;",
        span(strong("Days:")),
        span(n_distinct(df$date))
      )
    )
  })

  # Top content table
  output$top_content_table <- DT::renderDataTable({
    req(values$data)

    # Get the total number of visits and days for percentage calculation
    total_visits <- nrow(filtered_data())
    total_days <- n_distinct(filtered_data()$date)

    top_content <- filtered_data() |>
      count(content_guid, sort = TRUE) |>
      head(as.numeric(input$top_n_content)) |>
      mutate(
        percentage = round(n / total_visits, 4),
        avg_daily = round(n / total_days, 1),
        # Get content title - use title column if available, otherwise create from content_guid
        content_title = map_chr(
          content_guid,
          ~ {
            if ("title" %in% colnames(filtered_data())) {
              title_val <- filtered_data() |>
                filter(content_guid == .x) |>
                pull(title) |>
                first()
              if (is.na(title_val) || title_val == "" || is.null(title_val)) {
                paste("Content", .x)
              } else {
                title_val
              }
            } else {
              paste("Content", .x)
            }
          }
        )
      ) |>
      select(
        `Content Title` = content_title,
        `Total Visits` = n,
        `Percentage` = percentage,
        `Avg Daily` = avg_daily
      )

    DT::datatable(
      top_content,
      options = list(
        pageLength = as.numeric(input$top_n_content),
        scrollX = TRUE,
        lengthChange = FALSE,
        paging = FALSE,
        info = FALSE
      ),
      rownames = FALSE
    ) |>
      formatPercentage("Percentage", digits = 2)
  })

  # Content distribution plot
  output$content_distribution_plot <- renderPlotly({
    req(values$data)

    # Get top content GUIDs
    top_content_guids <- filtered_data() |>
      count(content_guid, sort = TRUE) |>
      head(as.numeric(input$top_n_content)) |>
      pull(content_guid)

    # Create time series data for top content
    content_timeseries <- filtered_data() |>
      filter(content_guid %in% top_content_guids) |>
      group_by(date, content_guid) |>
      summarise(visits = n(), .groups = 'drop') |>
      # Complete the data to include all dates for each content
      complete(date, content_guid, fill = list(visits = 0)) |>
      mutate(
        # Get content title for plotting
        content_title = map_chr(
          content_guid,
          ~ {
            if ("title" %in% colnames(filtered_data())) {
              title_val <- filtered_data() |>
                filter(content_guid == .x) |>
                pull(title) |>
                first()
              if (is.na(title_val) || title_val == "" || is.null(title_val)) {
                paste("Content", .x)
              } else {
                # Truncate long titles for display
                if (nchar(title_val) > 25) {
                  paste0(substr(title_val, 1, 22), "...")
                } else {
                  title_val
                }
              }
            } else {
              paste("Content", .x)
            }
          }
        )
      )

    p <- ggplot(
      content_timeseries,
      aes(x = date, y = visits, color = content_title)
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.5) +
      labs(
        x = "Date",
        y = "Number of Visits",
        color = "Content Title",
        title = paste("Top", input$top_n_content, "Content Visits Over Time")
      ) +
      theme_minimal() +
      scale_y_continuous(labels = comma_format()) +
      theme(legend.position = "bottom")

    ggplotly(p, tooltip = c("colour", "x", "y"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
