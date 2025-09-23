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

base_path <- "/Users/marktucker/work/chronicle-data/posit-it/2025-09"

# Load data from Chronicle Connect files
load_data <- function() {
  # Try to load visit data using arrow dataset
  visits_base_path <- file.path(base_path, "daily/v2/connect_content_visits")
  visits_data <- NULL

  if (dir.exists(visits_base_path)) {
    tryCatch(
      {
        visits_dataset <- open_dataset(
          visits_base_path,
          hive_style = FALSE,
          partitioning = c("Year", "Month", "Day"),
          format = "parquet"
        )

        visits_data <- visits_dataset %>%
          collect() %>%
          # Create date column from partition columns if they exist
          mutate(
            date = if (all(c("Year", "Month", "Day") %in% colnames(.))) {
              as.Date(paste(
                Year,
                sprintf("%02d", Month),
                sprintf("%02d", Day),
                sep = "-"
              ))
            } else if ("timestamp" %in% colnames(.)) {
              as.Date(timestamp)
            } else {
              # If we can't determine the date, this is an error condition
              NA_Date_
            }
          ) %>%
          # Deduplicate visits based on timestamp, content_guid, and user_guid
          # This removes double-counting from multiple hosts reporting the same visit
          distinct(timestamp, content_guid, user_guid, .keep_all = TRUE)
      },
      error = function(e) {
        message("Failed to load visits data: ", e$message)
        visits_data <- NULL
      }
    )
  }

  # Try to load content metadata using arrow dataset
  content_base_path <- file.path(base_path, "daily/v2/connect_contents")
  content_data <- NULL

  if (dir.exists(content_base_path)) {
    tryCatch(
      {
        content_dataset <- open_dataset(
          content_base_path,
          hive_style = FALSE,
          partitioning = c("Year", "Month", "Day"),
          format = "parquet"
        )
        content_data <- content_dataset %>%
          collect() %>%
          # Add date column for content data as well if needed
          mutate(
            date = if (all(c("Year", "Month", "Day") %in% colnames(.))) {
              as.Date(paste(
                Year,
                sprintf("%02d", Month),
                sprintf("%02d", Day),
                sep = "-"
              ))
            } else if ("timestamp" %in% colnames(.)) {
              as.Date(timestamp)
            } else {
              # Content metadata might not need a date column
              NA_Date_
            }
          ) %>%
          # Deduplicate content metadata records as well
          distinct()
      },
      error = function(e) {
        message("Failed to load content metadata: ", e$message)
        content_data <- NULL
      }
    )
  }

  # If we have both datasets, try to join them
  if (
    !is.null(visits_data) &&
      !is.null(content_data) &&
      nrow(visits_data) > 0 &&
      nrow(content_data) > 0
  ) {
    # Check what columns are available in content_data
    content_cols <- colnames(content_data)

    # Try to find guid and title columns with different possible names
    guid_col <- NULL
    title_col <- NULL

    if ("content_guid" %in% content_cols) {
      guid_col <- "content_guid"
    } else if ("guid" %in% content_cols) {
      guid_col <- "guid"
    } else if ("id" %in% content_cols) {
      guid_col <- "id"
    }

    if ("title" %in% content_cols) {
      title_col <- "title"
    } else if ("name" %in% content_cols) {
      title_col <- "name"
    }

    # Only proceed if we found both columns
    if (!is.null(guid_col) && !is.null(title_col)) {
      tryCatch(
        {
          # Get unique content metadata
          content_metadata <- content_data %>%
            select(all_of(c(guid_col, title_col))) %>%
            distinct() %>%
            filter(!is.na(.data[[guid_col]])) %>%
            rename(content_guid = all_of(guid_col), title = all_of(title_col))

          # Join visits with content metadata
          all_data <- visits_data %>%
            left_join(content_metadata, by = "content_guid")

          return(all_data)
        },
        error = function(e) {
          # If joining fails, just return visits data
          message("Failed to join content metadata: ", e$message)
          return(visits_data)
        }
      )
    }
  }

  # Return just visits data if content metadata isn't available
  return(visits_data)
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
    uiOutput("sidebar_content")
  ),
  uiOutput("main_content")
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values for data loading state
  values <- reactiveValues(
    data = NULL,
    has_data = FALSE,
    loading = TRUE
  )

  # Render sidebar content based on loading state
  output$sidebar_content <- renderUI({
    if (values$loading) {
      div(
        class = "alert alert-info",
        style = "text-align: center;",
        h5(icon("spinner", class = "fa-spin"), " Loading Data..."),
        p("Please wait while Chronicle Connect data is being loaded.")
      )
    } else if (values$has_data) {
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
    } else {
      div(
        class = "alert alert-warning",
        h5("No Data Found"),
        p("Chronicle Connect data files could not be found or loaded."),
        p(
          "Expected location: posit-it/2025-09/daily/v2/connect_content_visits/"
        ),
        p("Please ensure the data files are in the correct location.")
      )
    }
  })

  # Render main content based on loading state
  output$main_content <- renderUI({
    if (values$loading) {
      div(
        class = "container-fluid mt-5",
        div(
          class = "row justify-content-center",
          div(
            class = "col-md-6 text-center",
            div(
              class = "alert alert-info",
              h3(icon("spinner", class = "fa-spin"), " Loading Data"),
              p(
                "Chronicle Connect data is being loaded. This may take a moment..."
              ),
              div(
                class = "progress",
                div(
                  class = "progress-bar progress-bar-striped progress-bar-animated",
                  style = "width: 100%"
                )
              )
            )
          )
        )
      )
    } else if (values$has_data) {
      tagList(
        card(
          card_header("Content Visits Over Time"),
          plotlyOutput("content_distribution_plot")
        ),

        br(),

        card(
          card_header("Top Content by Visits"),
          DT::dataTableOutput("top_content_table")
        )
      )
    } else {
      div(
        class = "container-fluid mt-5",
        div(
          class = "row justify-content-center",
          div(
            class = "col-md-8",
            div(
              class = "alert alert-danger",
              h3("Data Not Available"),
              p(
                "This dashboard requires Chronicle Connect data files to be present in the following directory structure:"
              ),
              code(
                "posit-it/2025-09/daily/v2/connect_content_visits/2025/09/[DD]/chronicle-data-aggregate.parquet"
              ),
              br(),
              br(),
              h5("Expected files:"),
              tags$ul(
                tags$li("Files for dates 01 through 18 in September 2025"),
                tags$li(
                  "Each file should be a Parquet format with Chronicle Connect visit data"
                ),
                tags$li(
                  "Required columns: content_guid, user_guid, path, timestamp"
                )
              ),
              br(),
              p(strong(
                "Please ensure the data files are available and try again."
              ))
            )
          )
        )
      )
    }
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
      values$has_data <- !is.null(loaded_data) && nrow(loaded_data) > 0
      values$loading <- FALSE
    }
  })

  # Dynamic UI for date range (only created after data loads)
  output$date_range_ui <- renderUI({
    req(values$has_data, values$data)

    dateRangeInput(
      "date_range",
      "Date Range:",
      start = min(values$data$date, na.rm = TRUE),
      end = max(values$data$date, na.rm = TRUE),
      min = min(values$data$date, na.rm = TRUE),
      max = max(values$data$date, na.rm = TRUE)
    )
  })

  # Reactive filtered data
  filtered_data <- reactive({
    req(values$has_data, values$data)
    df <- values$data

    if (!is.null(input$date_range)) {
      df <- df %>%
        filter(date >= input$date_range[1] & date <= input$date_range[2])
    }

    return(df)
  })

  # Data summary
  output$data_summary <- renderUI({
    req(values$has_data)
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
    req(values$has_data)

    # Get the total number of visits and days for percentage calculation
    total_visits <- nrow(filtered_data())
    total_days <- n_distinct(filtered_data()$date)

    top_content <- filtered_data() %>%
      count(content_guid, sort = TRUE) %>%
      head(as.numeric(input$top_n_content)) %>%
      mutate(
        percentage = round(n / total_visits, 4),
        avg_daily = round(n / total_days, 1),
        # Get content title - use title column if available, otherwise create from content_guid
        content_title = map_chr(
          content_guid,
          ~ {
            if ("title" %in% colnames(filtered_data())) {
              title_val <- filtered_data() %>%
                filter(content_guid == .x) %>%
                pull(title) %>%
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
      ) %>%
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
    ) %>%
      formatPercentage("Percentage", digits = 2)
  })

  # Content distribution plot
  output$content_distribution_plot <- renderPlotly({
    req(values$has_data)

    # Get top content GUIDs
    top_content_guids <- filtered_data() %>%
      count(content_guid, sort = TRUE) %>%
      head(as.numeric(input$top_n_content)) %>%
      pull(content_guid)

    # Create time series data for top content
    content_timeseries <- filtered_data() %>%
      filter(content_guid %in% top_content_guids) %>%
      group_by(date, content_guid) %>%
      summarise(visits = n(), .groups = 'drop') %>%
      # Complete the data to include all dates for each content
      complete(date, content_guid, fill = list(visits = 0)) %>%
      mutate(
        # Get content title for plotting
        content_title = map_chr(
          content_guid,
          ~ {
            if ("title" %in% colnames(filtered_data())) {
              title_val <- filtered_data() %>%
                filter(content_guid == .x) %>%
                pull(title) %>%
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
      geom_line(size = 1) +
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
