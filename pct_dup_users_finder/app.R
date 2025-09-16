library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(arrow)
library(lubridate)

ui <- page_fluid(
  title = "Duplicate Users Finder",
  card(
    card_header("Configuration"),
    selectInput(
      "dupType",
      "Check Duplicates By:",
      choices = c("Username" = "username", "Email" = "email", "Both" = "both")
    ),
    actionButton("refresh", "Refresh Data", icon = icon("refresh"))
  ),
  card(
    full_screen = TRUE,
    card_header("Duplicate Users"),
    DTOutput("dupTable")
  ),
  card(
    card_header("Summary"),
    textOutput("summary")
  )
)

server <- function(input, output, session) {
  # Reactive data reading from S3
  users_data <- eventReactive(
    input$refresh,
    {
      base_path = Sys.getenv(
        "CHRONICLE_BASE_PATH",
        "/var/lib/posit-chronicle/data"
      )

      # First find the latest partition
      ds <- open_dataset(
        paste0(base_path, "/daily/v2/connect_users"),
        partitioning = c("Year", "Month", "Day")
      )

      # Get latest date from partitions
      latest <- ds |>
        select(Year, Month, Day) |>
        collect() |>
        mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) |>
        pull(date) |>
        max()

      # Only read data from latest partition
      df <- ds |>
        filter(
          Year == year(latest),
          Month == month(latest),
          Day == day(latest)
        ) |>
        select(id, username, email, created_at, locked) |>
        collect() |>
        # First check if any record for an ID is locked
        group_by(id) |>
        mutate(ever_locked = any(locked)) |>
        ungroup() |>
        # Filter out any users who were ever locked
        filter(!ever_locked) |>
        # Now keep most recent record for remaining users
        arrange(desc(created_at)) |>
        distinct(id, .keep_all = TRUE) |>
        select(-ever_locked) # Clean up temporary column

      # Ensure required columns exist
      required_cols <- c("id", "username", "email")
      if (!all(required_cols %in% colnames(df))) {
        showNotification(
          "Data must contain columns: id, username, email",
          type = "error"
        )
        return(NULL)
      }

      df
    },
    ignoreNULL = FALSE
  )

  # Find duplicates based on selected criteria
  duplicates <- reactive({
    req(users_data())
    df <- users_data()

    if (input$dupType == "username") {
      dups <- df |>
        group_by(username) |>
        filter(n() > 1) |>
        arrange(username)
    } else if (input$dupType == "email") {
      dups <- df |>
        group_by(email) |>
        filter(n() > 1) |>
        arrange(email)
    } else {
      # both
      dups <- df |>
        group_by(username) |>
        filter(n() > 1) |>
        ungroup() |>
        bind_rows(
          df |>
            group_by(email) |>
            filter(n() > 1)
        ) |>
        distinct() |>
        arrange(username, email)
    }

    dups
  })

  # Render the duplicate users table
  output$dupTable <- renderDT({
    req(duplicates())
    datatable(
      duplicates(),
      options = list(
        pageLength = 25,
        order = list(list(1, 'asc')), # Sort by username by default
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })

  # Display summary statistics
  output$summary <- renderText({
    req(duplicates(), users_data())
    dups <- duplicates()
    total <- nrow(users_data())
    dup_count <- nrow(dups)

    paste0(
      "Found ",
      dup_count,
      " duplicate entries out of ",
      total,
      " total users ",
      "(",
      round(dup_count / total * 100, 1),
      "%)"
    )
  })
}

shinyApp(ui, server)
