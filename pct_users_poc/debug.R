# Load required packages
library(shiny)
library(arrow)
library(dplyr)
library(bslib)
library(ggplot2)
library(DT)
library(tidyr)
library(lubridate)

# Function to read all parquet files with error handling
read_all_parquet <- function(
  s3_path = "s3://posit-dsp-chronicle/daily/v2/connect_users"
) {
  # Read and combine all files
  tryCatch(
    {
      data <- open_dataset(
        s3_path,
        format = "parquet",
        partitioning = c("Year", "Month", "Day"),
        hive_style = FALSE
      )
      # Process the data to get the metrics we want
      data <- data %>%
        # Filter out locked users first
        filter(!locked) %>%
        mutate(
          date = as.Date(timestamp)
        ) %>%
        collect() %>% # Bring data into memory to avoid arrow limitations
        group_by(date) %>%
        summarise(
          total_users = n_distinct(username),
          active_users = n_distinct(
            username[
              !is.na(last_active_at) &
                as.Date(last_active_at) >= date - 7
            ]
          ),
          publishers = n_distinct(
            username[user_role %in% c("publisher", "admin")]
          ),
          # Count users active on this specific day
          users_active_today = n_distinct(
            username[as.Date(last_active_at) == date]
          ),
          .groups = "drop"
        ) %>%
        arrange(date)

      return(data)
    },
    error = function(e) {
      stop("Error reading parquet files from S3: ", e$message)
    }
  )
}


# Function to read all parquet files with error handling
users_list <- function(
  s3_path = "s3://posit-dsp-chronicle/daily/v2/connect_users"
) {
  # Read and combine all files
  tryCatch(
    {
      data <- open_dataset(
        s3_path,
        format = "parquet",
        partitioning = c("Year", "Month", "Day"),
        hive_style = FALSE
      )
      # Process the data to get the metrics we want
      data <- data %>%
        # Filter out locked users first
        filter(!locked) %>%
        mutate(
          date = as.Date(timestamp)
        ) %>%
        collect() %>% # Bring data into memory to avoid arrow limitations
        # Keep only the most recent row for each username
        group_by(username) %>%
        slice_max(timestamp, n = 1) %>%
        ungroup() %>%
        arrange(date)

      return(data)
    },
    error = function(e) {
      stop("Error reading parquet files from S3: ", e$message)
    }
  )
}

data <-
  tryCatch(
    {
      users_list()
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
