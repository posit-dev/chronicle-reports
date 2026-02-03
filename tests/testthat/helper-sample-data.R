# Helper functions for generating sample Chronicle data for tests
#
# This file provides convenient wrappers for tests. The actual data generation
# is handled by internal functions in R/sample-data.R

#' Create a complete sample Chronicle data directory for testing
#'
#' @param base_path Base path where Chronicle data should be created.
#'   If NULL, creates in a temporary directory.
#' @return The base_path
create_sample_chronicle_data <- function(base_path = NULL) {
  if (is.null(base_path)) {
    base_path <- file.path(
      tempdir(),
      paste0("chronicle-test-", as.integer(Sys.time()))
    )
  }

  create_sample_chronicle_data_internal(base_path)
  base_path
}

#' Write raw parquet data in Chronicle's raw data directory structure
#'
#' @param data Data frame with a date column
#' @param base_path Base path for Chronicle data
#' @param metric Metric name (e.g., "connect_users")
#' @param frequency Frequency of data ("daily" or "hourly")
write_raw_parquet_internal <- function(
  data,
  base_path,
  metric,
  frequency = "daily"
) {
  dates <- unique(data$date)

  for (d in dates) {
    date_obj <- as.Date(d, origin = "1970-01-01")
    year <- format(date_obj, "%Y")
    month <- format(date_obj, "%m")
    day <- format(date_obj, "%d")

    subset_data <- data[data$date == d, ]

    metric_path <- file.path(
      base_path,
      frequency,
      "v2",
      metric,
      year,
      month,
      day
    )

    dir.create(metric_path, recursive = TRUE, showWarnings = FALSE)

    arrow::write_parquet(
      subset_data,
      file.path(metric_path, "data.parquet")
    )
  }
}

#' Create raw sample data for tests
#'
#' @param base_path Base path for Chronicle data
create_raw_test_data <- function(base_path) {
  write_raw_parquet_internal(
    sample_raw_connect_users_internal(),
    base_path,
    "connect_users",
    frequency = "daily"
  )
}
