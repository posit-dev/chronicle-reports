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
#' Thin wrapper over the package internal so tests and the bundled sample data
#' lay out raw partitions identically.
#'
#' @param data Data frame with a date column
#' @param base_path Base path for Chronicle data
#' @param metric Metric name (e.g., "connect_users")
#' @param frequency Frequency of data ("daily")
write_raw_parquet_internal <- function(
  data,
  base_path,
  metric,
  frequency = "daily"
) {
  write_sample_raw_parquet_internal(data, base_path, metric, frequency)
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
