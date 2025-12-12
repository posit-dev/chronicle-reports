#' Build path to Chronicle metric data
#'
#' @keywords internal
#' @noRd
#'
#' @param base_path Base path to Chronicle data directory
#' @param metric Name of the metric (e.g., "connect_users")
#' @param frequency Frequency of data collection ("daily" or "hourly" or "curated")
#'
#' @return Character string with the full path to the metric data
chronicle_path <- function(
  base_path,
  metric = NULL,
  frequency = c("daily", "hourly", "curated")
) {
  frequency <- match.arg(frequency)
  glue::glue("{base_path}/{frequency}/v2/{metric}/", .null = "")
}

#' Load raw Chronicle data (Advanced)
#'
#' Loads raw Chronicle metric data. **Most users should use [chronicle_data()]
#' instead**, which provides curated data that is faster and easier
#' to work with.
#'
#' Use raw data only when you need:
#' - Custom aggregations not available in curated data
#' - Hourly granularity
#' - Specific timestamp filtering
#'
#' @param metric Name of the metric to retrieve (e.g., "connect_users")
#' @param base_path Base path to Chronicle data directory
#' @param frequency Frequency of data collection: "daily" (default) or "hourly"
#' @param ymd Optional list with year, month, day for specific date filtering
#' @param schema Optional Arrow schema for the dataset
#'
#' @return Arrow dataset object
#' @export
#'
#' @examples
#' # Load raw daily Connect users data with sample data
#' sample_path <- chronicle_sample_data()
#' data <- chronicle_raw_data("connect_users", sample_path, frequency = "daily")
#'
#' # View the data
#' dplyr::collect(data)
#'
#' # Filter to a specific date using ymd parameter
#' data <- chronicle_raw_data(
#'   "connect_users",
#'   sample_path,
#'   frequency = "daily",
#'   ymd = list(year = 2024, month = 1, day = 1)
#' )
#' dplyr::collect(data)
#'
#' \dontrun{
#' # Load from production Chronicle data
#' data <- chronicle_raw_data("connect_users", "/var/lib/posit-chronicle/data")
#'
#' # Load hourly data for a specific date
#' data <- chronicle_raw_data(
#'   "connect_users",
#'   "/var/lib/posit-chronicle/data",
#'   frequency = "hourly",
#'   ymd = list(year = 2024, month = 12, day = 10)
#' )
#' }
chronicle_raw_data <- function(
  metric,
  base_path,
  frequency = c("daily", "hourly"),
  ymd = NULL,
  schema = NULL
) {
  frequency <- match.arg(frequency)
  path <- chronicle_path(base_path, metric, frequency)

  if (!is.null(ymd)) {
    # Format month and day with leading zeros
    year_str <- ymd[["year"]]
    month_str <- sprintf("%02d", as.integer(ymd[["month"]]))
    day_str <- sprintf("%02d", as.integer(ymd[["day"]]))
    path <- glue::glue("{path}{year_str}/{month_str}/{day_str}/")
    partitioning <- NULL
  } else {
    partitioning <- c("Year", "Month", "Day")
  }

  arrow::open_dataset(
    path,
    hive_style = FALSE,
    schema = schema,
    format = "parquet",
    partitioning = partitioning
  )
}

#' Load Chronicle data
#'
#' Loads curated Chronicle metric data. This is the
#' recommended way to access Chronicle data for most use cases.
#'
#' @param metric Name of the curated metric to retrieve (e.g., "connect/user_totals")
#' @param base_path Base path to Chronicle data directory
#'
#' @return Arrow dataset object
#' @export
#'
#' @examples
#' # Load curated Connect user totals with sample data
#' sample_path <- chronicle_sample_data()
#' data <- chronicle_data("connect/user_totals", sample_path)
#'
#' # View the data
#' dplyr::collect(data)
#'
#' # Filter by date
#' data |>
#'   dplyr::filter(date >= as.Date("2024-01-02")) |>
#'   dplyr::collect()
#'
#' \dontrun{
#' # Load from production Chronicle data
#' data <- chronicle_data("connect/user_totals", "/var/lib/posit-chronicle/data")
#'
#' # Load from S3
#' data <- chronicle_data("connect/user_totals", "s3://chronicle-bucket/data")
#' }
chronicle_data <- function(
  metric,
  base_path
) {
  path <- chronicle_path(base_path, metric, "curated")

  arrow::open_dataset(
    path,
    hive_style = TRUE,
    partitioning = arrow::schema(date = arrow::date32()),
    format = "parquet"
  )
}


#' List available curated Chronicle metrics
#'
#' Lists all available curated metrics in the Chronicle data
#' directory. This is useful for discovering what data is available before
#' loading it with [chronicle_data()].
#'
#' @param base_path Base path to Chronicle data directory
#'
#' @return Character vector of available metric paths in the format
#'   "product/metric" (e.g., "connect/content_list", "workbench/user_totals")
#' @export
#'
#' @examples
#' # List all available curated metrics with sample data
#' sample_path <- chronicle_sample_data()
#' metrics <- chronicle_list_data(sample_path)
#' print(metrics)
#'
#' # Load one of the available metrics
#' data <- chronicle_data(metrics[1], sample_path)
#' dplyr::collect(data)
#'
#' \dontrun{
#' # List metrics from production Chronicle data
#' metrics <- chronicle_list_data("/var/lib/posit-chronicle/data")
#' print(metrics)
#' }
chronicle_list_data <- function(base_path) {
  data_path <- chronicle_path(base_path, frequency = "curated")
  product_dirs <- list.dirs(data_path, recursive = FALSE, full.names = FALSE)

  # Get two levels of directory names: product/metric
  all_dirs <- unlist(
    lapply(product_dirs, function(product_dir) {
      metric_dirs <- list.dirs(
        file.path(data_path, product_dir),
        recursive = FALSE,
        full.names = FALSE
      )
      file.path(product_dir, metric_dirs)
    }),
    use.names = FALSE
  )

  all_dirs
}


#' List available raw Chronicle metrics
#'
#' Lists all available raw metrics available in the Chronicle data
#' directory at a specified frequency. This is useful for discovering what
#' raw data is available before loading it with [chronicle_raw_data()].
#'
#' **Most users should use [chronicle_list_data()] instead**, which lists
#' curated metrics that are faster and easier to work with.
#'
#' @param base_path Base path to Chronicle data directory
#' @param frequency Frequency of data collection: "daily" (default) or "hourly"
#'
#' @return Character vector of available raw metric names
#'   (e.g., "connect_users", "workbench_sessions")
#' @export
#'
#' @examples
#' # List available daily raw metrics with sample data
#' sample_path <- chronicle_sample_data()
#' metrics <- chronicle_list_raw_data(sample_path, "daily")
#' print(metrics)
#'
#' # Load one of the available metrics
#' data <- chronicle_raw_data(metrics[1], sample_path)
#' dplyr::collect(data)
#'
#' \dontrun{
#' # List metrics from production Chronicle data
#' metrics <- chronicle_list_raw_data("/var/lib/posit-chronicle/data", "daily")
#' print(metrics)
#'
#' # List available hourly raw metrics
#' hourly_metrics <- chronicle_list_raw_data(
#'   "/var/lib/posit-chronicle/data",
#'   "hourly"
#' )
#'
#' # Load one of the available metrics
#' data <- chronicle_raw_data(hourly_metrics[1], "/var/lib/posit-chronicle/data", frequency = "hourly")
#' }
chronicle_list_raw_data <- function(
  base_path,
  frequency = c("daily", "hourly")
) {
  frequency <- match.arg(frequency)
  data_path <- chronicle_path(base_path, frequency = frequency)

  list.dirs(data_path, recursive = FALSE, full.names = FALSE)
}
