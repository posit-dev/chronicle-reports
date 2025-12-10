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
chr_path <- function(
  base_path,
  metric = NULL,
  frequency = c("daily", "hourly", "curated")
) {
  frequency <- match.arg(frequency)
  glue::glue("{base_path}/{frequency}/v2/{metric}/")
}

#' Load raw Chronicle data (Advanced)
#'
#' Loads raw Chronicle metric data. **Most users should use [chr_data()]
#' instead**, which provides pre-aggregated data that is faster and easier
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
#' \dontrun{
#' # Load raw daily Connect users data
#' data <- chr_raw_data("connect_users", base_path, frequency = "daily")
#'
#' # Load hourly data for a specific date
#' data <- chr_raw_data(
#'   "connect_users",
#'   base_path,
#'   frequency = "hourly",
#'   ymd = list(year = 2024, month = 12, day = 10)
#' )
#' }
chr_raw_data <- function(
  metric,
  base_path,
  frequency = c("daily", "hourly"),
  ymd = NULL,
  schema = NULL
) {
  frequency <- match.arg(frequency)
  path <- chr_path(base_path, metric, frequency)

  if (!is.null(ymd)) {
    path <- glue::glue("{path}{ymd[['year']]}/{ymd[['month']]}/{ymd[['day']]}/")
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
#' Loads pre-aggregated (curated) Chronicle metric data. This is the
#' recommended way to access Chronicle data for most use cases.
#'
#' For raw/unaggregated data, see [chr_raw_data()].
#'
#' @param metric Name of the curated metric to retrieve (e.g., "connect/user_totals")
#' @param base_path Base path to Chronicle data directory
#'
#' @return Arrow dataset object
#' @export
#'
#' @examples
#' \dontrun{
#' # Load curated Connect user totals
#' data <- chr_data("connect/user_totals", "/var/lib/posit-chronicle/data")
#'
#' # Load from S3
#' data <- chr_data("connect/user_totals", "s3://chronicle-bucket/data")
#' }
chr_data <- function(
  metric,
  base_path
) {
  path <- chr_path(base_path, metric, "curated")

  arrow::open_dataset(
    path,
    hive_style = TRUE,
    partitioning = arrow::schema(date = arrow::date32()),
    format = "parquet"
  )
}

# Deprecated aliases for backward compatibility --------------------------------

#' @rdname chr_raw_data
#' @export
chr_get_metric_data <- function(
  metric,
  base_path,
  frequency = c("daily", "hourly"),
  ymd = NULL,
  schema = NULL
) {
  lifecycle::deprecate_warn(
    "0.2.0",
    "chr_get_metric_data()",
    "chr_raw_data()"
  )
  chr_raw_data(metric, base_path, frequency, ymd, schema)
}

#' @rdname chr_data
#' @export
chr_get_curated_metric_data <- function(
  metric,
  base_path
) {
  lifecycle::deprecate_warn(
    "0.2.0",
    "chr_get_curated_metric_data()",
    "chr_data()"
  )
  chr_data(metric, base_path)
}
