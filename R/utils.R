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
#' # Filter to a specific date using dplyr
#' first_date <- min(dplyr::collect(data)$date)
#' filtered <- data |>
#'   dplyr::filter(date == first_date) |>
#'   dplyr::collect()
#' head(filtered)
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
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", APP_CONFIG$DEFAULT_BASE_PATH),
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
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", APP_CONFIG$DEFAULT_BASE_PATH)
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
chronicle_list_data <- function(
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", APP_CONFIG$DEFAULT_BASE_PATH)
) {
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
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", APP_CONFIG$DEFAULT_BASE_PATH),
  frequency = c("daily", "hourly")
) {
  frequency <- match.arg(frequency)
  data_path <- chronicle_path(base_path, frequency = frequency)

  list.dirs(data_path, recursive = FALSE, full.names = FALSE)
}


#' Write an empty "No data available" CSV
#'
#' Used by download handlers when reactive data is NULL or empty.
#'
#' @param file Path to write the CSV
#' @keywords internal
#' @noRd
write_empty_csv <- function(file) {
  utils::write.csv(
    data.frame(message = "No data available"),
    file,
    row.names = FALSE
  )
}

#' Normalize environment column values
#'
#' Replaces NA, empty string, and single-space values with "(Not Set)".
#'
#' @param x Character vector of environment values
#' @return Character vector with normalized values
#' @keywords internal
#' @noRd
normalize_environment <- function(x) {
  ifelse(is.na(x) | trimws(x) == "", "(Not Set)", x)
}

#' Enrich a summary data frame with user and content names
#'
#' Joins user names and content titles onto a summary data frame that contains
#' `user_guid` and/or `content_guid` columns. Replaces missing user names with
#' "(anonymous)" and normalizes environment values.
#'
#' @param summary_df Data frame with columns like user_guid, content_guid, environment
#' @param user_df Optional data frame with at least "id" and "username" columns
#' @param content_df Optional data frame with at least "id", "environment", and "title" columns
#'
#' @return The enriched data frame
#' @keywords internal
#' @noRd
enrich_with_names <- function(summary_df, user_df = NULL, content_df = NULL) {
  if (
    !is.null(user_df) &&
      all(c("id", "username") %in% names(user_df)) &&
      "user_guid" %in% names(summary_df)
  ) {
    user_join <- user_df |>
      dplyr::select("id", "username")
    summary_df <- summary_df |>
      dplyr::left_join(user_join, by = c("user_guid" = "id"))
  }

  if (
    !is.null(content_df) &&
      all(c("id", "environment", "title") %in% names(content_df)) &&
      all(c("content_guid", "environment") %in% names(summary_df))
  ) {
    content_join <- content_df |>
      dplyr::select("id", "environment", "title")
    summary_df <- summary_df |>
      dplyr::left_join(
        content_join,
        by = c("content_guid" = "id", "environment" = "environment")
      )
  }

  if ("user_guid" %in% names(summary_df)) {
    if ("username" %in% names(summary_df)) {
      summary_df <- summary_df |>
        dplyr::mutate(
          username = ifelse(
            is.na(.data$user_guid) | is.na(.data$username),
            "(anonymous)",
            .data$username
          )
        )
    } else {
      summary_df <- summary_df |>
        dplyr::mutate(
          username = ifelse(
            is.na(.data$user_guid), "(anonymous)", .data$user_guid
          )
        )
    }
  }

  if ("environment" %in% names(summary_df)) {
    summary_df <- summary_df |>
      dplyr::mutate(environment = normalize_environment(.data$environment))
  }

  summary_df
}

#' Compute average duration in minutes from total duration and session count
#'
#' Expects a data frame with `total_sessions` and `total_duration` columns.
#' Adds `avg_duration_minutes` and removes the intermediate `total_duration`.
#'
#' @param df Data frame with `total_sessions` and `total_duration` columns
#'
#' @return The data frame with `avg_duration_minutes` added and `total_duration` removed
#' @keywords internal
#' @noRd
add_avg_duration <- function(df) {
  df |>
    dplyr::mutate(
      avg_duration_minutes = dplyr::if_else(
        is.na(.data$total_duration) | .data$total_sessions == 0,
        NA_real_,
        round((.data$total_duration / .data$total_sessions) / 60, 2)
      )
    ) |>
    dplyr::select(-"total_duration")
}

#' Create a card header with a CSV download button
#'
#' Creates a bslib card header with a title on the left and a small
#' CSV download button on the right. Used in dashboard charts to allow
#' users to download the chart data.
#'
#' @param title Character string or UI element for the card header title
#' @param download_id Character string for the Shiny download button output ID
#'
#' @return A bslib card_header element
#' @keywords internal
#' @noRd
card_header_with_download <- function(title, download_id) {
  bslib::card_header(
    class = "d-flex justify-content-between align-items-center",
    title,
    shiny::downloadButton(
      download_id,
      label = "CSV",
      class = "btn btn-outline-secondary btn-sm",
      icon = shiny::icon("download")
    )
  )
}
