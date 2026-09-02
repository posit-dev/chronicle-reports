#' Resolve a Chronicle data path to an Arrow-compatible source
#'
#' Local paths are returned unchanged. For S3 paths, returns an Arrow
#' filesystem rooted at the path. On Posit Connect, attempts to exchange the
#' content session token for temporary AWS credentials via a configured AWS
#' integration; if that fails (e.g., no AWS integration is assigned to the
#' content), falls back to Arrow's default AWS credential chain.
#'
#' @param path Directory path (local or s3://)
#'
#' @return The path unchanged (local), or an Arrow SubTreeFileSystem (S3)
#'
#' @keywords internal
#' @noRd
chronicle_fs <- function(path) {
  if (!startsWith(path, "s3://")) {
    return(path)
  }

  if (Sys.getenv("POSIT_PRODUCT") == "CONNECT") {
    fs <- tryCatch(
      {
        credentials <- connectapi::get_aws_content_credentials(
          connectapi::connect()
        )
        arrow::s3_bucket(
          sub("^s3://", "", path),
          access_key = credentials$access_key_id,
          secret_key = credentials$secret_access_key,
          session_token = credentials$session_token
        )
      },
      error = function(e) {
        message(
          "Could not fetch AWS credentials from Posit Connect, ",
          "falling back to default AWS credentials: ",
          conditionMessage(e)
        )
        NULL
      }
    )
    if (!is.null(fs)) {
      return(fs)
    }
  }

  arrow::SubTreeFileSystem$create(path)
}

#' Build path to Chronicle metric data
#'
#' @keywords internal
#' @noRd
#'
#' @param base_path Base path to Chronicle data directory
#' @param metric Name of the metric (e.g., "connect_users")
#' @param frequency Frequency of data collection ("daily" or "curated")
#'
#' @return Character string with the full path to the metric data
chronicle_path <- function(
  base_path,
  metric = NULL,
  frequency = c("daily", "curated")
) {
  frequency <- match.arg(frequency)
  glue::glue("{base_path}/{frequency}/v2/{metric}/", .null = "")
}

#' List immediate subdirectories of a path
#'
#' Works with both local filesystem paths and S3 URIs.
#' For S3 paths, uses Arrow's S3 filesystem to list directories.
#'
#' @param path Directory path (local or s3://)
#'
#' @return Character vector of subdirectory names (basenames only)
#'
#' @keywords internal
#' @noRd
chronicle_list_dirs <- function(path) {
  if (startsWith(path, "s3://")) {
    fs <- chronicle_fs(path)
    selector <- arrow::FileSelector$create("", recursive = FALSE)
    info <- fs$GetFileInfo(selector)
    dirs <- Filter(function(fi) fi$type == arrow::FileType$Directory, info)
    vapply(dirs, function(fi) basename(fi$path), character(1))
  } else {
    list.dirs(path, recursive = FALSE, full.names = FALSE)
  }
}

#' Check whether a Chronicle dataset directory exists
#'
#' Works with both local filesystem paths and S3 URIs. On S3 a "directory"
#' exists when any objects are stored under the prefix. If the S3 existence
#' check itself fails (e.g., a credentials problem), returns TRUE so that
#' [arrow::open_dataset()] surfaces the real error instead of the path being
#' misclassified as missing.
#'
#' @param path Directory path (local or s3://)
#'
#' @return TRUE if the path exists, FALSE otherwise
#'
#' @keywords internal
#' @noRd
chronicle_dataset_exists <- function(path) {
  if (!startsWith(path, "s3://")) {
    return(dir.exists(path))
  }

  tryCatch(
    {
      fs <- chronicle_fs(path)
      info <- fs$GetFileInfo("")[[1]]
      info$type != arrow::FileType$NotFound
    },
    error = function(e) TRUE
  )
}

#' Load raw Chronicle data (Advanced)
#'
#' Loads raw Chronicle metric data. **Most users should use [chronicle_data()]
#' instead**, which provides curated data that is faster and easier
#' to work with.
#'
#' Use raw data only when you need:
#' - Custom aggregations not available in curated data
#' - Specific timestamp filtering
#'
#' @param metric Name of the metric to retrieve (e.g., "connect_users")
#' @param base_path Base path to Chronicle data directory
#' @param frequency Frequency of data collection: "daily" (default)
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
#' # Load daily data for a specific date
#' data <- chronicle_raw_data(
#'   "connect_users",
#'   "/var/lib/posit-chronicle/data",
#'   ymd = list(year = 2024, month = 12, day = 10)
#' )
#' }
chronicle_raw_data <- function(
  metric,
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", APP_CONFIG$DEFAULT_BASE_PATH),
  frequency = c("daily"),
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
    chronicle_fs(path),
    hive_style = FALSE,
    schema = schema,
    format = "parquet",
    partitioning = partitioning
  )
}

#' Find the most recent Y/M/D partition of a daily dataset
#'
#' Daily data is partitioned as `<metric>/YYYY/MM/DD/` and is not hive-styled,
#' so the newest partition is found by walking one level at a time and taking
#' the numerically largest directory at each step. This keeps the cost to three
#' directory listings instead of a scan of the whole daily tree, which matters
#' on S3.
#'
#' @param metric Name of the raw metric (e.g. "connect_users")
#' @param base_path Base path to Chronicle data directory
#'
#' @return A list with `year`, `month` and `day` character components suitable
#'   for the `ymd` argument of [chronicle_raw_data()], or `NULL` when the
#'   dataset is absent or holds no date partitions.
#'
#' @keywords internal
#' @noRd
chronicle_latest_daily_ymd <- function(metric, base_path) {
  path <- chronicle_path(base_path, metric, "daily")

  if (!chronicle_dataset_exists(path)) {
    return(NULL)
  }

  latest_child <- function(parent) {
    dirs <- chronicle_list_dirs(parent)
    dirs <- dirs[grepl("^[0-9]+$", dirs)]
    if (length(dirs) == 0) {
      return(NULL)
    }
    dirs[which.max(as.integer(dirs))]
  }

  year <- latest_child(path)
  if (is.null(year)) {
    return(NULL)
  }

  month <- latest_child(paste0(path, year, "/"))
  if (is.null(month)) {
    return(NULL)
  }

  day <- latest_child(paste0(path, year, "/", month, "/"))
  if (is.null(day)) {
    return(NULL)
  }

  list(year = year, month = month, day = day)
}

#' Build a user GUID to username lookup from the latest daily snapshot
#'
#' Resolves display names for the user GUIDs referenced by other Chronicle
#' datasets -- `owner_guid` on `connect/content_list`, `user_guid` on
#' `connect/content_hits_totals_by_user`, and `user_guid` on the Workbench
#' session datasets.
#'
#' The curated `user_list` dataset is deliberately not used for this. It
#' deduplicates users by email address, so when two accounts share an email
#' only one survives curation and content owned by the others has no row to
#' join against, surfacing in reports as an unattributed owner. The daily layer
#' keys on GUID and so retains every account.
#'
#' Only the most recent daily partition is read. That is enough to name every
#' account the product currently knows about, and costs a single file read
#' rather than a scan of the full daily history.
#'
#' @param metric Name of the raw users metric, either `"connect_users"` or
#'   `"workbench_users"`.
#' @param base_path Base path to Chronicle data directory
#'
#' @return A two-column data frame of `id` and `username`, one row per GUID
#'   holding the most recently observed username, or `NULL` when the daily
#'   dataset is unavailable or lacks the required columns. Callers should treat
#'   `NULL` as "fall back to the curated user list".
#'
#' @export
#'
#' @examples
#' # Build a lookup from the bundled sample data
#' sample_path <- chronicle_sample_data()
#' lookup <- chronicle_user_lookup("connect_users", sample_path)
#' head(lookup)
chronicle_user_lookup <- function(
  metric,
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", APP_CONFIG$DEFAULT_BASE_PATH)
) {
  ymd <- chronicle_latest_daily_ymd(metric, base_path)
  if (is.null(ymd)) {
    return(NULL)
  }

  df <- tryCatch(
    chronicle_raw_data(metric, base_path, frequency = "daily", ymd = ymd) |>
      dplyr::select(dplyr::any_of(c("id", "username", "timestamp"))) |>
      dplyr::collect(),
    error = function(e) {
      message(
        "Could not read the latest daily '",
        metric,
        "' snapshot: ",
        conditionMessage(e)
      )
      NULL
    }
  )

  if (
    is.null(df) ||
      nrow(df) == 0 ||
      !all(c("id", "username") %in% names(df))
  ) {
    return(NULL)
  }

  # The daily layer holds one row per (host, environment, GUID) for every
  # observed state change, so collapse to the most recent row per GUID. Keying
  # on GUID -- rather than email or username -- is precisely what keeps
  # distinct accounts distinct.
  if ("timestamp" %in% names(df)) {
    df <- df[order(df$id, df$timestamp), , drop = FALSE]
  }
  df <- df[!duplicated(df$id, fromLast = TRUE), , drop = FALSE]

  data.frame(
    id = df$id,
    username = df$username,
    stringsAsFactors = FALSE
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
#' @return Arrow dataset object, or `NULL` (with a message) when the curated
#'   dataset directory does not exist yet -- for example on a new install
#'   where the Chronicle Agent has not completed its first
#'   collection and curation cycle, or when data for that product is not
#'   being collected.
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

  if (!chronicle_dataset_exists(path)) {
    message(
      "Chronicle dataset '",
      metric,
      "' not found at ",
      path,
      " -- the dataset has not been curated yet. Confirm that Chronicle ",
      "data collection is enabled for this product, that at least 30 hours ",
      "have passed since collection began, and that base_path is correct."
    )
    return(NULL)
  }

  arrow::open_dataset(
    chronicle_fs(path),
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
  product_dirs <- chronicle_list_dirs(data_path)

  # Get two levels of directory names: product/metric
  all_dirs <- unlist(
    lapply(product_dirs, function(product_dir) {
      # Build path to product directory
      if (startsWith(data_path, "s3://")) {
        product_path <- paste0(sub("/+$", "", data_path), "/", product_dir, "/")
      } else {
        product_path <- file.path(data_path, product_dir)
      }
      metric_dirs <- chronicle_list_dirs(product_path)
      file.path(product_dir, metric_dirs)
    }),
    use.names = FALSE
  )

  all_dirs
}


#' List available raw Chronicle metrics
#'
#' Lists all available raw daily metrics available in the Chronicle data
#' directory. This is useful for discovering what
#' raw data is available before loading it with [chronicle_raw_data()].
#'
#' **Most users should use [chronicle_list_data()] instead**, which lists
#' curated metrics that are faster and easier to work with.
#'
#' @param base_path Base path to Chronicle data directory
#' @param frequency Frequency of data collection: "daily" (default)
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
#' }
chronicle_list_raw_data <- function(
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", APP_CONFIG$DEFAULT_BASE_PATH),
  frequency = c("daily")
) {
  frequency <- match.arg(frequency)
  data_path <- chronicle_path(base_path, frequency = frequency)

  chronicle_list_dirs(data_path)
}
