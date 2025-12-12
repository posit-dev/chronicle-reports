# Sample data generation for Chronicle Reports

# Package-level environment to cache sample data path
.chronicle_sample_env <- new.env(parent = emptyenv())

#' Get path to sample Chronicle data
#'
#' This function creates a minimal sample Chronicle data directory for
#' exploring package functionality without needing access to real Chronicle
#' data. The sample data is created once per R session and cached.
#'
#' The sample data includes:
#' \itemize{
#'   \item Connect user totals (3 days of data)
#'   \item Connect user list (26 sample users)
#'   \item Workbench user totals (3 days of data)
#'   \item Workbench user list (21 sample users)
#'   \item Raw Connect users data (3 days)
#' }
#'
#' @param refresh Logical. If TRUE, regenerate sample data even if cached.
#'   Default is FALSE.
#'
#' @return Character path to the sample Chronicle data directory
#'
#' @export
#'
#' @examples
#' # Get path to sample data
#' sample_path <- chronicle_sample_path()
#'
#' # List available metrics
#' chronicle_list_data(sample_path)
#'
#' # Load sample data
#' data <- chronicle_data("connect/user_totals", sample_path)
#' dplyr::collect(data)
#'
#' # Clean up (optional - automatically cleaned at session end)
#' unlink(sample_path, recursive = TRUE)
chronicle_sample_path <- function(refresh = FALSE) {
  # Check if we have a cached path and it still exists
  if (!refresh && exists("sample_path", envir = .chronicle_sample_env)) {
    cached_path <- get("sample_path", envir = .chronicle_sample_env)
    if (dir.exists(cached_path)) {
      return(cached_path)
    }
  }

  # Create new sample data directory
  sample_dir <- file.path(tempdir(), "chronicle-sample-data")

  # Remove if it exists (in case of refresh)
  if (dir.exists(sample_dir)) {
    unlink(sample_dir, recursive = TRUE)
  }

  # Create the sample data
  create_sample_chronicle_data_internal(sample_dir)

  # Cache the path
  assign("sample_path", sample_dir, envir = .chronicle_sample_env)

  # Register cleanup on exit
  reg.finalizer(
    .chronicle_sample_env,
    function(e) {
      if (exists("sample_path", envir = e)) {
        path <- get("sample_path", envir = e)
        if (dir.exists(path)) {
          unlink(path, recursive = TRUE)
        }
      }
    },
    onexit = TRUE
  )

  sample_dir
}

# Internal helper functions for creating sample data
# These are not exported but are used by chronicle_sample_path()

#' @noRd
sample_connect_user_totals_internal <- function() {
  data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    named_users = c(25L, 25L, 26L),
    active_users_1day = c(12L, 15L, 10L),
    publishers = c(5L, 5L, 6L),
    stringsAsFactors = FALSE
  )
}

#' @noRd
sample_connect_user_list_internal <- function() {
  data.frame(
    date = as.Date(rep("2024-01-03", 26)),
    username = paste0("user", sprintf("%02d", 1:26)),
    email = paste0("user", 1:26, "@example.com"),
    first_name = paste0("First", 1:26),
    last_name = paste0("Last", 1:26),
    environment = c(
      rep("Production", 10),
      rep("Development", 8),
      rep("Staging", 3),
      rep(NA_character_, 5)
    ),
    user_role = c(
      rep("viewer", 15),
      rep("publisher", 8),
      rep("administrator", 3)
    ),
    last_active_at = as.POSIXct(
      c(
        rep("2024-01-03 10:00:00", 10),
        rep("2024-01-02 14:00:00", 8),
        rep("2024-01-01 09:00:00", 5),
        rep("2023-12-15 08:00:00", 3)
      ),
      tz = "UTC"
    ),
    active_today = c(
      rep(TRUE, 10),
      rep(FALSE, 16)
    ),
    stringsAsFactors = FALSE
  )
}

#' @noRd
sample_workbench_user_totals_internal <- function() {
  data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    named_users = c(20L, 20L, 21L),
    active_users_1day = c(8L, 10L, 7L),
    administrators = c(3L, 3L, 4L),
    super_administrators = c(1L, 1L, 1L),
    stringsAsFactors = FALSE
  )
}

#' @noRd
sample_workbench_user_list_internal <- function() {
  data.frame(
    date = as.Date(rep("2024-01-03", 21)),
    username = paste0("wbuser", sprintf("%02d", 1:21)),
    user_role = c(
      rep("user", 16),
      rep("admin", 4),
      "super_admin"
    ),
    environment = c(
      rep("Production", 8),
      rep("Development", 6),
      rep("Staging", 4),
      rep(NA_character_, 3)
    ),
    last_active_at = as.POSIXct(
      c(
        rep("2024-01-03 11:00:00", 7),
        rep("2024-01-02 15:00:00", 8),
        rep("2024-01-01 10:00:00", 4),
        rep("2023-12-20 09:00:00", 2)
      ),
      tz = "UTC"
    ),
    active_today = c(
      rep(TRUE, 7),
      rep(FALSE, 14)
    ),
    stringsAsFactors = FALSE
  )
}

#' @noRd
sample_raw_connect_users_internal <- function() {
  dates <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))

  users <- lapply(dates, function(d) {
    data.frame(
      date = d,
      timestamp = as.POSIXct(paste(d, "08:00:00"), tz = "UTC"),
      email = paste0("user", 1:15, "@example.com"),
      created_at = as.POSIXct("2023-06-01 10:00:00", tz = "UTC"),
      last_active_at = as.POSIXct(paste(d, "07:00:00"), tz = "UTC"),
      locked = rep(FALSE, 15),
      user_role = c(
        rep("viewer", 10),
        rep("publisher", 4),
        "administrator"
      ),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, users)
}

#' @noRd
write_sample_parquet_internal <- function(
  data,
  base_path,
  metric,
  partition_col = "date"
) {
  metric_path <- file.path(base_path, "curated", "v2", metric)

  arrow::write_dataset(
    data,
    path = metric_path,
    format = "parquet",
    partitioning = partition_col,
    hive_style = TRUE
  )
}

#' @noRd
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

#' @noRd
create_sample_chronicle_data_internal <- function(base_path) {
  dir.create(base_path, recursive = TRUE, showWarnings = FALSE)

  # Create curated data
  write_sample_parquet_internal(
    sample_connect_user_totals_internal(),
    base_path,
    "connect/user_totals"
  )

  write_sample_parquet_internal(
    sample_connect_user_list_internal(),
    base_path,
    "connect/user_list"
  )

  write_sample_parquet_internal(
    sample_workbench_user_totals_internal(),
    base_path,
    "workbench/user_totals"
  )

  write_sample_parquet_internal(
    sample_workbench_user_list_internal(),
    base_path,
    "workbench/user_list"
  )

  # Create raw data
  write_raw_parquet_internal(
    sample_raw_connect_users_internal(),
    base_path,
    "connect_users",
    frequency = "daily"
  )

  invisible(base_path)
}
