# Small hand-computable fixtures for the usage data layer.
#
# The window used by the tests is anchored on `USAGE_TEST_TODAY`, so the
# 12-month window runs 2025-12 through 2026-11 (the last month is partial).

USAGE_TEST_TODAY <- as.Date("2026-11-06")

#' Write raw daily rows, adding the `date` column the writer partitions on
#' @param data Data frame with a `timestamp` column (POSIXct, UTC)
write_usage_raw <- function(data, base_path, metric) {
  data$date <- as.Date(data$timestamp)
  write_sample_raw_parquet_internal(data, base_path, metric)
}

#' Build a Chronicle data directory with just the datasets the usage report
#' reads. Authenticated users are identified by `user_guid`; anonymous hits use
#' either NA or an empty string.
create_usage_fixture <- function(base_path = NULL) {
  if (is.null(base_path)) {
    base_path <- tempfile("chronicle-usage-")
  }

  hits <- data.frame(
    date = as.Date(c(
      "2026-10-03",
      "2026-10-03",
      "2026-10-03",
      "2026-10-20",
      "2026-10-20",
      "2026-11-02",
      "2026-11-02",
      "2024-01-01"
    )),
    product_id = c("p1", "p1", "p1", "p1", "p1", "p1", "p2", "p1"),
    environment = "prod",
    content_guid = c("c1", "c1", "c1", "c2", "c2", "c1", "c9", "c1"),
    user_guid = c("u1", "u2", NA, "u1", "", "u1", "u9", "u5"),
    hits = c(5L, 2L, 4L, 3L, 1L, 1L, 7L, 100L),
    app_mode = c(
      "shiny",
      "shiny",
      "shiny",
      "quarto-static",
      "quarto-static",
      "shiny",
      "shiny",
      "shiny"
    ),
    stringsAsFactors = FALSE
  )
  write_sample_parquet_internal(
    hits,
    base_path,
    "connect/content_hits_totals_by_user"
  )

  sessions <- data.frame(
    date = as.Date(c(
      "2026-10-03",
      "2026-10-03",
      "2026-10-04",
      "2026-10-04",
      "2026-11-01"
    )),
    product_id = "p1",
    environment = "prod",
    user_guid = c("w1", "w2", "w1", "w1", "w3"),
    session_type = c("RStudio", "Positron", "RStudio", "Positron", "RStudio"),
    sessions_started = c(2L, 1L, 1L, 1L, 4L),
    stringsAsFactors = FALSE
  )
  write_sample_parquet_internal(
    sessions,
    base_path,
    "workbench/session_start_totals_by_user"
  )

  published <- data.frame(
    timestamp = as.POSIXct(
      c(
        "2026-10-05 09:00:00",
        "2026-10-05 15:00:00",
        "2026-10-05 16:00:00",
        "2026-10-25 12:00:00",
        "2026-11-03 12:00:00",
        "2024-01-01 12:00:00"
      ),
      tz = "UTC"
    ),
    product_id = "p1",
    content_guid = c("c1", "c1", "c2", "c1", "c3", "c1"),
    user_guid = "u1",
    bundle_id = c("1", "2", "3", "4", "5", "6"),
    stringsAsFactors = FALSE
  )
  write_usage_raw(published, base_path, "connect_content_published")

  # c2 changed type between snapshots: the latest snapshot must win.
  # c3 never appears in a snapshot, so its type is unknown.
  contents <- data.frame(
    timestamp = as.POSIXct(
      c(
        "2026-10-01 08:00:00",
        "2026-10-01 08:00:00",
        "2026-11-05 08:00:00",
        "2026-11-05 08:00:00"
      ),
      tz = "UTC"
    ),
    product_id = "p1",
    guid = c("c1", "c2", "c1", "c2"),
    app_mode = c("shiny", "shiny", "shiny", "quarto-static"),
    stringsAsFactors = FALSE
  )
  write_usage_raw(contents, base_path, "connect_contents")

  base_path
}
