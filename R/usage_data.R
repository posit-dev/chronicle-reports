# Data access for the usage report.
#
# Everything the usage report draws is produced by `chronicle_usage_data()`
# as a plain list of small monthly data frames. The report only ever consumes
# that list, so it does not depend on how the data was read. To support another
# storage backend, provide a function that returns the same list (see
# `chronicle_usage_from_json()` for a second producer).
#
# Counts are calendar-month counts, always keyed by `product_id`, which ties the
# usage back to the license it was collected for.

USAGE_UNSPECIFIED_PRODUCT <- "(unspecified)"
USAGE_UNKNOWN_APP_MODE <- "unknown"
USAGE_TABLE_NAMES <- c(
  "active_users",
  "content_hits",
  "content_deployments",
  "sessions"
)

#' First day of the month containing each date
#' @noRd
usage_month <- function(x) {
  if (inherits(x, "POSIXt")) {
    as.Date(format(x, "%Y-%m-01", tz = "UTC"))
  } else {
    as.Date(format(x, "%Y-%m-01"))
  }
}

#' First and last month of the reporting window
#'
#' The window is `months` calendar months ending with the month of `today`.
#' @return Length-2 Date vector, the first day of the first and last month
#' @noRd
usage_window <- function(months, today) {
  last <- usage_month(today)
  first <- seq(last, by = "-1 month", length.out = months)[months]
  c(first, last)
}

#' @noRd
usage_label_product <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | x == "", USAGE_UNSPECIFIED_PRODUCT, x)
}

#' @noRd
usage_label_app_mode <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | x == "", USAGE_UNKNOWN_APP_MODE, x)
}

#' Empty versions of the four report tables
#' @noRd
usage_empty_tables <- function(window = NULL) {
  tables <- list(
    active_users = dplyr::tibble(
      month = as.Date(character()),
      product = character(),
      product_id = character(),
      users = numeric()
    ),
    content_hits = dplyr::tibble(
      month = as.Date(character()),
      product_id = character(),
      app_mode = character(),
      authenticated = logical(),
      hits = numeric()
    ),
    content_deployments = dplyr::tibble(
      month = as.Date(character()),
      product_id = character(),
      app_mode = character(),
      deployments = numeric(),
      unique_content = numeric()
    ),
    sessions = dplyr::tibble(
      month = as.Date(character()),
      product_id = character(),
      session_type = character(),
      sessions = numeric()
    )
  )
  attr(tables, "window") <- window
  tables
}

#' Add constant columns that older datasets may not have
#' @noRd
usage_with_defaults <- function(ds, defaults) {
  for (col in setdiff(names(defaults), names(ds))) {
    ds <- dplyr::mutate(ds, !!!stats::setNames(list(defaults[[col]]), col))
  }
  ds
}

#' Count distinct users per month and product
#' @param users Data frame with `date`, `product_id` and `user_guid`
#' @noRd
usage_monthly_users <- function(users, product) {
  if (nrow(users) == 0) {
    return(usage_empty_tables()$active_users)
  }
  users |>
    dplyr::mutate(
      month = usage_month(.data$date),
      product_id = usage_label_product(.data$product_id)
    ) |>
    dplyr::distinct(.data$month, .data$product_id, .data$user_guid) |>
    dplyr::count(.data$month, .data$product_id, name = "users") |>
    dplyr::mutate(product = product, users = as.numeric(.data$users)) |>
    dplyr::select("month", "product", "product_id", "users")
}

#' Connect content hits and active users from the curated per-user hits
#' @return list(active_users, content_hits)
#' @noRd
usage_read_connect_hits <- function(base_path, window) {
  empty <- usage_empty_tables()
  ds <- chronicle_data("connect/content_hits_totals_by_user", base_path)
  if (is.null(ds)) {
    return(list(active_users = empty$active_users, content_hits = empty$content_hits))
  }

  ds <- ds |>
    usage_with_defaults(list(product_id = "", app_mode = "")) |>
    dplyr::filter(.data$date >= !!window[1]) |>
    dplyr::mutate(authenticated = !is.na(.data$user_guid) & .data$user_guid != "")

  hits <- ds |>
    dplyr::group_by(.data$date, .data$product_id, .data$app_mode, .data$authenticated) |>
    dplyr::summarise(hits = sum(.data$hits, na.rm = TRUE)) |>
    dplyr::collect()

  users <- ds |>
    dplyr::filter(.data$authenticated) |>
    dplyr::distinct(.data$date, .data$product_id, .data$user_guid) |>
    dplyr::collect()

  hits <- hits |>
    dplyr::mutate(
      month = usage_month(.data$date),
      product_id = usage_label_product(.data$product_id),
      app_mode = usage_label_app_mode(.data$app_mode)
    ) |>
    dplyr::group_by(.data$month, .data$product_id, .data$app_mode, .data$authenticated) |>
    dplyr::summarise(hits = as.numeric(sum(.data$hits)), .groups = "drop")

  list(
    active_users = usage_monthly_users(users, "connect"),
    content_hits = dplyr::as_tibble(hits)
  )
}

#' Load a raw daily dataset, or NULL (with a message) when it is absent
#' @noRd
usage_open_raw <- function(metric, base_path) {
  path <- chronicle_path(base_path, metric, "daily")
  if (!chronicle_dataset_exists(path)) {
    message("Chronicle dataset '", metric, "' not found at ", path)
    return(NULL)
  }
  chronicle_raw_data(metric, base_path)
}

#' Connect deployments from raw publish events, typed by the latest snapshot
#' @noRd
usage_read_connect_deployments <- function(base_path, window) {
  empty <- usage_empty_tables()$content_deployments
  published <- usage_open_raw("connect_content_published", base_path)
  if (is.null(published)) {
    return(empty)
  }

  since <- as.POSIXct(format(window[1]), tz = "UTC")
  year_since <- as.integer(format(window[1], "%Y"))

  published <- published |>
    usage_with_defaults(list(product_id = "")) |>
    dplyr::filter(.data$Year >= !!year_since, .data$timestamp >= !!since) |>
    dplyr::select("timestamp", "product_id", "content_guid") |>
    dplyr::collect()
  if (nrow(published) == 0) {
    return(empty)
  }

  # A content item's type can change, so use its most recent snapshot. Content
  # that is in no snapshot (for example, since deleted) has an unknown type.
  types <- usage_open_raw("connect_contents", base_path)
  latest <- if (is.null(types)) {
    dplyr::tibble(product_id = character(), content_guid = character(), app_mode = character())
  } else {
    types |>
      usage_with_defaults(list(product_id = "")) |>
      dplyr::filter(
        .data$Year >= !!year_since,
        .data$timestamp >= !!since,
        # Only read snapshot rows for content that was published in the window
        .data$guid %in% !!unique(published$content_guid)
      ) |>
      dplyr::select("timestamp", "product_id", "guid", "app_mode") |>
      dplyr::collect() |>
      dplyr::group_by(.data$product_id, .data$guid) |>
      dplyr::slice_max(.data$timestamp, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select("product_id", content_guid = "guid", "app_mode")
  }

  published |>
    dplyr::left_join(latest, by = c("product_id", "content_guid")) |>
    dplyr::mutate(
      month = usage_month(.data$timestamp),
      product_id = usage_label_product(.data$product_id),
      app_mode = usage_label_app_mode(.data$app_mode)
    ) |>
    dplyr::group_by(.data$month, .data$product_id, .data$app_mode) |>
    dplyr::summarise(
      deployments = as.numeric(dplyr::n()),
      unique_content = as.numeric(dplyr::n_distinct(.data$content_guid)),
      .groups = "drop"
    ) |>
    dplyr::as_tibble()
}

#' Workbench active users and session starts from curated per-user starts
#' @return list(active_users, sessions)
#' @noRd
usage_read_workbench_sessions <- function(base_path, window) {
  empty <- usage_empty_tables()
  ds <- chronicle_data("workbench/session_start_totals_by_user", base_path)
  if (is.null(ds)) {
    return(list(active_users = empty$active_users, sessions = empty$sessions))
  }

  ds <- ds |>
    usage_with_defaults(list(product_id = "")) |>
    dplyr::filter(
      .data$date >= !!window[1],
      !is.na(.data$user_guid),
      .data$user_guid != ""
    )

  sessions <- ds |>
    dplyr::group_by(.data$date, .data$product_id, .data$session_type) |>
    dplyr::summarise(sessions = sum(.data$sessions_started, na.rm = TRUE)) |>
    dplyr::collect()

  users <- ds |>
    dplyr::distinct(.data$date, .data$product_id, .data$user_guid) |>
    dplyr::collect()

  sessions <- sessions |>
    dplyr::mutate(
      month = usage_month(.data$date),
      product_id = usage_label_product(.data$product_id),
      session_type = usage_label_app_mode(.data$session_type)
    ) |>
    dplyr::group_by(.data$month, .data$product_id, .data$session_type) |>
    dplyr::summarise(sessions = as.numeric(sum(.data$sessions)), .groups = "drop")

  list(
    active_users = usage_monthly_users(users, "workbench"),
    sessions = dplyr::as_tibble(sessions)
  )
}

#' Load monthly usage data
#'
#' Builds the monthly usage counts behind the usage report from Chronicle
#' curated and daily data. Every table is grouped by `product_id`, which ties
#' the usage back to the license it was collected for; usage recorded without a
#' product id is reported under `"(unspecified)"`.
#'
#' The result is the single input the usage report needs, and can be written
#' to a small file with [chronicle_usage_to_json()].
#'
#' Definitions, all per calendar month:
#' - **Active users**: distinct user ids with at least one authenticated Connect
#'   content hit, or at least one started Workbench session.
#' - **Content hits**: hits by authenticated and anonymous visitors. Hits with a
#'   missing or empty user id are anonymous.
#' - **Content deployments**: Connect publish events, so redeploying an item
#'   counts each time. `unique_content` counts each item once.
#' - **Sessions**: started Workbench sessions by IDE (`session_type`).
#'
#' @param base_path Base path to Chronicle data directory
#' @param months Number of calendar months to include, ending with the month
#'   that contains `today`. The current month is therefore usually partial.
#' @param today The date the window ends on. Defaults to the current date.
#'
#' @return A list of four tibbles: `active_users` (`month`, `product`,
#'   `product_id`, `users`), `content_hits` (`month`, `product_id`, `app_mode`,
#'   `authenticated`, `hits`), `content_deployments` (`month`, `product_id`,
#'   `app_mode`, `deployments`, `unique_content`) and `sessions` (`month`,
#'   `product_id`, `session_type`, `sessions`). `month` is the first day of the
#'   month. The reporting window is stored in the `"window"` attribute.
#'   A dataset that is not available yields an empty table.
#' @export
#'
#' @examples
#' \dontrun{
#' tables <- chronicle_usage_data("/var/lib/posit-chronicle/data")
#' tables$active_users
#' }
chronicle_usage_data <- function(
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", APP_CONFIG$DEFAULT_BASE_PATH),
  months = 12,
  today = Sys.Date()
) {
  window <- usage_window(months, today)

  connect <- usage_read_connect_hits(base_path, window)
  workbench <- usage_read_workbench_sessions(base_path, window)

  tables <- list(
    active_users = dplyr::bind_rows(connect$active_users, workbench$active_users),
    content_hits = connect$content_hits,
    content_deployments = usage_read_connect_deployments(base_path, window),
    sessions = workbench$sessions
  )

  tables <- lapply(tables, function(tbl) {
    tbl[tbl$month >= window[1] & tbl$month <= window[2], ] |>
      dplyr::arrange(dplyr::across(dplyr::everything()))
  })
  attr(tables, "window") <- window
  tables
}
