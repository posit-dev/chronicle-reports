# Servers running per day, derived from curated Chronicle heartbeat rows.
#
# Hosts send a heartbeat roughly every 5 minutes. Time is cut into 5-minute UTC
# windows and a host counts as running in a window if it sent a heartbeat in
# that window or the one directly before it, so a single late heartbeat does not
# register as an outage.

USAGE_HEARTBEAT_WINDOW_SECONDS <- 300
USAGE_WINDOWS_PER_DAY <- 86400 / USAGE_HEARTBEAT_WINDOW_SECONDS

#' Daily min, max and median of servers running, plus distinct hosts seen
#'
#' @param heartbeats Data frame with `timestamp` (POSIXct), `host_name` and
#'   `product_id`. Rows before `first_date` may be included so the first window
#'   has its predecessor; they produce no output rows.
#' @param first_date,last_date Inclusive Date bounds of the returned days.
#' @return A tibble with `date`, `product_id`, `servers_min`, `servers_max`,
#'   `servers_median` and `hosts_seen`. Each product_id is zero-filled from its
#'   first heartbeat window onward and has no rows before it.
#' @noRd
usage_server_counts <- function(heartbeats, first_date, last_date) {
  empty <- dplyr::tibble(
    date = as.Date(character()),
    product_id = character(),
    servers_min = numeric(),
    servers_max = numeric(),
    servers_median = numeric(),
    hosts_seen = numeric()
  )

  last_window <- (as.numeric(as.Date(last_date)) + 1) *
    USAGE_WINDOWS_PER_DAY -
    1

  beats <- heartbeats |>
    dplyr::filter(!is.na(.data$host_name), .data$host_name != "") |>
    dplyr::mutate(
      window = floor(
        as.numeric(.data$timestamp) / USAGE_HEARTBEAT_WINDOW_SECONDS
      )
    ) |>
    dplyr::filter(.data$window <= last_window) |>
    dplyr::distinct(.data$product_id, .data$host_name, .data$window)
  if (nrow(beats) == 0) {
    return(empty)
  }

  window_date <- function(window) {
    as.Date(window %/% USAGE_WINDOWS_PER_DAY, origin = "1970-01-01")
  }

  # A host runs in the window of its heartbeat and the next one
  running <- dplyr::bind_rows(
    beats,
    dplyr::mutate(beats, window = .data$window + 1)
  ) |>
    dplyr::distinct(.data$product_id, .data$host_name, .data$window) |>
    dplyr::count(.data$product_id, .data$window, name = "servers")

  starts <- beats |>
    dplyr::summarise(start = min(.data$window), .by = "product_id")

  series <- lapply(seq_len(nrow(starts)), function(i) {
    dplyr::tibble(
      product_id = starts$product_id[i],
      window = seq(starts$start[i], last_window)
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::left_join(running, by = c("product_id", "window")) |>
    dplyr::mutate(
      servers = dplyr::coalesce(.data$servers, 0L),
      date = window_date(.data$window)
    ) |>
    dplyr::filter(.data$date >= first_date, .data$date <= last_date)
  if (nrow(series) == 0) {
    return(empty)
  }

  hosts <- beats |>
    dplyr::mutate(date = window_date(.data$window)) |>
    dplyr::summarise(
      hosts_seen = dplyr::n_distinct(.data$host_name),
      .by = c("product_id", "date")
    )

  series |>
    dplyr::summarise(
      servers_min = min(.data$servers),
      servers_max = max(.data$servers),
      servers_median = stats::median(.data$servers),
      .by = c("date", "product_id")
    ) |>
    dplyr::left_join(hosts, by = c("product_id", "date")) |>
    dplyr::mutate(
      hosts_seen = as.numeric(dplyr::coalesce(.data$hosts_seen, 0L)),
      dplyr::across(c("servers_min", "servers_max"), as.numeric)
    ) |>
    dplyr::arrange(.data$product_id, .data$date) |>
    dplyr::select(
      "date",
      "product_id",
      "servers_min",
      "servers_max",
      "servers_median",
      "hosts_seen"
    )
}

#' Servers running per day for one product, from its curated heartbeats
#'
#' Reads heartbeats from one day before the window so the first window has its
#' predecessor. The as-of day is excluded: days run from `window[1]` through
#' `today - 1`.
#'
#' @param product `"connect"` or `"workbench"`.
#' @param base_path Base path to Chronicle data directory.
#' @param window Length-2 Date vector from `usage_window()`.
#' @param today The as-of date.
#' @return A tibble with the columns of `usage_empty_tables()$servers`.
#' @noRd
usage_read_heartbeats <- function(product, base_path, window, today) {
  empty <- usage_empty_tables()$servers
  ds <- chronicle_data(paste0(product, "/heartbeats"), base_path)
  if (is.null(ds)) {
    return(empty)
  }

  beats <- ds |>
    usage_with_defaults(list(product_id = "")) |>
    dplyr::filter(
      .data$date >= !!(window[1] - 1),
      .data$date < !!as.Date(today)
    ) |>
    dplyr::select("timestamp", "host_name", "product_id") |>
    dplyr::collect() |>
    dplyr::mutate(product_id = usage_label_product(.data$product_id))

  usage_server_counts(
    beats,
    first_date = window[1],
    last_date = as.Date(today) - 1
  ) |>
    dplyr::mutate(product = product) |>
    dplyr::select(
      "date",
      "product",
      "product_id",
      "servers_min",
      "servers_max",
      "servers_median",
      "hosts_seen"
    )
}
