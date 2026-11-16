# Servers running per day, derived from curated Chronicle heartbeat rows.
#
# Hosts send a heartbeat roughly every 5 minutes. Time is cut into 5-minute UTC
# windows and a host counts as running in a window if it sent a heartbeat in
# that window or the one directly before it, so a single late heartbeat does not
# register as an outage.

USAGE_HEARTBEAT_WINDOW_SECONDS <- 300
USAGE_WINDOWS_PER_DAY <- 86400 / USAGE_HEARTBEAT_WINDOW_SECONDS

#' Index of the 5-minute UTC window a timestamp falls in
#' @noRd
usage_heartbeat_window <- function(timestamp) {
  floor(as.numeric(timestamp) / USAGE_HEARTBEAT_WINDOW_SECONDS)
}

#' Daily min, max and median of servers running, plus distinct hosts seen
#'
#' @param heartbeats Data frame with `timestamp` (POSIXct), `host_name` and
#'   `product_id`. Rows before `first_date` may be included so the first window
#'   has its predecessor; they produce no output rows. Rows after `last_date`
#'   are not counted, but show that the data reaches `last_date`.
#' @param first_date,last_date Inclusive Date bounds of the returned days.
#' @param bounds Optional data frame with `product_id` and the timestamps of
#'   each product_id's `first` and `last` heartbeat over all the data, from
#'   `usage_heartbeat_bounds()`. Lets the days be counted in chunks: pass the
#'   same bounds with each chunk of `heartbeats`. By default they are taken from
#'   `heartbeats`.
#' @return A tibble with `date`, `product_id`, `servers_min`, `servers_max`,
#'   `servers_median` and `hosts_seen`. Each product_id is zero-filled from its
#'   first heartbeat window up to the latest heartbeat window of any
#'   product_id (or the end of `last_date`, if earlier), and has no rows outside
#'   that span. A product_id that stops reporting therefore shows 0 from then
#'   on, while days the data does not reach yet get no rows.
#' @noRd
usage_server_counts <- function(
  heartbeats,
  first_date,
  last_date,
  bounds = NULL
) {
  empty <- dplyr::tibble(
    date = as.Date(character()),
    product_id = character(),
    servers_min = numeric(),
    servers_max = numeric(),
    servers_median = numeric(),
    hosts_seen = numeric()
  )

  beats <- heartbeats |>
    dplyr::filter(!is.na(.data$host_name), .data$host_name != "") |>
    dplyr::mutate(window = usage_heartbeat_window(.data$timestamp)) |>
    dplyr::distinct(.data$product_id, .data$host_name, .data$window)

  if (is.null(bounds)) {
    if (nrow(beats) == 0) {
      return(empty)
    }
    bounds <- beats |>
      dplyr::summarise(
        first = min(.data$window),
        last = max(.data$window),
        .by = "product_id"
      )
  } else {
    bounds <- bounds |>
      dplyr::mutate(
        first = usage_heartbeat_window(.data$first),
        last = usage_heartbeat_window(.data$last)
      )
  }
  if (nrow(bounds) == 0) {
    return(empty)
  }

  # Stop at the latest heartbeat of any product_id, so days the data does not
  # reach yet are left out instead of reading as an outage
  first_window <- as.numeric(as.Date(first_date)) * USAGE_WINDOWS_PER_DAY
  last_window <- min(
    (as.numeric(as.Date(last_date)) + 1) * USAGE_WINDOWS_PER_DAY - 1,
    max(bounds$last)
  )
  starts <- bounds |>
    dplyr::mutate(start = pmax(.data$first, first_window)) |>
    dplyr::filter(.data$start <= last_window)
  if (nrow(starts) == 0) {
    return(empty)
  }
  beats <- dplyr::filter(beats, .data$window <= last_window)

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

#' First and last heartbeat of each product_id
#'
#' @param heartbeats Heartbeat rows as a data frame or an arrow query, with
#'   `timestamp`, `host_name` and an unlabelled `product_id`.
#' @return A tibble with the labelled `product_id` and the timestamps of its
#'   `first` and `last` heartbeat. Rows without a host name are ignored.
#' @noRd
usage_heartbeat_bounds <- function(heartbeats) {
  bounds <- heartbeats |>
    dplyr::filter(!is.na(.data$host_name), .data$host_name != "") |>
    dplyr::group_by(.data$product_id) |>
    dplyr::summarise(
      first = min(.data$timestamp),
      last = max(.data$timestamp)
    ) |>
    dplyr::collect()
  if (nrow(bounds) == 0) {
    return(dplyr::select(bounds, "product_id", "first", "last"))
  }
  bounds |>
    dplyr::mutate(product_id = usage_label_product(.data$product_id)) |>
    dplyr::summarise(
      first = min(.data$first),
      last = max(.data$last),
      .by = "product_id"
    )
}

#' Calendar-month chunks of the days from `first_date` through `last_date`
#' @return A tibble with the inclusive Date bounds `from` and `to` of each
#'   chunk; no rows when `last_date` is before `first_date`.
#' @noRd
usage_heartbeat_chunks <- function(first_date, last_date) {
  first_date <- as.Date(first_date)
  last_date <- as.Date(last_date)
  if (last_date < first_date) {
    return(dplyr::tibble(
      from = as.Date(character()),
      to = as.Date(character())
    ))
  }
  months <- seq(usage_month(first_date), usage_month(last_date), by = "month")
  dplyr::tibble(
    from = pmax(months, first_date),
    to = pmin(c(months[-1] - 1, last_date), last_date)
  )
}

#' Servers running per day for one product, from its curated heartbeats
#'
#' Counts one calendar month at a time so only a month of heartbeats is in
#' memory, reading from one day before each month so its first window has its
#' predecessor. Each product_id's first heartbeat and the latest heartbeat come
#' from an aggregate over all the days, so the result is the same as counting
#' every heartbeat at once. The as-of day is excluded: days run from
#' `window[1]` through `today - 1`, or through the latest heartbeat if the data
#' ends earlier. The as-of day's heartbeats only show the data reaches
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

  ds <- ds |>
    usage_with_defaults(list(product_id = "")) |>
    dplyr::filter(
      .data$date >= !!(window[1] - 1),
      .data$date <= !!as.Date(today)
    )
  bounds <- usage_heartbeat_bounds(ds)
  if (nrow(bounds) == 0) {
    return(empty)
  }

  chunks <- usage_heartbeat_chunks(window[1], as.Date(today) - 1)
  counts <- lapply(seq_len(nrow(chunks)), function(i) {
    from <- chunks$from[i]
    to <- chunks$to[i]
    beats <- ds |>
      dplyr::filter(.data$date >= !!(from - 1), .data$date <= !!to) |>
      dplyr::select("timestamp", "host_name", "product_id") |>
      dplyr::collect() |>
      dplyr::mutate(
        # as.character() keeps the type when the chunk has no rows
        product_id = as.character(usage_label_product(.data$product_id))
      )
    usage_server_counts(beats, from, to, bounds)
  })
  counts <- dplyr::bind_rows(empty[0, setdiff(names(empty), "product")], counts)
  if (nrow(counts) == 0) {
    return(empty)
  }

  counts |>
    dplyr::arrange(.data$product_id, .data$date) |>
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
