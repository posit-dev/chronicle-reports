beats <- function(host, from, to, product_id = "p1", by = 300) {
  ts <- seq(
    as.POSIXct(from, tz = "UTC"),
    as.POSIXct(to, tz = "UTC"),
    by = by
  )
  dplyr::tibble(timestamp = ts, host_name = host, product_id = product_id)
}

d <- function(x) as.Date(x)

test_that("steady host", {
  hb <- beats("a", "2026-11-01 00:00:00", "2026-11-01 23:55:00")
  out <- usage_server_counts(hb, d("2026-11-01"), d("2026-11-01"))
  expect_equal(nrow(out), 1)
  expect_equal(
    names(out),
    c(
      "date",
      "product_id",
      "servers_min",
      "servers_max",
      "servers_median",
      "hosts_seen"
    )
  )
  expect_equal(out$servers_min, 1)
  expect_equal(out$servers_max, 1)
  expect_equal(out$servers_median, 1)
  expect_equal(out$hosts_seen, 1)
})

test_that("late heartbeat does not dip", {
  hb <- beats("a", "2026-11-01 00:00:00", "2026-11-01 23:55:00")
  hb <- hb[hb$timestamp != as.POSIXct("2026-11-01 10:00:00", tz = "UTC"), ]
  out <- usage_server_counts(hb, d("2026-11-01"), d("2026-11-01"))
  expect_equal(out$servers_min, 1)
})

test_that("two missed heartbeats dip", {
  hb <- beats("a", "2026-11-01 00:00:00", "2026-11-01 23:55:00")
  drop <- as.POSIXct(
    c("2026-11-01 10:00:00", "2026-11-01 10:05:00"),
    tz = "UTC"
  )
  hb <- hb[!hb$timestamp %in% drop, ]
  out <- usage_server_counts(hb, d("2026-11-01"), d("2026-11-01"))
  expect_equal(out$servers_min, 0)
  expect_equal(out$servers_max, 1)
})

test_that("single startup heartbeat counts two windows", {
  # p2 keeps beating, so p1's day runs on to the end of the day
  hb <- rbind(
    beats("a", "2026-11-01 12:00:30", "2026-11-01 12:00:30"),
    beats("b", "2026-11-01 00:00:00", "2026-11-01 23:55:00", "p2")
  )
  out <- usage_server_counts(hb, d("2026-11-01"), d("2026-11-01"))
  out <- out[out$product_id == "p1", ]
  expect_equal(out$servers_max, 1)
  expect_equal(out$servers_median, 0)
})

test_that("churn", {
  hb <- rbind(
    beats("a", "2026-11-01 00:00:00", "2026-11-01 11:55:00"),
    beats("b", "2026-11-01 12:00:00", "2026-11-01 23:55:00")
  )
  out <- usage_server_counts(hb, d("2026-11-01"), d("2026-11-01"))
  expect_equal(out$servers_max, 2)
  expect_equal(out$hosts_seen, 2)
  expect_equal(out$servers_median, 1)
})

test_that("duplicate beats in one window count once", {
  hb <- beats("a", "2026-11-01 12:00:00", "2026-11-01 12:00:30", by = 30)
  out <- usage_server_counts(hb, d("2026-11-01"), d("2026-11-01"))
  expect_equal(out$servers_max, 1)
})

test_that("ignores missing host names", {
  hb <- rbind(
    beats("a", "2026-11-01 00:00:00", "2026-11-01 23:55:00"),
    beats(NA_character_, "2026-11-01 00:00:00", "2026-11-01 23:55:00"),
    beats("", "2026-11-01 00:00:00", "2026-11-01 23:55:00")
  )
  out <- usage_server_counts(hb, d("2026-11-01"), d("2026-11-01"))
  expect_equal(out$hosts_seen, 1)
  expect_equal(out$servers_max, 1)
})

test_that("midnight", {
  day1 <- beats("a", "2026-11-01 23:55:00", "2026-11-01 23:55:00")
  day2 <- beats("a", "2026-11-02 00:05:00", "2026-11-02 23:55:00")
  out <- usage_server_counts(
    rbind(day1, day2),
    d("2026-11-02"),
    d("2026-11-02")
  )
  expect_equal(out$servers_min, 1)
  # Without the 23:55 beat the 00:00 window is uncovered. An earlier beat keeps
  # the product's series running through the night.
  earlier <- beats("a", "2026-11-01 12:00:00", "2026-11-01 12:00:00")
  out <- usage_server_counts(
    rbind(earlier, day2),
    d("2026-11-02"),
    d("2026-11-02")
  )
  expect_equal(out$servers_min, 0)
})

test_that("zero fill", {
  # Day one stops before midnight so its last beat does not reach 11-02
  hb <- rbind(
    beats("a", "2026-11-01 00:00:00", "2026-11-01 20:00:00"),
    beats("a", "2026-11-03 00:00:00", "2026-11-03 23:55:00")
  )
  out <- usage_server_counts(hb, d("2026-11-01"), d("2026-11-03"))
  mid <- out[out$date == d("2026-11-02"), ]
  expect_equal(nrow(mid), 1)
  expect_equal(mid$servers_min, 0)
  expect_equal(mid$servers_max, 0)
  expect_equal(mid$servers_median, 0)
  expect_equal(mid$hosts_seen, 0)
})

test_that("no mid-day start penalty", {
  hb <- beats("a", "2026-11-01 18:00:00", "2026-11-01 23:55:00")
  out <- usage_server_counts(hb, d("2026-11-01"), d("2026-11-01"))
  expect_equal(out$servers_min, 1)
})

test_that("products are independent", {
  hb <- rbind(
    beats("a", "2026-11-01 00:00:00", "2026-11-01 23:55:00", "p1"),
    beats("b", "2026-11-02 12:00:00", "2026-11-02 23:55:00", "p2")
  )
  out <- usage_server_counts(hb, d("2026-11-01"), d("2026-11-02"))
  expect_equal(sum(out$product_id == "p1"), 2)
  expect_equal(sum(out$product_id == "p2"), 1)
  p2 <- out[out$product_id == "p2", ]
  expect_equal(p2$date, d("2026-11-02"))
  expect_equal(p2$servers_min, 1)
})

test_that("empty input", {
  hb <- dplyr::tibble(
    timestamp = as.POSIXct(character(), tz = "UTC"),
    host_name = character(),
    product_id = character()
  )
  out <- usage_server_counts(hb, d("2026-11-01"), d("2026-11-03"))
  expect_equal(nrow(out), 0)
  expect_equal(
    names(out),
    c(
      "date",
      "product_id",
      "servers_min",
      "servers_max",
      "servers_median",
      "hosts_seen"
    )
  )
})

test_that("days after the latest heartbeat get no rows", {
  # The data lags: heartbeats end on 11-02 but the report runs through 11-03
  hb <- beats("a", "2026-11-01 00:00:00", "2026-11-02 23:55:00")
  out <- usage_server_counts(hb, d("2026-11-01"), d("2026-11-03"))
  expect_equal(out$date, d(c("2026-11-01", "2026-11-02")))
  expect_equal(out$servers_min, c(1, 1))
})

test_that("zero fill stops at the latest window of the newest day", {
  # The last beat is at 20:00, so the day ends there instead of dipping to 0
  hb <- beats("a", "2026-11-01 00:00:00", "2026-11-01 20:00:00")
  out <- usage_server_counts(hb, d("2026-11-01"), d("2026-11-02"))
  expect_equal(out$date, d("2026-11-01"))
  expect_equal(out$servers_min, 1)
})

test_that("a product that goes quiet is zero-filled to the latest beat", {
  hb <- rbind(
    beats("a", "2026-11-01 00:00:00", "2026-11-01 23:55:00", "p1"),
    beats("b", "2026-11-01 00:00:00", "2026-11-03 11:55:00", "p2")
  )
  out <- usage_server_counts(hb, d("2026-11-01"), d("2026-11-04"))
  p1 <- out[out$product_id == "p1", ]
  expect_equal(p1$date, d(c("2026-11-01", "2026-11-02", "2026-11-03")))
  expect_equal(p1$servers_max, c(1, 1, 0))
  expect_equal(p1$servers_min, c(1, 0, 0))
  expect_equal(max(out$date), d("2026-11-03"))
})

test_that("heartbeats after last_date are not counted but show data is current", {
  hb <- beats("a", "2026-11-01 00:00:00", "2026-11-01 20:00:00")
  later <- beats("a", "2026-11-03 12:00:00", "2026-11-03 12:00:00")
  out <- usage_server_counts(rbind(hb, later), d("2026-11-01"), d("2026-11-02"))
  expect_equal(out$date, d(c("2026-11-01", "2026-11-02")))
  expect_equal(out$servers_max, c(1, 0))
  expect_equal(out$hosts_seen, c(1, 0))
})

test_that("the chunked heartbeat read equals counting all heartbeats at once", {
  today <- d("2026-11-06")
  window <- usage_window(12, today)
  outage_start <- as.POSIXct("2026-10-31 23:45:00", tz = "UTC")
  outage <- outage_start + 300 * 0:6
  hb <- rbind(
    # p1 runs throughout, with host churn and an outage across 10-31/11-01
    beats("a", "2025-11-30 00:00:00", "2026-10-15 11:55:00", "p1"),
    beats("b", "2026-10-15 12:00:00", "2026-11-06 06:00:00", "p1"),
    beats("c", "2026-09-01 00:00:00", "2026-09-03 00:00:00", "p1"),
    # p2 first appears mid-range and stops before the end
    beats("x", "2026-08-14 13:00:00", "2026-10-20 08:00:00", "p2", by = 290),
    # p3 is silent for all of February, then comes back
    beats("y", "2025-12-10 00:00:00", "2026-01-31 23:55:00", "p3"),
    beats("y", "2026-03-01 00:10:00", "2026-03-02 00:00:00", "p3")
  )
  hb <- hb[!hb$timestamp %in% outage, ]

  path <- withr::local_tempdir()
  write_usage_heartbeats(hb, path, "connect/heartbeats")
  chunked <- usage_read_heartbeats("connect", path, window, today)

  whole <- usage_server_counts(hb, window[1], today - 1) |>
    dplyr::mutate(product = "connect", .after = "date")

  expect_identical(as.data.frame(chunked), as.data.frame(whole))
  expect_identical(
    usage_heartbeat_chunks(window[1], today - 1)$from[1:2],
    d(c("2025-12-01", "2026-01-01"))
  )
  # the outage across the chunk boundary shows on both days
  p1 <- chunked[chunked$product_id == "p1", ]
  expect_equal(
    p1$servers_min[p1$date %in% d(c("2026-10-31", "2026-11-01"))],
    c(0, 0)
  )
  # p3 is zero for February after 02-01 (its 00:00 window is still covered),
  # p2 has no rows before it first appears
  p3 <- chunked[chunked$product_id == "p3", ]
  february <- p3$date >= d("2026-02-02") & p3$date <= d("2026-02-28")
  expect_equal(sum(february), 27)
  expect_true(all(p3$servers_max[february] == 0))
  expect_equal(min(chunked$date[chunked$product_id == "p2"]), d("2026-08-14"))
})

test_that("column types do not depend on the number of windows", {
  classes <- function(x) vapply(x, function(col) class(col)[1], "")
  empty <- usage_server_counts(
    beats("a", "2026-11-01 00:00:00", "2026-11-01 00:00:00")[0, ],
    d("2026-11-01"),
    d("2026-11-01")
  )
  # 241 windows, an odd count
  odd <- usage_server_counts(
    beats("a", "2026-11-01 00:00:00", "2026-11-01 20:00:00"),
    d("2026-11-01"),
    d("2026-11-01")
  )
  multi <- usage_server_counts(
    beats("a", "2026-11-01 00:00:00", "2026-11-02 23:55:00"),
    d("2026-11-01"),
    d("2026-11-02")
  )
  expect_identical(classes(odd), classes(empty))
  expect_identical(classes(multi), classes(empty))
  expect_type(odd$servers_median, "double")
})
