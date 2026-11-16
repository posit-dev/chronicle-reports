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
  hb <- beats("a", "2026-11-01 12:00:30", "2026-11-01 12:00:30")
  out <- usage_server_counts(hb, d("2026-11-01"), d("2026-11-01"))
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
