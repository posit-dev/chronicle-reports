library(testthat)
library(dplyr)
library(lubridate)

# Load the function being tested
source("../../app.R")

test_that("process_daily_metrics works with basic data", {
  # Create sample data
  test_data <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02", "2024-01-02")),
    timestamp = as.POSIXct(c(
      "2024-01-01 10:00:00",
      "2024-01-01 15:00:00",
      "2024-01-02 09:00:00",
      "2024-01-02 14:00:00"
    )),
    id = c("user1", "user2", "user1", "user2"),
    created_at = as.POSIXct(c(
      "2023-01-01 00:00:00",
      "2023-01-01 00:00:00",
      "2023-01-01 00:00:00",
      "2023-01-01 00:00:00"
    )),
    last_active_at = as.POSIXct(c(
      "2024-01-01 10:00:00",
      "2024-01-01 15:00:00",
      "2024-01-02 09:00:00",
      "2024-01-02 14:00:00"
    )),
    locked = c(FALSE, FALSE, FALSE, FALSE),
    user_role = c("viewer", "publisher", "viewer", "publisher")
  )

  result <- process_daily_metrics(test_data)

  # Verify function returns correct data structure
  expect_s3_class(result, "data.frame")

  # Verify that all expected columns are present in output
  expect_true(all(
    c("date", "licensed_users", "daily_users", "publishers") %in% names(result)
  ))

  # We expect to have one row per unique date (2 dates: 2024-01-01, 2024-01-02)
  expect_equal(nrow(result), 2)

  # Verify that the various user counts are correct for each day
  expect_equal(result$licensed_users, c(2, 2))
  expect_equal(result$daily_users, c(2, 2))
  expect_equal(result$publishers, c(1, 1))
})

test_that("process_daily_metrics handles locked users correctly", {
  test_data <- data.frame(
    date = as.Date("2024-01-01"),
    timestamp = as.POSIXct("2024-01-01 10:00:00"),
    id = c("user1", "user2", "user3"),
    created_at = as.POSIXct("2023-01-01 00:00:00"),
    last_active_at = as.POSIXct("2024-01-01 10:00:00"),
    locked = c(FALSE, TRUE, FALSE), # user2 is locked
    user_role = c("viewer", "viewer", "publisher")
  )

  result <- process_daily_metrics(test_data)

  # Locked users should not be counted
  expect_equal(result$licensed_users, 2)
  expect_equal(result$daily_users, 2)
  expect_equal(result$publishers, 1)
})

test_that("process_daily_metrics filters out inactive users (>1 year)", {
  current_date <- as.Date("2024-01-01")

  test_data <- data.frame(
    date = current_date,
    timestamp = as.POSIXct("2024-01-01 10:00:00"),
    id = c("user1", "user2", "user3"),
    created_at = as.POSIXct("2023-01-01 00:00:00"),
    last_active_at = as.POSIXct(c(
      "2024-01-01 10:00:00", # active today
      "2022-06-01 10:00:00", # inactive >1 year
      "2023-06-01 10:00:00" # active within 1 year
    )),
    locked = FALSE,
    user_role = "viewer"
  )

  result <- process_daily_metrics(test_data)

  # Only users active within 1 year should be included
  expect_equal(result$licensed_users, 2) # user1 and user3
  expect_equal(result$daily_users, 1) # only user1 (active today)
})

test_that("process_daily_metrics counts publishers and admins correctly", {
  test_data <- data.frame(
    date = as.Date("2024-01-01"),
    timestamp = as.POSIXct("2024-01-01 10:00:00"),
    id = c("user1", "user2", "user3", "user4"),
    created_at = as.POSIXct("2023-01-01 00:00:00"),
    last_active_at = as.POSIXct("2024-01-01 10:00:00"),
    locked = FALSE,
    user_role = c("viewer", "publisher", "admin", "viewer")
  )

  result <- process_daily_metrics(test_data)

  # Should count both publishers and admins
  expect_equal(result$publishers, 2) # user2 (publisher) and user3 (admin)
})

test_that("process_daily_metrics handles multiple timestamps per user per day", {
  test_data <- data.frame(
    date = as.Date("2024-01-01"),
    timestamp = as.POSIXct(c(
      "2024-01-01 09:00:00",
      "2024-01-01 10:00:00",
      "2024-01-01 15:00:00"
    )),
    id = c("user1", "user1", "user1"), # same user, multiple entries
    created_at = as.POSIXct("2023-01-01 00:00:00"),
    last_active_at = as.POSIXct(c(
      "2024-01-01 09:00:00",
      "2024-01-01 10:00:00",
      "2024-01-01 15:00:00"
    )),
    locked = FALSE,
    user_role = "publisher"
  )

  result <- process_daily_metrics(test_data)

  # Should take the latest timestamp and count user only once
  expect_equal(result$licensed_users, 1)
  expect_equal(result$daily_users, 1)
  expect_equal(result$publishers, 1)
})

test_that("process_daily_metrics handles empty data", {
  test_data <- data.frame(
    date = as.Date(character(0)),
    timestamp = as.POSIXct(character(0)),
    id = character(0),
    created_at = as.POSIXct(character(0)),
    last_active_at = as.POSIXct(character(0)),
    locked = logical(0),
    user_role = character(0)
  )

  result <- process_daily_metrics(test_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(
    c("date", "licensed_users", "daily_users", "publishers") %in% names(result)
  ))
})

test_that("process_daily_metrics sorts results by date", {
  test_data <- data.frame(
    date = as.Date(c("2024-01-03", "2024-01-01", "2024-01-02")),
    timestamp = as.POSIXct(c(
      "2024-01-03 10:00:00",
      "2024-01-01 10:00:00",
      "2024-01-02 10:00:00"
    )),
    id = c("user1", "user1", "user1"),
    created_at = as.POSIXct("2023-01-01 00:00:00"),
    last_active_at = as.POSIXct(c(
      "2024-01-03 10:00:00",
      "2024-01-01 10:00:00",
      "2024-01-02 10:00:00"
    )),
    locked = FALSE,
    user_role = "viewer"
  )

  result <- process_daily_metrics(test_data)

  # Results should be sorted by date
  expect_equal(
    result$date,
    as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
  )
})
