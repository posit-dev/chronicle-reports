# tests/testthat/test_workbench_users_app.R
library(testthat)
library(dplyr)
library(lubrusernameate)
library(chronicle.reports)

test_that("calculate_workbench_daily_user_counts works with basic data", {
  test_data <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02", "2024-01-02")),
    timestamp = as.POSIXct(c(
      "2024-01-01 10:00:00",
      "2024-01-01 15:00:00",
      "2024-01-02 09:00:00",
      "2024-01-02 14:00:00"
    )),
    username = c("user1", "user2", "user1", "user2"),
    created_at = as.POSIXct(rep("2023-01-01 00:00:00", 4)),
    last_active_at = as.POSIXct(c(
      "2024-01-01 10:00:00",
      "2024-01-01 15:00:00",
      "2024-01-02 09:00:00",
      "2024-01-02 14:00:00"
    )),
    status = c("Active", "Active", "Active", "Active")
  )

  result <- calculate_workbench_daily_user_counts(test_data)
  expect_s3_class(result, "data.frame")
  expect_true(all(
    c("date", "licensed_users", "daily_users") %in% names(result)
  ))
  expect_equal(nrow(result), 2)
  expect_equal(result$licensed_users, c(2, 2))
  expect_equal(result$daily_users, c(2, 2))
})

test_that("calculate_workbench_daily_user_counts excludes locked users", {
  test_data <- data.frame(
    date = as.Date("2024-01-01"),
    timestamp = as.POSIXct(c(
      "2024-01-01 10:00:00",
      "2024-01-01 11:00:00",
      "2024-01-01 12:00:00"
    )),
    username = c("user1", "user2", "user3"),
    created_at = as.POSIXct(rep("2023-01-01 00:00:00", 3)),
    last_active_at = as.POSIXct(rep("2024-01-01 10:00:00", 3)),
    status = c("Active", "Locked", "Active")
  )

  result <- calculate_workbench_daily_user_counts(test_data)
  expect_equal(result$licensed_users, 2)
  expect_equal(result$daily_users, 2)
})

test_that("calculate_workbench_daily_user_counts filters out inactive >1 year users", {
  current_date <- as.Date("2024-01-01")
  test_data <- data.frame(
    date = current_date,
    timestamp = as.POSIXct(c(
      "2024-01-01 09:00:00",
      "2024-01-01 09:05:00",
      "2024-01-01 09:10:00"
    )),
    username = c("user1", "user2", "user3"),
    created_at = as.POSIXct(rep("2023-01-01 00:00:00", 3)),
    last_active_at = as.POSIXct(c(
      "2024-01-01 09:00:00", # active today
      "2022-06-01 09:00:00", # inactive >1 year
      "2023-06-01 09:00:00" # active within 1 year
    )),
    status = c("Active", "Active", "Active")
  )

  result <- calculate_workbench_daily_user_counts(test_data)
  expect_equal(result$licensed_users, 2) # user1 & user3
  expect_equal(result$daily_users, 1) # only user1 active today
})

test_that("calculate_workbench_daily_user_counts uses latest timestamp per user per day", {
  test_data <- data.frame(
    date = as.Date("2024-01-01"),
    timestamp = as.POSIXct(c(
      "2024-01-01 09:00:00",
      "2024-01-01 10:30:00",
      "2024-01-01 15:00:00"
    )),
    username = c("user1", "user1", "user1"),
    created_at = as.POSIXct("2023-01-01 00:00:00"),
    last_active_at = as.POSIXct(c(
      "2024-01-01 09:00:00",
      "2024-01-01 10:30:00",
      "2024-01-01 15:00:00"
    )),
    status = "Active"
  )

  result <- calculate_workbench_daily_user_counts(test_data)
  expect_equal(result$licensed_users, 1)
  expect_equal(result$daily_users, 1)
})

test_that("calculate_workbench_daily_user_counts handles users without last_active_at", {
  test_data <- data.frame(
    date = as.Date("2024-01-01"),
    timestamp = as.POSIXct(c("2024-01-01 09:00:00", "2024-01-01 10:00:00")),
    username = c("user1", "user2"),
    created_at = as.POSIXct(rep("2023-01-01 00:00:00", 2)),
    last_active_at = as.POSIXct(c(NA, "2024-01-01 10:00:00")),
    status = c("Active", "Active")
  )

  result <- calculate_workbench_daily_user_counts(test_data)
  # user1 counts as licensed (no last_active_at -> retained by filter), not daily
  expect_equal(result$licensed_users, 2)
  expect_equal(result$daily_users, 1)
})

test_that("calculate_workbench_daily_user_counts excludes users created after date", {
  test_data <- data.frame(
    date = as.Date("2024-01-01"),
    timestamp = as.POSIXct(c("2024-01-01 09:00:00", "2024-01-01 09:05:00")),
    username = c("user1", "user2"),
    created_at = as.POSIXct(c("2023-12-31 00:00:00", "2024-02-01 00:00:00")),
    last_active_at = as.POSIXct(c(
      "2024-01-01 09:00:00",
      "2024-02-10 09:00:00"
    )),
    status = c("Active", "Active")
  )

  result <- calculate_workbench_daily_user_counts(test_data)
  expect_equal(result$licensed_users, 1)
  expect_equal(result$daily_users, 1)
})

test_that("calculate_workbench_daily_user_counts handles empty data", {
  test_data <- data.frame(
    date = as.Date(character(0)),
    timestamp = as.POSIXct(character(0)),
    username = character(0),
    created_at = as.POSIXct(character(0)),
    last_active_at = as.POSIXct(character(0)),
    status = character(0)
  )

  result <- calculate_workbench_daily_user_counts(test_data)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(
    c("date", "licensed_users", "daily_users") %in% names(result)
  ))
})

test_that("calculate_workbench_daily_user_counts sorts by date", {
  test_data <- data.frame(
    date = as.Date(c("2024-01-03", "2024-01-01", "2024-01-02")),
    timestamp = as.POSIXct(c(
      "2024-01-03 10:00:00",
      "2024-01-01 10:00:00",
      "2024-01-02 10:00:00"
    )),
    username = c("user1", "user1", "user1"),
    created_at = as.POSIXct("2023-01-01 00:00:00"),
    last_active_at = as.POSIXct(c(
      "2024-01-03 10:00:00",
      "2024-01-01 10:00:00",
      "2024-01-02 10:00:00"
    )),
    status = "Active"
  )

  result <- calculate_workbench_daily_user_counts(test_data)
  expect_equal(
    result$date,
    as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
  )
})
