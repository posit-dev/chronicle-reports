test_that("chronicle_raw_data loads raw daily data successfully", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Load raw Connect users data
  data <- chronicle_raw_data("connect_users", base_path, frequency = "daily")

  # Check that we get an Arrow dataset
  expect_s3_class(data, "FileSystemDataset")

  # Check that we can collect data
  collected <- dplyr::collect(data)
  expect_s3_class(collected, "data.frame")

  # Check expected columns from raw data
  expect_true("date" %in% names(collected))
  expect_true("timestamp" %in% names(collected))
  expect_true("email" %in% names(collected))
  expect_true("user_role" %in% names(collected))
  expect_true("locked" %in% names(collected))

  # Should have data from multiple days (3 days × 15 users = 45 rows)
  expect_equal(nrow(collected), 45)
})

test_that("chronicle_raw_data defaults to daily frequency", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Call without frequency parameter
  data <- chronicle_raw_data("connect_users", base_path)

  collected <- dplyr::collect(data)
  expect_s3_class(collected, "data.frame")
  expect_gt(nrow(collected), 0)
})

test_that("chronicle_raw_data supports date filtering", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Load and filter by specific date
  data <- chronicle_raw_data("connect_users", base_path, frequency = "daily")
  filtered <- data |>
    dplyr::filter(date == as.Date("2024-01-01")) |>
    dplyr::collect()

  expect_equal(nrow(filtered), 15) # 15 users for that day
  expect_true(all(filtered$date == as.Date("2024-01-01")))
})

test_that("chronicle_raw_data supports ymd filtering", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Load data for a specific date using ymd parameter
  data <- chronicle_raw_data(
    "connect_users",
    base_path,
    frequency = "daily",
    ymd = list(year = 2024, month = 1, day = 1)
  )

  collected <- dplyr::collect(data)

  # Should only have data from 2024-01-01
  expect_equal(nrow(collected), 15) # 15 users
  expect_true(all(collected$date == as.Date("2024-01-01")))
})

test_that("chronicle_raw_data ymd filtering with different dates", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Load data for 2024-01-03
  data <- chronicle_raw_data(
    "connect_users",
    base_path,
    frequency = "daily",
    ymd = list(year = 2024, month = 1, day = 3)
  )

  collected <- dplyr::collect(data)
  expect_equal(nrow(collected), 15)
  expect_true(all(collected$date == as.Date("2024-01-03")))
})

test_that("chronicle_raw_data preserves data integrity", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Load and check specific data values
  data <- chronicle_raw_data("connect_users", base_path, frequency = "daily")
  collected <- data |>
    dplyr::filter(date == as.Date("2024-01-01")) |>
    dplyr::collect()

  # Verify we have all 15 users
  expect_equal(nrow(collected), 15)

  # Check that all users have the same created_at
  expect_equal(
    unique(collected$created_at),
    as.POSIXct("2023-06-01 10:00:00", tz = "UTC")
  )

  # Check that user1@example.com specifically exists with correct data
  user1_row <- collected[collected$email == "user1@example.com", ]
  expect_equal(nrow(user1_row), 1)
  expect_equal(user1_row$user_role, "viewer")
  expect_false(user1_row$locked)

  # Check that all expected emails exist
  expected_emails <- paste0("user", 1:15, "@example.com")
  expect_true(all(expected_emails %in% collected$email))

  # Check that locked column is all FALSE
  expect_true(all(collected$locked == FALSE))
})

test_that("chronicle_raw_data includes expected user roles", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Load data and check role distribution
  data <- chronicle_raw_data("connect_users", base_path, frequency = "daily")
  collected <- data |>
    dplyr::filter(date == as.Date("2024-01-01")) |>
    dplyr::collect()

  role_counts <- table(collected$user_role)

  # Check expected distribution (per our sample data)
  expect_equal(as.integer(role_counts["viewer"]), 10L)
  expect_equal(as.integer(role_counts["publisher"]), 4L)
  expect_equal(as.integer(role_counts["administrator"]), 1L)
})

test_that("chronicle_raw_data can be used with dplyr operations", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  data <- chronicle_raw_data("connect_users", base_path, frequency = "daily")

  # Test select
  result <- data |>
    dplyr::select(date, email, user_role) |>
    dplyr::collect()
  expect_equal(ncol(result), 3)

  # Test group by and summarize
  result <- data |>
    dplyr::group_by(date) |>
    dplyr::summarise(user_count = dplyr::n()) |>
    dplyr::collect()

  expect_equal(nrow(result), 3) # 3 days
  expect_true(all(result$user_count == 15)) # 15 users per day
})

test_that("chronicle_raw_data validates frequency parameter", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Invalid frequency should error
  expect_error(
    chronicle_raw_data("connect_users", base_path, frequency = "invalid"),
    regexp = "should be one of"
  )
})

test_that("chronicle_raw_data handles multiple days correctly", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Load all days without filtering
  data <- chronicle_raw_data("connect_users", base_path, frequency = "daily")
  collected <- dplyr::collect(data)

  # Check that we have 3 distinct dates
  dates <- unique(collected$date)
  expect_equal(length(dates), 3)
  expect_true(as.Date("2024-01-01") %in% dates)
  expect_true(as.Date("2024-01-02") %in% dates)
  expect_true(as.Date("2024-01-03") %in% dates)

  # Each date should have 15 users
  for (d in dates) {
    count <- sum(collected$date == d)
    expect_equal(count, 15)
  }
})

test_that("chronicle_raw_data timestamp field is POSIXct", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  data <- chronicle_raw_data("connect_users", base_path, frequency = "daily")
  collected <- dplyr::collect(data)

  # Check timestamp types
  expect_s3_class(collected$timestamp, "POSIXct")
  expect_s3_class(collected$created_at, "POSIXct")
  expect_s3_class(collected$last_active_at, "POSIXct")
})

test_that("chronicle_raw_data ymd with non-existent date", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Try to load data for a date that doesn't exist in sample data
  # Arrow might not error immediately due to lazy evaluation
  expect_error(
    {
      data <- chronicle_raw_data(
        "connect_users",
        base_path,
        frequency = "daily",
        ymd = list(year = 2024, month = 12, day = 25)
      )
      dplyr::collect(data)
    },
    regexp = ".*" # Any error is acceptable
  )
})
