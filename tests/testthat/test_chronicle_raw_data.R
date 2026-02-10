test_that("chronicle_raw_data loads raw daily data successfully", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  create_raw_test_data(base_path)

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

  # Should have data from multiple days (30 days × 15 users = 450 rows)
  expect_equal(nrow(collected), 450)
})

test_that("chronicle_raw_data defaults to daily frequency", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  create_raw_test_data(base_path)

  # Call without frequency parameter
  data <- chronicle_raw_data("connect_users", base_path)

  collected <- dplyr::collect(data)
  expect_s3_class(collected, "data.frame")
  expect_gt(nrow(collected), 0)
})

test_that("chronicle_raw_data supports date filtering", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  create_raw_test_data(base_path)

  # Load all data and get the first date dynamically
  data <- chronicle_raw_data("connect_users", base_path, frequency = "daily")
  all_data <- dplyr::collect(data)
  first_date <- min(all_data$date)

  # Filter by specific date
  filtered <- data |>
    dplyr::filter(date == first_date) |>
    dplyr::collect()

  expect_equal(nrow(filtered), 15) # 15 users for that day
  expect_true(all(filtered$date == first_date))
})

test_that("chronicle_raw_data supports ymd filtering", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  create_raw_test_data(base_path)

  # First, get the actual dates in the sample data
  all_data <- chronicle_raw_data(
    "connect_users",
    base_path,
    frequency = "daily"
  ) |>
    dplyr::collect()
  first_date <- min(all_data$date)

  # Load data for a specific date using ymd parameter
  data <- chronicle_raw_data(
    "connect_users",
    base_path,
    frequency = "daily",
    ymd = list(
      year = as.integer(format(first_date, "%Y")),
      month = as.integer(format(first_date, "%m")),
      day = as.integer(format(first_date, "%d"))
    )
  )

  collected <- dplyr::collect(data)

  # Should only have data from that date
  expect_equal(nrow(collected), 15) # 15 users
  expect_true(all(collected$date == first_date))
})

test_that("chronicle_raw_data ymd filtering with different dates", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  create_raw_test_data(base_path)

  # First, get the actual dates in the sample data
  all_data <- chronicle_raw_data(
    "connect_users",
    base_path,
    frequency = "daily"
  ) |>
    dplyr::collect()
  sorted_dates <- sort(unique(all_data$date))
  third_date <- sorted_dates[3] # Day 3

  # Load data for day 3
  data <- chronicle_raw_data(
    "connect_users",
    base_path,
    frequency = "daily",
    ymd = list(
      year = as.integer(format(third_date, "%Y")),
      month = as.integer(format(third_date, "%m")),
      day = as.integer(format(third_date, "%d"))
    )
  )

  collected <- dplyr::collect(data)
  expect_equal(nrow(collected), 15)
  expect_true(all(collected$date == third_date))
})

test_that("chronicle_raw_data preserves data integrity", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  create_raw_test_data(base_path)

  # Load all data and get the first date dynamically
  data <- chronicle_raw_data("connect_users", base_path, frequency = "daily")
  all_data <- dplyr::collect(data)
  first_date <- min(all_data$date)

  # Filter to first date
  collected <- all_data |>
    dplyr::filter(date == first_date)

  # Verify we have all 15 users
  expect_equal(nrow(collected), 15)

  # Check that all users have the same created_at
  expect_equal(
    unique(collected$created_at),
    as.POSIXct("2023-06-01 10:00:00", tz = "UTC")
  )

  # Check that all emails are valid (should end with @example.com)
  expect_true(all(grepl("@example.com$", collected$email)))

  # Check that we have various user roles
  expect_true("viewer" %in% collected$user_role)
  expect_true("publisher" %in% collected$user_role)

  # Check that most users are not locked (allow up to 2 locked)
  locked_count <- sum(collected$locked)
  expect_true(locked_count <= 2)
})

test_that("chronicle_raw_data includes expected user roles", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  create_raw_test_data(base_path)

  # Load all data and get the first date dynamically
  data <- chronicle_raw_data("connect_users", base_path, frequency = "daily")
  all_data <- dplyr::collect(data)
  first_date <- min(all_data$date)

  # Filter to first date
  collected <- all_data |>
    dplyr::filter(date == first_date)

  # Check that we have role variety (since we're sampling 15 from 26 users)
  # We should see at least viewers and publishers
  role_counts <- table(collected$user_role)
  expect_true("viewer" %in% names(role_counts))
  expect_true("publisher" %in% names(role_counts))

  # Total should be 15 users
  expect_equal(sum(role_counts), 15)
})

test_that("chronicle_raw_data can be used with dplyr operations", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  create_raw_test_data(base_path)

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

  expect_equal(nrow(result), 30) # 30 days
  expect_true(all(result$user_count == 15)) # 15 users per day
})

test_that("chronicle_raw_data validates frequency parameter", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  create_raw_test_data(base_path)

  # Invalid frequency should error
  expect_error(
    chronicle_raw_data("connect_users", base_path, frequency = "invalid"),
    regexp = "should be one of"
  )
})

test_that("chronicle_raw_data handles multiple days correctly", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  create_raw_test_data(base_path)

  # Load all days without filtering
  data <- chronicle_raw_data("connect_users", base_path, frequency = "daily")
  collected <- dplyr::collect(data)

  # Check that we have 30 distinct dates
  dates <- unique(collected$date)
  expect_equal(length(dates), 30)

  # Verify dates are sequential (30 days ending yesterday)
  sorted_dates <- sort(dates)
  date_diffs <- diff(as.numeric(sorted_dates))
  expect_true(all(date_diffs == 1)) # All dates should be 1 day apart

  # Each date should have 15 users
  for (d in dates) {
    count <- sum(collected$date == d)
    expect_equal(count, 15)
  }
})

test_that("chronicle_raw_data timestamp field is POSIXct", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  create_raw_test_data(base_path)

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
  create_raw_test_data(base_path)

  # Try to load data for a date that doesn't exist in sample data
  # Arrow should error with IOError about missing directory
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
    regexp = "(IOError|does not exist|no such file)"
  )
})
