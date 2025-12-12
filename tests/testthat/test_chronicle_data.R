test_that("chronicle_data loads curated data successfully", {
  # Create sample data
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Load Connect user totals
  data <- chronicle_data("connect/user_totals", base_path)

  # Check that we get an Arrow dataset
  expect_s3_class(data, "FileSystemDataset")

  # Check that we can collect data
  collected <- dplyr::collect(data)
  expect_s3_class(collected, "data.frame")

  # Check expected columns
  expect_true("date" %in% names(collected))
  expect_true("named_users" %in% names(collected))
  expect_true("active_users_1day" %in% names(collected))
  expect_true("publishers" %in% names(collected))

  # Check expected number of rows (3 days of data)
  expect_equal(nrow(collected), 3)

  # Check data types
  expect_s3_class(collected$date, "Date")
  expect_type(collected$named_users, "integer")
})

test_that("chronicle_data works with different metrics", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Test Connect user list
  data <- chronicle_data("connect/user_list", base_path)
  collected <- dplyr::collect(data)
  expect_true("username" %in% names(collected))
  expect_true("email" %in% names(collected))
  expect_equal(nrow(collected), 26) # 26 sample users

  # Test Workbench user totals
  data <- chronicle_data("workbench/user_totals", base_path)
  collected <- dplyr::collect(data)
  expect_true("administrators" %in% names(collected))
  expect_true("super_administrators" %in% names(collected))
  expect_equal(nrow(collected), 3) # 3 days

  # Test Workbench user list
  data <- chronicle_data("workbench/user_list", base_path)
  collected <- dplyr::collect(data)
  expect_true("username" %in% names(collected))
  expect_equal(nrow(collected), 21) # 21 sample users
})

test_that("chronicle_data supports date filtering", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Load and filter by date
  data <- chronicle_data("connect/user_totals", base_path)
  filtered <- data |>
    dplyr::filter(date == as.Date("2024-01-01")) |>
    dplyr::collect()

  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$date, as.Date("2024-01-01"))
  expect_equal(filtered$named_users, 25L)
  expect_equal(filtered$active_users_1day, 12L)
})

test_that("chronicle_data supports date range filtering", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Load and filter by date range
  data <- chronicle_data("connect/user_totals", base_path)
  filtered <- data |>
    dplyr::filter(
      date >= as.Date("2024-01-02"),
      date <= as.Date("2024-01-03")
    ) |>
    dplyr::collect()

  expect_equal(nrow(filtered), 2)
  expect_true(all(filtered$date >= as.Date("2024-01-02")))
})

test_that("chronicle_data can be used with dplyr operations", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Test various dplyr operations
  data <- chronicle_data("connect/user_totals", base_path)

  # Select columns
  result <- data |>
    dplyr::select(date, named_users) |>
    dplyr::collect()
  expect_equal(ncol(result), 2)
  expect_true(all(c("date", "named_users") %in% names(result)))

  # Arrange
  result <- data |>
    dplyr::arrange(dplyr::desc(active_users_1day)) |>
    dplyr::collect()
  expect_equal(result$active_users_1day[1], 15L) # Max value

  # Summarize
  result <- data |>
    dplyr::summarise(
      avg_users = mean(named_users, na.rm = TRUE),
      max_active = max(active_users_1day, na.rm = TRUE)
    ) |>
    dplyr::collect()
  expect_equal(nrow(result), 1)
  expect_true(result$avg_users > 0)
  expect_equal(result$max_active, 15L)
})

test_that("chronicle_data handles non-existent metric gracefully", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Try to load a metric that doesn't exist
  # Arrow's open_dataset doesn't error immediately (lazy evaluation)
  # But trying to collect should fail with IOError about missing directory
  expect_error(
    {
      data <- chronicle_data("fake/metric", base_path)
      dplyr::collect(data)
    },
    regexp = "(IOError|does not exist|no such file)"
  )
})

test_that("chronicle_data preserves data integrity", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Load Connect user totals and verify values
  data <- chronicle_data("connect/user_totals", base_path) |>
    dplyr::arrange(date) |>
    dplyr::collect()

  # Verify first row
  expect_equal(data$date[1], as.Date("2024-01-01"))
  expect_equal(data$named_users[1], 25L)
  expect_equal(data$active_users_1day[1], 12L)
  expect_equal(data$publishers[1], 5L)

  # Verify last row
  expect_equal(data$date[3], as.Date("2024-01-03"))
  expect_equal(data$named_users[3], 26L)
  expect_equal(data$active_users_1day[3], 10L)
  expect_equal(data$publishers[3], 6L)
})

test_that("chronicle_data handles user list data correctly", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Load Connect user list
  data <- chronicle_data("connect/user_list", base_path) |>
    dplyr::collect()

  # Check user roles distribution
  role_counts <- table(data$user_role)
  expect_equal(as.integer(role_counts["viewer"]), 15L)
  expect_equal(as.integer(role_counts["publisher"]), 8L)
  expect_equal(as.integer(role_counts["administrator"]), 3L)

  # Check that we have active and inactive users
  expect_true(any(data$active_today == TRUE))
  expect_true(any(data$active_today == FALSE))

  # Check environment field (should include NAs)
  expect_true(any(is.na(data$environment)))
  expect_true(any(!is.na(data$environment)))
})
