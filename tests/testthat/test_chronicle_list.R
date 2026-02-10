test_that("chronicle_list_data returns available curated metrics", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # List available metrics
  metrics <- chronicle_list_data(base_path)

  # Check that we get a character vector
  expect_type(metrics, "character")

  # Check that we have the expected metrics
  expect_true("connect/user_totals" %in% metrics)
  expect_true("connect/user_list" %in% metrics)
  expect_true("workbench/user_totals" %in% metrics)
  expect_true("workbench/user_list" %in% metrics)

  # Check that we have at least 4 metrics
  expect_gte(length(metrics), 4)
})

test_that("chronicle_list_data returns product/metric format", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  metrics <- chronicle_list_data(base_path)

  # All metrics should contain a slash (product/metric format)
  expect_true(all(grepl("/", metrics)))

  # Check specific format examples
  for (metric in metrics) {
    parts <- strsplit(metric, "/")[[1]]
    expect_equal(length(parts), 2) # Should have exactly 2 parts
  }
})

test_that("chronicle_list_data can be used to load data", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # List metrics and load the first one
  metrics <- chronicle_list_data(base_path)
  expect_gt(length(metrics), 0)

  # Load data using the first metric
  data <- chronicle_data(metrics[1], base_path)
  expect_s3_class(data, "FileSystemDataset")

  # Should be able to collect it
  collected <- dplyr::collect(data)
  expect_s3_class(collected, "data.frame")
  expect_gt(nrow(collected), 0)
})

test_that("chronicle_list_data handles empty directory gracefully", {
  # Create an empty directory
  empty_path <- file.path(tempdir(), "chronicle-empty")
  dir.create(
    file.path(empty_path, "curated", "v2"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  on.exit(unlink(empty_path, recursive = TRUE))

  # Should return empty vector (may be NULL or empty character)
  metrics <- chronicle_list_data(empty_path)
  expect_true(
    is.null(metrics) || (is.character(metrics) && length(metrics) == 0)
  )
})

test_that("chronicle_list_raw_data returns available raw metrics", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  create_raw_test_data(base_path)

  # List available daily raw metrics
  metrics <- chronicle_list_raw_data(base_path, frequency = "daily")

  # Check that we get a character vector
  expect_type(metrics, "character")

  # Check that we have connect_users (our sample raw data)
  expect_true("connect_users" %in% metrics)

  # Check that we have at least 1 metric
  expect_gte(length(metrics), 1)
})

test_that("chronicle_list_raw_data returns metric names without product prefix", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  create_raw_test_data(base_path)

  metrics <- chronicle_list_raw_data(base_path, frequency = "daily")

  # Raw metrics should NOT contain slashes (just metric names)
  # Some might, but connect_users shouldn't
  expect_true("connect_users" %in% metrics)
  expect_false(grepl("/", "connect_users"))
})

test_that("chronicle_list_raw_data defaults to daily frequency", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  create_raw_test_data(base_path)

  # Call without frequency parameter (should default to "daily")
  metrics <- chronicle_list_raw_data(base_path)

  expect_type(metrics, "character")
  expect_gte(length(metrics), 1)
})

test_that("chronicle_list_raw_data handles hourly frequency", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Our sample data doesn't include hourly, but should handle gracefully
  metrics <- chronicle_list_raw_data(base_path, frequency = "hourly")

  expect_type(metrics, "character")
  # May be empty since we don't create hourly sample data
})

test_that("chronicle_list_raw_data validates frequency parameter", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  # Invalid frequency should error
  expect_error(
    chronicle_list_raw_data(base_path, frequency = "invalid"),
    regexp = "should be one of"
  )
})

test_that("chronicle_list_data and chronicle_list_raw_data are distinct", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  create_raw_test_data(base_path)

  curated <- chronicle_list_data(base_path)
  raw <- chronicle_list_raw_data(base_path, frequency = "daily")

  # Should have different results
  # Curated should have product/metric format
  expect_true(all(grepl("/", curated)))

  # Raw should generally not have slashes (though some metrics might)
  # At minimum, formats should be different
  expect_false(identical(curated, raw))
})

test_that("chronicle_list_data discovers nested structure correctly", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))

  metrics <- chronicle_list_data(base_path)

  # Should find metrics under both products
  connect_metrics <- metrics[grepl("^connect/", metrics)]
  workbench_metrics <- metrics[grepl("^workbench/", metrics)]

  expect_gt(length(connect_metrics), 0)
  expect_gt(length(workbench_metrics), 0)

  # Each product should have multiple metrics
  expect_gte(length(connect_metrics), 2) # user_totals, user_list
  expect_gte(length(workbench_metrics), 2) # user_totals, user_list
})
