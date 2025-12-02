library(testthat)
library(dplyr)
library(shiny)

# Load internal functions for testing
devtools::load_all()

# =============================================================================
# Tests for calculate_connect_totals_user_counts()
# =============================================================================

test_that("calculate_connect_totals_user_counts returns data unchanged", {
  test_data <- data.frame(
    date = as.Date(c("2025-09-01", "2025-09-02")),
    named_users = c(367, 370),
    active_users_30days = c(288, 290),
    active_users_1day = c(28, 30),
    administrators = c(13, 13),
    publishers = c(354, 356),
    viewers = c(0, 1),
    licensed_user_seats = c(1000, 1000)
  )

  result <- calculate_connect_totals_user_counts(test_data)

  # Should return data unchanged (pass-through function)
  expect_equal(result, test_data)
})

test_that("calculate_connect_totals_user_counts handles empty data", {
  test_data <- data.frame(
    date = as.Date(character(0)),
    named_users = integer(0),
    active_users_30days = integer(0),
    active_users_1day = integer(0),
    administrators = integer(0),
    publishers = integer(0),
    viewers = integer(0),
    licensed_user_seats = integer(0)
  )

  result <- calculate_connect_totals_user_counts(test_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(result, test_data)
})

test_that("calculate_connect_totals_user_counts preserves tibbles", {
  test_data <- tibble::tibble(
    date = as.Date(c("2025-09-01")),
    named_users = 367,
    active_users_1day = 28,
    publishers = 354
  )

  result <- calculate_connect_totals_user_counts(test_data)

  expect_s3_class(result, "tbl_df")
  expect_equal(result, test_data)
})

test_that("calculate_connect_totals_user_counts preserves column names and types", {
  test_data <- data.frame(
    date = as.Date(c("2025-09-01", "2025-09-02")),
    named_users = c(367L, 370L),
    active_users_1day = c(28L, 30L),
    publishers = c(354L, 356L)
  )

  result <- calculate_connect_totals_user_counts(test_data)

  expect_equal(names(result), names(test_data))
  expect_type(result$named_users, "integer")
  expect_type(result$active_users_1day, "integer")
  expect_type(result$publishers, "integer")
  expect_s3_class(result$date, "Date")
})

# =============================================================================
# Tests for UI structure
# =============================================================================

test_that("connect_user_totals_ui has expected structure", {
  ui <- connect_user_totals_ui

  # UI should be a bslib page (which is a list/shiny.tag.list)
  expect_true(inherits(ui, "bslib_page") || inherits(ui, "shiny.tag.list"))

  # Convert to HTML to inspect structure
  ui_html <- as.character(ui)

  # Check for key elements
  expect_true(grepl("Licensed Users", ui_html, fixed = TRUE))
  expect_true(grepl("Daily Users", ui_html, fixed = TRUE))
  expect_true(grepl("Publishers", ui_html, fixed = TRUE))
  expect_true(grepl("date_range", ui_html, fixed = TRUE))
  expect_true(grepl("user_trend_plot", ui_html, fixed = TRUE))
  expect_true(grepl("activity_pattern_plot", ui_html, fixed = TRUE))
})

# =============================================================================
# Tests for server logic - testing data transformation functions
# =============================================================================
# Note: Full server testing with testServer() is complex due to Arrow dataset
# dependencies. These tests focus on the data transformation logic.

test_that("latest_data logic extracts most recent date", {
  # Test the slice_max logic used in the server
  mock_data <- data.frame(
    date = as.Date(c("2025-09-01", "2025-09-02", "2025-09-03")),
    named_users = c(367, 370, 375),
    active_users_1day = c(28, 30, 32),
    publishers = c(354, 356, 358)
  )

  latest_data <- mock_data |>
    slice_max(date, n = 1)

  # Latest data should be the most recent date
  expect_equal(nrow(latest_data), 1)
  expect_equal(latest_data$date, as.Date("2025-09-03"))
  expect_equal(latest_data$named_users, 375)
  expect_equal(latest_data$active_users_1day, 32)
  expect_equal(latest_data$publishers, 358)
})

test_that("date range filtering logic works correctly", {
  # Test the filtering logic used in the server
  mock_data <- data.frame(
    date = as.Date(c("2025-09-01", "2025-09-02", "2025-09-03", "2025-09-04")),
    named_users = c(367, 370, 375, 380),
    active_users_1day = c(28, 30, 32, 35),
    publishers = c(354, 356, 358, 360)
  )

  # Simulate date range filtering
  date_start <- as.Date("2025-09-02")
  date_end <- as.Date("2025-09-03")

  filtered_data <- mock_data |>
    filter(date >= date_start, date <= date_end)

  # Should only have 2 rows (Sep 2 and Sep 3)
  expect_equal(nrow(filtered_data), 2)
  expect_equal(filtered_data$date, as.Date(c("2025-09-02", "2025-09-03")))
})

test_that("prettyNum formats numbers with commas correctly", {
  # Test the number formatting logic used in value box outputs
  expect_equal(prettyNum(1234, big.mark = ","), "1,234")
  expect_equal(prettyNum(5678, big.mark = ","), "5,678")
  expect_equal(prettyNum(9012, big.mark = ","), "9,012")
  expect_equal(prettyNum(1234567, big.mark = ","), "1,234,567")
})

test_that("plot data transformation handles missing values", {
  mock_data <- data.frame(
    date = as.Date(c("2025-09-01", "2025-09-02", "2025-09-03")),
    named_users = c(367, NA, 375),
    active_users_1day = c(28, 30, NA),
    publishers = c(NA, 356, 358)
  )

  # Test the data transformation logic used in the plot
  plot_data <- mock_data |>
    select(date, named_users, active_users_1day, publishers) |>
    filter(!is.na(date)) |>
    tidyr::pivot_longer(-date, names_to = "metric", values_to = "value") |>
    filter(!is.na(value), is.finite(value))

  # Should filter out NA values
  expect_true(all(!is.na(plot_data$value)))
  expect_true(all(is.finite(plot_data$value)))

  # Should have fewer rows than original due to NAs
  expect_lt(nrow(plot_data), 9) # 3 dates * 3 metrics = 9, but we have NAs
})

test_that("plot data transformation handles all-NA data", {
  mock_data <- data.frame(
    date = as.Date(c("2025-09-01", "2025-09-02")),
    named_users = c(NA, NA),
    active_users_1day = c(NA, NA),
    publishers = c(NA, NA)
  )

  plot_data <- mock_data |>
    select(date, named_users, active_users_1day, publishers) |>
    filter(!is.na(date)) |>
    tidyr::pivot_longer(-date, names_to = "metric", values_to = "value") |>
    filter(!is.na(value), is.finite(value))

  # Should result in empty data frame
  expect_equal(nrow(plot_data), 0)
})

test_that("activity pattern plot aggregates by day of week", {
  # Create data spanning a week
  mock_data <- data.frame(
    date = seq(as.Date("2025-09-01"), as.Date("2025-09-07"), by = "day"),
    active_users_1day = c(20, 25, 30, 28, 26, 15, 10) # Mon-Sun pattern
  )

  # Replicate the aggregation logic from the server
  day_summary <- mock_data |>
    mutate(day_of_week = lubridate::wday(date, label = TRUE, abbr = FALSE)) |>
    group_by(day_of_week) |>
    summarise(
      avg_active_users = mean(active_users_1day, na.rm = TRUE),
      .groups = "drop"
    )

  # Should have 7 rows (one per day of week)
  expect_equal(nrow(day_summary), 7)

  # Should have correct columns
  expect_true(all(c("day_of_week", "avg_active_users") %in% names(day_summary)))

  # Values should match original (no averaging needed with one week)
  expect_equal(day_summary$avg_active_users, c(10, 20, 25, 30, 28, 26, 15))
})

# =============================================================================
# Tests for main app function
# =============================================================================

test_that("connect_user_totals_app returns a Shiny app object", {
  app <- connect_user_totals_app(base_path = "/fake/path")

  expect_s3_class(app, "shiny.appobj")
  # App objects have serverFuncSource, not direct ui/server
  expect_true(!is.null(app$serverFuncSource))
})

test_that("connect_user_totals_app sets shinyOptions correctly", {
  # Clear any existing options
  shinyOptions(base_path = NULL)

  app <- connect_user_totals_app(base_path = "/test/path")

  # Check that base_path option was set
  expect_equal(getShinyOption("base_path"), "/test/path")

  # Clean up
  shinyOptions(base_path = NULL)
})

test_that("connect_user_totals_app uses environment variable", {
  # Set environment variable
  withr::with_envvar(
    c(CHRONICLE_BASE_PATH = "/env/path"),
    {
      shinyOptions(base_path = NULL)
      app <- connect_user_totals_app()
      expect_equal(getShinyOption("base_path"), "/env/path")
      shinyOptions(base_path = NULL)
    }
  )
})

test_that("connect_user_totals_app uses default when no env var", {
  # Ensure env var is not set
  withr::with_envvar(
    c(CHRONICLE_BASE_PATH = NA),
    {
      shinyOptions(base_path = NULL)
      app <- connect_user_totals_app()
      # Should use APP_CONFIG$DEFAULT_BASE_PATH
      expect_equal(getShinyOption("base_path"), APP_CONFIG$DEFAULT_BASE_PATH)
      shinyOptions(base_path = NULL)
    }
  )
})
