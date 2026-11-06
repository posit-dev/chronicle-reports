# Tests for the package's sample data: its span, product IDs, content types and
# deployments. These use the exported function with its default, so they cover
# what users see. The sample is generated once for the whole file.

sample_path <- suppressMessages(
  chronicle_sample_data(
    base_path = withr::local_tempdir(.local_envir = testthat::teardown_env())
  )
)

collect_curated <- function(metric) {
  dplyr::collect(chronicle_data(metric, sample_path))
}

collect_raw <- function(metric) {
  dplyr::collect(chronicle_raw_data(metric, sample_path))
}

test_that("the sample covers about six months of daily data", {
  hits <- collect_curated("connect/content_hits_totals_by_user")
  dates <- sort(unique(hits$date))

  expect_gte(length(dates), 170)
  expect_equal(max(dates), Sys.Date() - 1)
  expect_gte(length(unique(format(dates, "%Y-%m"))), 6)

  # the totals have a row for every day; hits can skip a quiet early weekend
  totals <- collect_curated("connect/user_totals")
  expect_equal(length(unique(totals$date)), 180)
})

test_that("every dataset has a populated product_id", {
  curated <- chronicle_list_data(sample_path)
  raw <- chronicle_list_raw_data(sample_path)

  for (metric in curated) {
    data <- collect_curated(metric)
    expect_true("product_id" %in% names(data), info = metric)
    expect_false(any(is.na(data$product_id) | data$product_id == ""), info = metric)
  }
  for (metric in raw) {
    data <- collect_raw(metric)
    expect_true("product_id" %in% names(data), info = metric)
    expect_false(any(is.na(data$product_id) | data$product_id == ""), info = metric)
  }
})

test_that("Connect moves between two product IDs and Workbench keeps one", {
  connect <- collect_curated("connect/user_totals")
  workbench <- collect_curated("workbench/user_totals")

  expect_equal(sort(unique(connect$product_id)), c("connect-license-a", "connect-license-b"))
  expect_equal(unique(workbench$product_id), "workbench-license")

  # one Connect product ID is in use on any day, and the second follows the first
  expect_equal(nrow(connect), length(unique(connect$date)))
  by_date <- connect[order(connect$date), ]
  expect_equal(rle(by_date$product_id)$values, c("connect-license-a", "connect-license-b"))
})

test_that("content hits carry a content type that matches the content list", {
  hits <- collect_curated("connect/content_hits_totals_by_user")
  totals <- collect_curated("connect/content_hits_totals")
  content <- collect_curated("connect/content_list")

  for (data in list(hits, totals)) {
    expect_false(any(is.na(data$app_mode) | data$app_mode == ""))
    expect_gte(length(unique(data$app_mode)), 4)
  }

  type_by_guid <- stats::setNames(content$type, content$id)
  expect_equal(hits$app_mode, unname(type_by_guid[hits$content_guid]))
  expect_true("shiny" %in% hits$app_mode)
  expect_true("quarto-static" %in% hits$app_mode)

  content_totals <- collect_curated("connect/content_totals")
  expect_true(all(content_totals$type %in% content$type))
})

test_that("the sample includes anonymous hits", {
  hits <- collect_curated("connect/content_hits_totals_by_user")
  expect_true(any(is.na(hits$user_guid)))
  expect_true(any(!is.na(hits$user_guid)))
})

test_that("the sample has deployments, including redeploys", {
  published <- collect_raw("connect_content_published")
  contents <- collect_raw("connect_contents")

  expect_gt(nrow(published), length(unique(published$content_guid)))
  expect_true(all(c("timestamp", "content_guid", "user_guid", "bundle_id") %in% names(published)))
  expect_equal(anyDuplicated(published$bundle_id), 0L)

  # every deployed item has a type in a snapshot of its own license
  described <- unique(paste(contents$product_id, contents$guid))
  expect_true(all(paste(published$product_id, published$content_guid) %in% described))
  expect_false(any(is.na(contents$app_mode) | contents$app_mode == ""))
})

test_that("content last_deployed_time agrees with the deployment events", {
  published <- collect_raw("connect_content_published")
  content <- collect_curated("connect/content_list")

  latest <- tapply(as.numeric(as.Date(published$timestamp)), published$content_guid, max)
  listed <- stats::setNames(as.numeric(as.Date(content$last_deployed_time)), content$id)

  expect_equal(unname(listed[names(latest)]), as.vector(latest))
})

test_that("activity grows over the period", {
  tables <- chronicle_usage_data(sample_path)
  users <- tables$active_users

  for (product in c("connect", "workbench")) {
    by_month <- tapply(users$users[users$product == product], users$month[users$product == product], max)
    expect_lt(by_month[[1]], by_month[[length(by_month) - 1]], label = product)
  }
})

test_that("the usage report has data in every table across several months", {
  tables <- chronicle_usage_data(sample_path)

  for (name in names(tables)) {
    expect_gte(length(unique(tables[[name]]$month)), 6, label = name)
  }
  expect_equal(
    sort(unique(tables$content_hits$product_id)),
    c("connect-license-a", "connect-license-b")
  )
  expect_equal(unique(tables$sessions$product_id), "workbench-license")
  expect_true(all(c(TRUE, FALSE) %in% tables$content_hits$authenticated))
  expect_gte(length(unique(tables$sessions$session_type)), 3)

  deployments <- tables$content_deployments
  expect_false("unknown" %in% deployments$app_mode)
  expect_true(any(deployments$deployments > deployments$unique_content))
})

test_that("a short span is still available for fast tests", {
  path <- create_sample_chronicle_data()
  on.exit(unlink(path, recursive = TRUE))

  hits <- dplyr::collect(chronicle_data("connect/content_hits_totals_by_user", path))
  expect_equal(length(unique(hits$date)), 30)
  expect_true("product_id" %in% names(hits))
})
