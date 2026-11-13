usage_fixture_tables <- function() {
  path <- create_usage_fixture()
  withr::defer(unlink(path, recursive = TRUE), envir = parent.frame())
  chronicle_usage_data(path, months = 12, today = USAGE_TEST_TODAY)
}

month_of <- function(x) as.Date(paste0(x, "-01"))

test_that("returns the four tables with the documented columns", {
  tables <- usage_fixture_tables()

  expect_named(
    tables,
    c("active_users", "content_hits", "content_deployments", "sessions")
  )
  expect_named(
    tables$active_users,
    c("month", "product", "product_id", "users")
  )
  expect_named(
    tables$content_hits,
    c("month", "product_id", "app_mode", "authenticated", "hits")
  )
  expect_named(
    tables$content_deployments,
    c("month", "product_id", "app_mode", "deployments", "unique_content")
  )
  expect_named(
    tables$sessions,
    c("month", "product_id", "session_type", "sessions")
  )
})

test_that("Connect active users are distinct per month and exclude anonymous", {
  au <- usage_fixture_tables()$active_users
  connect <- au[au$product == "connect", ]

  oct <- connect[connect$month == month_of("2026-10") & connect$product_id == "p1", ]
  expect_equal(oct$users, 2L) # u1 on two days counts once; NA and "" excluded

  nov <- connect[connect$month == month_of("2026-11"), ]
  expect_equal(nov$users[nov$product_id == "p1"], 1L)
  expect_equal(nov$users[nov$product_id == "p2"], 1L)
})

test_that("Workbench active users are distinct per month", {
  au <- usage_fixture_tables()$active_users
  wb <- au[au$product == "workbench", ]

  expect_equal(wb$users[wb$month == month_of("2026-10")], 2L)
  expect_equal(wb$users[wb$month == month_of("2026-11")], 1L)
})

test_that("content hits split authenticated from anonymous by app_mode", {
  hits <- usage_fixture_tables()$content_hits
  oct <- hits[hits$month == month_of("2026-10") & hits$product_id == "p1", ]

  get <- function(mode, auth) {
    oct$hits[oct$app_mode == mode & oct$authenticated == auth]
  }
  expect_equal(get("shiny", TRUE), 7L)
  expect_equal(get("shiny", FALSE), 4L) # NA user_guid
  expect_equal(get("quarto-static", TRUE), 3L)
  expect_equal(get("quarto-static", FALSE), 1L) # empty-string user_guid
})

test_that("deployments count redeploys, unique content counts each item once", {
  dep <- usage_fixture_tables()$content_deployments
  oct <- dep[dep$month == month_of("2026-10"), ]

  shiny <- oct[oct$app_mode == "shiny", ]
  expect_equal(shiny$deployments, 3L) # c1 deployed 3 times
  expect_equal(shiny$unique_content, 1L)

  # c2 moved to quarto-static in the latest snapshot
  quarto <- oct[oct$app_mode == "quarto-static", ]
  expect_equal(quarto$deployments, 1L)
  expect_equal(quarto$unique_content, 1L)
})

test_that("content missing from the snapshots has an unknown app_mode", {
  dep <- usage_fixture_tables()$content_deployments
  nov <- dep[dep$month == month_of("2026-11"), ]
  expect_equal(nov$app_mode, "unknown")
  expect_equal(nov$deployments, 1L)
})

test_that("sessions are grouped by IDE", {
  s <- usage_fixture_tables()$sessions
  oct <- s[s$month == month_of("2026-10"), ]

  expect_equal(oct$sessions[oct$session_type == "RStudio"], 3L)
  expect_equal(oct$sessions[oct$session_type == "Positron"], 2L)
})

test_that("data outside the 12-month window is excluded", {
  tables <- usage_fixture_tables()
  first_month <- month_of("2025-12")

  for (tbl in tables) {
    expect_true(all(tbl$month >= first_month))
    expect_true(all(tbl$month <= month_of("2026-11")))
  }
  hits <- tables$content_hits
  expect_false(any(hits$hits == 100L))
})

test_that("an empty product_id is labelled rather than dropped", {
  path <- create_usage_fixture()
  on.exit(unlink(path, recursive = TRUE))
  hits <- data.frame(
    date = as.Date("2026-10-10"),
    product_id = "",
    environment = "prod",
    content_guid = "c1",
    user_guid = "u1",
    hits = 3L,
    app_mode = "shiny"
  )
  unlink(file.path(path, "curated/v2/connect/content_hits_totals_by_user"), recursive = TRUE)
  write_sample_parquet_internal(hits, path, "connect/content_hits_totals_by_user")

  tables <- chronicle_usage_data(path, today = USAGE_TEST_TODAY)
  expect_equal(unique(tables$content_hits$product_id), USAGE_UNSPECIFIED_PRODUCT)
})

test_that("missing datasets give empty tables with the right columns", {
  empty <- withr::local_tempdir()
  expect_message(
    tables <- chronicle_usage_data(empty, today = USAGE_TEST_TODAY),
    "not found"
  )
  for (tbl in tables) {
    expect_equal(nrow(tbl), 0L)
  }
  expect_named(tables$content_hits, c("month", "product_id", "app_mode", "authenticated", "hits"))
})
