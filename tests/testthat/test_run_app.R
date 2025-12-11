test_that("chronicle_list_apps returns character vector", {
  apps <- chronicle_list_apps()
  expect_type(apps, "character")
  expect_true(length(apps) > 0)
})

test_that("chronicle_list_apps returns expected app names", {
  apps <- chronicle_list_apps()
  expected_apps <- c(
    "connect",
    "workbench"
  )
  expect_equal(sort(apps), sort(expected_apps))
})

test_that("chronicle_run_app validates app_name parameter", {
  expect_error(
    chronicle_run_app(),
    "app_name is required"
  )
})

test_that("chronicle_run_app errors on invalid app name", {
  expect_error(
    chronicle_run_app("nonexistent_app"),
    "App 'nonexistent_app' not found"
  )
})
