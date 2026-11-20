fixture_tables <- function() {
  path <- create_usage_fixture()
  withr::defer(unlink(path, recursive = TRUE), envir = parent.frame())
  chronicle_usage_data(path, today = USAGE_TEST_TODAY)
}

test_that("export round-trips to identical tables", {
  tables <- fixture_tables()
  file <- withr::local_tempfile(fileext = ".json")

  chronicle_usage_to_json(tables, file)
  restored <- chronicle_usage_from_json(file)

  expect_equal(restored, tables)
})

test_that("export carries a schema version and the month range", {
  tables <- fixture_tables()
  file <- withr::local_tempfile(fileext = ".json")
  chronicle_usage_to_json(tables, file)

  parsed <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_equal(parsed$schema_version, 1L)
  expect_match(parsed$generated_at, "^\\d{4}-\\d{2}-\\d{2}T")
  expect_equal(parsed$first_month, "2025-12") # window bounds, not data bounds
  expect_equal(parsed$last_month, "2026-11")
  expect_true(all(
    c("active_users", "content_hits", "content_deployments", "sessions") %in%
      names(parsed$tables)
  ))
})

test_that("export contains only aggregates, with no identifiers", {
  tables <- fixture_tables()
  file <- withr::local_tempfile(fileext = ".json")
  chronicle_usage_to_json(tables, file)

  text <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_false(grepl(
    "user_guid|content_guid|username|email|host|environment",
    text
  ))
  for (guid in c("u1", "u2", "c1", "c2", "w1")) {
    expect_false(grepl(sprintf("\"%s\"", guid), text, fixed = TRUE))
  }
  expect_lt(file.size(file), 10 * 1024)
})

test_that("importing rejects an unknown schema version", {
  file <- withr::local_tempfile(fileext = ".json")
  writeLines('{"schema_version": 99, "tables": {}}', file)
  expect_error(chronicle_usage_from_json(file), "schema")
})

test_that("an empty report round-trips", {
  empty <- withr::local_tempdir()
  tables <- suppressMessages(chronicle_usage_data(
    empty,
    today = USAGE_TEST_TODAY
  ))
  file <- withr::local_tempfile(fileext = ".json")

  chronicle_usage_to_json(tables, file)
  expect_equal(chronicle_usage_from_json(file), tables)
})

test_that("the as-of date survives a round trip, whatever day it is read", {
  tables <- fixture_tables()
  file <- withr::local_tempfile(fileext = ".json")
  chronicle_usage_to_json(tables, file)

  expect_equal(attr(chronicle_usage_from_json(file), "as_of"), USAGE_TEST_TODAY)
})

test_that("an export without an as-of date falls back to its generation date", {
  file <- withr::local_tempfile(fileext = ".json")
  writeLines(
    '{"schema_version": 1, "generated_at": "2026-10-31T12:00:00Z", "tables": {}}',
    file
  )

  expect_equal(
    attr(chronicle_usage_from_json(file), "as_of"),
    as.Date("2026-10-31")
  )
})

test_that("an export from before a column existed loads with defaults", {
  tables <- fixture_tables()
  file <- withr::local_tempfile(fileext = ".json")
  chronicle_usage_to_json(tables, file)

  parsed <- jsonlite::fromJSON(file, simplifyVector = TRUE)
  parsed$tables$content_deployments$unique_content <- NULL
  parsed$tables$content_hits$app_mode <- NULL
  parsed$tables$sessions$product_id <- NULL
  jsonlite::write_json(parsed, file, auto_unbox = TRUE, dataframe = "rows")

  restored <- chronicle_usage_from_json(file)

  expect_equal(
    names(restored$content_deployments),
    names(tables$content_deployments)
  )
  expect_true(all(restored$content_deployments$unique_content == 0))
  expect_true(all(restored$content_hits$app_mode == "unknown"))
  expect_true(all(restored$sessions$product_id == "(unspecified)"))
  expect_equal(restored$active_users, tables$active_users)
})
