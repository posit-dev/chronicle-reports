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
    "user_guid|content_guid|username|email|host_name|environment",
    text
  ))
  for (guid in c("u1", "u2", "c1", "c2", "w1")) {
    expect_false(grepl(sprintf("\"%s\"", guid), text, fixed = TRUE))
  }
  expect_lt(file.size(file), 20 * 1024)
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

test_that("round trip keeps the servers table", {
  tables <- fixture_tables()
  expect_gt(nrow(tables$servers), 0)
  file <- withr::local_tempfile(fileext = ".json")

  chronicle_usage_to_json(tables, file)
  restored <- chronicle_usage_from_json(file)

  expect_s3_class(restored$servers$date, "Date")
  expect_equal(restored$servers, tables$servers)
})

test_that("old exports without servers load with it empty", {
  tables <- fixture_tables()
  file <- withr::local_tempfile(fileext = ".json")
  chronicle_usage_to_json(tables, file)

  parsed <- jsonlite::fromJSON(file, simplifyVector = TRUE)
  parsed$tables$servers <- NULL
  jsonlite::write_json(parsed, file, auto_unbox = TRUE, dataframe = "rows")

  restored <- chronicle_usage_from_json(file)
  expect_equal(nrow(restored$servers), 0)
  expect_equal(names(restored$servers), names(tables$servers))
  expect_equal(restored$active_users, tables$active_users)
})

test_that("the export contains no host names", {
  tables <- fixture_tables()
  file <- withr::local_tempfile(fileext = ".json")
  chronicle_usage_to_json(tables, file)

  text <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_false(grepl("host_name", text, fixed = TRUE))
  for (host in c("h1", "h2", "w1")) {
    expect_false(grepl(sprintf("\"%s\"", host), text, fixed = TRUE))
  }
})

test_that("servers rows are one per product and product id per day", {
  tables <- fixture_tables()
  file <- withr::local_tempfile(fileext = ".json")
  chronicle_usage_to_json(tables, file)

  servers <- chronicle_usage_from_json(file)$servers
  # Each product only has rows from its first heartbeat day, so there is no
  # full days x products grid in the fixture
  expect_equal(nrow(servers), nrow(tables$servers))
  expect_false(anyDuplicated(servers[c("date", "product", "product_id")]) > 0)
})
