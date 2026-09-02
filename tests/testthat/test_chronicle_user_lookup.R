# Fixture: two accounts that share an email address, as happens when a Connect
# install has both a legacy and a current account for the same person. The
# curated user_list collapses these to one row; the daily layer keeps both.
write_shared_email_users <- function(base_path, ymd = c("2026", "08", "26")) {
  users <- data.frame(
    timestamp = as.POSIXct(
      c(
        "2026-08-26 08:00:00",
        "2026-08-26 09:00:00",
        "2026-08-26 08:00:00"
      ),
      tz = "UTC"
    ),
    id = c("guid-rstub", "guid-rstub", "guid-ralf"),
    username = c("rstub-stale", "rstub", "ralf"),
    email = c("ralf@posit.co", "ralf@posit.co", "ralf@posit.co"),
    stringsAsFactors = FALSE
  )

  metric_path <- file.path(
    base_path,
    "daily",
    "v2",
    "connect_users",
    ymd[1],
    ymd[2],
    ymd[3]
  )
  dir.create(metric_path, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(users, file.path(metric_path, "data.parquet"))

  invisible(base_path)
}

test_that("chronicle_user_lookup keeps accounts that share an email", {
  base_path <- withr::local_tempdir()
  write_shared_email_users(base_path)

  lookup <- chronicle_user_lookup("connect_users", base_path)

  expect_s3_class(lookup, "data.frame")
  expect_named(lookup, c("id", "username"))
  # Both GUIDs survive even though they share ralf@posit.co
  expect_setequal(lookup$id, c("guid-rstub", "guid-ralf"))
})

test_that("chronicle_user_lookup returns one row per GUID, newest username", {
  base_path <- withr::local_tempdir()
  write_shared_email_users(base_path)

  lookup <- chronicle_user_lookup("connect_users", base_path)

  expect_equal(nrow(lookup), 2)
  expect_false(any(duplicated(lookup$id)))
  # guid-rstub has two rows; the 09:00 observation wins over the 08:00 one
  expect_equal(lookup$username[lookup$id == "guid-rstub"], "rstub")
})

test_that("chronicle_user_lookup reads only the most recent partition", {
  base_path <- withr::local_tempdir()
  write_shared_email_users(base_path, ymd = c("2026", "08", "26"))

  # An older partition holding a since-renamed account must not win
  older <- data.frame(
    timestamp = as.POSIXct("2026-07-01 08:00:00", tz = "UTC"),
    id = "guid-rstub",
    username = "old-name",
    email = "ralf@posit.co",
    stringsAsFactors = FALSE
  )
  older_path <- file.path(
    base_path,
    "daily",
    "v2",
    "connect_users",
    "2026",
    "07",
    "01"
  )
  dir.create(older_path, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(older, file.path(older_path, "data.parquet"))

  lookup <- chronicle_user_lookup("connect_users", base_path)

  expect_equal(nrow(lookup), 2)
  expect_false("old-name" %in% lookup$username)
})

test_that("chronicle_user_lookup returns NULL when no daily data exists", {
  base_path <- withr::local_tempdir()

  expect_null(chronicle_user_lookup("connect_users", base_path))
})

test_that("chronicle_user_lookup returns NULL when id/username are absent", {
  base_path <- withr::local_tempdir()
  metric_path <- file.path(
    base_path,
    "daily",
    "v2",
    "connect_users",
    "2026",
    "08",
    "26"
  )
  dir.create(metric_path, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(
    data.frame(timestamp = Sys.time(), email = "a@b.com"),
    file.path(metric_path, "data.parquet")
  )

  expect_null(chronicle_user_lookup("connect_users", base_path))
})

test_that("chronicle_user_lookup works against the bundled sample data", {
  sample_path <- chronicle_sample_data()

  lookup <- chronicle_user_lookup("connect_users", sample_path)

  expect_s3_class(lookup, "data.frame")
  expect_named(lookup, c("id", "username"))
  expect_gt(nrow(lookup), 0)
  expect_false(any(duplicated(lookup$id)))
})

test_that("chronicle_user_lookup reads Workbench users from pwb_users", {
  base_path <- withr::local_tempdir()
  users <- data.frame(
    timestamp = as.POSIXct(
      c("2026-08-26 08:00:00", "2026-08-26 09:00:00"),
      tz = "UTC"
    ),
    id = c("wb-guid-1", "wb-guid-2"),
    username = c("ralf", "ralf"),
    stringsAsFactors = FALSE
  )
  metric_path <- file.path(
    base_path,
    "daily",
    "v2",
    "pwb_users",
    "2026",
    "08",
    "26"
  )
  dir.create(metric_path, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(users, file.path(metric_path, "data.parquet"))

  lookup <- chronicle_user_lookup("pwb_users", base_path)

  # Both accounts survive despite sharing a username
  expect_equal(nrow(lookup), 2)
  expect_setequal(lookup$id, c("wb-guid-1", "wb-guid-2"))
})

test_that("chronicle_user_lookup works on sample Workbench data", {
  sample_path <- chronicle_sample_data()

  lookup <- chronicle_user_lookup("pwb_users", sample_path)

  expect_s3_class(lookup, "data.frame")
  expect_named(lookup, c("id", "username"))
  expect_gt(nrow(lookup), 0)
  expect_false(any(duplicated(lookup$id)))
})
