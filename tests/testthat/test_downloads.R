# Tests for CSV download functionality in Connect and Workbench apps
#
# These tests verify that:
# 1. Download helper functions produce correct UI structures
# 2. Download handlers produce valid CSV content
# 3. Empty/NULL data states are handled gracefully

# ============================================================
# Helper: source an app file in its own environment so we can
# call the sub-server functions and helpers without collisions.
# ============================================================
source_app_env <- function(app_name, bp) {
  env <- new.env(parent = globalenv())

  app_file <- system.file(
    "apps", app_name, "app.R",
    package = "chronicle.reports"
  )

  # Set CHRONICLE_BASE_PATH so the sourced app picks
  # up sample data instead of the default system path.
  withr::with_envvar(
    c(CHRONICLE_BASE_PATH = bp),
    source(app_file, local = env)
  )
  env
}

# Helper: wrap a sub-server function that takes extra arguments
# beyond (input, output, session) into a plain
# (input, output, session) function that testServer() can call.
wrap_server <- function(server_fn, ...) {
  extra_args <- list(...)
  function(input, output, session) {
    do.call(
      server_fn,
      c(list(input, output, session), extra_args)
    )
  }
}

# Helper: safely read a CSV that might be empty (0 rows).
# write.csv(data.frame(), ...) writes an empty file that
# read.csv chokes on. Return a 0-row data.frame in that case.
read_csv_safe <- function(path) {
  tryCatch(
    utils::read.csv(path),
    error = function(e) data.frame()
  )
}

# ============================================================
# Tests for download UI helper functions
# ============================================================

test_that("card_header_with_download produces correct structure", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  env <- source_app_env("connect", base_path)

  header <- env$card_header_with_download("My Table", "dl_table")
  expect_s3_class(header, "shiny.tag")

  html <- as.character(header)
  expect_true(grepl("My Table", html))
  expect_true(grepl("dl_table", html))
  expect_true(grepl("display: flex", html))
  expect_true(grepl("gap: 16px", html))
})

test_that("card_header_with_download supports subtitle_output", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  env <- source_app_env("connect", base_path)

  subtitle <- shiny::span("as of 2025-01-01")
  header <- env$card_header_with_download(
    "Title", "dl_id",
    subtitle_output = subtitle
  )
  html <- as.character(header)
  expect_true(grepl("Title", html))
  expect_true(grepl("as of 2025-01-01", html))
})

test_that(
  "card_header_with_chart_downloads has popover with two links",
  {
    base_path <- create_sample_chronicle_data()
    on.exit(unlink(base_path, recursive = TRUE))
    env <- source_app_env("connect", base_path)

    header <- env$card_header_with_chart_downloads(
      "My Chart", "dl_chart", "dl_raw"
    )
    html <- as.character(header)
    expect_true(grepl("My Chart", html))
    expect_true(grepl("dl_chart", html))
    expect_true(grepl("dl_raw", html))
    expect_true(grepl("Chart data", html))
    expect_true(grepl("Raw data", html))
    expect_true(grepl("gap: 16px", html))
  }
)

# ============================================================
# Tests for Connect app download handlers
# ============================================================

test_that("Connect: user trends download handlers work", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  env <- source_app_env("connect", base_path)

  raw_data <- chronicle_data(
    "connect/user_totals", base_path
  ) |> dplyr::collect()
  date_range <- c(
    min(raw_data$date), max(raw_data$date)
  )

  shiny::testServer(
    wrap_server(
      env$users_overview_server,
      shiny::reactive(raw_data)
    ),
    {
      session$setInputs(
        users_overview_date_range = date_range
      )

      # Chart download
      f <- output$download_user_trends_chart
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)
      expect_true("metric" %in% names(csv))

      # Raw download
      f <- output$download_user_trends_raw
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)
      expect_true("named_users" %in% names(csv))

      # DOW chart download
      f <- output$download_user_dow_chart
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)
      expect_true("day_of_week" %in% names(csv))

      # DOW raw download
      f <- output$download_user_dow_raw
      expect_true(file.exists(f))

      # Filename convention
      f <- output$download_user_trends_chart
      fn <- basename(f)
      expect_true(grepl(
        "^chronicle_connect_user_trends_chart_",
        fn
      ))
      expect_true(grepl("\\.csv$", fn))
    }
  )
})

test_that("Connect: users list download works", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  env <- source_app_env("connect", base_path)

  user_list <- chronicle_data(
    "connect/user_list", base_path
  ) |> dplyr::collect()

  shiny::testServer(
    wrap_server(
      env$users_list_server,
      shiny::reactive(user_list)
    ),
    {
      session$setInputs(
        users_list_environment = "All",
        users_list_role = "All"
      )

      f <- output$download_users_list
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)
      expect_true("username" %in% names(csv))
      expect_true("user_role" %in% names(csv))
    }
  )
})

test_that("Connect: content downloads work", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  env <- source_app_env("connect", base_path)

  content_totals <- chronicle_data(
    "connect/content_totals", base_path
  ) |> dplyr::collect()
  date_range <- c(
    min(content_totals$date),
    max(content_totals$date)
  )

  shiny::testServer(
    wrap_server(
      env$content_overview_server,
      shiny::reactive(content_totals)
    ),
    {
      session$setInputs(
        content_overview_date_range = date_range,
        content_overview_environment = "All",
        content_overview_type = "All"
      )

      f <- output$download_content_trends_chart
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)

      f <- output$download_content_trends_raw
      expect_true(file.exists(f))

      f <- output$download_content_type_chart
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)
      expect_true("content_type" %in% names(csv))

      f <- output$download_content_type_raw
      expect_true(file.exists(f))
    }
  )
})

test_that("Connect: usage overview downloads work", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  env <- source_app_env("connect", base_path)

  visits <- chronicle_data(
    "connect/content_visits_totals_by_user",
    base_path
  ) |> dplyr::collect()
  date_range <- c(
    min(visits$date), max(visits$date)
  )

  shiny::testServer(
    wrap_server(
      env$usage_overview_server,
      shiny::reactive(visits)
    ),
    {
      session$setInputs(
        usage_overview_date_range = date_range,
        usage_overview_environment = "All"
      )

      f <- output$download_usage_visits_chart
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)

      f <- output$download_usage_visits_raw
      expect_true(file.exists(f))

      f <- output$download_usage_unique_chart
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)

      f <- output$download_usage_unique_raw
      expect_true(file.exists(f))
    }
  )
})

test_that("Connect: shiny apps downloads work", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  env <- source_app_env("connect", base_path)

  shiny_usage <- chronicle_data(
    "connect/shiny_usage_totals_by_user",
    base_path
  ) |> dplyr::collect()
  content_list <- chronicle_data(
    "connect/content_list", base_path
  ) |> dplyr::collect()
  date_range <- c(
    min(shiny_usage$date), max(shiny_usage$date)
  )

  shiny::testServer(
    wrap_server(
      env$shiny_apps_server,
      shiny::reactive(shiny_usage),
      shiny::reactive(content_list)
    ),
    {
      session$setInputs(
        shiny_apps_date_range = date_range,
        shiny_apps_environment = "All"
      )

      f <- output$download_shiny_sessions_chart
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)

      f <- output$download_shiny_sessions_raw
      expect_true(file.exists(f))

      f <- output$download_shiny_apps
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)
    }
  )
})

test_that("Connect: content by user download works", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  env <- source_app_env("connect", base_path)

  visits <- chronicle_data(
    "connect/content_visits_totals_by_user",
    base_path
  ) |> dplyr::collect()
  content_list <- chronicle_data(
    "connect/content_list", base_path
  ) |> dplyr::collect()
  user_list <- chronicle_data(
    "connect/user_list", base_path
  ) |> dplyr::collect()
  date_range <- c(
    min(visits$date), max(visits$date)
  )

  shiny::testServer(
    wrap_server(
      env$content_by_user_server,
      shiny::reactive(visits),
      shiny::reactive(content_list),
      shiny::reactive(user_list)
    ),
    {
      session$setInputs(
        content_by_user_date_range = date_range,
        content_by_user_environment = "All"
      )

      f <- output$download_content_by_user
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)
    }
  )
})

test_that("Connect: shiny sessions by user download works", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  env <- source_app_env("connect", base_path)

  shiny_usage <- chronicle_data(
    "connect/shiny_usage_totals_by_user",
    base_path
  ) |> dplyr::collect()
  content_list <- chronicle_data(
    "connect/content_list", base_path
  ) |> dplyr::collect()
  user_list <- chronicle_data(
    "connect/user_list", base_path
  ) |> dplyr::collect()
  date_range <- c(
    min(shiny_usage$date), max(shiny_usage$date)
  )

  shiny::testServer(
    wrap_server(
      env$shiny_sessions_by_user_server,
      shiny::reactive(shiny_usage),
      shiny::reactive(content_list),
      shiny::reactive(user_list)
    ),
    {
      session$setInputs(
        shiny_sessions_user_date_range = date_range,
        shiny_sessions_user_environment = "All"
      )

      f <- output$download_shiny_sessions_by_user
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)
    }
  )
})

test_that("Connect: content list download works", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  env <- source_app_env("connect", base_path)

  user_list <- chronicle_data(
    "connect/user_list", base_path
  ) |> dplyr::collect()
  content_list <- chronicle_data(
    "connect/content_list", base_path
  ) |> dplyr::collect()

  shiny::testServer(
    wrap_server(
      env$content_list_server,
      shiny::reactive(user_list),
      shiny::reactive(content_list)
    ),
    {
      session$setInputs(
        content_list_environment = "All",
        content_list_type = "All",
        content_list_owner = "All"
      )

      f <- output$download_content_list
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)
    }
  )
})

# ============================================================
# Tests for Connect download handlers with empty data
# ============================================================

test_that("Connect: downloads handle NULL data gracefully", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  env <- source_app_env("connect", base_path)

  shiny::testServer(
    wrap_server(
      env$users_overview_server,
      shiny::reactive(NULL)
    ),
    {
      # Chart download with NULL data should produce CSV
      f <- output$download_user_trends_chart
      expect_true(file.exists(f))
      csv <- read_csv_safe(f)
      expect_equal(nrow(csv), 0)

      f <- output$download_user_dow_chart
      expect_true(file.exists(f))
      csv <- read_csv_safe(f)
      expect_equal(nrow(csv), 0)
    }
  )
})

test_that("Connect: users list handles NULL data gracefully", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  env <- source_app_env("connect", base_path)

  shiny::testServer(
    wrap_server(
      env$users_list_server,
      shiny::reactive(NULL)
    ),
    {
      session$setInputs(
        users_list_environment = "All",
        users_list_role = "All"
      )

      f <- output$download_users_list
      expect_true(file.exists(f))
      csv <- read_csv_safe(f)
      expect_equal(nrow(csv), 0)
    }
  )
})

# ============================================================
# Tests for Workbench app download handlers
# ============================================================

test_that("Workbench: user trends downloads work", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  env <- source_app_env("workbench", base_path)

  raw_data <- chronicle_data(
    "workbench/user_totals", base_path
  ) |> dplyr::collect()
  date_range <- c(
    min(raw_data$date), max(raw_data$date)
  )

  # Workbench users_overview_server takes only
  # (input, output, session) but reads data via base_path
  # from its closure. We wrap to ensure the correct
  # base_path is in scope.
  wb_server <- function(input, output, session) {
    env$users_overview_server(input, output, session)
  }

  shiny::testServer(
    wb_server,
    {
      session$setInputs(
        users_overview_date_range = date_range
      )

      # Chart download
      f <- output$download_user_trends_chart
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)
      expect_true("metric" %in% names(csv))

      # Raw download
      f <- output$download_user_trends_raw
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)
      expect_true("named_users" %in% names(csv))

      # DOW chart download
      f <- output$download_user_dow_chart
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)
      expect_true("day_of_week" %in% names(csv))

      # DOW raw download
      f <- output$download_user_dow_raw
      expect_true(file.exists(f))

      # Filename convention
      f <- output$download_user_trends_chart
      fn <- basename(f)
      expect_true(grepl(
        "^chronicle_workbench_user_trends_chart_",
        fn
      ))
      expect_true(grepl("\\.csv$", fn))
    }
  )
})

test_that("Workbench: user list download works", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  env <- source_app_env("workbench", base_path)

  should_load <- shiny::reactive(TRUE)
  wb_list_server <- function(input, output, session) {
    env$user_list_server(
      input, output, session, should_load
    )
  }

  shiny::testServer(
    wb_list_server,
    {
      session$setInputs(
        user_list_environment = "All",
        user_list_role = "All",
        user_list_search = ""
      )

      f <- output$download_user_list
      expect_true(file.exists(f))
      csv <- utils::read.csv(f)
      expect_true(nrow(csv) > 0)
      expect_true("username" %in% names(csv))
      expect_true("user_role" %in% names(csv))
      expect_true("environment" %in% names(csv))
      expect_true("last_active_at" %in% names(csv))
    }
  )
})

# ============================================================
# Tests for Workbench download handlers with empty data
# ============================================================

test_that("Workbench: downloads handle empty data gracefully", {
  base_path <- create_sample_chronicle_data()
  on.exit(unlink(base_path, recursive = TRUE))
  env <- source_app_env("workbench", base_path)

  wb_server <- function(input, output, session) {
    env$users_overview_server(input, output, session)
  }

  shiny::testServer(
    wb_server,
    {
      # Set a date range that yields no data
      session$setInputs(
        users_overview_date_range = c(
          as.Date("1990-01-01"),
          as.Date("1990-01-02")
        )
      )

      f <- output$download_user_trends_chart
      expect_true(file.exists(f))
      csv <- read_csv_safe(f)
      expect_equal(nrow(csv), 0)

      f <- output$download_user_dow_chart
      expect_true(file.exists(f))
      csv <- read_csv_safe(f)
      expect_equal(nrow(csv), 0)
    }
  )
})
