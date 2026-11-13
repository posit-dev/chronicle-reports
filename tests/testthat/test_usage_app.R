# Tests for the usage app: its chart-data helpers and its server.
#
# The server tests load the app from an exported JSON file with a fixed window,
# so they do not depend on the current date.

source_usage_app <- function() {
  env <- new.env(parent = globalenv())
  app_file <- system.file(
    "apps",
    "usage",
    "app.R",
    package = "chronicle.reports"
  )
  withr::with_envvar(
    c(CHRONICLE_BASE_PATH = withr::local_tempdir(), CHRONICLE_USAGE_FILE = ""),
    source(app_file, local = env)
  )
  env
}

# Export the fixture to a file and return its path along with the tables
usage_export_file <- function(frame = parent.frame()) {
  path <- create_usage_fixture()
  withr::defer(unlink(path, recursive = TRUE), envir = frame)
  tables <- chronicle_usage_data(path, today = USAGE_TEST_TODAY)
  file <- withr::local_tempfile(fileext = ".json", .local_envir = frame)
  chronicle_usage_to_json(tables, file)
  list(file = file, tables = tables)
}

test_that("usage_series fills empty months with zero", {
  env <- source_usage_app()
  months <- seq(as.Date("2026-09-01"), as.Date("2026-11-01"), by = "month")
  data <- dplyr::tibble(
    month = as.Date("2026-10-01"),
    product_id = "p1",
    users = 4
  )

  out <- env$usage_series(data, "users", months)

  expect_equal(out$month, months)
  expect_equal(out$value, c(0, 4, 0))
})

test_that("usage_series returns one series per product id", {
  env <- source_usage_app()
  months <- as.Date(c("2026-10-01", "2026-11-01"))
  data <- dplyr::tibble(
    month = as.Date(c("2026-10-01", "2026-11-01", "2026-11-01")),
    product_id = c("p1", "p1", "p2"),
    users = c(4, 6, 9)
  )

  out <- env$usage_series(data, "users", months)

  expect_equal(nrow(out), 4L)
  expect_equal(out$value[out$product_id == "p1"], c(4, 6))
  expect_equal(out$value[out$product_id == "p2"], c(0, 9))
})

test_that("usage_series gives every product id every group", {
  env <- source_usage_app()
  months <- as.Date(c("2026-10-01", "2026-11-01"))
  data <- dplyr::tibble(
    month = as.Date(c("2026-10-01", "2026-11-01", "2026-11-01")),
    product_id = c("p1", "p1", "p2"),
    app_mode = c("shiny", "quarto", "shiny"),
    hits = c(3, 5, 7)
  )

  out <- env$usage_series(data, "hits", months, group = "app_mode")

  # 2 products x 2 groups x 2 months
  expect_equal(nrow(out), 8L)
  p1_shiny <- out[out$product_id == "p1" & out$app_mode == "shiny", ]
  expect_equal(p1_shiny$value, c(3, 0))
  p2_quarto <- out[out$product_id == "p2" & out$app_mode == "quarto", ]
  expect_equal(p2_quarto$value, c(0, 0))
})

test_that("usage_series copes with no rows", {
  env <- source_usage_app()
  months <- as.Date(c("2026-10-01", "2026-11-01"))
  data <- dplyr::tibble(
    month = as.Date(character()),
    product_id = character(),
    users = numeric()
  )

  out <- env$usage_series(data, "users", months)
  expect_false(env$usage_has_data(out))

  grouped <- env$usage_series(
    dplyr::mutate(data, app_mode = character()),
    "users",
    months,
    group = "app_mode"
  )
  expect_false(env$usage_has_data(grouped))
})

test_that("a name keeps its color however the chart is filtered", {
  env <- source_usage_app()

  all <- env$usage_colors(c("shiny", "quarto", "static"))
  some <- env$usage_colors(c("shiny", "static"))

  expect_equal(all[["static"]], env$SERIES_COLORS[3])
  expect_equal(some[["static"]], env$SERIES_COLORS[2])
  # colors depend only on the names in the chart, so the same chart always matches
  expect_equal(env$usage_colors(c("static", "shiny")), some)
})

test_that("stacked charts get taller with more product ids", {
  env <- source_usage_app()
  one <- dplyr::tibble(product_id = "p1")
  four <- dplyr::tibble(product_id = paste0("p", 1:4))

  expect_equal(env$usage_panel_height(one), 340)
  expect_equal(env$usage_panel_height(four), 4 * env$PANEL_HEIGHT)
})

test_that("the y-axis title names the product id, except when there are none", {
  env <- source_usage_app()
  title <- env$usage_panel_y_title

  # no product ids at all: just the metric
  expect_equal(
    title("(unspecified)", "(unspecified)", "Deployments"),
    "Deployments"
  )
  expect_equal(title("(unspecified)", "(unspecified)"), "")

  # real ids: each panel is labelled, including "(unspecified)" next to them
  expect_equal(title("p1", "p1", "Deployments"), "p1<br>Deployments")
  ids <- c("(unspecified)", "p1")
  expect_equal(
    title("(unspecified)", ids, "Deployments"),
    "(unspecified)<br>Deployments"
  )
  expect_equal(title("p1", ids, "Deployments"), "p1<br>Deployments")
})

test_that("a stacked chart without product ids has a plain y-axis title", {
  env <- source_usage_app()
  months <- as.Date(c("2026-10-01", "2026-11-01"))
  data <- dplyr::tibble(
    month = months[1],
    product_id = "(unspecified)",
    app_mode = "shiny",
    deployments = 3
  )
  series <- env$usage_series(data, "deployments", months, group = "app_mode")
  chart <- plotly::plotly_build(env$usage_stacked_chart(
    series,
    months,
    "app_mode",
    "Deployments"
  ))
  title <- chart$x$layout$yaxis$title
  expect_equal(if (is.list(title)) title$text else title, "Deployments")
})

test_that("charts build for several product ids", {
  env <- source_usage_app()
  tables <- usage_export_file()$tables
  months <- env$usage_window_months(tables)

  line <- env$usage_series(
    tables$active_users[tables$active_users$product == "connect", ],
    "users",
    months
  )
  expect_s3_class(env$usage_line_chart(line, months, "Active users"), "plotly")

  stacked <- env$usage_series(
    tables$content_hits,
    "hits",
    months,
    group = "app_mode"
  )
  chart <- plotly::plotly_build(env$usage_stacked_chart(
    stacked,
    months,
    "app_mode",
    "Hits"
  ))
  # one trace per product id and content type
  expect_equal(length(chart$x$data), 2 * 2)
  # only the first panel contributes to the legend
  shown <- vapply(
    chart$x$data,
    function(trace) isTRUE(trace$showlegend),
    logical(1)
  )
  expect_equal(sum(shown), 2L)
})

test_that("the month still in progress is flagged on the axis", {
  env <- source_usage_app()
  months <- as.Date(c("2026-10-01", "2026-11-01"))

  labels <- env$usage_month_labels(months, today = as.Date("2026-11-06"))

  expect_equal(labels, c("Oct 2026", "Nov 2026*"))
})

test_that("the window covers 12 months even when only some have data", {
  env <- source_usage_app()
  tables <- usage_export_file()$tables

  months <- env$usage_window_months(tables)

  expect_length(months, 12L)
  expect_equal(months[1], as.Date("2025-12-01"))
  expect_equal(months[12], as.Date("2026-11-01"))
})

test_that("load_usage_tables reads CHRONICLE_USAGE_FILE when set", {
  env <- source_usage_app()
  exported <- usage_export_file()

  loaded <- withr::with_envvar(
    c(CHRONICLE_USAGE_FILE = exported$file),
    env$load_usage_tables()
  )

  expect_equal(loaded, exported$tables)
})

test_that("the server serves a download that rebuilds the report", {
  env <- source_usage_app()
  exported <- usage_export_file()

  withr::with_envvar(c(CHRONICLE_USAGE_FILE = exported$file), {
    shiny::testServer(env$server, {
      downloaded <- output$download_data
      expect_true(file.exists(downloaded))
      expect_equal(chronicle_usage_from_json(downloaded), exported$tables)
    })
  })
})

test_that("the server renders every chart with several product ids", {
  env <- source_usage_app()
  exported <- usage_export_file()

  withr::with_envvar(c(CHRONICLE_USAGE_FILE = exported$file), {
    shiny::testServer(env$server, {
      session$setInputs(
        hits_group = "authenticated",
        deployments_metric = "deployments"
      )
      for (id in c(
        "connect_active_users",
        "workbench_active_users",
        "content_hits",
        "content_deployments",
        "sessions"
      )) {
        expect_no_error(output[[id]])
        expect_no_error(output[[paste0(id, "_plot")]])
      }

      session$setInputs(
        hits_group = "app_mode",
        deployments_metric = "unique_content"
      )
      expect_no_error(output$content_hits)
      expect_no_error(output$content_deployments)
    })
  })
})

test_that("the chart for two products is sized for two panels", {
  env <- source_usage_app()
  exported <- usage_export_file()

  withr::with_envvar(c(CHRONICLE_USAGE_FILE = exported$file), {
    shiny::testServer(env$server, {
      session$setInputs(hits_group = "authenticated")
      # the fixture has Connect hits for two product ids
      expect_match(as.character(output$content_hits_plot$html), "height:440px")
      # and Workbench sessions for one
      expect_match(as.character(output$sessions_plot$html), "height:340px")
    })
  })
})

test_that("an empty report shows a message, not an error", {
  env <- source_usage_app()
  empty <- withr::local_tempdir()

  withr::with_envvar(
    c(CHRONICLE_BASE_PATH = empty, CHRONICLE_USAGE_FILE = ""),
    {
      shiny::testServer(env$server, {
        session$setInputs(
          hits_group = "authenticated",
          deployments_metric = "deployments"
        )
        expect_error(
          suppressMessages(output$content_hits),
          class = "shiny.silent.error"
        )
        expect_error(
          suppressMessages(output$sessions),
          class = "shiny.silent.error"
        )
      })
    }
  )
})

test_that("a report rebuilt from an export flags the export's month, not today's", {
  env <- source_usage_app()
  tables <- usage_export_file()$tables

  as_of <- env$usage_as_of(tables)

  expect_equal(as_of, attr(tables, "as_of"))
  months <- as.Date(c("2026-10-01", "2026-11-01"))
  expect_equal(
    env$usage_month_labels(months, today = as.Date("2026-10-20")),
    c("Oct 2026*", "Nov 2026")
  )
  expect_equal(env$usage_as_of(list()), Sys.Date())
})

test_that("groups beyond the palette are folded into Other", {
  env <- source_usage_app()
  n <- length(env$SERIES_COLORS) + 3
  months <- as.Date("2026-11-01")
  data <- dplyr::tibble(
    month = months,
    product_id = rep(c("p1", "p2"), each = n),
    app_mode = rep(sprintf("type%02d", seq_len(n)), 2),
    hits = rep(seq_len(n), 2)
  )

  out <- env$usage_series(data, "hits", months, group = "app_mode")

  groups <- unique(out$app_mode)
  expect_length(groups, length(env$SERIES_COLORS))
  expect_true("Other" %in% groups)
  # the largest types survive and the smallest are folded, in every panel
  expect_true(sprintf("type%02d", n) %in% groups)
  expect_false("type01" %in% groups)
  expect_equal(sum(out$value), sum(data$hits))
  expect_false(anyDuplicated(env$usage_colors(groups)) > 0)
})

test_that("groups within the palette are left alone", {
  env <- source_usage_app()
  months <- as.Date("2026-11-01")
  data <- dplyr::tibble(
    month = months,
    product_id = "p1",
    app_mode = c("shiny", "quarto"),
    hits = c(1, 2)
  )

  out <- env$usage_series(data, "hits", months, group = "app_mode")

  expect_setequal(out$app_mode, c("shiny", "quarto"))
})
