USAGE_EXPORT_SCHEMA_VERSION <- 1L

#' Export usage data to a portable JSON file
#'
#' Writes the monthly tables from [chronicle_usage_data()] to a small JSON
#' file. The file holds only aggregate counts, keyed by month and `product_id`
#' and, where the report groups them, `app_mode` and IDE. It contains no user,
#' content, host or environment identifiers, so it can be shared without
#' exposing anything beyond the product id.
#'
#' The file has everything needed to rebuild the usage report. Read it back
#' with [chronicle_usage_from_json()].
#'
#' @param tables The list returned by [chronicle_usage_data()].
#' @param path File to write.
#'
#' @return `path`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' tables <- chronicle_usage_data("/var/lib/posit-chronicle/data")
#' chronicle_usage_to_json(tables, "chronicle-usage.json")
#' }
chronicle_usage_to_json <- function(tables, path) {
  window <- attr(tables, "window")
  rows <- lapply(tables[USAGE_TABLE_NAMES], function(tbl) {
    tbl$month <- format(tbl$month, "%Y-%m")
    as.data.frame(tbl)
  })

  export <- list(
    schema_version = USAGE_EXPORT_SCHEMA_VERSION,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    # The date the last month is counted up to, so a later reader can tell
    # whether that month was still in progress
    as_of = format(if (is.null(attr(tables, "as_of"))) Sys.Date() else attr(tables, "as_of"), "%Y-%m-%d"),
    first_month = if (!is.null(window)) format(window[1], "%Y-%m"),
    last_month = if (!is.null(window)) format(window[2], "%Y-%m"),
    tables = rows
  )

  jsonlite::write_json(
    export,
    path,
    auto_unbox = TRUE,
    pretty = TRUE,
    dataframe = "rows",
    null = "null"
  )
  invisible(path)
}

#' Read usage data from a JSON export
#'
#' Reads a file written by [chronicle_usage_to_json()] and returns the same
#' list of tables as [chronicle_usage_data()], so the usage report can be
#' rebuilt from the file alone.
#'
#' @param path Path to the JSON file.
#'
#' @return A list of tables, as returned by [chronicle_usage_data()], with the
#'   `"as_of"` attribute restored from the file.
#' @export
#'
#' @examples
#' \dontrun{
#' tables <- chronicle_usage_from_json("chronicle-usage.json")
#' }
chronicle_usage_from_json <- function(path) {
  parsed <- jsonlite::fromJSON(path, simplifyVector = TRUE)

  if (!identical(as.integer(parsed$schema_version), USAGE_EXPORT_SCHEMA_VERSION)) {
    stop(
      "Unsupported usage export schema version: ",
      format(parsed$schema_version),
      ". Expected ",
      USAGE_EXPORT_SCHEMA_VERSION,
      ".",
      call. = FALSE
    )
  }

  window <- if (!is.null(parsed$first_month) && !is.null(parsed$last_month)) {
    as.Date(paste0(c(parsed$first_month, parsed$last_month), "-01"))
  }
  tables <- usage_empty_tables(window)
  as_of <- if (is.null(parsed$as_of)) substr(parsed$generated_at, 1, 10) else parsed$as_of
  if (length(as_of) == 1 && !is.na(as_of) && nzchar(as_of)) {
    attr(tables, "as_of") <- as.Date(as_of)
  }

  for (name in USAGE_TABLE_NAMES) {
    rows <- parsed$tables[[name]]
    if (!is.data.frame(rows) || nrow(rows) == 0) {
      next
    }
    template <- tables[[name]]
    rows$month <- as.Date(paste0(rows$month, "-01"))
    for (col in setdiff(names(template), "month")) {
      coerce <- switch(
        class(template[[col]])[1],
        numeric = as.numeric,
        logical = as.logical,
        as.character
      )
      rows[[col]] <- coerce(rows[[col]])
    }
    tables[[name]] <- dplyr::as_tibble(rows[names(template)])
  }
  tables
}
