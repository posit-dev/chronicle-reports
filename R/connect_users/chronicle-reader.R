chr_path <- function(
  base_path,
  metric = NULL,
  frequency = c("daily", "hourly")
) {
  frequency <- match.arg(frequency)
  glue::glue("{base_path}/{frequency}/v2/{metric}/")
}

chr_get_metric_data <- function(
  metric,
  base_path,
  frequency = c("daily", "hourly"),
  ymd = NULL,
  schema = NULL
) {
  frequency <- match.arg(frequency)
  path <- chr_path(base_path, metric, frequency)

  if (!is.null(ymd)) {
    path <- glue::glue("{path}{ymd[['year']]}/{ymd[['month']]}/{ymd[['day']]}/")
    partitioning <- NULL
  } else {
    partitioning <- c("Year", "Month", "Day")
  }
  arrow::open_dataset(
    path,
    hive_style = FALSE,
    schema = schema,
    format = "parquet",
    partitioning = partitioning
  )
}
