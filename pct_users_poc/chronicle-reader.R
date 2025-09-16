#' Construct chronicle path.
#'
#' @param base_path chronicle base_path name
#' @param frequency Either daily or hourly
#' @param metric (Optional) name of metric
#'
#' @return A character vector combining base_path, frequency and metric
chr_path <- function(
  base_path,
  metric = NULL,
  frequency = c("daily", "hourly")
) {
  frequency <- match.arg(frequency)
  glue::glue("{base_path}/{frequency}/v2/{metric}/")
}

#' List chronicle objects in a base_path.
#'
#' @inheritParams chr_path
chr_list_objects <- function(
  base_path,
  metric = NULL,
  frequency = c("daily", "hourly")
) {
  frequency <- match.arg(frequency)
  prefix <- glue::glue("{frequency}/v2/{metric}/")

  if (startsWith(base_path, "s3://")) {
    bucket_name <- sub("s3://", "", base_path)
    s3_client <- paws::s3()
    res <- s3_client$list_objects_v2(
      bucket_name,
      Prefix = prefix,
      Delimiter = "/"
    )
  } else {
    # Local filesystem
    full_path <- file.path(base_path, prefix)

    if (dir.exists(full_path)) {
      dirs <- list.dirs(full_path, recursive = FALSE, full.names = FALSE)
      # Create structure similar to S3 CommonPrefixes
      res <- list(
        CommonPrefixes = map(dirs, ~ list(Prefix = paste0(prefix, .x, "/")))
      )
    } else {
      res <- list(CommonPrefixes = list())
    }
  }
  res$CommonPrefixes |> purrr::map_chr("Prefix")
}

#' Get latest window of year, month, and day for a given Chronicle metric.
#'
#' @inheritParams chr_path
#' @inheritParams chr_list_objects
#' @export
chr_latest_window <- function(
  base_path,
  metric,
  frequency = c("daily", "hourly")
) {
  tail_base <- function(x) {
    # extract the basename from the last element in a char vector
    basename(utils::tail(x, 1))
  }
  y <- chr_list_objects(base_path, metric, frequency) |>
    tail_base()
  m <- chr_list_objects(base_path, file.path(metric, y), frequency) |>
    tail_base()
  d <- chr_list_objects(base_path, file.path(metric, y, m), frequency) |>
    tail_base()
  c(year = y, month = m, day = d)
}


#' List Chronicle metrics in a base_path
#'
#' @inheritParams chr_path
#' @importFrom stringr str_detect str_replace str_replace_all
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select case_when
#' @export
chr_list_metrics <- function(base_path, frequency = c("daily", "hourly")) {
  frequency = match.arg(frequency)
  prefix <- glue::glue("{frequency}/v2/")
  s3_client <- paws::s3()
  result <- s3_client$list_objects_v2(
    base_path,
    Prefix = prefix,
    Delimiter = "/"
  )
  m <-
    result$CommonPrefixes |>
    purrr::map(unlist) |>
    purrr::map_chr("Prefix") |>
    stringr::str_replace_all(prefix, "") |>
    stringr::str_replace("/$", "")

  tibble(metric = m, frequency = frequency) |>
    mutate(
      product = case_when(
        str_detect(metric, "chronicle") ~ "chronicle",
        str_detect(metric, "connect") ~ "connect",
        str_detect(metric, "pwb") ~ "workbench",
        .default = "other"
      )
    ) |>
    select(product, metric, frequency = frequency)
}

#' Read Chronicle data for a given metric.
#'
#' @inheritParams chr_path
#' @param metric Name of metric
#' @param ymd A list with components year, month and day
#' @param schema Passed to [arrow::open_dataset]
#' @export
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
