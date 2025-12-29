# Sample data generation for Chronicle Reports

# Package-level environment to cache sample data path
.chronicle_sample_env <- new.env(parent = emptyenv())

#' Get path to sample Chronicle data
#'
#' This function creates a minimal sample Chronicle data directory for
#' exploring package functionality without needing access to real Chronicle
#' data. The sample data is created once per R session and cached.
#'
#' The sample data includes:
#' \itemize{
#'   \item Connect user totals (3 days of data)
#'   \item Connect user list (26 sample users)
#'   \item Workbench user totals (3 days of data)
#'   \item Workbench user list (21 sample users)
#'   \item Raw Connect users data (3 days)
#' }
#'
#' @param refresh Logical. If TRUE, regenerate sample data even if cached.
#'   Default is FALSE.
#'
#' @return Character path to the sample Chronicle data directory
#'
#' @export
#'
#' @examples
#' # Get path to sample data
#' sample_path <- chronicle_sample_data()
#'
#' # List available metrics
#' chronicle_list_data(sample_path)
#'
#' # Load sample data
#' data <- chronicle_data("connect/user_totals", sample_path)
#' dplyr::collect(data)
#'
#' # Clean up (optional - automatically cleaned at session end)
#' unlink(sample_path, recursive = TRUE)
chronicle_sample_data <- function(refresh = FALSE) {
  # Check if we have a cached path and it still exists
  if (!refresh && exists("sample_path", envir = .chronicle_sample_env)) {
    cached_path <- get("sample_path", envir = .chronicle_sample_env)
    if (dir.exists(cached_path)) {
      return(cached_path)
    }
  }

  # Create new sample data directory
  sample_dir <- file.path(tempdir(), "chronicle-sample-data")

  # Remove if it exists (in case of refresh)
  if (dir.exists(sample_dir)) {
    unlink(sample_dir, recursive = TRUE)
  }

  # Create the sample data
  create_sample_chronicle_data_internal(sample_dir)

  # Cache the path
  assign("sample_path", sample_dir, envir = .chronicle_sample_env)

  # Register cleanup on exit
  reg.finalizer(
    .chronicle_sample_env,
    function(e) {
      if (exists("sample_path", envir = e)) {
        path <- get("sample_path", envir = e)
        if (dir.exists(path)) {
          unlink(path, recursive = TRUE)
        }
      }
    },
    onexit = TRUE
  )

  sample_dir
}

# Internal helper functions for creating sample data
# These are not exported but are used by chronicle_sample_data()

#' @noRd
sample_connect_user_totals_internal <- function() {
  data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    named_users = c(25L, 25L, 26L),
    active_users_1day = c(12L, 15L, 10L),
    publishers = c(5L, 5L, 6L),
    stringsAsFactors = FALSE
  )
}

#' @noRd
sample_connect_user_list_internal <- function() {
  data.frame(
    date = as.Date(rep("2024-01-03", 26)),
    username = paste0("user", sprintf("%02d", 1:26)),
    id = paste0(
      sprintf("user-guid-%03d", 1:26)
    ),
    email = paste0("user", 1:26, "@example.com"),
    first_name = paste0("First", 1:26),
    last_name = paste0("Last", 1:26),
    environment = c(
      rep("Production", 10),
      rep("Development", 8),
      rep("Staging", 3),
      rep(NA_character_, 5)
    ),
    user_role = c(
      rep("viewer", 15),
      rep("publisher", 8),
      rep("administrator", 3)
    ),
    last_active_at = as.POSIXct(
      c(
        rep("2024-01-03 10:00:00", 10),
        rep("2024-01-02 14:00:00", 8),
        rep("2024-01-01 09:00:00", 5),
        rep("2023-12-15 08:00:00", 3)
      ),
      tz = "UTC"
    ),
    active_today = c(
      rep(TRUE, 10),
      rep(FALSE, 16)
    ),
    stringsAsFactors = FALSE
  )
}

#' @noRd
sample_connect_content_list_internal <- function() {
  data.frame(
    date = as.Date(rep("2024-01-03", 3)),
    environment = c("Production", "Development", "Staging"),
    id = c("content-001", "content-002", "content-003"),
    name = c("sales-dashboard", "usage-report", "dev-app"),
    title = c("Sales Dashboard", "Usage Report", "Development App"),
    created_time = as.POSIXct(
      c(
        "2023-11-01 12:00:00",
        "2023-11-15 09:30:00",
        "2023-12-01 14:45:00"
      ),
      tz = "UTC"
    ),
    last_deployed_time = as.POSIXct(
      c(
        "2023-12-20 16:00:00",
        "2023-12-18 10:15:00",
        "2023-12-22 08:30:00"
      ),
      tz = "UTC"
    ),
    type = c("shiny", "rmarkdown", "quarto"),
    description = c(
      "Production sales dashboard",
      "Usage summary report",
      "Development example application"
    ),
    access_type = c("all", "all", "restricted"),
    locked = c(FALSE, FALSE, TRUE),
    locked_message = c("", "", "Locked for maintenance"),
    connection_timeout = as.integer(c(60, 60, 120)),
    read_timeout = as.integer(c(3600, 3600, 3600)),
    init_timeout = as.integer(c(300, 300, 300)),
    idle_timeout = as.integer(c(900, 900, 900)),
    max_processes = as.integer(c(3, 2, 2)),
    min_processes = as.integer(c(1, 1, 1)),
    max_conns_per_process = as.integer(c(20, 20, 10)),
    load_factor = c(1.0, 1.0, 0.5),
    cpu_request = c(0.5, 0.5, 0.25),
    cpu_limit = c(1.0, 1.0, 0.5),
    memory_request = c(512, 512, 256),
    memory_limit = c(1024, 1024, 512),
    amd_gpu_limit = as.integer(c(0, 0, 0)),
    nvidia_gpu_limit = as.integer(c(0, 0, 0)),
    bundle_id = c("bundle-001", "bundle-002", "bundle-003"),
    content_category = c("dashboard", "report", "application"),
    parameterized = c(FALSE, TRUE, FALSE),
    cluster_name = c(NA_character_, NA_character_, "on-prem-cluster"),
    image_name = c(NA_character_, NA_character_, "rstudio/shiny"),
    default_image_name = c(
      NA_character_,
      NA_character_,
      "rstudio/shiny-default"
    ),
    default_r_environment_management = c(TRUE, TRUE, TRUE),
    default_py_environment_management = c(FALSE, FALSE, TRUE),
    service_account_name = c(
      NA_character_,
      NA_character_,
      "service-account-dev"
    ),
    r_version = c("4.3.0", "4.3.0", NA_character_),
    r_environment_management = c(TRUE, TRUE, NA),
    py_version = c(NA_character_, NA_character_, "3.11"),
    py_environment_management = c(NA, NA, TRUE),
    quarto_version = c(NA_character_, "1.3.0", NA_character_),
    run_as = c("rstudio-connect", "rstudio-connect", "service-account"),
    run_as_current_user = c(FALSE, FALSE, FALSE),
    owner_guid = c("owner-001", "owner-002", "owner-003"),
    content_url = c("/content/1", "/content/2", "/content/3"),
    dashboard_url = c("/dashboard/1", "/dashboard/2", "/dashboard/3"),
    app_role = c("viewer", "viewer", "editor"),
    vanity_url = c("/sales", NA_character_, NA_character_),
    tags = I(list(
      c("sales", "dashboard"),
      c("usage", "report"),
      c("development", "example")
    )),
    extension = c(FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

#' @noRd
sample_connect_content_totals_internal <- function() {
  data.frame(
    date = as.Date(
      c(
        "2024-01-01",
        "2024-01-01",
        "2024-01-02",
        "2024-01-03"
      )
    ),
    count = as.integer(c(10, 5, 12, 7)),
    type = c("shiny", "rmarkdown", "shiny", "quarto"),
    environment = c("Production", "Production", "Development", "Staging"),
    stringsAsFactors = FALSE
  )
}

#' @noRd
sample_connect_content_visits_totals_by_user_internal <- function() {
  data.frame(
    date = as.Date(rep("2024-01-03", 4)),
    environment = c("Production", "Production", "Development", "Staging"),
    content_guid = c(
      "content-001",
      "content-001",
      "content-002",
      "content-003"
    ),
    user_guid = c(
      "user-guid-001",
      "user-guid-002",
      "user-guid-003",
      "user-guid-004"
    ),
    visits = as.integer(c(5, 3, 4, 2)),
    path = c("/", "/detail", "/usage", NA_character_),
    stringsAsFactors = FALSE
  )
}

#' @noRd
sample_connect_shiny_usage_totals_by_user_internal <- function() {
  data.frame(
    date = as.Date(rep("2024-01-03", 4)),
    environment = c("Production", "Production", "Development", "Staging"),
    content_guid = c(
      "content-001",
      "content-001",
      "content-002",
      "content-003"
    ),
    user_guid = c(
      "user-guid-001",
      "user-guid-002",
      "user-guid-003",
      "user-guid-004"
    ),
    num_sessions = as.integer(c(3, 2, 4, 1)),
    duration = as.integer(c(3600, 2400, 5400, 1800)),
    stringsAsFactors = FALSE
  )
}

#' @noRd
sample_workbench_user_totals_internal <- function() {
  data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    named_users = c(20L, 20L, 21L),
    active_users_1day = c(8L, 10L, 7L),
    administrators = c(3L, 3L, 4L),
    super_administrators = c(1L, 1L, 1L),
    stringsAsFactors = FALSE
  )
}

#' @noRd
sample_workbench_user_list_internal <- function() {
  data.frame(
    date = as.Date(rep("2024-01-03", 21)),
    username = paste0("wbuser", sprintf("%02d", 1:21)),
    user_role = c(
      rep("user", 16),
      rep("admin", 4),
      "super_admin"
    ),
    environment = c(
      rep("Production", 8),
      rep("Development", 6),
      rep("Staging", 4),
      rep(NA_character_, 3)
    ),
    last_active_at = as.POSIXct(
      c(
        rep("2024-01-03 11:00:00", 7),
        rep("2024-01-02 15:00:00", 8),
        rep("2024-01-01 10:00:00", 4),
        rep("2023-12-20 09:00:00", 2)
      ),
      tz = "UTC"
    ),
    active_today = c(
      rep(TRUE, 7),
      rep(FALSE, 14)
    ),
    stringsAsFactors = FALSE
  )
}

#' @noRd
sample_raw_connect_users_internal <- function() {
  dates <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))

  users <- lapply(dates, function(d) {
    data.frame(
      date = d,
      timestamp = as.POSIXct(paste(d, "08:00:00"), tz = "UTC"),
      email = paste0("user", 1:15, "@example.com"),
      created_at = as.POSIXct("2023-06-01 10:00:00", tz = "UTC"),
      last_active_at = as.POSIXct(paste(d, "07:00:00"), tz = "UTC"),
      locked = rep(FALSE, 15),
      user_role = c(
        rep("viewer", 10),
        rep("publisher", 4),
        "administrator"
      ),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, users)
}

#' @noRd
write_sample_parquet_internal <- function(
  data,
  base_path,
  metric,
  partition_col = "date"
) {
  metric_path <- file.path(base_path, "curated", "v2", metric)

  arrow::write_dataset(
    data,
    path = metric_path,
    format = "parquet",
    partitioning = partition_col,
    hive_style = TRUE
  )
}

#' @noRd
write_raw_parquet_internal <- function(
  data,
  base_path,
  metric,
  frequency = "daily"
) {
  dates <- unique(data$date)

  for (d in dates) {
    date_obj <- as.Date(d, origin = "1970-01-01")
    year <- format(date_obj, "%Y")
    month <- format(date_obj, "%m")
    day <- format(date_obj, "%d")

    subset_data <- data[data$date == d, ]

    metric_path <- file.path(
      base_path,
      frequency,
      "v2",
      metric,
      year,
      month,
      day
    )

    dir.create(metric_path, recursive = TRUE, showWarnings = FALSE)

    arrow::write_parquet(
      subset_data,
      file.path(metric_path, "data.parquet")
    )
  }
}

#' @noRd
create_sample_chronicle_data_internal <- function(base_path) {
  dir.create(base_path, recursive = TRUE, showWarnings = FALSE)

  # Create curated data
  write_sample_parquet_internal(
    sample_connect_user_totals_internal(),
    base_path,
    "connect/user_totals"
  )

  write_sample_parquet_internal(
    sample_connect_user_list_internal(),
    base_path,
    "connect/user_list"
  )

  write_sample_parquet_internal(
    sample_connect_content_totals_internal(),
    base_path,
    "connect/content_totals"
  )

  write_sample_parquet_internal(
    sample_connect_content_list_internal(),
    base_path,
    "connect/content_list"
  )

  write_sample_parquet_internal(
    sample_connect_content_visits_totals_by_user_internal(),
    base_path,
    "connect/content_visits_totals_by_user"
  )

  write_sample_parquet_internal(
    sample_connect_shiny_usage_totals_by_user_internal(),
    base_path,
    "connect/shiny_usage_totals_by_user"
  )

  write_sample_parquet_internal(
    sample_workbench_user_totals_internal(),
    base_path,
    "workbench/user_totals"
  )

  write_sample_parquet_internal(
    sample_workbench_user_list_internal(),
    base_path,
    "workbench/user_list"
  )

  # Create raw data
  write_raw_parquet_internal(
    sample_raw_connect_users_internal(),
    base_path,
    "connect_users",
    frequency = "daily"
  )

  base_path
}
