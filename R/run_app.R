#' Run a Chronicle Reports Shiny App
#'
#' Launch one of the available Chronicle Reports Shiny dashboards. Apps are
#' located in the package's `inst/apps/` directory and can be listed with
#' [list_apps()].
#'
#' @param app_name Name of the app to run. Available apps:
#'   \itemize{
#'     \item `"connect_users"` - Connect Users Dashboard (legacy, raw data)
#'     \item `"workbench_users"` - Workbench Users Dashboard (legacy, raw data)
#'     \item `"connect_user_totals"` - Connect Users Dashboard (curated data)
#'     \item `"connect"` - Comprehensive Connect Dashboard
#'     \item `"workbench"` - Comprehensive Workbench Dashboard
#'   }
#'   Use [list_apps()] to see all available apps.
#' @param base_path The base path where Chronicle data files are stored.
#'   Defaults to the value of the `CHRONICLE_BASE_PATH` environment variable,
#'   or `"/var/lib/posit-chronicle/data"` if the environment variable is not set.
#'   This path should be accessible by the Shiny app.
#'   If you deploy this app to Posit Connect, you can set this environment variable
#'   in the Connect UI. See
#'   <https://docs.posit.co/connect/user/content-settings/#content-vars>
#'   for more information.
#'
#' @return This function does not return a value. It launches the Shiny app.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # List available apps
#'   list_apps()
#'
#'   # Run an app with default data path
#'   run_app("connect_users")
#'
#'   # Run an app with custom filesystem path
#'   run_app("connect_users", "/path/to/chronicle/data")
#'
#'   # Run an app with S3 path
#'   run_app("connect_users", "s3://chronicle-bucket/data")
#' }
run_app <- function(
  app_name,
  base_path = Sys.getenv("CHRONICLE_BASE_PATH", APP_CONFIG$DEFAULT_BASE_PATH)
) {
  # Validate app_name
  if (missing(app_name)) {
    stop(
      "app_name is required. Use list_apps() to see available apps.",
      call. = FALSE
    )
  }

  # Get the app directory
  app_dir <- system.file(
    file.path("apps", app_name),
    package = "chronicle.reports"
  )

  # Check if app exists
  if (app_dir == "" || !dir.exists(app_dir)) {
    available_apps <- list_apps()
    stop(
      "App '",
      app_name,
      "' not found.\n",
      "Available apps: ",
      paste(available_apps, collapse = ", "),
      call. = FALSE
    )
  }

  # Check if app.R exists
  app_file <- file.path(app_dir, "app.R")
  if (!file.exists(app_file)) {
    stop(
      "App '",
      app_name,
      "' exists but does not contain an app.R file.",
      call. = FALSE
    )
  }

  # Set environment variable and run the app
  withr::with_envvar(
    c(CHRONICLE_BASE_PATH = base_path),
    shiny::runApp(app_dir)
  )
}


#' List Available Chronicle Reports Apps
#'
#' Returns a character vector of available Chronicle Reports Shiny dashboards
#' that can be run with [run_app()].
#'
#' @return A character vector of app names.
#'
#' @export
#'
#' @examples
#' # List all available apps
#' list_apps()
list_apps <- function() {
  apps_dir <- system.file("apps", package = "chronicle.reports")

  # If apps directory doesn't exist, return empty vector
  if (apps_dir == "" || !dir.exists(apps_dir)) {
    return(character(0))
  }

  # List subdirectories in apps/
  app_names <- list.dirs(apps_dir, full.names = FALSE, recursive = FALSE)

  # Filter to only those with app.R files
  app_names[sapply(app_names, function(name) {
    app_file <- file.path(apps_dir, name, "app.R")
    file.exists(app_file)
  })]
}
