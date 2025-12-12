# Helper functions for generating sample Chronicle data for tests
#
# This file provides convenient wrappers for tests. The actual data generation
# is handled by internal functions in R/sample-data.R

#' Create a complete sample Chronicle data directory for testing
#'
#' @param base_path Base path where Chronicle data should be created.
#'   If NULL, creates in a temporary directory.
#' @return The base_path
create_sample_chronicle_data <- function(base_path = NULL) {
  if (is.null(base_path)) {
    base_path <- file.path(
      tempdir(),
      paste0("chronicle-test-", as.integer(Sys.time()))
    )
  }

  create_sample_chronicle_data_internal(base_path)
  base_path
}
