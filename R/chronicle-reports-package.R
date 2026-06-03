#' @keywords internal
#' @importFrom stats aggregate runif
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("chronicle.reports loaded (aws-oauth-test branch)")
}
