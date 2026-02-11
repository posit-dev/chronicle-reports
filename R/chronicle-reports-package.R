#' @keywords internal
#' @importFrom stats aggregate runif
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  if (!("package:dplyr" %in% search())) {
    do.call("library", list("dplyr"))
  }
}
