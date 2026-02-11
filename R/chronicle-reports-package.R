#' @keywords internal
#' @importFrom stats aggregate runif
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# Re-export commonly used dplyr verbs so users don't need library(dplyr)
#' @importFrom dplyr collect filter select mutate group_by summarise arrange
#' @importFrom dplyr slice ungroup pull desc n left_join inner_join
#' @export
dplyr::collect
#' @export
dplyr::filter
#' @export
dplyr::select
#' @export
dplyr::mutate
#' @export
dplyr::group_by
#' @export
dplyr::summarise
#' @export
dplyr::arrange
#' @export
dplyr::slice
#' @export
dplyr::ungroup
#' @export
dplyr::pull
#' @export
dplyr::desc
#' @export
dplyr::n
#' @export
dplyr::left_join
#' @export
dplyr::inner_join
