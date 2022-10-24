#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
use_core_indicators_only <- function (x) {
  x |>
    dplyr::filter(core_indicator == TRUE) |>
    dplyr::select(-core_indicator)
}
