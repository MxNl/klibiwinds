#' use_core_indicators_only
#'
#' @param x text...
#'
#' @return
#' @export
#'

use_core_indicators_only <- function (x) {
  x |>
    dplyr::filter(core_indicator == TRUE) |>
    dplyr::select(-core_indicator)
}
