#' add_indicators_gwdynindices_shape_scale
#'
#' @param x
#' @param y
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_indicators_gwdynindices_shape_scale <- function(x, y, ...) {
  x |>
    add_indicator_6_1_3_1(y, ...) |>
    add_indicator_6_1_3_2(y, ...)
}
