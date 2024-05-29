#' add_indicators_gwdynindices
#'
#' @param x
#' @param y
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_indicators_gwdynindices <- function(x, y) {
  x |>
    add_indicators_gwdynindices_structure(y) |>
    add_indicators_gwdynindices_shape(y)
}
