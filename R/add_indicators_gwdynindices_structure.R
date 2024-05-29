#' add_indicators_gwdynindices_structure
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
add_indicators_gwdynindices_structure <- function(x, y) {
  x |>
    add_indicators_gwdynindices_structure_seasonalitytiming(y)
}
