#' add_indicators_klibiw
#'
#' @param x
#' @param y
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_indicators_klibiw <- function(x, y, ...) {
  x |>
    add_indicators_general(y) |>
    add_indicators_extremes(y, ...) |>
    add_indicators_seasonality(y)
}
