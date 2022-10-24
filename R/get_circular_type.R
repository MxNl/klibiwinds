#' get_circular_type
#'
#' @param x text...
#' @param circ text...
#' @param int text...
#' @param type text...
#'
#' @return
get_circular_type <- function(
  x,
  circ,
  int,
  type = c("minusPiPlusPi", "ZeroPlus2Pi")
) {
  type <- match.arg(type)

  if (type == "minusPiPlusPi") {
    x <- circular::minusPiPlusPi(x)
  }

  res <- as.numeric(x / circ)

  if (type == "ZeroPlus2Pi") {
    res <- ZeroPlus2Pi(res, int)
  }

  res
}
