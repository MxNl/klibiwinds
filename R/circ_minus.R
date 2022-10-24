#' Calculate the circular subtraction \var{x - y}
#'
#' @param x A numeric vector or array.
#' @param y A numeric vector or array.
#' @param circular
#'

circ_minus <- function(x, y, int, type = c("minusPiPlusPi", "ZeroPlus2Pi")) {
  stopifnot(all(dim(x) == dim(y)))

  type <- match.arg(type)

  if (requireNamespace("circular", quietly = TRUE)) {
    circ <- 2 * pi / int

    d_circ <- circular::circular(
      (x - y) * circ,
      type = "angles",
      units = "radians",
      rotation = "clock",
      modulo = "asis"
    )

    res <- get_circular_type(d_circ, circ, int, type)
  } else {
    res <- rep(NA, length(x))
  }

  if (is.array(x)) {
    array(res, dim = dim(x), dimnames = dimnames(x))
  } else {
    res
  }
}