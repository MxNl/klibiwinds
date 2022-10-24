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