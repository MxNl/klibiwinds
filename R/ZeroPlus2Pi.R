ZeroPlus2Pi <- function(x, int) {
  rSW2_glovars <- new.env()
  rSW2_glovars[["tol"]] + (x - rSW2_glovars[["tol"]]) %% int
}