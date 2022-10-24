round_nextsignif <- function(x) {
  if(length(x) != 1) stop("'x' must be of length 1")
  if(abs(x) < 1) {
    x |> signif(1)
  }
  else {
    x |> round()
  }
}
