#' split_observed
#'
#' @param x text...
#'
#' @return
#' @export
#'

split_observed <- function(x) {
  x |>
    dplyr::filter(climate_model_name == "observed" & reference_period == "Z1")
}
