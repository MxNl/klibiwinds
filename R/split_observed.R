#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
split_observed <- function(x) {
  x |>
    dplyr::filter(climate_model_name == "observed" & reference_period == "Z1")
}
