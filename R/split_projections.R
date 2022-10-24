#' split_projections
#'
#' @param x text...
#'
#' @return
#' @export
#'

split_projections <- function(x) {
  x |>
    dplyr::filter(climate_model_name != "observed")
}
