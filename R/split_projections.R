#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
split_projections <- function(x) {
  x |>
    dplyr::filter(climate_model_name != "observed")
}
