#' Title
#'
#' @param x
#'
#' @return
#' @export
#'

#' @export
use_water_year <- function(x) {
  x |>
    dplyr::mutate(date = lubridate::add_with_rollback(date, months(2)))
}
