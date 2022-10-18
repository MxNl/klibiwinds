use_water_year <- function(x) {
  x |>
    dplyr::mutate(date = lubridate::add_with_rollback(date, lubridate::months(2)))
}