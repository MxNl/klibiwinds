#' Add a column with reference period classes
#'
#' @param x A tibble with a date column.
#' @param reference_period A tibble.
#' @return A tibble x with one additional column containing reference period classes.
#' @export
add_reference_period_column <- function(x, reference_period) {
  if (missing(reference_period)) {
    reference_period <- reference_periods
  }

  max_year_historical <- reference_period |>
    dplyr::filter(reference_period == "Z1") |>
    dplyr::filter(year == max(year)) |>
    dplyr::pull(year)

  x |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::left_join(reference_period, by = "year") |>
    dplyr::select(-year) |>
    dplyr::arrange(well_id, climate_model_name, date) |>
    dplyr::relocate(well_id, climate_model_name, date, reference_period) |>
    dplyr::filter(!(lubridate::year(date) > max_year_historical &
      climate_model_name == "observed")) |>
    tidyr::drop_na(reference_period)
}
