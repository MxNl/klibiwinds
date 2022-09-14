add_reference_period_column <- function(x) {
  x |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::left_join(reference_periods, by = "year") |>
    dplyr::select(-year) |>
    dplyr::arrange(well_id, climate_model_name, date) |>
    dplyr::relocate(well_id, climate_model_name, date, reference_period) %>%
    dplyr::filter(!(lubridate::year(date) >= 2021 & climate_model_name == "historical")) %>%
    tidyr::drop_na(reference_period)
}
