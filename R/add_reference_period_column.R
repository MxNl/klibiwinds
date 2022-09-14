add_reference_period_column <- function(x, reference_periods = reference_periods) {
  max_year_historical <- reference_periods %>%
    filter(reference_period == "Z1") %>%
    filter(year == max(year)) %>%
    pull(year)

  x |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::left_join(reference_periods, by = "year") |>
    dplyr::select(-year) |>
    dplyr::arrange(well_id, climate_model_name, date) |>
    dplyr::relocate(well_id, climate_model_name, date, reference_period) %>%
    dplyr::filter(!(lubridate::year(date) > max_year_historical &
      climate_model_name == "historical")) %>%
    tidyr::drop_na(reference_period)
}
