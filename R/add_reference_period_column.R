add_reference_period_column <- function(x) {
  x %>%
    mutate(year = lubridate::year(date)) %>%
    left_join(reference_periods, by = "year") %>%
    select(-year) %>%
    arrange(well_id, climate_model_name, date) %>%
    relocate(well_id, climate_model_name, date, reference_period)
}