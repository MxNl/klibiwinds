drop_incomplete_z1_period <- function(x) {
  well_ids_with_complete_z1 <- x |>
    group_by(well_id, climate_model_name, reference_period) |>
    filter(
      min(lubridate::year(date)) <= 1983 &
        reference_period == "Z1" &
        climate_model_name == "observed" &
        n() >= 360 * 0.95
    ) %>%
    pull(well_id) %>%
    unique()

  x %>%
    filter(well_id %in% well_ids_with_complete_z1)
}