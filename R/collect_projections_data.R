collect_projections_data <- function (filepath) {
  files <- filepath %>%
  list.files(
    pattern = "ensemble_mean_values_CNN",
    recursive = TRUE,
    full.names = TRUE
  )

files %>%
  purrr::map_dfr(read_csv_and_add_path_as_column) %>%
  as_tibble() %>%
  mutate(
    identifier = word(identifier, sep = "/", start = -1),
    well_id = word(identifier, sep = "_", start = 5, end = 5),
    climate_model_name = word(identifier, sep = "_", start = 6, end = -1),
    climate_model_name = str_remove_all(climate_model_name, ".txt")
  ) %>%
  rename_all(stringr::str_to_lower) %>%
  rename(
    date = dates,
    gwl_projections = sim
  ) %>%
  relocate(well_id, date, gwl_projections) %>%
  select(-identifier, -sim_error)
}