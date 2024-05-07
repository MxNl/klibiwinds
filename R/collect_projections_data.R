collect_projections_data <- function(filepath) {
  files <- filepath |>
    list.files(
      pattern = "ensemble_mean_values_CNN",
      recursive = TRUE,
      full.names = TRUE
    )

  purrr::map_dfr(
    cli::cli_progress_along(
      files,
      name = "collecting projected data (2/3): ",
      clear = FALSE
    ),
    ~read_csv_and_add_path_as_column(files[[.x]])) |>
    dplyr::mutate(
      identifier = stringr::word(identifier, sep = "/", start = -1),
      well_id = stringr::word(identifier, sep = "_", start = 5, end = 5),
      climate_model_name = stringr::word(identifier, sep = "_", start = 6, end = -1),
      climate_model_name = stringr::str_remove_all(climate_model_name, ".txt"),
    ) |>
    dplyr::rename_all(stringr::str_to_lower) |>
    dplyr::rename(
      date = dates,
      gwl = sim
    ) |>
    dplyr::relocate(well_id, date, gwl) |>
    dplyr::select(-identifier, -sim_error) |>
    dplyr::mutate(date = lubridate::as_date(date))
}
