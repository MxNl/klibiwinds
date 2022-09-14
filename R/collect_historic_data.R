collect_historic_data <- function(filepath) {
  files <- filepath |>
    list.files(pattern = ".txt", full.names = TRUE)

  purrr::map_dfr(
    cli::cli_progress_along(
      files,
      name = "collecting historical data (1/3): ",
      clear = FALSE
    ),
    ~ data.table::fread(files[[.x]], sep = ";")
  ) |>
    janitor::clean_names() |>
    dplyr::select(mest_id, datum, gw_nn) |>
    dplyr::rename(
      well_id = mest_id,
      date = datum,
      gwl = gw_nn
    ) |>
    dplyr::mutate(
      date = lubridate::dmy_hms(date),
      date = as.Date(date),
      gwl = as.numeric(gwl),
      well_id = as.character(well_id)
    ) |>
    # dplyr::mutate(year = lubridate::year(date), .after = date) |>
    dplyr::group_by(well_id) |>
    dplyr::arrange(date) |>
    dplyr::ungroup() |>
    tidyr::drop_na(gwl) |>
    dplyr::mutate(climate_model_name = "historical") |>
    dplyr::as_tibble()
}
