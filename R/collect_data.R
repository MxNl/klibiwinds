collect_data <- function(filepath_historic, filepath_projections) {
  data_gwl_historic <- collect_historic_data(filepath_historic)
  data_gwl_projections <- collect_projections_data(filepath_projections)

  well_ids_projections <- data_gwl_projections |>
    dplyr::pull(well_id) |>
    unique()

  data_gwl_historic <- data_gwl_historic |>
    dplyr::group_split(well_id) %>%
    purrr::set_names(purrr::map_chr(., ~ .x$well_id[1])) |>
    magrittr::extract(well_ids_projections) |>
    purrr::reduce(dplyr::bind_rows)

  cat("merge data (3/3)")
  data_gwl_historic |>
    dplyr::mutate(well_id = as.character(well_id)) |>
    dplyr::bind_rows(data_gwl_projections) |>
    dplyr::arrange(well_id, climate_model_name, date)
}
