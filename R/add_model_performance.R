add_model_performance <- function(x, path) {
  score_files <- path |>
    list.files(full.names = TRUE, pattern = "log_summary") |>
    purrr::map_dfr(read_txt_and_add_path_as_column)

  scores <- score_files |>
    dplyr::mutate(
      well_id = stringr::word(identifier, start = -1, sep = "_"),
      well_id = stringr::str_remove_all(well_id, ".txt"),
      .before = 1
    ) |>
    dplyr::select(-identifier) |>
    tidyr::pivot_wider() |>
    janitor::clean_names()

  x |>
    dplyr::left_join(scores, by = "well_id")
}