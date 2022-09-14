read_csv_and_add_path_as_column <- function (filepath) {
  filepath |>
    data.table::fread(sep = ";") |>
    dplyr::mutate(identifier = filepath) |>
    tidyr::as_tibble()
}