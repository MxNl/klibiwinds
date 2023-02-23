read_txt_and_add_path_as_column <- function(filepath) {
  filepath |>
    readr::read_lines(skip = 3, n_max = 6, locale = readr::locale(encoding = "ISO-8859-1")) |>
    magrittr::extract(c(1, 2)) |>
    tibble::enframe() |>
    dplyr::mutate(
      name = stringr::word(value, end = 1, sep = " "),
      value = stringr::word(value, start = -1, sep = " ") |> as.numeric()
    ) |>
    dplyr::mutate(identifier = filepath) |>
    tidyr::as_tibble()
}
