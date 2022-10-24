#' Title
#'
#' @param x text...
#'
#' @return
#' @export
#'

add_indicator_names <- function(x) {
  x |>
    dplyr::left_join(indicators, by = "indicator") |>
    dplyr::mutate(
      name = dplyr::if_else(
        id == "3.3",
        stringr::str_glue("{name} ({stringr::word(indicator, start = 4, sep = '_')})", .na = "-"),
        name
      ),
    ) |>
    dplyr::rename(indicator_name = name) |>
    dplyr::select(-id, -class)
}
