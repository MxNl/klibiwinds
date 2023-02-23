#' labels_add_text_to_limits
#'
#' @param x character vector containing the labels
#'
#' @return
#' @export
labels_add_text_to_limits <- function(x) {
  x |>
    as.character() |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      value = dplyr::if_else(
        dplyr::row_number() == 1,
        paste("<", value),
        value
      ),
      value = dplyr::if_else(
        dplyr::row_number() == dplyr::n(),
        paste(">", value),
        value
      )
    ) |>
    dplyr::pull(value)
}
