#' z_to_yearrange_period_names
#'
#' @param x_long_format text...
#'
#' @return
#' @export
#'

z_to_yearrange_period_names <- function(x_long_format) {
  new_perido_names <- reference_periods |>
    dplyr::group_by(reference_period) |>
    dplyr::filter(year %in% range(year)) |>
    dplyr::mutate(start_end = c("start", "end")) |>
    tidyr::pivot_wider(names_from = start_end, values_from = year) |>
    dplyr::mutate(period = stringr::str_c(start, end, sep = " - ")) |>
    dplyr::pull(period)

  x_long_format |>
    dplyr::mutate(
      period = dplyr::case_when(
        period == 1 ~ new_perido_names[1],
        period == 2 ~ new_perido_names[2],
        period == 3 ~ new_perido_names[3],
      ),
      period = period |>
        forcats::as_factor() |>
        forcats::fct_rev()
    )
}
