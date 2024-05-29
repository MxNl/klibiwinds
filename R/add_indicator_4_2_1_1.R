#' add_indicator_4_2_1_1
#'
#' @param x tibble containing gwl level head time series
#' @param y tibble containing the summary table
#'
#' @return
#' @export
#'
#' @examples
add_indicator_4_2_1_1 <- function(x, y, ...) {
  indicator_4211 <- y |>
    tidytable::group_by(well_id, climate_model_name, reference_period) |>
    tidytable::summarise(indicator_4211 = indicator_colwell(date, gwl), .groups = "drop")

  x |>
    tidytable::as_tidytable() |>
    tidytable::left_join(
      indicator_4211,
      by = c("well_id", "climate_model_name", "reference_period")
    ) |>
    dplyr::as_tibble()
}
