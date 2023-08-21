#' add_indicator_1_3
#'
#' @param x tibble containing gwl level head time series
#' @param y tibble containing the summary table
#'
#' @return
#' @export
#'
#' @examples
add_indicator_1_3 <- function(x, y) {
  indicator_13 <- y |>
    dplyr::group_by(well_id, climate_model_name, reference_period) |>
    dplyr::summarise(indicator_13 = mean(gwl, na.rm = TRUE), .groups = "drop") |>
    dplyr::ungroup()

  x |>
    dplyr::left_join(
      indicator_13,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}
