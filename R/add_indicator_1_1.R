#' add_indicator_1_1
#'
#' @param x tibble containing gwl level head time series
#' @param y tibble containing the summary table
#'
#' @return
#' @export
#'
#' @examples
add_indicator_1_1 <- function(x, y) {
  indicator_11 <- y |>
    dplyr::group_by(well_id, climate_model_name, reference_period) |>
    dplyr::summarise(indicator_11 = min(gwl, na.rm = TRUE), .groups = "drop")

  x |>
    dplyr::left_join(
      indicator_11,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}
