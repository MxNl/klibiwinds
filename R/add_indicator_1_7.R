#' add_indicator_1_7
#'
#' @param x tibble containing gwl level head time series
#' @param y tibble containing the summary table
#'
#' @return
#' @export
#'
#' @examples
add_indicator_1_7 <- function(x, y) {
  indicator_17 <- y |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period,
      year = lubridate::year(date)
    ) |>
    dplyr::summarise(
      annual_amplitude = max(gwl, na.rm = TRUE) - min(gwl, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    dplyr::summarise(
      indicator_17 = mean(annual_amplitude, na.rm = TRUE),
      .groups = "drop"
    )

  x |>
    dplyr::left_join(
      indicator_17,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}
