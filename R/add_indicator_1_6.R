#' add_indicator_1_6
#'
#' @param x tibble containing gwl level head time series
#' @param y tibble containing the summary table
#'
#' @return
#' @export
#'
#' @examples
add_indicator_1_6 <- function(x, y) {
  indicator_16 <- y |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period,
      year = lubridate::year(date)
    ) |>
    dplyr::summarise(
      annual_max = max(gwl, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    dplyr::summarise(
      indicator_16 = mean(annual_max, na.rm = TRUE),
      .groups = "drop"
    )

  x |>
    dplyr::left_join(
      indicator_16,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}
