#' add_indicator_1_5
#'
#' @param x tibble containing gwl level head time series
#' @param y tibble containing the summary table
#'
#' @return
#' @export
#'
#' @examples
add_indicator_1_5 <- function(x, y) {
  indicator_15 <- y |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period,
      year = lubridate::year(date)
    ) |>
    dplyr::summarise(
      annual_min = min(gwl, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    dplyr::summarise(
      indicator_15 = mean(annual_min, na.rm = TRUE),
      .groups = "drop"
    )

  x |>
    dplyr::left_join(
      indicator_15,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}
