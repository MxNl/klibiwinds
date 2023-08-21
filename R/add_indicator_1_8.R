#' add_indicator_1_8
#'
#' @param x tibble containing gwl level head time series
#' @param y tibble containing the summary table
#'
#' @return
#' @export
#'
#' @examples
add_indicator_1_8 <- function(x, y) {
  indicator_18 <- y |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period
    ) |>
    dplyr::summarise(
      indicator_18 = max(gwl, na.rm = TRUE) - min(gwl, na.rm = TRUE),
      .groups = "drop"
    )

  x |>
    dplyr::left_join(
      indicator_18,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}
