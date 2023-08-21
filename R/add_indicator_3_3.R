#' add_indicator_3_3
#'
#' @param x tibble containing gwl level head time series
#' @param y tibble containing the summary table
#'
#' @return
#' @export
#'
#' @examples
add_indicator_3_3 <- function(x, y) {
  indicator_33 <- y |>
    # slice_sample(prop = 0.01) |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period,
      month = lubridate::month(date)
    ) |>
    dplyr::summarise(
      # quantile_df(gwl, probs = c(0.05, 0.15)),
      mean = mean(gwl),
      .groups = "drop"
    ) |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period
    ) |>
    tidyr::nest() |>
    dplyr::ungroup() |>
    dplyr::rename(indicator_33 = data)

  x |>
    dplyr::left_join(
      indicator_33,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}
