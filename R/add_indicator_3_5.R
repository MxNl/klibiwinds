#' add_indicator_3_5
#'
#' @param x tibble containing gwl level head time series
#' @param y tibble containing the summary table
#'
#' @return
#' @export
#'
#' @examples
add_indicator_3_5 <- function(x, y) {
  indicator_35 <- y |>
    use_calendar_year() |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period,
      year = lubridate::year(date)
    ) |>
    dplyr::filter(
      gwl == max(gwl, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      month_water = lubridate::month(date)
    ) |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period
    ) |>
    dplyr::summarise(
      indicator_35 = as.numeric(psych::circadian.sd(month_water * 2)$sd),
      .groups = "drop"
    )

  x |>
    dplyr::left_join(
      indicator_35,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}
