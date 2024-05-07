#' add_indicator_3_1
#'
#' @param x tibble containing gwl level head time series
#' @param y tibble containing the summary table
#'
#' @return
#' @export
#'
#' @examples
add_indicator_3_1 <- function(x, y) {
  indicator_31 <- y |>
    use_calendar_year() |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period,
      year = lubridate::year(date)
    ) |>
      dplyr::filter(
        gwl == min(gwl, na.rm = TRUE)
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
        indicator_31 = as.numeric(psych::circadian.mean(month_water * 2) / 2),
        .groups = "drop"
      )

  x |>
    dplyr::left_join(
      indicator_31,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}
