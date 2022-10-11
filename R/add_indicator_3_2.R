add_indicator_3_2 <- function(x, y) {
  indicator_32 <- y |>
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
      month_water = date_to_water_month(date)
    ) |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period
    ) |>
    summarise(
      mean = as.numeric(psych::circadian.mean(month_water * 2) / 2),
      .groups = "drop"
    )

  x |>
    dplyr::left_join(
      indicator_32,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}