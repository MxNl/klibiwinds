add_indicator_3_1 <- function(x, y) {
  indicator_31 <- y |>
    use_calendar_year() |>
    dplyr::select(-reference_period) |>
    add_reference_period_column() |>
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
        indicator_31 = as.numeric(psych::circadian.mean(month_water * 2) / 2),
        .groups = "drop"
      )

  x |>
    dplyr::left_join(
      indicator_31,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}