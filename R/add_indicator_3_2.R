add_indicator_3_2 <- function(x, y) {
  indicator_32 <- y |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period,
      year = year(date)
    ) |>
    dplyr::filter(
      gwl == max(gwl, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      month_num = month(date),
      month_norm = BBmisc::normalize(
        month_num,
        method = "range",
        range = c(0, 1 - 1 / 12)
      ),
      month_sin = sin(month_norm)
    ) |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period
    ) |>
    dplyr::summarise(
      month_mean = mean(month_sin),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      indicator_32 = month_mean |>
        asin() |>
        BBmisc::normalize(method = "range", range = c(1, 12))
    ) |>
    dplyr::select(-month_mean)

  x |>
    dplyr::left_join(
      indicator_32,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}