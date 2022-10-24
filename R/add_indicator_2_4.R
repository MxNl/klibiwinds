add_indicator_2_4 <- function(x, y, selected_reference_period = "Z1") {

  if (!("indicator_16" %in% names(x))) {
    x_dep <- x |>
      dplyr::select(-contains("indicator_")) |>
      add_indicator_1_6(y) |>
      dplyr::filter(reference_period == selected_reference_period) |>
      dplyr::select(-reference_period) |>
      dplyr::rename(indicator_16_z1 = indicator_16)
  } else {
    x_dep <- x |>
      dplyr::select(
        well_id,
        climate_model_name,
        indicator_16) |>
      dplyr::rename(indicator_16_z1 = indicator_16)
  }

  indicator_24 <- y |>
    dplyr::left_join(
      x_dep,
      by = c("well_id", "climate_model_name")
    ) |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period,
      year = lubridate::year(date)
    ) |>
    dplyr::filter(gwl > indicator_16_z1) |>
    dplyr::summarise(
      mo_gw_low = dplyr::n(),
      .groups = "drop_last"
    ) |>
    dplyr::filter(mo_gw_low >= 1) |>
    dplyr::summarise(
      indicator_24 = dplyr::n(),
      .groups = "drop"
    )

  x |>
    dplyr::left_join(
      indicator_24,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}