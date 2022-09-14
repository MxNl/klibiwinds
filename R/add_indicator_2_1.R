add_indicator_2_1 <- function(x, y, selected_reference_period = "Z1") {

  if (!("indicator_15" %in% names(x))) {
    x_dep <- x |>
      dplyr::select(-contains("indicator_")) |>
      add_indicator_1_5(y) |>
      dplyr::filter(reference_period == selected_reference_period) |>
      dplyr::select(-reference_period) |>
      dplyr::rename(indicator_15_z1 = indicator_15)
  } else {
    x_dep <- x |>
      dplyr::select(
        well_id,
        climate_model_name,
        indicator_15) |>
      dplyr::rename(indicator_15_z1 = indicator_15)
  }

  indicator_21 <- y |>
    dplyr::left_join(
      x_dep,
      by = c("well_id", "climate_model_name")
    ) |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period
    ) |>
    dplyr::filter(gwl < indicator_15_z1) |>
    count(
      name = "indicator_21"
    ) |>
    dplyr::ungroup()

  x |>
    dplyr::left_join(
      indicator_21,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}