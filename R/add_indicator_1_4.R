add_indicator_1_4 <- function(x, y) {
  indicator_14 <- y |>
    dplyr::group_by(well_id, climate_model_name, reference_period) |>
    dplyr::summarise(indicator_14 = sd(gwl, na.rm = TRUE), .groups = "drop") |>
    dplyr::ungroup()

  x |>
    dplyr::left_join(
      indicator_14,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}