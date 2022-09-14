add_indicator_1_2 <- function(x, y) {
  indicator_12 <- y |>
    dplyr::group_by(well_id, climate_model_name, reference_period) |>
    dplyr::summarise(indicator_12 = max(gwl, na.rm = TRUE), .groups = "drop") |>
    dplyr::ungroup()

  x |>
    dplyr::left_join(
      indicator_12,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}