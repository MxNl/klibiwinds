add_indicator_1_7 <- function(x, y) {
  indicator_17 <- y %>%
    group_by(
      well_id,
      climate_model_name,
      reference_period,
      year = year(date)
    ) %>%
    summarise(
      annual_amplitude = max(gwl_projections, na.rm = TRUE) - min(gwl_projections, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    summarise(
      indicator_17 = mean(annual_amplitude, na.rm = TRUE),
      .groups = "drop"
    )

  x %>%
    left_join(
      indicator_17,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}