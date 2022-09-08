add_indicator_1_7 <- function(x, y) {
  indicator_1.7 <- y %>%
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
      indicator_1.7 = mean(annual_amplitude, na.rm = TRUE),
      .groups = "drop"
    )

  x %>%
    left_join(
      indicator_1.7,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}