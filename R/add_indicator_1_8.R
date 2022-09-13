add_indicator_1_8 <- function(x, y) {
  indicator_1.8 <- y %>%
    group_by(
      well_id,
      climate_model_name,
      reference_period
    ) %>%
    summarise(
      indicator_1.8 = max(gwl_projections, na.rm = TRUE) - min(gwl_projections, na.rm = TRUE),
      .groups = "drop"
    )

  x %>%
    left_join(
      indicator_1.8,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}