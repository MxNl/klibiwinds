add_indicator_1_8 <- function(x, y) {
  indicator_18 <- y %>%
    group_by(
      well_id,
      climate_model_name,
      reference_period
    ) %>%
    summarise(
      indicator_18 = max(gwl_projections, na.rm = TRUE) - min(gwl_projections, na.rm = TRUE),
      .groups = "drop"
    )

  x %>%
    left_join(
      indicator_18,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}