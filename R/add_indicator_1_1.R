add_indicator_1_1 <- function(x, y) {
  indicator_1.1 <- y %>%
    group_by(well_id, climate_model_name, reference_period) %>%
    summarise(indicator_1.1 = min(gwl_projections, na.rm = TRUE), .groups = "drop")

  x %>%
    left_join(
      indicator_1.1,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}