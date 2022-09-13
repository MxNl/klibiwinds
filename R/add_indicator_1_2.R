add_indicator_1_2 <- function(x, y) {
  indicator_12 <- y %>%
    group_by(well_id, climate_model_name, reference_period) %>%
    summarise(indicator_12 = max(gwl_projections, na.rm = TRUE), .groups = "drop") %>%
    ungroup()

  x %>%
    left_join(
      indicator_12,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}