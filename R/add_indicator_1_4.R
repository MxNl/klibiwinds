add_indicator_1_4 <- function(x, y) {
  indicator_14 <- y %>%
    group_by(well_id, climate_model_name, reference_period) %>%
    summarise(indicator_14 = sd(gwl_projections, na.rm = TRUE), .groups = "drop") %>%
    ungroup()

  x %>%
    left_join(
      indicator_14,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}