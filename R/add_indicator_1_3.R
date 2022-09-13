add_indicator_1_3 <- function(x, y) {
  indicator_13 <- y %>%
    group_by(well_id, climate_model_name, reference_period) %>%
    summarise(indicator_13 = mean(gwl_projections, na.rm = TRUE), .groups = "drop") %>%
    ungroup()

  x %>%
    left_join(
      indicator_13,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}