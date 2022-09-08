add_indicator_1_2 <- function(x, y) {
  indicator_1.2 <- y %>%
    group_by(well_id, climate_model_name, reference_period) %>%
    summarise(indicator_1.2 = max(gwl_projections, na.rm = TRUE)) %>%
    ungroup()

  x %>%
    left_join(
      indicator_1.2,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}