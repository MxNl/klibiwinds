add_indicator_1_3 <- function(x, y) {
  indicator_1.3 <- y %>%
    group_by(well_id, climate_model_name, reference_period) %>%
    summarise(indicator_1.3 = mean(gwl_projections, na.rm = TRUE)) %>%
    ungroup()

  x %>%
    left_join(
      indicator_1.3,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}