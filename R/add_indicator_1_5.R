add_indicator_1_5 <- function(x, y) {
  indicator_1.5 <- y %>%
    group_by(
      well_id,
      climate_model_name,
      reference_period,
      year = year(date)
    ) %>%
    summarise(
      annual_min = min(gwl_projections, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    summarise(
      indicator_1.5 = mean(annual_min, na.rm = TRUE),
      .groups = "drop"
    )

  x %>%
    left_join(
      indicator_1.5,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}