add_indicator_1_6 <- function(x, y) {
  indicator_1.6 <- y %>%
    group_by(
      well_id,
      climate_model_name,
      reference_period,
      year = year(date)
    ) %>%
    summarise(
      annual_max = max(gwl_projections, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    summarise(
      indicator_1.6 = mean(annual_max, na.rm = TRUE),
      .groups = "drop"
    )

  x %>%
    left_join(
      indicator_1.6,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}