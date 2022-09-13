add_indicator_1_6 <- function(x, y) {
  indicator_16 <- y %>%
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
      indicator_16 = mean(annual_max, na.rm = TRUE),
      .groups = "drop"
    )

  x %>%
    left_join(
      indicator_16,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}