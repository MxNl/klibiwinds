add_indicator_3_3 <- function(x, y) {
  indicator_3.3 <- y %>%
    # slice_sample(prop = 0.01) %>%
    group_by(
      well_id,
      climate_model_name,
      reference_period,
      month = month(date)
    ) %>%
    summarise(
      # quantile_df(gwl_projections, probs = c(0.05, 0.15)),
      mean = mean(gwl_projections),
      .groups = "drop"
    ) %>%
    group_by(
      well_id,
      climate_model_name,
      reference_period
    ) %>%
    nest() %>%
    ungroup() %>%
    rename(indicator_3.3 = data)

  x %>%
    left_join(
      indicator_3.3,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}