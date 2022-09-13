add_indicator_2_2 <- function(x, y, selected_reference_period = "Z2") {
  if (!("indicator_16" %in% names(x))) {
    x_dep <- x %>%
      select(-contains("indicator_")) %>%
      add_indicator_1_6(y) %>%
      filter(reference_period == selected_reference_period) %>%
      select(-reference_period) %>%
      rename(indicator_16_z1 = indicator_16)
  } else {
    x_dep <- x %>%
      select(
        well_id,
        climate_model_name,
        indicator_16) %>%
      rename(indicator_16_z1 = indicator_16)
  }

  indicator_22 <- y %>%
    left_join(
      x_dep,
      by = c("well_id", "climate_model_name")
    ) %>%
    group_by(
      well_id,
      climate_model_name,
      reference_period
    ) %>%
    filter(gwl_projections > indicator_16_z1) %>%
    count(
      name = "indicator_22"
    ) %>%
    ungroup()

  x %>%
    left_join(
      indicator_22,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}