add_indicator_2_2 <- function(x, y, selected_reference_period = "Z2") {
  if (!("indicator_1.6" %in% names(x))) {
    x_dep <- x %>%
      select(-contains("indicator_")) %>%
      add_indicator_1_6(y) %>%
      filter(reference_period == selected_reference_period) %>%
      select(-reference_period) %>%
      rename(indicator_1.6_z1 = indicator_1.6)
  } else {
    x_dep <- x %>%
      select(
        well_id,
        climate_model_name,
        indicator_1.6) %>%
      rename(indicator_1.6_z1 = indicator_1.6)
  }

  indicator_2.2 <- y %>%
    left_join(
      x_dep,
      by = c("well_id", "climate_model_name")
    ) %>%
    group_by(
      well_id,
      climate_model_name,
      reference_period
    ) %>%
    filter(gwl_projections > indicator_1.6_z1) %>%
    count(
      name = "indicator_2.2"
    ) %>%
    ungroup()

  x %>%
    left_join(
      indicator_2.2,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}