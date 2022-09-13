add_indicator_2_1 <- function(x, y, selected_reference_period = "Z2") {

  if (!("indicator_15" %in% names(x))) {
    x_dep <- x %>%
      select(-contains("indicator_")) %>%
      add_indicator_1_5(y) %>%
      filter(reference_period == selected_reference_period) %>%
      select(-reference_period) %>%
      rename(indicator_15_z1 = indicator_15)
  } else {
    x_dep <- x %>%
      select(
        well_id,
        climate_model_name,
        indicator_15) %>%
      rename(indicator_15_z1 = indicator_15)
  }

  indicator_21 <- y %>%
    left_join(
      x_dep,
      by = c("well_id", "climate_model_name")
    ) %>%
    group_by(
      well_id,
      climate_model_name,
      reference_period
    ) %>%
    filter(gwl_projections < indicator_15_z1) %>%
    count(
      name = "indicator_21"
    ) %>%
    ungroup()

  x %>%
    left_join(
      indicator_21,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}