add_indicator_2_3 <- function(x, y, selected_reference_period = "Z2") {

  if (!("indicator_1.5" %in% names(x))) {
    x_dep <- x %>%
      select(-contains("indicator_")) %>%
      add_indicator_1_5(y) %>%
      filter(reference_period == selected_reference_period) %>%
      select(-reference_period) %>%
      rename(indicator_1.5_z1 = indicator_1.5)
  } else {
    x_dep <- x %>%
      select(
        well_id,
        climate_model_name,
        indicator_1.5) %>%
      rename(indicator_1.5_z1 = indicator_1.5)
  }

  indicator_2.3 <- y %>%
    left_join(
      x_dep,
      by = c("well_id", "climate_model_name")
    ) %>%
    group_by(
      well_id,
      climate_model_name,
      reference_period,
      year = year(date)
    ) %>%
    filter(gwl_projections < indicator_1.5_z1) %>%
    summarise(
      mo_gw_low = n(),
      .groups = "drop_last"
    ) %>%
    filter(mo_gw_low >= 1) %>%
    summarise(
      indicator_2.3 = n(),
      .groups = "drop"
    )

  x %>%
    left_join(
      indicator_2.3,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}