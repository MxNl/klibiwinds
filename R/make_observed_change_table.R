make_observed_change_table <- function(x, y) {
  x %>%
    pivot_longer(
      cols = contains("indicator_"),
      names_to = "indicator",
      values_to = "observed"
    ) %>%
    select(-climate_model_name) %>%
    left_join(y, by = c("well_id", "indicator")) %>%
    mutate(
      absolute_change_z2 = observed * relative_change_z2,
      absolute_change_z3 = observed * relative_change_z3,
      absolute_value_z2 = observed + absolute_change_z2,
      absolute_value_z3 = observed + absolute_change_z3
    ) %>%
    select(-starts_with("relative_change"), -reference_period) %>%
    relocate(well_id, climate_model_name, indicator, observed)
}