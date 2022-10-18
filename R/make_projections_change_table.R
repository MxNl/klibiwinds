make_projections_change_table <- function(summary_table) {
  summary_table %>%
    pivot_longer(
      cols = contains("indicator_"),
      names_to = "indicator",
      values_to = "projected"
    ) %>%
    pivot_wider(names_from = reference_period, values_from = projected) %>%
    mutate(
      relative_change_z2 = (Z2 - Z1) / Z1,
      relative_change_z3 = (Z3 - Z1) / Z1,
    ) %>%
    select(-starts_with("Z"))
}