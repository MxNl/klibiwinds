add_indicator_3_2 <- function(x, y) {
  indicator_3.2 <- y %>%
    group_by(
      well_id,
      climate_model_name,
      reference_period,
      year = year(date)
    ) %>%
    filter(
      gwl_projections == max(gwl_projections, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      month_num = month(date),
      month_norm = BBmisc::normalize(
        month_num,
        method = "range",
        range = c(0, 1 - 1 / 12)
      ),
      month_sin = sin(month_norm)
    ) %>%
    group_by(
      well_id,
      climate_model_name,
      reference_period
    ) %>%
    summarise(
      month_mean = mean(month_sin),
      .groups = "drop"
    ) %>%
    mutate(
      indicator_3.2 = month_mean %>%
        asin() %>%
        BBmisc::normalize(method = "range", range = c(1, 12))
    ) %>%
    select(-month_mean)

  x %>%
    left_join(
      indicator_3.2,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}