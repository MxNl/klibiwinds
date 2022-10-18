z_to_yearrange_period_names <- function(x_long_format) {
  new_perido_names <- reference_periods %>%
    group_by(reference_period) %>%
    filter(year %in% range(year)) %>%
    mutate(start_end = c("start", "end")) %>%
    pivot_wider(names_from = start_end, values_from = year) %>%
    mutate(period = str_c(start, end, sep = " - ")) %>%
    pull(period)

  x_long_format %>%
    mutate(
      period = case_when(
        period == 1 ~ new_perido_names[1],
        period == 2 ~ new_perido_names[2],
        period == 3 ~ new_perido_names[3],
      ),
      period = period %>%
        forcats::as_factor() %>%
        forcats::fct_rev()
    )
}