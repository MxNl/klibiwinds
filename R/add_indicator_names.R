add_indicator_names <- function(x) {
  x %>%
    left_join(indicators, by = "indicator") %>%
    mutate(
      name = if_else(
        id == "3.3",
        str_glue("{name} ({word(indicator, start = 4, sep = '_')})", .na = "-"),
        name
      ),
    )
}
