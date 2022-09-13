add_indicators_extremes <- function(x, y) {
  x %>%
    add_indicator_2_1(y) %>%
    add_indicator_2_2(y) %>%
    add_indicator_2_3(y) %>%
    add_indicator_2_4(y)
}