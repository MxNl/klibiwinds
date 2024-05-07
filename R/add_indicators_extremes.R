add_indicators_extremes <- function(x, y, ...) {
  x |>
    add_indicator_2_1(y, selected_reference_period = ...) |>
    add_indicator_2_2(y, selected_reference_period = ...) |>
    add_indicator_2_3(y, selected_reference_period = ...) |>
    add_indicator_2_4(y, selected_reference_period = ...)
}
