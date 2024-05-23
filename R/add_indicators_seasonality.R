add_indicators_seasonality <- function(x, y) {
  x |>
    add_indicator_3_1(y) |>
    add_indicator_3_2(y) |>
    add_indicator_3_3(y) |>
    add_indicator_3_4(y) |>
    add_indicator_3_5(y)
}
