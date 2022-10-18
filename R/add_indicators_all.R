#' @export
add_indicators_all <- function(x, y) {
  x |>
    add_indicators_general(y) |>
    add_indicators_extremes(y) |>
    add_indicators_seasonality(y) |>
    dplyr::arrange(well_id, climate_model_name, reference_period)
}