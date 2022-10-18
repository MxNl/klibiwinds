#' add_indicators_all
#'
#' Add all klibiwind7 indicators to a summary table
#'
#' @param x A summary table (tibble), that has distinct rows for the columns well_id, climate_model_name and reference_period
#'
#' @param y The base table (tibble) containing the time series in long format
#'
#' @export
add_indicators_all <- function(x, y) {
  x |>
    add_indicators_general(y) |>
    add_indicators_extremes(y) |>
    add_indicators_seasonality(y) |>
    dplyr::arrange(well_id, climate_model_name, reference_period)
}
