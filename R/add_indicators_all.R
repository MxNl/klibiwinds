#' add_indicators_all
#'
#' Add all klibiwind7 indicators to a summary table
#'
#' @param x A summary table (tibble), that has distinct rows for the columns well_id, climate_model_name and reference_period
#'
#' @param y The base table (tibble) containing the time series in long format
#' @param ... Additional arguments for the functions that calculate the indicators. Currently implemented:
#'  * selected_reference_period: to definewerwe the reference period
#' @md
#'
#' @export
add_indicators_all <- function(x, y, ...) {
  x |>
    add_indicators_klibiw(y, ...) |>
    add_indicators_gwdynindices(y) |>
    dplyr::arrange(well_id, climate_model_name, reference_period)
}
