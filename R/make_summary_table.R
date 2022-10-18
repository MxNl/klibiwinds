#' make_summary_table
#'
#' Create a summary table
#'
#' @param x The base table (tibble) containing the time series in long format
#'
#' @export
make_summary_table <- function(x) {
  x |>
    dplyr::distinct(well_id, climate_model_name, reference_period)
}
