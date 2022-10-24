#' filter_criterion_model_performance
#'
#' Drop...
#'
#' @param x text...
#'
#' @export
filter_criterion_model_performance <- function(x) {
  performance_threshold <- 0.6
  well_ids_below_performance_threshold <- x |>
    dplyr::filter(
      nse >= performance_threshold
    ) |>
    dplyr::pull(well_id) |>
    unique()

  x |>
    dplyr::filter(well_id %in% well_ids_below_performance_threshold) |>
    dplyr::select(-nse, -r2)
}
