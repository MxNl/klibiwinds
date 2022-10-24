#' filter_criterion_incomplete_z1_period
#'
#' Drop...
#'
#' @param x asda
#'
#' @export
filter_criterion_incomplete_z1_period <- function(x) {
  well_ids_with_complete_z1 <- x |>
    dplyr::group_by(well_id, climate_model_name, reference_period) |>
    dplyr::filter(
      # min(lubridate::year(date)) <= 1983 &
        reference_period == "Z1" &
        climate_model_name == "observed" &
        dplyr::n() >= 360 * 0.95
    ) |>
    dplyr::pull(well_id) |>
    unique()

  x |>
    dplyr::filter(well_id %in% well_ids_with_complete_z1)
}