#' add_indicator_6_1_3_1
#'
#' @param x
#' @param y
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_indicator_6_1_3_1 <- function(x, y, ...) {
  indicator_6131 <- y |>
    dplyr::group_by(well_id, climate_model_name, reference_period) |>
    dplyr::summarise(indicator_6131 = indicator_lowpulseduration(date, gwl), .groups = "drop")

  x |>
    dplyr::left_join(
      indicator_6131,
      by = c("well_id", "climate_model_name", "reference_period")
    ) |>
    dplyr::as_tibble()
}
