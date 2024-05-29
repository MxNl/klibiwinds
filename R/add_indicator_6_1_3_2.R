#' add_indicator_6_1_3_2
#'
#' @param x
#' @param y
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_indicator_6_1_3_2 <- function(x, y, ...) {
  indicator_6132 <- y |>
    dplyr::group_by(well_id, climate_model_name, reference_period) |>
    dplyr::summarise(indicator_6132 = indicator_highpulseduration(date, gwl), .groups = "drop")

  x |>
    dplyr::left_join(
      indicator_6132,
      by = c("well_id", "climate_model_name", "reference_period")
    ) |>
    dplyr::as_tibble()
}
