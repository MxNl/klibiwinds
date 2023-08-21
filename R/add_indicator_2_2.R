#' add_indicator_2_2
#'
#' @param x tibble containing gwl level head time series
#' @param y tibble containing the summary table
#' @param selected_reference_period default
#'
#' @return
#' @export
#'
#' @examples
add_indicator_2_2 <- function(x, y, selected_reference_period = "Z1") {
  if (!("indicator_16" %in% names(x))) {
    x_dep <- x |>
      dplyr::select(-contains("indicator_")) |>
      add_indicator_1_6(y) |>
      dplyr::filter(reference_period == selected_reference_period) |>
      dplyr::select(-reference_period) |>
      dplyr::rename(indicator_16_z1 = indicator_16)
  } else {
    x_dep <- x |>
      dplyr::filter(reference_period == selected_reference_period) |>
      dplyr::select(
        well_id,
        climate_model_name,
        indicator_16
      ) |>
        dplyr::rename(indicator_16_z1 = indicator_16)
  }

  indicator_22 <- y |>
    dplyr::left_join(
      x_dep,
      by = c("well_id", "climate_model_name")
    ) |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period
    ) |>
    dplyr::summarise(indicator_22 = sum(gwl > indicator_16_z1)) |>
    dplyr::ungroup()

  x |>
    dplyr::left_join(
      indicator_22,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}
