#' add_indicator_2_1
#'
#' @param x tibble containing gwl level head time series
#' @param y tibble containing the summary table
#' @param selected_reference_period default
#'
#' @return
#' @export
#'
#' @examples
add_indicator_2_1 <- function(x, y, selected_reference_period = "Z1") {

  if (!("indicator_15" %in% names(x))) {
    x_dep <- x |>
      dplyr::select(-contains("indicator_")) |>
      add_indicator_1_5(y) |>
      dplyr::filter(reference_period == selected_reference_period) |>
      dplyr::select(-reference_period) |>
      dplyr::rename(indicator_15_z1 = indicator_15)
  } else {
    x_dep <- x |>
      dplyr::filter(reference_period == selected_reference_period) |>
      dplyr::select(
        well_id,
        climate_model_name,
        indicator_15
      ) |>
        dplyr::rename(indicator_15_z1 = indicator_15)
  }

  indicator_21 <- y |>
    dplyr::left_join(
      x_dep,
      by = c("well_id", "climate_model_name")
    ) |>
    dplyr::group_by(
      well_id,
      climate_model_name,
      reference_period
    ) |>
    dplyr::summarise(indicator_21 = sum(gwl < indicator_15_z1)) |>
    dplyr::ungroup()

  x |>
    dplyr::left_join(
      indicator_21,
      by = c("well_id", "climate_model_name", "reference_period")
    )
}
