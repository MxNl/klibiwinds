#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
make_observed_change_table <- function(x, y) {
  x |>
    tidyr::pivot_longer(
      cols = dplyr::contains("indicator_"),
      names_to = "indicator",
      values_to = "observed"
    ) |>
    dplyr::select(-climate_model_name) |>
    dplyr::left_join(y, by = c("well_id", "indicator")) |>
    dplyr::mutate(
      absolute_change_z2 = observed * relative_change_z2,
      absolute_change_z3 = observed * relative_change_z3,
      absolute_change_z2 = dplyr::if_else(
        indicator %in% c("indicator_31", "indicator_32"),
        relative_change_z2,
        absolute_change_z2
      ),
      absolute_change_z3 = dplyr::if_else(
        indicator %in% c("indicator_31", "indicator_32"),
        relative_change_z3,
        absolute_change_z3
      ),
      absolute_value_z2 = observed + absolute_change_z2,
      absolute_value_z3 = observed + absolute_change_z3,
      absolute_value_z2 = dplyr::if_else(
        indicator %in% c("indicator_31", "indicator_32"),
        circ_add(observed, absolute_change_z2, int = 12),
        absolute_value_z2
      ),
      absolute_value_z2 = dplyr::if_else(
        indicator %in% c("indicator_31", "indicator_32"),
        circ_add(observed, absolute_change_z2, int = 12),
        absolute_value_z2
      )
    ) |>
    dplyr::select(-dplyr::starts_with("relative_change"), -reference_period) |>
    dplyr::relocate(well_id, climate_model_name, indicator, observed)
}
