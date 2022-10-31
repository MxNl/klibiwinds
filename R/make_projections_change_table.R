#' Title
#'
#' @param summary_table text...
#'
#' @return
#' @export
#'

make_projections_change_table <- function(summary_table) {
  summary_table |>
    tidyr::pivot_longer(
      cols = dplyr::contains("indicator_"),
      names_to = "indicator",
      values_to = "projected"
    ) |>
    tidyr::pivot_wider(names_from = reference_period, values_from = projected) |>
    dplyr::mutate(
      relative_change_z2 = (Z2 - Z1) / Z1,
      relative_change_z3 = (Z3 - Z1) / Z1,
      relative_change_z2 = dplyr::if_else(
        indicator %in% c("indicator_31", "indicator_32"),
        rSW2utils::circ_minus(Z2, Z1, int = 12, type = "minusPiPlusPi"),
        relative_change_z2
      ),
      relative_change_z3 = dplyr::if_else(
        indicator %in% c("indicator_31", "indicator_32"),
        rSW2utils::circ_minus(Z3, Z1, int = 12, type = "minusPiPlusPi"),
        relative_change_z3
      )
    ) |>
  dplyr::select(-dplyr::starts_with("Z"))
}
