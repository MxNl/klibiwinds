#' @export
unnest_indicator_3_3 <- function(x) {
  x |>
    tidyr::unnest(cols = indicator_33) |>
    tidyr::pivot_wider(
      names_from = month,
      values_from = dplyr::contains(c("mean", "qp")),
      names_glue = "indicator_33_{.value}_month{month}"
    )
}