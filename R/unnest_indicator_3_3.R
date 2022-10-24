#' unnest_indicator_3_3
#'
#'
#' unnest_indicator_3_3
#'
#' Unnest the tibble column of indicator 3.3
#'
#' @param x A summary table (tibble) with the nested indicator 3.3
#'
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
