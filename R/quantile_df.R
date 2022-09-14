quantile_df <- function(x, probs = c(0, 0.05, 0.15, 0.25, 0.5, 0.75, 0.85, 0.95, 1.0)) {
  x |>
    quantile(probs = probs) |>
    set_names(probs) |>
    enframe() |>
    dplyr::mutate(
      name = as.numeric(name) * 100,
      name = str_pad(name, 3, "left", "0"),
      name = str_c("qp", name, sep = "")
    ) |>
    pivot_wider()
}