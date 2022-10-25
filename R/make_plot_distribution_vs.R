#' make_plot_distribution_vs
#'
#' @param plot_data text...
#' @param var_rows text...
#' @param var_columns text...
#' @param absolute_value_or_change text...
#'
#' @return
#' @export
#'

make_plot_distribution_vs <- function(plot_data, var_rows, var_columns, absolute_value_or_change = "value") {
  var_columns <- dplyr::enquo(var_columns)
  var_rows <- dplyr::enquo(var_rows)

  if (!(absolute_value_or_change %in% c("value", "change"))) {
    stop("Please provide on of the following values for the argument absolute_value_or_change: value or change")
  }

  var_rows_title <- var_rows |>
    substitute() |>
    deparse() |>
    stringr::str_remove("~") |>
    stringr::str_replace("_", " ") |>
    stringr::str_to_sentence()

  var_columns_title <- var_columns |>
    substitute() |>
    deparse() |>
    stringr::str_remove("~") |>
    stringr::str_replace("_", " ") |>
    stringr::str_to_sentence()

    column_selector <- absolute_value_or_change
    string_to_remove <- stringr::str_glue("absolute_{absolute_value_or_change}_z")

  plot_data |>
    dplyr::rename(absolute_value_z1 = observed) |>
    tidyr::pivot_longer(cols = dplyr::contains(column_selector), names_to = "period") |>
    dplyr::mutate(period = period |> stringr::str_remove(string_to_remove)) |>
    z_to_yearrange_period_names() |>
    ggplot2::ggplot(
      ggplot2::aes(value, period, height = ..density.., fill = period)
    ) +
    ggridges::geom_density_ridges(
      ggplot2::aes(colour = period),
      quantile_lines = TRUE,
      quantiles = 2,
      size = .4,
      alpha = .75,
      vline_color = "white",
      # colour = "white",
      scale = 2.5,
      show.legend = FALSE
    ) +
    paletteer::scale_colour_paletteer_d("beyonce::X54") +
    paletteer::scale_fill_paletteer_d("beyonce::X54") +
    ggplot2::facet_grid(
      rows = ggplot2::vars(!!var_rows),
      cols = ggplot2::vars(!!var_columns),
      scales = "free",
      labeller = ggplot2::label_wrap_gen(15)
    ) +
    ggplot2::labs(
      title = stringr::str_glue("Distribution of {var_columns_title} by {var_rows_title}"),
      y = "Period"
    ) +
    ggridges::theme_ridges() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "base_font", size = 10),
      axis.text = ggplot2::element_text(size = 8),
      title = ggplot2::element_text(hjust = .5, size = 11),
      axis.title.x = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey90"),
      strip.text = ggplot2::element_text(margin = ggplot2::margin(rep_len(3, 4))),
      strip.text.y = ggplot2::element_text(angle = 0)
    )
}
