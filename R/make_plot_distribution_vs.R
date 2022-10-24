#' make_plot_distribution_vs
#'
#' @param plot_data
#' @param var_rows
#' @param var_columns
#'
#' @return
#' @export
#'
#' @examples
make_plot_distribution_vs <- function(plot_data, var_rows, var_columns) {
  var_columns <- dplyr::enquo(var_columns)
  var_rows <- dplyr::enquo(var_rows)

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

  plot_data |>
    dplyr::rename(absolute_value_z1 = observed) |>
    tidyr::pivot_longer(cols = dplyr::contains("value"), names_to = "period") |>
    dplyr::mutate(period = period |> stringr::str_remove("absolute_value_z")) |>
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
      rows = vars(!!var_rows),
      cols = vars(!!var_columns),
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
      strip.background =  element_rect(fill = "grey90"),
      strip.text = element_text(margin = margin(rep_len(3, 4))),
      strip.text.y = element_text(angle = 0)
        )
}
