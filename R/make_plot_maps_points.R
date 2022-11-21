#' make_plot_maps_points
#'
#' @param plot_data text...
#' @param regions text...
#' @param absolute_value_or_change text...
#'
#' @return
#' @export
make_plot_maps_points <- function(plot_data, regions, absolute_value_or_change = "change") {
  if (!(absolute_value_or_change %in% c("value", "change"))) {
    stop("Please provide on of the following values for the argument absolute_value_or_change: value or change")
  }

  string_to_remove <- stringr::str_glue("absolute_{absolute_value_or_change}_z")

  legend_title <- plot_data |>
    dplyr::mutate(legend_name = stringr::str_c(indicator_name, " in ", unit)) |>
    dplyr::pull(legend_name) |>
    unique() |>
    stringr::str_replace_all("- ", "")

  plot_data <- plot_data |>
    dplyr::select(
      well_id, climate_model_name, indicator_name, unit,
      dplyr::contains(absolute_value_or_change)
    ) |>
    tidyr::pivot_longer(cols = dplyr::contains(absolute_value_or_change), names_to = "period") |>
    dplyr::mutate(period = period |> stringr::str_remove(string_to_remove)) |>
    z_to_yearrange_period_names() |>
    dplyr::group_by(period, well_id) |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      Minimum = min(value),
      Mittelwert = mean(value),
      Maximum = max(value),
      unit = unique(unit),
      .groups = "drop"
    ) |>
    dplyr::left_join(x = plot_data |> dplyr::select(well_id)) |>
    tidyr::pivot_longer(cols = dplyr::all_of(c("Minimum", "Mittelwert", "Maximum"))) |>
    dplyr::mutate(name = factor(name, levels = c("Minimum", "Mittelwert", "Maximum")))

  # regions_overlay <- regions |>
  #   sf::st_cast("LINESTRING") |>
  #   sf::st_intersection(plot_data_grid)

  plot_data |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = plot_data,
      ggplot2::aes(colour = value),
      size = 0.8
    ) +
    ggplot2::geom_sf(
      data = regions,
      size = 0.25,
      colour = "grey",
      fill = NA
    ) +
    # ggplot2::geom_sf(
    #   data = regions_overlay,
    #   size = 0.5,
    #   colour = "white",
    #   fill = NA
    # ) +
    # ggplot2::geom_sf(
    #   ggplot2::aes(colour = value)
    # ) +
    ggplot2::facet_grid(
      forcats::fct_rev(name) ~ forcats::fct_rev(period),
      labeller = ggplot2::as_labeller(stringr::str_to_sentence)
    ) +
    ggplot2::scale_colour_steps2(
      n.breaks = 9,
      low='red',
      mid='white',
      high='darkblue',
      midpoint=0,
      guide = ggplot2::guide_coloursteps(
        barheight = .3,
        barwidth = 20,
        title.position = "top"
      )
    ) +
    # paletteer::scale_fill_paletteer_c(
    #   "scico::vik",
    #   rescale = rescaler_mid(),
    #   guide = ggplot2::guide_colourbar(
    #     barheight = .3,
    #     barwidth = 20,
    #     title.position = "top"
    #   )
    # ) +
    ggplot2::labs(
      colour = legend_title
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(hjust = 0.5),
      plot.margin = ggplot2::margin(.5, 0, .5, 0, "cm"),
      legend.position = "top",
      axis.title.x = ggplot2::element_text(size = 9, vjust = -1),
      text = ggplot2::element_text(family = "base_font", size = 10),
      axis.text = ggplot2::element_text(size = 8),
      title = ggplot2::element_text(hjust = .5, size = 11),
      strip.background = ggplot2::element_rect(fill = "grey90", colour = NA),
      strip.text = ggplot2::element_text(margin = ggplot2::margin(rep_len(3, 4))),
      strip.text.y = ggplot2::element_text(angle = 0)
    )
}
