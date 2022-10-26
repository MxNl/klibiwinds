#' make_plot_timeseries_heatmap
#'
#' @param x text...
#' @param facet_var text...
#'
#' @return
#' @export
make_plot_timeseries_heatmap <- function(x, facet_var) {
  facet_var <- dplyr::enquo(facet_var)

  plot_data <- x |>
    dplyr::group_by(well_id, reference_period, climate_model_name) |>
    # mutate(gwl = gwl - first(gwl)) |>
    dplyr::mutate(gwl = BBmisc::normalize(gwl, method = "center")) |>
    dplyr::group_by(reference_period, !!facet_var, date) |>
    dplyr::summarise(gwl = mean(gwl)) |>
    dplyr::mutate(
      month = date |>
        lubridate::month() |>
        integer_to_monthabbr() |>
        factor(levels = month.abb),
      year = lubridate::year(date)
    ) |>
    dplyr::ungroup()

  scale_fill_limits <- plot_data |>
    dplyr::pull(gwl) |>
    range()

  scale_y_limits <- reference_periods |>
    dplyr::pull(year) |>
    range() |>
    magrittr::add(c(-1, 1))

  plot_data |>
    dplyr::group_by(!!facet_var) |>
    dplyr::group_split() |>
    purrr::map(~.x |>
      ggplot2::ggplot() +
      ggplot2::aes(month, year, fill = gwl) +
      ggplot2::geom_tile() +
      ggplot2::scale_x_discrete(labels = label_every_nth) +
      ggplot2::scale_y_continuous(
        breaks = reference_periods |> dplyr::pull(year),
        minor_breaks = NULL,
        labels = ~.x |> label_every_nth(3),
        limits = scale_y_limits,
        expand = c(.01, .01)
      ) +
      paletteer::scale_fill_paletteer_c(
        "scico::vik",
        rescaler = rescaler_mid(),
        direction = -1,
        limits = scale_fill_limits,
        guide = ggplot2::guide_colourbar(
          barheight = .3,
          barwidth = 20,
          title.position = "bottom",
          title.hjust = .5
        )
      ) +
      ggplot2::labs(
        title = .x |>
          dplyr::slice(1) |>
          dplyr::pull(!!facet_var),
        y = "Year",
        fill = "GWL in m (centered to mean)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        text = ggplot2::element_text(family = "base_font", size = 10),
        axis.text = ggplot2::element_text(size = 8),
        title = ggplot2::element_text(hjust = .5, size = 11),
        legend.position = "top",
        axis.title.x = ggplot2::element_blank()
      )
    ) |>
    purrr::map_at(
      -1,
      ~.x +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank())
    ) |>
    patchwork::wrap_plots(nrow = 1) +
    patchwork::plot_layout(guides = 'collect') +
    patchwork::plot_annotation(
      title = deparse(substitute(facet_var)) |>
        stringr::str_remove("~") |>
        stringr::str_replace("_", " ") |>
        stringr::str_to_sentence()
        # stringr::str_replace("_", " ") |>
        # stringr::str_to_title()
      ) &
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = .5, size = 12)
    )
}
