#' make_plot_simple_histogram
#'
#' @param plot_data text...
#' @param group_var text...
#' @param absolute_value_or_change text...
#'
#' @return
#' @export
make_plot_simple_histogram <- function(plot_data, group_var, absolute_value_or_change = "change") {
  group_var <- dplyr::enquo(group_var)

  if (!(absolute_value_or_change %in% c("value", "change"))) {
    stop("Please provide on of the following values for the argument absolute_value_or_change: value or change")
  }

  string_to_remove <- stringr::str_glue("absolute_{absolute_value_or_change}_z")
  # max_length_indicator_name <- plot_data |>
  #   dplyr::pull(indicator_name) |>
  #   nchar() |>
  #   max()

  plot_data <- plot_data |>
    dplyr::select(
      well_id, climate_model_name, indicator_name, unit,
      contains(absolute_value_or_change), !!group_var
    ) |>
    tidyr::pivot_longer(cols = dplyr::contains(absolute_value_or_change), names_to = "period") |>
    dplyr::mutate(period = period |> stringr::str_remove(string_to_remove)) |>
    dplyr::mutate(
      region_natur = factor(
        region_natur,
        levels = c("Inseln", "Marschen", "Niederungsgebiete", "Geestgebiete", "B\U00F6rden", "Bergland")
      ) |>
        forcats::fct_rev()
    ) |>
    z_to_yearrange_period_names() |>
    dplyr::group_by(period, indicator_name, !!group_var) |>
    dplyr::summarise(
      min = min(value),
      mean = mean(value),
      max = max(value),
      n_obs = paste(dplyr::n() / length(unique(period))),
      unit = unique(unit),
      .groups = "drop"
    ) |>
    # dplyr::mutate(
    #   indicator_name = stringr::str_pad(
    #     indicator_name,
    #     max_length_indicator_name,
    #     side = "both",
    #     " "
    #     )
    #   ) |>
    dplyr::group_by(indicator_name) |>
    dplyr::group_split()

  plot_data |>
    purrr::map(~ .x |>
      ggplot2::ggplot(
        ggplot2::aes(mean, !!group_var, colour = period)
      ) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey80") +
      # ggplot2::geom_line(
      #   position = ggplot2::position_dodge(0.6)
      # ) +
      ggplot2::geom_point(
        position = ggplot2::position_dodge(0.6),
        size = 3.2
      ) +
      ggstance::geom_linerangeh(
        ggplot2::aes(xmin = min, xmax = max, y = !!group_var, colour = period),
        position = ggplot2::position_dodge(0.6),
        size = 1.2
      ) +
      ggplot2::facet_grid(
        . ~ indicator_name,
        # nrow = 1,
        scales = "free_x",
        labeller = ggplot2::label_wrap_gen(15)
      ) +
      # paletteer::scale_colour_paletteer_d(
      #   "beyonce::X54",
      #   guide = ggplot2::guide_legend(
      #     reverse = TRUE,
      #     title.position = "top",
      #     ncol = 1
      #   )
      # ) +
      ggplot2::scale_colour_manual(
        values = c("#65A8AB", "#9FBAAB"),
        guide = ggplot2::guide_legend(
          reverse = TRUE,
          title.position = "top",
          ncol = 1
        )
      ) +
      ggplot2::labs(
        colour = "Betrachtete Periode",
        x = .x |> dplyr::pull(unit) |> unique()
      ) +
      # ggplot2::scale_y_discrete(expand = ggplot2::expansion(c(0.1, 0.2))) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_line(size = 0.25),
        panel.grid.major = ggplot2::element_line(size = 0.25),
        axis.title.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_text(hjust = 0.5),
        legend.position = "top",
        axis.title.x = ggplot2::element_text(size = 9, vjust = -1),
        text = ggplot2::element_text(family = "base_font", size = 10),
        axis.text = ggplot2::element_text(size = 8),
        title = ggplot2::element_text(hjust = .5, size = 11),
        strip.background = ggplot2::element_rect(fill = "grey90", colour = NA),
        strip.text = ggplot2::element_text(margin = ggplot2::margin(rep_len(3, 4)), lineheight = 1.15),
        strip.text.y = ggplot2::element_text(angle = 0)
      )) |>
    purrr::map_at(
      -1,
      ~ .x +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank()
        )
    ) |>
    patchwork::wrap_plots(nrow = 1) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(
      legend.position = "top",
      plot.title = ggplot2::element_text(hjust = .5, size = 12)
    )
}
