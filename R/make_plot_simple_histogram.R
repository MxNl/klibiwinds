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
      n_obs = dplyr::n() / length(unique(period)) / length(unique(climate_model_name)),
      unit = unique(unit),
      .groups = "drop"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      region_natur = factor(
        stringr::str_glue("{region_natur}\nn = {n_obs}"),
        levels = unique(stringr::str_glue("{region_natur}\nn = {n_obs}"))
      ),
      indicator_name =  factor(
        pad_to_max_lines(indicator_name, 4),
        levels = pad_to_max_lines(indicator_name, 4)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(indicator_name) |>
    dplyr::group_split()

  plot_data |>
    purrr::map(~ .x |>
                 ggplot2::ggplot(
                   ggplot2::aes(y = !!group_var, colour = period)
                 ) +
                 # ggplot2::geom_line(
                 #   position = ggplot2::position_dodge(0.6)
                 # ) +
                 ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey80") +
                 ggplot2::geom_point(
                   ggplot2::aes(x = mean),
                   position = ggplot2::position_dodge(0.6),
                   size = 3.2,
                   show.legend = TRUE
                 ) +
                 # gghighlight::gghighlight(n_obs > 20) +
                 ggstance::geom_linerangeh(
                   ggplot2::aes(xmin = min, xmax = max),
                   position = ggplot2::position_dodge(0.6),
                   size = 1.2,
                   show.legend = TRUE
                 ) +
                 gghighlight::gghighlight(
                   n_obs > 20, use_direct_label = FALSE,
                   unhighlighted_params = list(alpha = 1)
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
                   values = colours_periods,
                   guide = ggplot2::guide_legend(
                     reverse = TRUE,
                     title.position = "left",
                     ncol = 1,
                     direction = "vertical",
                     title = "Betrachtete Zeitr\u00E4ume"
                   )
                 ) +
                 ggplot2::labs(
                   x = .x |> dplyr::pull(unit) |> unique()
                 ) +
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
                   strip.background = ggplot2::element_rect(fill = "grey90", colour = NA),
                   strip.text = ggtext::element_markdown(margin = ggplot2::margin(rep_len(3, 4)), lineheight = 1.15),
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
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      # title = stringr::str_glue(
      #   "Betrachtete Perioden <span style = 'color:{colours_periods[1]};'>{names(colours_periods)[1]}</span> und
      #    <span style = 'color:{colours_periods[2]};'>{names(colours_periods)[2]}</span>"),
      theme = ggplot2::theme(
        plot.title = ggtext::element_textbox_simple(
          halign = .5, size = 11, face = "bold"
        ),
        legend.position = "top"
      )
    )
}
