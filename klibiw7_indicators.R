
# Analysis ----------------------------------------------------------

filepath_projections <- "D:/Data/students/mariana/Results/Projections_hist"
filepath_historic <- "J:/PROJEKTE/KliBiW7/Daten/Grundwasserstandsdaten/Einzelmessstellen"

data_gwl <- collect_data(
  filepath_historic,
  filepath_projections
)

data_gwl <- data_gwl |>
  add_model_performance("D:/Data/students/mariana/Results/Optimization/PT")

data_gwl |> arrow::write_parquet("data_export/data_gwl.parquet")
data_gwl <- arrow::read_parquet("data_export/data_gwl.parquet")
data_gwl |> dplyr::select(-nse, -r2) |> arrow::write_parquet("data_export/result_gwl_historic_and_projected.parquet")
data_gwl |> dplyr::select(-nse, -r2) |> readr::write_csv2("data_export/result_gwl_historic_and_projected.csv")

data_gwl_ref <- data_gwl |>
  filter_criterion_model_performance() |>
  use_water_year() |>
  add_reference_period_column() |>
  filter_criterion_incomplete_z1_period()

##### Data export Tobias
data_gwl |>
  distinct(well_id) |>
  inner_join(lbeg_wells)

data_gwl |>
  add_reference_period_column() |>
  dplyr::rename(
    MEST_ID = well_id,
    GWL.CALC = gwl,
    DATUM = date,
    Projektion = climate_model_name
  ) |>
  dplyr::mutate(
    Institution = "BGR",
    JAHR = lubridate::year(DATUM),
    MONAT = lubridate::month(DATUM)
  ) |>
  dplyr::relocate(
    MEST_ID,
    JAHR,
    MONAT,
    GWL.CALC,
    DATUM,
    Projektion,
    Institution
  ) |>
  saveRDS("data_export/result_gwl_historic_and_projected_tobias.Rds")
##### end

indicators_summary <- data_gwl_ref |>
  make_summary_table() |>
  add_indicators_gwdynindices_shape_scale(data_gwl_ref)
  add_indicators_all(data_gwl_ref)
  # add_indicators_gwdynindices(data_gwl_ref)
  # add_indicators_klibiw(data_gwl_ref)

indicators_summary <- indicators_summary |>
  unnest_indicator_3_3()
indicators_summary |> readr::write_csv2("data_export/result_indicators.csv")
indicators_summary |> readr::write_rds("data_export/result_indicators.rds")
indicators_summary <- readr::read_rds("data_export/result_indicators.rds")

indicators_summary_observed <- indicators_summary |>
  split_observed()
indicators_summary_projections <- indicators_summary |>
  split_projections()

projections_change_table <-
  indicators_summary_projections |>
    make_projections_change_table()

projections_change_table |> readr::write_csv2("data_export/result_relative_changes.csv")
projections_change_table <- readr::read_csv2("data_export/result_relative_changes.csv")

observed_change_table <-
  indicators_summary_observed |>
    make_observed_change_table(projections_change_table)

observed_change_table |> readr::write_csv2("data_export/result_absolute_changes_and_values.csv")

# Plotting ----------------------------------------------------------
library(ggplot2)
library(dplyr)


sysfonts::font_add_google("Abel", "base_font")
showtext::showtext_auto()

regions_climate <- sf::read_sf(here::here("data_export", "Klimaregionen_Nds.shp"))
regions_natur <- sf::read_sf(here::here("data_export", "Naturraum_Reg_DTK50.shp"))
regions_natur <- regions_natur |>
  rmapshaper::ms_simplify(keep = 0.015, explode = TRUE, keep_shapes = TRUE) |>
  smoothr::smooth(method = "chaikin")

###### Plot map overview
plot_data <- observed_change_table |>
  dplyr::distinct(well_id, .keep_all = TRUE) |>
  add_indicator_names() |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp"), as_sf = TRUE) |>
  dplyr::mutate(
    region_natur = factor(
      region_natur,
      levels = c("Inseln", "Marschen", "Niederungsgebiete", "Geestgebiete", "B\U00F6rden", "Bergland")
    )
  )

colours_regions <- c(
  "#690034",
           "#9d8700",
           "#803aac",
           "#ff7f6d",
           "#2a215c",
           "#e8a3ff"
)

# colours_regions <- c("#c6614d",
#                               "#9ec34f",
#                               "#9346b0",
#                               "#87b796",
#                               "#4e3c3c",
#                               "#9690be")

plot_a <-
  ggplot() +
    geom_sf(data = regions_climate |> summarise(), colour = NA) +
    geom_sf(data = plot_data, aes(colour = region_natur)) +
    # geom_sf_label(data = regions_natur, aes(label = NATREGNAME)) +
    labs(colour = "Raumzugehörigkeit") +
    scale_colour_manual(values = colours_regions) +
    guides(
      colour = guide_legend(
        direction = "horizontal",
        title.position = "left",
        label.position = "bottom"
      )
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "base_font"),
      panel.grid.major = element_blank(),
      axis.text = element_blank(),
      legend.position = "none",
      legend.title = element_text(face = "bold", family = "base_font"),
    )

plot_b <-
  plot_data |>
    sf::st_drop_geometry() |>
    group_by(region_natur) |>
    count() |>
    ggplot(aes(y = region_natur, x = n)) +
    geom_col(aes(fill = region_natur), show.legend = FALSE) +
    geom_label(aes(label = n), alpha = 0.5, nudge_x = 7) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = colours_regions) +
    scale_x_continuous(minor_breaks = scales::breaks_width(10)) +
    labs(x = "Anzahl der Grundwassermessstellen") +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_text(vjust = -5),
      plot.margin = margin(0.5,0.5,1,0.5, "cm")
    )

p1 <- (plot_a | plot_b) +
  # patchwork::plot_layout(tag_level = "new") +
  patchwork::plot_annotation(tag_levels = "a") &
  theme(
    text = element_text(family = "base_font", size = 14)
  )


showtext::showtext_opts(dpi = 300)
list(p1) |>
  purrr::map2(
    c("data_export/plot_map_locations") |> paste0(".pdf"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "pdf",
      scale = 3, units = "cm", width = 8, height = 3.4, dpi = 300
    )
  )
list(p1) |>
  purrr::map2(
    c("data_export/plot_map_locations") |> paste0(".png"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "png",
      scale = 3, units = "cm", width = 8, height = 3.4, dpi = 300
    )
  )
showtext::showtext_opts(dpi = 96)

###### Plot simple histogram
plot_data <- observed_change_table |>
  add_indicator_names() |>
  use_core_indicators_only() |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp")) |>
  dplyr::mutate(indicator_name = forcats::as_factor(indicator_name))

p1 <- plot_data |>
  make_plot_simple_histogram(region_natur)

showtext::showtext_opts(dpi = 300)
list(p1) |>
  purrr::map2(
    c("data_export/plot_simple_histogram") |> paste0(".pdf"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "pdf",
      scale = 3, units = "cm", width = 10, height = 5, dpi = 300
    )
  )
list(p1) |>
  purrr::map2(
    c("data_export/plot_simple_histogram") |> paste0(".png"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "png",
      scale = 3, units = "cm", width = 10, height = 5, dpi = 300
    )
  )
showtext::showtext_opts(dpi = 96)

###### Plot maps
plot_data <- observed_change_table |>
  add_indicator_names() |>
  use_core_indicators_only() |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp"), as_sf = TRUE) |>
  dplyr::group_by(indicator_name) |>
  dplyr::group_split()

plot_list <- plot_data |>
  purrr::map(make_plot_maps_hex, regions_natur)

showtext::showtext_opts(dpi = 300)
plot_list |>
  purrr::map2(
    c("data_export/plot_maps") |> paste0(seq_along(plot_list), ".pdf"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "pdf",
      scale = 2, units = "cm", width = 8, height = 10, dpi = 300
    )
  )
plot_list |>
  purrr::map2(
    c("data_export/plot_maps") |> paste0(seq_along(plot_list), ".png"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "png",
      scale = 2, units = "cm", width = 8, height = 10, dpi = 300
    )
  )
showtext::showtext_opts(dpi = 96)

###### Plot maps points
plot_data <- observed_change_table |>
  add_indicator_names() |>
  use_core_indicators_only() |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp"), as_sf = TRUE) |>
  dplyr::group_by(indicator_name) |>
  dplyr::group_split()

plot_list <- plot_data |>
  purrr::map(make_plot_maps_points, regions_natur)

showtext::showtext_opts(dpi = 300)
plot_list |>
  purrr::map2(
    c("data_export/plot_maps_points") |> paste0(seq_along(plot_list), ".pdf"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "pdf",
      scale = 2, units = "cm", width = 8, height = 8, dpi = 300
    )
  )
plot_list |>
  purrr::map2(
    c("data_export/plot_maps_points") |> paste0(seq_along(plot_list), ".png"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "png",
      scale = 2, units = "cm", width = 8, height = 8, dpi = 300, bg = "white"
    )
  )
showtext::showtext_opts(dpi = 96)

###### Plot distributions vs
plot_data <- observed_change_table |>
  add_indicator_names() |>
  use_core_indicators_only() |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp"))

p1 <- plot_data |>
  make_plot_distribution_vs(region_natur, indicator_name)
p2 <- plot_data |>
  make_plot_distribution_vs(region_climate, indicator_name)
p3 <- plot_data |>
  make_plot_distribution_vs(screen_top, indicator_name)
p4 <- plot_data |>
  make_plot_distribution_vs(depth_to_gwl, indicator_name)
p5 <- plot_data |>
  make_plot_distribution_vs(climate_model_name, indicator_name)

showtext::showtext_opts(dpi = 300)
list(p1, p2, p3, p4, p5) |>
  purrr::map2(
    c("data_export/plot_distribution_vs") |> paste(1:5, sep = "_") |> paste0(".pdf"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "pdf",
      scale = 2.5, units = "cm", width = 12, height = 6, dpi = 300
    )
  )
list(p1, p2, p3, p4, p5) |>
  purrr::map2(
    c("data_export/plot_distribution_vs") |> paste(1:5, sep = "_") |> paste0(".png"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "png",
      scale = 2.5, units = "cm", width = 12, height = 6, dpi = 300
    )
  )
showtext::showtext_opts(dpi = 96)


########## Plot timeseries heatmap vs
plot_data <- data_gwl |>
  filter_criterion_model_performance() |>
  add_reference_period_column() |>
  filter_criterion_incomplete_z1_period() |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp"))

p1 <- plot_data |>
  make_plot_timeseries_heatmap(region_natur)
p2 <- plot_data |>
  make_plot_timeseries_heatmap(region_climate)
p3 <- plot_data |>
  make_plot_timeseries_heatmap(depth_to_gwl)
p4 <- plot_data |>
  make_plot_timeseries_heatmap(screen_top)

showtext::showtext_opts(dpi = 300)
list(p1, p2, p3, p4) |>
  purrr::map2(
    c("data_export/plot_timeseries_heatmap") |> paste(1:4, sep = "_") |> paste0(".pdf"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "pdf",
      scale = 1.8, units = "cm", width = 10, height = 10, dpi = 300
    )
  )
list(p1, p2, p3, p4) |>
  purrr::map2(
    c("data_export/plot_timeseries_heatmap") |> paste(1:4, sep = "_") |> paste0(".png"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "png",
      scale = 1.8, units = "cm", width = 10, height = 10, dpi = 300
    )
  )
showtext::showtext_opts(dpi = 96)

########## Plot special single timeseries heatmap
selection_well_ids <- data_gwl |>
  filter_criterion_model_performance() |>
  add_reference_period_column() |>
  filter_criterion_incomplete_z1_period() |>
  dplyr::left_join(indicators_summary) |>
  dplyr::group_by(well_id, climate_model_name) |>
  dplyr::summarise(selection_indicator = range(indicator_32) |> diff() |> abs()) |>
  dplyr::ungroup() |>
  dplyr::arrange(-selection_indicator)

plot_data <- data_gwl |>
  filter_criterion_model_performance() |>
  dplyr::filter(
    well_id == selection_well_ids |>
      dplyr::slice(1) |>
      dplyr::pull(well_id) &
      climate_model_name == selection_well_ids |>
        dplyr::slice(1) |>
        dplyr::pull(climate_model_name)
  )

p1 <- plot_data |>
  dplyr::mutate(
    month = date |>
      lubridate::month() |>
      integer_to_monthabbr() |>
      factor(levels = month.abb),
    year = lubridate::year(date)
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(month, year, fill = gwl) +
  ggplot2::geom_tile() +
  ggplot2::scale_x_discrete(labels = label_every_nth) +
  ggplot2::scale_y_continuous(
    breaks = 1981:2100,
    # minor_breaks = NULL,
    labels = label_every_nth,
    # limits = scale_y_limits,
    expand = c(.01, .01)
  ) +
  ggplot2::scale_fill_viridis_c(
    # "scico::vik",
    direction = -1,
    # limits = scale_fill_limits,
    guide = ggplot2::guide_colourbar(
      barheight = .3,
      barwidth = 7,
      title.position = "bottom",
      title.hjust = .5
    )
  ) +
  ggplot2::labs(
    title = selection_well_ids |>
      dplyr::slice(1) |>
      dplyr::pull(well_id) |>
      stringr::str_replace("_", " ") |>
      stringr::str_to_sentence(),
    subtitle = selection_well_ids |>
      dplyr::slice(1) |>
      dplyr::pull(climate_model_name) |>
      stringr::str_replace("_", " ") |>
      stringr::str_to_sentence(),
    y = "Year",
    fill = "GWL (centered to mean)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    text = ggplot2::element_text(family = "base_font", size = 10),
    axis.text = ggplot2::element_text(size = 8),
    title = ggplot2::element_text(hjust = .5, size = 11),
    legend.position = "bottom",
    axis.title.x = ggplot2::element_blank()
  )

showtext::showtext_opts(dpi = 300)
p1 |>
  ggplot2::ggsave(
    filename = "data_export/plot_timeseries_heatmap_single.pdf", device = "pdf",
    scale = 1.8, units = "cm", width = 3, height = 10, dpi = 300
  )
p1 |>
  ggplot2::ggsave(
    filename = "data_export/plot_timeseries_heatmap_single.png", device = "png",
    scale = 1.8, units = "cm", width = 3, height = 10, dpi = 300
  )
showtext::showtext_opts(dpi = 96)


##### Annual dynamics
plot_data <- observed_change_table |>
  add_indicator_names() |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp")) |>
  dplyr::filter(stringr::str_detect(indicator, "indicator_33")) |>
  mutate(month = stringr::str_remove_all(indicator, "indicator_33_mean_month") |> as.integer())

# plot_data |>
#   # group_by(climate_model_name, well_id) |>
#   # group_split() |>
#   # purrr::chuck(1) |>
#   tidyr::pivot_longer(cols = contains("absolute_value")) |>
#   mutate(value = BBmisc::normalize(value)) |>
#   filter(value <= 2) |>
#   # dplyr::group_by(climate_model_name, indicator) |>
#   ggplot() +
#   # geom_line(ggplot2::aes(x = month, y = value)) +
#   ggridges::geom_density_ridges(
#     ggplot2::aes(y = factor(month, levels = as.character(1:12)), x = value, height = ..density..),
#     quantile_lines = TRUE,
#     quantiles = 2,
#     size = .4,
#     alpha = .75,
#     vline_color = "white",
#     # colour = "white",
#     scale = 2.5,
#     show.legend = FALSE
#   ) +
#   coord_flip() +
#   facet_wrap(~name)
#
# plot_data |>
#   group_by(climate_model_name, well_id) |>
#   group_split() |>
#   purrr::chuck(1) |>
#   tidyr::pivot_longer(cols = contains("absolute_value")) |>
#   # dplyr::group_by(climate_model_name, indicator) |>
#   ggplot() +
#   geom_line(ggplot2::aes(x = month, y = value)) +
#   theme_minimal() +
#   facet_wrap(~name, ncol = 1)
#
#
# plot_data |>
#   select(
#     well_id,
#     climate_model_name,
#     indicator, indicator_name,
#     contains("absolute_change"),
#     region_climate
#   ) |>
#   tidyr::pivot_longer(cols = contains("absolute_change")) |>
#   group_by(indicator, name) |>
#   summarise(value = mean(value), .groups = "drop") |>
#   mutate(month = stringr::str_remove(
#     indicator, "indicator_33_mean_month"
#   ) |>
#     factor(levels = as.character(1:12))) |>
#   ggplot(aes(month, value)) +
#   geom_segment(
#     aes(x = month, xend = month, y = 0, yend = value),
#     size = 1, colour = "grey"
#   ) +
#   geom_point(
#     aes(colour = factor(sign(value))),
#     show.legend = FALSE,
#     size = 3
#   ) +
#   scale_colour_manual(values = c("#E26831", "#829D36")) +
#   scale_x_discrete(labels = month.abb) +
#   facet_wrap(~name, ncol = 1) +
#   labs(y = "change of mean GWL [m]") +
#   theme_minimal() +
#   theme(
#     axis.title.x = element_blank(),
#     text = element_text(family = "base_font")
#   )

p1 <- plot_data |>
  select(
    well_id,
    climate_model_name,
    indicator, indicator_name,
    contains("absolute_change"),
    region_natur
  ) |>
  tidyr::pivot_longer(cols = contains("absolute_change"), names_to = "period") |>
  dplyr::mutate(period = period |> stringr::str_remove("absolute_change_z")) |>
  z_to_yearrange_period_names() |>
  group_by(indicator, period, region_natur) |>
  summarise(value = mean(value), region_natur = unique(region_natur), .groups = "drop") |>
  mutate(month = stringr::str_remove(
    indicator, "indicator_33_mean_month"
  ) |>
    factor(levels = as.character(1:12))) |>
  ggplot(aes(month, value)) +
  geom_segment(
    aes(x = month, xend = month, y = 0, yend = value),
    size = 1, colour = "grey"
  ) +
  geom_point(
    aes(colour = factor(sign(value))),
    show.legend = FALSE,
    size = 2.3
  ) +
  scale_colour_manual(values = c("#E26831", "#829D36")) +
  scale_x_discrete(labels = month.abb) +
  facet_grid(region_natur ~ period, labeller = ggplot2::label_wrap_gen(15)) +
  labs(y = "Change of mean GWL [m]") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    text = element_text(family = "base_font"),
    panel.grid.major.x = element_blank(),
    strip.background = ggplot2::element_rect(fill = "grey90", colour = NA),
    strip.text = ggplot2::element_text(
      lineheight = 1.25,
      margin = ggplot2::margin(rep_len(3, 4))
    ),
    strip.text.y = ggplot2::element_text(angle = 0)
  )

showtext::showtext_opts(dpi = 300)
list(p1) |>
  purrr::map2(
    c("data_export/absolute_change_annual_dynamic") |> paste0(".pdf"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "pdf",
      scale = 3, units = "cm", width = 6, height = 4, dpi = 300
    )
  )
showtext::showtext_opts(dpi = 96)


plot_data <- observed_change_table |>
  add_indicator_names() |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp")) |>
  dplyr::filter(stringr::str_detect(indicator, "indicator_33")) |>
  mutate(month = stringr::str_remove_all(indicator, "indicator_33_mean_month") |> as.integer()) |>
  select(
    well_id,
    climate_model_name,
    indicator, indicator_name,
    contains("absolute_value"),
    region_climate
  ) |>
  tidyr::pivot_longer(cols = contains("absolute_value"), names_to = "period") |>
  dplyr::mutate(period = period |> stringr::str_remove("absolute_value_z")) |>
  z_to_yearrange_period_names() |>
  group_by(indicator, period, region_climate) |>
  summarise(value = mean(value), region_climate = unique(region_climate), .groups = "drop") |>
  mutate(month = stringr::str_remove(
    indicator, "indicator_33_mean_month"
  ) |>
    factor(levels = as.character(1:12)))

plot_data_z1 <- indicators_summary_observed |>
  select(
    well_id, climate_model_name, reference_period,
    contains("indicator_33")
  ) |>
  dplyr::filter(
    # stringr::str_detect(indicator, "indicator_33"),
    reference_period == "Z1"
  ) |>
  tidyr::pivot_longer(cols = contains("indicator"), names_to = "indicator", values_to = "value_z1") |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp")) |>
  dplyr::mutate(period = reference_period |> stringr::str_remove("Z")) |>
  select(-reference_period) |>
  z_to_yearrange_period_names() |>
  group_by(indicator, period, region_climate) |>
  summarise(value_z1 = mean(value_z1), region_climate = unique(region_climate), .groups = "drop") |>
  mutate(month = stringr::str_remove(
    indicator, "indicator_33_mean_month"
  ) |>
    factor(levels = as.character(1:12))) |>
  rename(period_z1 = period)

plot_data <- plot_data |>
  left_join(plot_data_z1, by = c("indicator", "region_climate", "month")) |>
  mutate(
    month = stringr::str_pad(month, 2, "left", pad = 0),
    month = lubridate::ymd(paste("2000-", month, "-01"))
  )


p1 <- plot_data |>
  ggplot() +
  # ggh4x::stat_difference(aes(x = month, ymin = value_z1, ymax = value)) +
  geom_line(
    aes(month, value_z1),
    linetype = "dashed",
    colour = "grey"
  ) +
  geom_segment(
    aes(x = month, xend = month, y = value_z1, yend = value),
    size = 1, colour = "grey"
  ) +
  geom_point(
    aes(month, value, colour = value_z1 <= value),
    show.legend = FALSE,
    size = 2
  ) +
  scale_y_continuous(expand = c(.05, .05)) +
  scale_colour_manual(values = c("#E26831", "#829D36")) +
  scale_x_date(date_breaks = "month", labels = scales::label_date(format = "%b")) +
  facet_grid(region_climate ~ period, labeller = ggplot2::label_wrap_gen(15), scales = "free_y") +
  labs(y = "Mean GWL [m]") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    text = element_text(family = "base_font"),
    panel.grid.major.x = element_blank(),
    strip.background = ggplot2::element_rect(fill = "grey90", colour = NA),
    strip.text = ggplot2::element_text(
      lineheight = 1.25,
      margin = ggplot2::margin(rep_len(3, 4))
    ),
    strip.text.y = ggplot2::element_text(angle = 0),
    panel.spacing = unit(7, "mm"),
    panel.grid.minor.x = element_blank()
  )

showtext::showtext_opts(dpi = 300)
list(p1) |>
  purrr::map2(
    c("data_export/absolute_values_annual_dynamic") |> paste0(".pdf"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "pdf",
      scale = 3, units = "cm", width = 6, height = 6, dpi = 300
    )
  )
list(p1) |>
  purrr::map2(
    c("data_export/absolute_values_annual_dynamic") |> paste0(".png"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "png",
      scale = 3, units = "cm", width = 6, height = 6, dpi = 300
    )
  )
showtext::showtext_opts(dpi = 96)



# Start experimental ------------------------------------------------------

###
plot_data <- observed_change_table |>
  add_indicator_names() |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp")) |>
  dplyr::filter(stringr::str_detect(indicator, "indicator_33")) |>
  mutate(month = stringr::str_remove_all(indicator, "indicator_33_mean_month") |> as.integer()) |>
  select(
    well_id,
    climate_model_name,
    indicator, indicator_name,
    contains("absolute_change"),
    region_climate
  ) |>
  tidyr::pivot_longer(cols = contains("absolute_change"), names_to = "period") |>
  dplyr::mutate(period = period |> stringr::str_remove("absolute_change_z")) |>
  z_to_yearrange_period_names() |>
  # group_by(well_id, region_climate) |>
  # mutate(value = value - mean(value)) |>
  group_by(indicator, period, region_climate) |>
  summarise(value = mean(value), region_climate = unique(region_climate), .groups = "drop") |>
  mutate(month = stringr::str_remove(
    indicator, "indicator_33_mean_month"
  ) |>
    factor(levels = as.character(1:12)))

plot_data_z1 <- indicators_summary_observed |>
  select(
    well_id, climate_model_name, reference_period,
    contains("indicator_33")
  ) |>
  dplyr::filter(
    # stringr::str_detect(indicator, "indicator_33"),
    reference_period == "Z1"
  ) |>
  tidyr::pivot_longer(cols = contains("indicator"), names_to = "indicator", values_to = "value_z1") |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp")) |>
  dplyr::mutate(period = reference_period |> stringr::str_remove("Z")) |>
  select(-reference_period) |>
  z_to_yearrange_period_names() |>
  group_by(well_id, region_climate) |>
  mutate(value_z1 = value_z1 - mean(value_z1)) |>
  group_by(indicator, period, region_climate) |>
  summarise(value_z1 = mean(value_z1), region_climate = unique(region_climate), .groups = "drop") |>
  mutate(month = stringr::str_remove(
    indicator, "indicator_33_mean_month"
  ) |>
    factor(levels = as.character(1:12))) |>
  rename(period_z1 = period)

plot_data <- plot_data |>
  left_join(plot_data_z1, by = c("indicator", "region_climate", "month")) |>
  mutate(
    month = stringr::str_pad(month, 2, "left", pad = 0),
    month = lubridate::ymd(paste("2000-", month, "-01"))
  )


p1 <- plot_data |>
  ggplot() +
  # ggh4x::stat_difference(aes(x = month, ymin = value_z1, ymax = value)) +
  geom_line(
    aes(month, value_z1),
    linetype = "dashed",
    colour = "grey"
  ) +
  geom_segment(
    aes(x = month, xend = month, y = value_z1, yend = value + value_z1),
    size = 1, colour = "grey"
  ) +
  geom_point(
    aes(month, value + value_z1, colour = value_z1 <= value + value_z1),
    show.legend = FALSE,
    size = 2
  ) +
  scale_y_continuous(expand = c(.05, .05)) +
  scale_colour_manual(values = c("#E26831", "#829D36")) +
  scale_x_date(date_breaks = "month", labels = scales::label_date(format = "%b")) +
  facet_grid(region_climate ~ period, labeller = ggplot2::label_wrap_gen(15), scales = "free_y") +
  labs(y = "Mean GWL [m]") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    text = element_text(family = "base_font"),
    panel.grid.major.x = element_blank(),
    strip.background = ggplot2::element_rect(fill = "grey90", colour = NA),
    strip.text = ggplot2::element_text(
      lineheight = 1.25,
      margin = ggplot2::margin(rep_len(3, 4))
    ),
    strip.text.y = ggplot2::element_text(angle = 0),
    panel.spacing = unit(7, "mm"),
    panel.grid.minor.x = element_blank()
  )

showtext::showtext_opts(dpi = 300)
list(p1) |>
  purrr::map2(
    c("data_export/absolute_values_annual_dynamic") |> paste0(".pdf"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "pdf",
      scale = 3, units = "cm", width = 6, height = 6, dpi = 300
    )
  )
showtext::showtext_opts(dpi = 96)

####

plot_data <- observed_change_table |>
  add_indicator_names() |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp")) |>
  dplyr::filter(stringr::str_detect(indicator, "indicator_33")) |>
  dplyr::mutate(month = stringr::str_remove_all(indicator, "indicator_33_mean_month") |> as.integer()) |>
  dplyr::select(
    well_id,
    climate_model_name,
    indicator, indicator_name,
    contains("absolute_change"),
    region_natur
  ) |>
  tidyr::pivot_longer(cols = contains("absolute_change"), names_to = "period") |>
  dplyr::mutate(period = period |> stringr::str_remove("absolute_change_z")) |>
  z_to_yearrange_period_names() |>
  # group_by(well_id, region_climate) |>
  # mutate(value = value - mean(value)) |>
  dplyr::group_by(indicator, period, region_natur) |>
  dplyr::summarise(
    value = mean(value),
    region_natur = unique(region_natur),
    n_obs = dplyr::n() / length(unique(period)) / length(unique(climate_model_name)),
    .groups = "drop") |>
  dplyr::mutate(month = stringr::str_remove(
    indicator, "indicator_33_mean_month"
  ) |>
    factor(levels = as.character(1:12)))

plot_data_z1 <- indicators_summary_observed |>
  dplyr::select(
    well_id, climate_model_name, reference_period,
    contains("indicator_33")
  ) |>
  dplyr::filter(
    # stringr::str_detect(indicator, "indicator_33"),
    reference_period == "Z1"
  ) |>
  tidyr::pivot_longer(cols = dplyr::contains("indicator"), names_to = "indicator", values_to = "value_z1") |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp")) |>
  dplyr::mutate(period = reference_period |> stringr::str_remove("Z")) |>
  dplyr::select(-reference_period) |>
  z_to_yearrange_period_names() |>
  dplyr::group_by(well_id, region_natur) |>
  dplyr::mutate(value_z1 = value_z1 - mean(value_z1)) |>
  dplyr::group_by(indicator, period, region_natur) |>
  dplyr::summarise(value_z1 = mean(value_z1), region_natur = unique(region_natur), .groups = "drop") |>
  dplyr::mutate(month = stringr::str_remove(
    indicator, "indicator_33_mean_month"
  ) |>
    factor(levels = as.character(1:12))) |>
  dplyr::rename(period_z1 = period)

plot_data <-
  plot_data |>
  dplyr::left_join(plot_data_z1, by = c("indicator", "region_natur", "month")) |>
  dplyr::mutate(
    month = stringr::str_pad(month, 2, "left", pad = 0),
    month = lubridate::ymd(paste("2000-", month, "-01"))
  ) |>
  dplyr::mutate(
    region_natur = factor(
      region_natur,
      levels = c("Inseln", "Marschen", "Niederungsgebiete", "Geestgebiete", "B\U00F6rden", "Bergland")
    ),
    region_natur = factor(
      stringr::str_glue("{region_natur}<br>n = {n_obs}"),
      levels = c("Inseln<br>n = 2", "Marschen<br>n = 2", "Niederungsgebiete<br>n = 134", "Geestgebiete<br>n = 102", "Börden<br>n = 2", "Bergland<br>n = 4")
    )
)


p1 <- plot_data |>
  ggplot() +
  ggh4x::stat_difference(
    aes(x = month, ymax = value_z1, ymin = value + value_z1),
                         alpha = .7,
    levels = c("Absenkung", "Anstieg")) +
  scale_colour_manual(values = c("#829D36", "#E26831")) +
  ggnewscale::new_scale_colour() +
  geom_line(
    aes(month, value_z1, colour = "Referenzzeitraum"),
    linetype = "dashed"
  ) +
  scale_colour_manual(values = "grey60") +
  ggnewscale::new_scale_colour() +
  geom_line(
    aes(month, value + value_z1, colour = "Zeitraum Vorhersage")
  ) +
  scale_colour_manual(values = "grey30") +
  scale_y_continuous(expand = c(.05, .05)) +
  scale_x_date(date_breaks = "month", labels = scales::label_date(format = "%b")) +
  facet_grid(region_natur ~ period, labeller = ggplot2::label_wrap_gen(15)) +
  labs(y = "Grundwasserstand, normiert [m]") +
  guides(colour = guide_legend(direction = "horizontal")) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(vjust = 4),
    # axis.text.x = element_text(size = 8),
    text = element_text(family = "base_font", size = 20),
    # panel.grid.major.x = element_blank(),
    strip.background = ggplot2::element_rect(fill = "grey90", colour = NA),
    strip.text = ggplot2::element_text(
      lineheight = 1.25,
      margin = ggplot2::margin(rep_len(3, 4))
    ),
    strip.text.y = ggtext::element_markdown(
      angle = 0,
      margin = ggplot2::margin(rep_len(6, 4)), lineheight = 1.15),
    panel.spacing = unit(7, "mm"),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "top",
    plot.margin = margin(0, 1, 0, 1, "cm")
  )

showtext::showtext_opts(dpi = 300)
list(p1) |>
  purrr::map2(
    c("data_export/absolute_values_annual_dynamic_norm_diff") |> paste0(".pdf"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "pdf",
      scale = 3, units = "cm", width = 10, height = 8, dpi = 300
    )
  )
showtext::showtext_opts(dpi = 300)
list(p1) |>
  purrr::map2(
    c("data_export/absolute_values_annual_dynamic_norm_diff") |> paste0(".png"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "png",
      scale = 3, units = "cm", width = 10, height = 8, dpi = 300
    )
  )
showtext::showtext_opts(dpi = 96)

############ Same but with min und max

plot_data <- observed_change_table |>
  add_indicator_names() |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp")) |>
  dplyr::filter(stringr::str_detect(indicator, "indicator_33")) |>
  dplyr::mutate(month = stringr::str_remove_all(indicator, "indicator_33_mean_month") |> as.integer()) |>
  dplyr::select(
    well_id,
    climate_model_name,
    indicator, indicator_name,
    contains("absolute_change"),
    region_natur
  ) |>
  tidyr::pivot_longer(cols = contains("absolute_change"), names_to = "period") |>
  dplyr::mutate(period = period |> stringr::str_remove("absolute_change_z")) |>
  z_to_yearrange_period_names() |>
  # group_by(well_id, region_climate) |>
  # mutate(value = value - mean(value)) |>
  dplyr::group_by(indicator, period, region_natur) |>
  dplyr::summarise(
    value_mean = mean(value),
    value_min = min(value),
    value_max = max(value),
    region_natur = unique(region_natur),
    .groups = "drop"
    ) |>
  dplyr::mutate(month = stringr::str_remove(
    indicator, "indicator_33_mean_month"
  ) |>
    factor(levels = as.character(1:12))) |>
  pivot_longer(cols = contains("value_"))

plot_data_z1 <- indicators_summary_observed |>
  dplyr::select(
    well_id, climate_model_name, reference_period,
    contains("indicator_33")
  ) |>
  dplyr::filter(
    # stringr::str_detect(indicator, "indicator_33"),
    reference_period == "Z1"
  ) |>
  tidyr::pivot_longer(cols = dplyr::contains("indicator"), names_to = "indicator", values_to = "value_z1") |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp")) |>
  dplyr::mutate(period = reference_period |> stringr::str_remove("Z")) |>
  dplyr::select(-reference_period) |>
  z_to_yearrange_period_names() |>
  dplyr::group_by(well_id, region_natur) |>
  dplyr::mutate(value_z1 = value_z1 - mean(value_z1)) |>
  dplyr::group_by(indicator, period, region_natur) |>
  dplyr::summarise(
    value_mean = mean(value_z1),
    value_min = min(value_z1),
    value_max = max(value_z1),
    region_natur = unique(region_natur),
    .groups = "drop"
    ) |>
  dplyr::mutate(month = stringr::str_remove(
    indicator, "indicator_33_mean_month"
  ) |>
    factor(levels = as.character(1:12))) |>
  dplyr::rename(period_z1 = period) |>
  pivot_longer(cols = contains("value"), values_to = "value_z1")

plot_data <- plot_data |>
  dplyr::left_join(plot_data_z1, by = c("indicator", "region_natur", "month", "name")) |>
  dplyr::mutate(
    month = stringr::str_pad(month, 2, "left", pad = 0),
    month = lubridate::ymd(paste("2000-", month, "-01"))
  ) |>
  dplyr::mutate(
    region_natur = factor(
      region_natur,
      levels = c("Inseln", "Marschen", "Niederungsgebiete", "Geestgebiete", "B\U00F6rden", "Bergland")
    )
)


p1 <- plot_data |>
  ggplot(aes(group = name)) +
  ggh4x::stat_difference(
    aes(x = month, ymax = value_z1, ymin = value + value_z1),
                         alpha = .7,
    levels = c("Absenkung", "Anstieg")) +
  scale_colour_manual(values = c("#829D36", "#E26831")) +
  ggnewscale::new_scale_colour() +
  geom_line(
    aes(month, value_z1, colour = "Referenzzeitraum"),
    linetype = "dashed"
  ) +
  scale_colour_manual(values = "grey60") +
  ggnewscale::new_scale_colour() +
  geom_line(
    aes(month, value + value_z1, colour = "Zeitraum Vorhersage")
  ) +
  scale_colour_manual(values = "grey30") +
  scale_y_continuous(expand = c(.05, .05)) +
  scale_x_date(date_breaks = "month", labels = scales::label_date(format = "%b")) +
  facet_grid(region_natur ~ period, labeller = ggplot2::label_wrap_gen(15), scales = "free") +
  labs(y = "Grundwasserstand, normiert [m]") +
  guides(colour = guide_legend(direction = "horizontal")) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    # axis.text.x = element_text(size = 8),
    text = element_text(family = "base_font", size = 20),
    # panel.grid.major.x = element_blank(),
    strip.background = ggplot2::element_rect(fill = "grey90", colour = NA),
    strip.text = ggplot2::element_text(
      lineheight = 1.25,
      margin = ggplot2::margin(rep_len(3, 4))
    ),
    strip.text.y = ggplot2::element_text(angle = 0),
    panel.spacing = unit(7, "mm"),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "top"
  )

showtext::showtext_opts(dpi = 300)
list(p1) |>
  purrr::map2(
    c("data_export/absolute_values_annual_dynamic_norm_diff") |> paste0(".pdf"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "pdf",
      scale = 3, units = "cm", width = 10, height = 8, dpi = 300
    )
  )
showtext::showtext_opts(dpi = 96)

