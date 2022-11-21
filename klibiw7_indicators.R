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
data_gwl |> readr::write_csv2("data_export/result_gwl_historic_and_projected.csv")

data_gwl_ref <- data_gwl |>
  filter_criterion_model_performance() |>
  use_water_year() |>
  add_reference_period_column() |>
  filter_criterion_incomplete_z1_period()

#Data export Tobias
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


indicators_summary <- data_gwl_ref |>
  make_summary_table() |>
  add_indicators_all(data_gwl_ref)

indicators_summary <- indicators_summary |>
  unnest_indicator_3_3()
indicators_summary |> readr::write_csv2("data_export/result_indicators.csv")

indicators_summary_observed <- indicators_summary |>
  split_observed()
indicators_summary_projections <- indicators_summary |>
  split_projections()

projections_change_table <-
  indicators_summary_projections |>
    make_projections_change_table()

projections_change_table |> readr::write_csv2("data_export/result_relative_changes.csv")

observed_change_table <-
  indicators_summary_observed |>
    make_observed_change_table(projections_change_table)

observed_change_table |> readr::write_csv2("data_export/result_absolute_changes_and_values.csv")

###### Start Plotting
sysfonts::font_add_google("Abel", "base_font")
showtext::showtext_auto()

regions_climate <- sf::read_sf(here::here("data_export", "Klimaregionen_Nds.shp"))
regions_natur <- sf::read_sf(here::here("data_export", "Naturraum_Reg_DTK50.shp"))
regions_natur <- regions_natur |>
  rmapshaper::ms_simplify(keep = 0.015, explode = TRUE, keep_shapes = TRUE) |>
  smoothr::smooth(method = "chaikin")

###### Plot simple histogram
plot_data <- observed_change_table |>
  add_indicator_names() |>
  use_core_indicators_only() |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp"))

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
      scale = 2, units = "cm", width = 8, height = 10, dpi = 300
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
showtext::showtext_opts(dpi = 96)











