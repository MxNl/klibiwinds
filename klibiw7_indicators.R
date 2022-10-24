sysfonts::font_add_google("Abel", "base_font")
showtext::showtext_opts(dpi = 300)
showtext::showtext_auto()

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

data_gwl_ref <- data_gwl |>
  filter_criterion_model_performance() |>
  use_water_year() |>
  add_reference_period_column() |>
  filter_criterion_incomplete_z1_period()

indicators_summary <- data_gwl_ref |>
  make_summary_table() |>
  add_indicators_all(data_gwl_ref)

indicators_summary <- indicators_summary |>
  unnest_indicator_3_3()

indicators_summary_observed <- indicators_summary |>
  split_observed()
indicators_summary_projections <- indicators_summary |>
  split_projections()

projections_change_table <-
  indicators_summary_projections |>
    make_projections_change_table()

observed_change_table <-
  indicators_summary_observed |>
    make_observed_change_table(projections_change_table)

# regions_climate <- sf::read_sf(here::here("data_export", "Klimaregionen_Nds.shp"))
# regions_natur <- sf::read_sf(here::here("data_export", "Naturraum_Reg_DTK50.shp"))
#
# dem_lowersaxony <- regions_climate |>
#   summarise() |>
#   sf::st_cast("POLYGON") |>
#   arrange(-sf::st_area(geometry)) |>
#   slice(1) |>
#   elevatr::get_elev_raster(
#     src = "gl3",
#     z = 8,
#     clip = "locations"
#   )
#
# dem_lowersaxony_stars <- dem_lowersaxony |>
#   stars::st_as_stars()
#
# ggplot() +
#   stars::geom_stars(data = dem_lowersaxony_stars, downsample = 10, interpolate = TRUE) +
#   coord_sf() +
#   scale_fill_gradientn(colours = terrain.colors(10)) +
#   theme_void()

###### Start Plotting
showtext::showtext_opts(dpi = 300)

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

list(p1, p2, p3, p4, p5) |>
  purrr::map2(
    c("data_export/plot_distribution_vs") |> paste(1:5, sep = "_") |> paste0(".pdf"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "pdf",
      scale = 1.7, units = "cm", width = 10, height = 8, dpi = 300
    )
  )


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

list(p1, p2, p3, p4) |>
  purrr::map2(
    c("data_export/plot_timeseries_heatmap") |> paste(1:4, sep = "_") |> paste0(".pdf"),
    ~ .x |> ggplot2::ggsave(
      filename = .y, device = "pdf",
      scale = 1.8, units = "cm", width = 10, height = 10, dpi = 300
    )
  )

# indicators_summary |> # distinct(climate_model_name)
#   group_by(well_id) |>
#   count() |>
#   ungroup() |> #filter(n == 14)
#   mutate(n = as.character(n)) |>
#   ggplot() +
#   aes(n) +
#   geom_histogram(stat = "count")


# indicators_summary |>
#   select(-c(1:3), -indicator_33) |>
#   GGally::ggpairs()

# indicators_summary |>
#   filter(well_id == "100000481") |>
#   ggplot() +
#   aes(reference_period, indicator_11) +
#   geom_smooth(method = "lm") +
#   geom_point()

# Check why Mittlerer Grundwasserstand distribution looks same for all periods and models
plot_data |>
  rename(absolute_value_z1 = observed) |>
  tidyr::pivot_longer(cols = contains("value"), names_to = "period") |>
  mutate(period = period |> str_remove("absolute_value_z")) |>
  group_by(indicator, period, climate_model_name) |>
  filter(value >= median(value) - 2 * sd(value) &
           value <= median(value) + 2 * sd(value)) |>
  ungroup() |>
  z_to_yearrange_period_names() |>
  select(well_id, climate_model_name, name, value, period) |>
  pivot_wider(names_from = period) |>
  filter(name == "Mittlerer Grundwasserstand")

plot_data |>
  rename(absolute_value_z1 = observed) |>
  tidyr::pivot_longer(cols = contains("value"), names_to = "period") |>
  mutate(period = period |> str_remove("absolute_value_z")) |>
  z_to_yearrange_period_names() |>
  ggplot(
    aes(value, period, height = ..density..)
  ) +
  ggridges::geom_density_ridges(
    aes(vline_color = ..quantile. |>fill = as.numeric(period)),
    quantile_lines = TRUE,
    quantiles = 2,
    alpha = .75,
    colour = "white",
    scale = 2.5,
    show.legend = FALSE,
    trim = TRUE
  ) +
  # scale_fill_viridis_c(option = "inferno", direction = -1) +
  scale_discrete_manual("vline_color", values = c("white", "grey")) +
  facet_grid(region ~ name, scales = "free") +
  ggridges::theme_ridges()

plot_data |>
  rename(absolute_value_z1 = observed) |>
  tidyr::pivot_longer(cols = contains("change"), names_to = "period") |>
  mutate(period = period |> str_remove("absolute_change_z")) |>
  z_to_yearrange_period_names() |>
  ggplot(
    aes(value, period, height = ..density..)
  ) +
  ggridges::geom_density_ridges(
    aes(vline_color = ..quantile. |>fill = as.numeric(period)),
    quantile_lines = TRUE,
    quantiles = 2,
    alpha = .75,
    colour = "white",
    scale = 2.5,
    show.legend = FALSE,
    trim = TRUE
  ) +
  # scale_fill_viridis_c(option = "inferno", direction = -1) +
  scale_discrete_manual("vline_color", values = c("white", "grey")) +
  facet_grid(region ~ name, scales = "free") +
  ggridges::theme_ridges()


projected_wells <- indicators_summary |>
  distinct(well_id)

wells_coords <- "D:/Data/students/mariana/data/SHP/gw_sel_int.shp" |>
  sf::read_sf()


####### Match with LBEG wells
lbeg_wells <- tribble(
  ~"well_id", ~"model",
  "9610997", "Sandelermoens",
  "9611193", "Sandelermoens",
  "9843241", "Sandelermoens",
  "9843252", "Sandelermoens",
  "9850270", "Sandelermoens",
  "9850360", "Sandelermoens",
  "9700133", "Voerden Hunteburg",
  "9700134", "Voerden Hunteburg",
  "9700135", "Voerden Hunteburg",
  "9700146", "Voerden Hunteburg",
  "9700218", "Voerden Hunteburg",
  "9700233", "Voerden Hunteburg",
  "9700234", "Voerden Hunteburg",
  "9700269", "Voerden Hunteburg"
)
scores |>
  inner_join(lbeg_wells, by = "well_id") |>
  arrange(-nse, r2) |>
  readr::write_csv2("data_export/bgr_wells_lbeg_match.csv")
####### Match with LBEG wells END

wells_coords |>
  mutate(MEST_ID = as.character(MEST_ID)) |>
  rename(well_id = MEST_ID) |>
  select(well_id) |>
  inner_join(projected_wells, by = "well_id") |>
  inner_join(scores, by = "well_id") |>
  rename_all(str_to_lower) |>
  filter(nse > 0.6 & r2 > 0.7) |>
  sf::write_sf("bgr_projected_wells_336.shp")

indicators_summary |>
  unnest_indicator_3_3() |>
  mutate(across(where(is.numeric), janitor::round_half_up, digits = 2)) |>
  readr::write_csv2("bgr_indicators_normalyear.csv")

indicators_summary <- indicators_summary |>
  unnest_indicator_3_3()

plot_data <- indicators_summary |>
  tidyr::pivot_longer(cols = contains("indicator")) |>
  as_tibble() |>
  group_by(well_id, reference_period, name) |>
  summarise(
    min = min(value),
    mean = mean(value),
    max = max(value),
    .groups = "drop"
  )

plot_data <- wells_coords |>
  mutate(MEST_ID = as.character(MEST_ID)) |>
  rename(well_id = MEST_ID) |>
  select(well_id) |>
  inner_join(plot_data, by = "well_id")
# tidyr::pivot_longer(cols = all_of(c("min", "mean", "max")))

p <- plot_data |>
  filter(name == "indicator_21") |>
  group_by(name) |>
  group_split() |>
  magrittr::extract2(1) |>
  ggplot() +
  geom_sf(aes(colour = mean)) +
  scale_colour_viridis_c() +
  theme_minimal() +
  facet_wrap(~reference_period) +
  labs(title = "Indicator 2.1 (Gesamtsumme der Niedrigstandsmonate)")

p |>
  ggsave(filename = "indicator_21_map.png", device = "png", scale = 2)

####### example 3d heatmap
library(tidyverse)
library(lubridate)
library(rayshader)

plot <- data_gwl_ref |>
  filter(reference_period == "Z3") |>
  group_by(well_id, climate_model_name, reference_period) |>
  group_split() |>
  chuck(34) |>
  ggplot(aes(lubridate::month(date), lubridate::year(date))) +
  geom_tile(aes(fill = gwl), colour = NA) +
  scale_fill_viridis_c(option = "inferno") +
  theme_minimal()

rayshader::plot_gg(plot, width = 5, height = 5, multicore = TRUE, scale = 250,
                   zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800))
Sys.sleep(0.2)
rayshader::render_snapshot(clear = TRUE)



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



p1 |>
  ggplot2::ggsave(
    filename = "data_export/plot_timeseries_heatmap_single.pdf", device = "pdf",
    scale = 1.8, units = "cm", width = 3, height = 10, dpi = 300
  )





data_gwl |>
  filter(year(date) >= 1981) |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp")) |>
  group_by(well_id, climate_model_name) |>
  mutate(gwl = gwl - first(gwl)) |>
  group_by(screen_top, date) |>
  # mutate(gwl = BBmisc::normalize(gwl)) |>
  summarise(gwl = mean(gwl)) |>
  mutate(
    month = date |>
      month |>
      integer_to_monthabbr |>
      factor(levels = month.abb),
    year = year(date) |>
      factor()
  ) |>
  group_by(screen_top) |>
  group_split() |>
  map(~.x |>
    ggplot() +
    aes(month, year, fill = gwl) +
    geom_tile(colour = NA) +
    scale_x_discrete(labels = label_every_nth) +
    scale_y_discrete(labels = label_every_nth) +
    scale_fill_viridis_c(
      guide = guide_colourbar(
        barheight = .4,
        title.position = "top",
        title.hjust = .5
      )
    ) +
    labs(
      title = .x |> slice(1) |> pull(screen_top),
      y = "Year",
      fill = "Gwl (normalized)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = .5),
      legend.position = "top",
      axis.title.x = element_blank()
    )
      # facet_grid(reference_period ~ region_natur, scales = "free")
  ) |>
  patchwork::wrap_plots(nrow = 1)
