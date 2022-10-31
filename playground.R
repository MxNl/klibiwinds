######### Plot Maps
showtext::showtext_opts(dpi = 96)

plot_data <- projections_change_table  |>
  add_indicator_names() |>
  use_core_indicators_only() |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp"), as_sf = TRUE) |>

  # dplyr::rename(absolute_value_z1 = observed) |>
  tidyr::pivot_longer(cols = dplyr::contains("change"), names_to = "period") |>
  dplyr::mutate(period = period |> stringr::str_remove("relative_change_z")) |>
  z_to_yearrange_period_names()

plot_data <- observed_change_table  |>
  add_indicator_names() |>
  use_core_indicators_only() |>
  add_geo_context(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp"), as_sf = TRUE) |>
  dplyr::rename(absolute_value_z1 = observed) |>
  tidyr::pivot_longer(cols = dplyr::contains("value"), names_to = "period") |>
  dplyr::mutate(period = period |> stringr::str_remove("absolute_value_z")) |>
  z_to_yearrange_period_names()

regions_climate <- sf::read_sf(here::here("data_export", "Klimaregionen_Nds.shp"))
regions_natur <- sf::read_sf(here::here("data_export", "Naturraum_Reg_DTK50.shp"))

vbreaks <- c(-1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5)

plot_data |>
  dplyr::group_by(indicator) |>
  dplyr::group_split() |>
  purrr::chuck(1) |>
  ggplot(aes(colour = value)) +
  geom_sf(
    data = regions_natur,
    fill = "grey80",
    colour = "black"
  ) +
  geom_sf() +
  coord_sf() +
  facet_grid(period ~ climate_model_name) +
  paletteer::scale_colour_paletteer_binned(
    "scico::vik",
    # guide='coloursteps',
    # breaks=vbreaks,
    guide = ggplot2::guide_colourbar(
      barheight = .3,
      barwidth = 20,
      title.position = "top",
      title.hjust = .5
    )
    # scale_colour_steps2(
    #   low = "red", mid = "white", high = "darkblue",
    #   midpoint = 0,
    #   # guide='coloursteps',
    #   breaks=vbreaks,
    #   guide = ggplot2::guide_colourbar(
    #     barheight = .3,
    #     barwidth = 20,
    #     title.position = "top",
    #     title.hjust = .5
    #   )
  ) +
  theme_void() +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.key.size = unit(2, "line"),
    panel.border = element_blank(),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 14),
    title = element_text(size = 16),
    legend.position = "top"
  )

data_gwl_ref |> dplyr::distinct(well_id)
plot_data |> dplyr::distinct(well_id)

regions_natur |>
  ggplot() +
  geom_sf() +
  geom_sf(data = plot_data |> dplyr::group_by(well_id) |> dplyr::slice(1))

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


plot_data <- data_gwl |>
  add_reference_period_column() |>
  filter_criterion_incomplete_z1_period()

plot_data |>
  dplyr::arrange(-nse, well_id, climate_model_name, reference_period) |>
  dplyr::group_by(well_id, climate_model_name, reference_period) |>
  dplyr::group_split() |>
  purrr::chuck(1) |>
  ggplot(aes(date, gwl)) +
  geom_line()
