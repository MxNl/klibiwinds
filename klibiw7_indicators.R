filepath_projections <- "D:/Data/students/mariana/Results/Projections"
filepath_historic <- "J:/PROJEKTE/KliBiW7/Daten/Grundwasserstandsdaten/Einzelmessstellen"

data_gwl <- collect_data(
  filepath_historic,
  filepath_projections
)

data_gwl |> arrow::write_parquet("data_export/data_gwl.parquet")
data_gwl <- arrow::read_parquet("data_export/data_gwl.parquet")

### Filter for test speed
# data_gwl <- data_gwl %>%
#   group_by(well_id) %>%
#   group_split() %>%
#   magrittr::extract(1:10) %>%
#   reduce(bind_rows)

data_gwl_ref <- data_gwl |>
  use_water_year() %>%
  add_reference_period_column() %>%
  drop_incomplete_z1_period()

## Fake data for climatemodels for Z1
data_gwl_Z1_fake <- data_gwl_ref |>
  filter(reference_period == "Z2") |>
  mutate(
    gwl = gwl +
      0.001 * gwl +
      rnorm(n(), mean = 0, sd = 0.1),
    reference_period = "Z1",
    date = add_with_rollback(date, years(-40))
  )

data_gwl_ref <- data_gwl_ref %>%
  bind_rows(data_gwl_Z1_fake) %>%
  arrange(well_id, climate_model_name, reference_period, date)

### Number of time series starting before 1983
data_gwl_ref |>
  filter(climate_model_name == "observed") %>%
  group_by(well_id) |>
    summarise(min = min(date)) |>
    filter(lubridate::year(min) <= 1983)

# data_gwl_ref |>
#   filter(reference_period == "Z1") |>
#   group_by(well_id) |>
#   count() |>
#   arrange(n) |>
#   ggplot() +
#   aes(n) +
#   geom_histogram() +
#   scale_x_reverse()

indicators_summary <- data_gwl_ref |>
  make_summary_table() |>
  add_indicators_all(data_gwl_ref)

indicators_summary <- indicators_summary |>
  # select(well_id, climate_model_name, reference_period, indicator_33) |>
  unnest_indicator_3_3()

indicators_summary_observed <- indicators_summary %>%
  split_observed()
indicators_summary_projections <- indicators_summary %>%
  split_projections()

projections_change_table <-
  indicators_summary_projections %>%
    make_projections_change_table()

observed_change_table <-
  indicators_summary_observed %>%
    make_observed_change_table(projections_change_table)

regions_well <- sf::read_sf(here::here("data_export", "klibiw7_gwmst_raumzuordnung.shp")) %>%
  janitor::clean_names() %>%
  rename(well_id = mest_id) %>%
  mutate(well_id = as.character(well_id))

regions_climate <- sf::read_sf(here::here("data_export", "Klimaregionen_Nds.shp"))
regions_natur <- sf::read_sf(here::here("data_export", "Naturraum_Reg_DTK50.shp"))

dem_lowersaxony <- regions_climate %>%
  summarise() %>%
  sf::st_cast("POLYGON") %>%
  arrange(-sf::st_area(geometry)) %>%
  slice(1) %>%
  elevatr::get_elev_raster(
    src = "gl3",
    z = 8,
    clip = "locations"
  )

dem_lowersaxony_stars <- dem_lowersaxony %>%
  stars::st_as_stars()

ggplot() +
  geom_stars(data = dem_lowersaxony_stars, downsample = 10, interpolate = TRUE) +
  coord_sf() +
  scale_fill_gradientn(colours = terrain.colors(10)) +
  theme_void()

regions_well %>%
  distinct(region)

regions_well %>%
  ggplot() +
  geom_sf()

###### Start Plotting
plot_data <- observed_change_table %>%
  add_indicator_names() %>%
  use_core_indicators_only() %>%
  left_join(regions_well %>% select(well_id, region), by = "well_id")

plot_data %>%
  pivot_longer(cols = contains("absolute_change"), names_to = "period") %>%
  ggplot(
    aes(value, climate_model_name, height = ..density..)
  ) +
  # ggridges::stat_density_ridges(
  #   aes(fill = 0.5 - abs(0.5 - stat(ecdf))),
  #   geom = "density_ridges_gradient",
  #   calc_ecdf = TRUE,
  #   alpha = .75,
  #   colour = "white"
  # ) +
  ggridges::geom_density_ridges(
    aes(vline_color = ..quantile..),
    quantile_lines = TRUE,
    quantiles = 2,
    fill = "grey",
    alpha = .75,
    colour = "white",
    show.legend = FALSE,
    trim = TRUE
  ) +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  scale_discrete_manual("vline_color", values = c("white", "grey")) +
  facet_grid(period ~ name, scales = "free") +
  ggridges::theme_ridges()

plot_data %>%
  rename(absolute_value_z1 = observed) %>%
  pivot_longer(cols = contains("value"), names_to = "period") %>%
  mutate(period = period %>% str_remove("absolute_value_z")) %>%
  group_by(indicator, period, climate_model_name) %>%
  filter(value >= median(value) - 2 * sd(value) &
           value <= median(value) + 2 * sd(value)) %>%
  ungroup() %>%
  z_to_yearrange_period_names() %>%
  ggplot(
    aes(value, period, height = ..density..)
  ) +
  ggridges::geom_density_ridges(
    aes(vline_color = ..quantile.., fill = as.numeric(period)),
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
  facet_grid(climate_model_name ~ name, scales = "free") +
  ggridges::theme_ridges()


observed_change_table %>%
  rename(absolute_value_z1 = observed) %>%
  add_indicator_names() %>%
  use_core_indicators_only() %>%
  pivot_longer(cols = contains("value"), names_to = "period") %>%
  mutate(period = period %>%
    str_remove("absolute_value_z") %>%
    as.numeric()) %>%
  # filter(well_id == "100000732") %>%
  ggplot(aes(period, value, group = interaction(well_id, climate_model_name))) +
  geom_line() +
  # scale_y_log10() +
  facet_wrap(~name, ncol = 1, scales = "free_y")


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
plot_data %>%
  rename(absolute_value_z1 = observed) %>%
  pivot_longer(cols = contains("value"), names_to = "period") %>%
  mutate(period = period %>% str_remove("absolute_value_z")) %>%
  group_by(indicator, period, climate_model_name) %>%
  filter(value >= median(value) - 2 * sd(value) &
           value <= median(value) + 2 * sd(value)) %>%
  ungroup() %>%
  z_to_yearrange_period_names() %>%
  select(well_id, climate_model_name, name, value, period) %>%
  pivot_wider(names_from = period) %>%
  filter(name == "Mittlerer Grundwasserstand")

plot_data %>%
  rename(absolute_value_z1 = observed) %>%
  pivot_longer(cols = contains("value"), names_to = "period") %>%
  mutate(period = period %>% str_remove("absolute_value_z")) %>%
  z_to_yearrange_period_names() %>%
  ggplot(
    aes(value, period, height = ..density..)
  ) +
  ggridges::geom_density_ridges(
    aes(vline_color = ..quantile.., fill = as.numeric(period)),
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

plot_data %>%
  rename(absolute_value_z1 = observed) %>%
  pivot_longer(cols = contains("change"), names_to = "period") %>%
  mutate(period = period %>% str_remove("absolute_change_z")) %>%
  z_to_yearrange_period_names() %>%
  ggplot(
    aes(value, period, height = ..density..)
  ) +
  ggridges::geom_density_ridges(
    aes(vline_color = ..quantile.., fill = as.numeric(period)),
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

read_txt_and_add_path_as_column <- function(filepath) {
  filepath |>
    readr::read_lines(skip = 3, n_max = 6, locale = readr::locale(encoding = "ISO-8859-1")) |>
    magrittr::extract(c(1, 2)) |>
    tibble::enframe() |>
    dplyr::mutate(
      name = stringr::word(value, end = 1, sep = " "),
      value = stringr::word(value, start = -1, sep = " ") |> as.numeric()
    ) |>
    dplyr::mutate(identifier = filepath) |>
    tidyr::as_tibble()
}

score_files <- "D:/Data/students/mariana/Results/Optimization/PT" |>
  list.files(full.names = TRUE, pattern = "log_summary") |>
  map_dfr(read_txt_and_add_path_as_column)


scores <- score_files |>
  mutate(
    well_id = stringr::word(identifier, start = -1, sep = "_"),
    well_id = stringr::str_remove_all(well_id, ".txt"),
    .before = 1
  ) |>
  select(-identifier) |>
  pivot_wider() |>
  janitor::clean_names()

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
  pivot_longer(cols = contains("indicator")) |>
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
# pivot_longer(cols = all_of(c("min", "mean", "max")))

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
