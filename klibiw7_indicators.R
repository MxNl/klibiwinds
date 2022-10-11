filepath_projections <- "D:/Data/students/mariana/Results/Projections"
filepath_historic <- "J:/PROJEKTE/KliBiW7/Daten/Grundwasserstandsdaten/Einzelmessstellen"

data_gwl <- collect_data(
  filepath_historic,
  filepath_projections
)

data_gwl %>% arrow::write_parquet("data_export/data_gwl.parquet")
data_gwl <- arrow::read_parquet("data_export/data_gwl.parquet")

data_gwl_ref <- data_gwl |> add_reference_period_column()

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

indicators_summary |>
  select(well_id, climate_model_name, reference_period, indicator_33) |>
  unnest_indicator_3_3()


tibble(
  a = rep("a", 30),
  b = c(12, 1, 1, 12, 11, 11, 9, 12, 12, 12, 9, 12, 10, 9, 12, 12, 1, 1, 11, 1, 1, 11, 10, 12, 1, 10, 12, 10, 12, 11)
) %>%
  # mutate(b = circular(b, units = "hours", template = "clock12")) %>%
  group_by(a) %>% #pull(b) %>% plot()
  summarise(mean = as.numeric(psych::circadian.mean(b * 2) / 2))

plopp <- circular(runif(50, circular(0), pi))
mean(plopp)


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
scores %>%
  inner_join(lbeg_wells, by = "well_id") %>%
  arrange(-nse, r2) %>%
  readr::write_csv2("data_export/bgr_wells_lbeg_match.csv")
####### Match with LBEG wells END

wells_coords |>
  mutate(MEST_ID = as.character(MEST_ID)) |>
  rename(well_id = MEST_ID) |>
  select(well_id) |>
  inner_join(projected_wells, by = "well_id") |>
  inner_join(scores, by = "well_id") |>
  rename_all(str_to_lower) |>
  filter(nse > 0.6 & r2 > 0.7) %>%
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
