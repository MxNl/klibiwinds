filepath_projections <- "D:/Data/students/mariana/Results/Projections"
filepath_historic <- "J:/PROJEKTE/KliBiW7/Daten/Grundwasserstandsdaten/Einzelmessstellen"

data_gwl <- collect_data(
  filepath_historic,
  filepath_projections
)

data_gwl_ref <- data_gwl |> add_reference_period_column()

tictoc::tic()
indicators_summary <- data_gwl_ref |>
  make_summary_table() |>
  add_indicators_all(data_gwl_ref)
tictoc::toc()
indicators_summary

# indicators_summary %>%
#   select(well_id, climate_model_name, reference_period, indicator_33) %>%
#   unnest_indicator_3_3()

indicators_summary %>% # distinct(climate_model_name)
  group_by(well_id) %>%
  count() %>%
  ungroup() %>% #filter(n == 14)
  mutate(n = as.character(n)) %>%
  ggplot() +
  aes(n) +
  geom_histogram(stat = "count")

# 6 * 2 + 1 = 13
