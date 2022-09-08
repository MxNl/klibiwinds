filepath <- "D:/Data/students/mariana/Results/Projections"

data_gwl_projections <- collect_projections_data(filepath)

data_gwl_projections <- data_gwl_projections %>%
  add_reference_period_column()

data_gwl_projections_ref <- data_gwl_projections %>%
  drop_na(reference_period)

data_gwl_projections_ref <- data_gwl_projections_ref %>%
  lazy_dt()

tictoc::tic()
indicators <- data_gwl_projections_ref %>%
  distinct(well_id, climate_model_name, reference_period) %>%
  add_indicator_1_1(data_gwl_projections_ref) %>%
  add_indicator_1_2(data_gwl_projections_ref) %>%
  add_indicator_1_3(data_gwl_projections_ref) %>%
  add_indicator_1_4(data_gwl_projections_ref) %>%
  add_indicator_1_5(data_gwl_projections_ref) %>%
  add_indicator_1_6(data_gwl_projections_ref) %>%
  add_indicator_1_7(data_gwl_projections_ref) %>%
  as_tibble()
tictoc::toc()

