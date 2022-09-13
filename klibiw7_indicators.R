filepath <- "D:/Data/students/mariana/Results/Projections"

data_gwl_projections <- collect_projections_data(filepath)

data_gwl_projections <- data_gwl_projections %>%
  add_reference_period_column()

data_gwl_projections_ref <- data_gwl_projections %>%
  drop_na(reference_period)

data_gwl_projections_ref <- data_gwl_projections_ref %>%
  lazy_dt()

tictoc::tic()
indicators_summary <- data_gwl_projections_ref %>%
  make_summary_table() %>%
  add_indicators_all(data_gwl_projections_ref)
tictoc::toc()
indicators_summary
