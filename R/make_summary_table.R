make_summary_table <- function(x) {
  x %>%
    distinct(well_id, climate_model_name, reference_period)
}