make_summary_table <- function(x) {
  x |>
    dplyr::distinct(well_id, climate_model_name, reference_period)
}