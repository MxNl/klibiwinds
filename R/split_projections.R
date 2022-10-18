split_projections <- function(x) {
  x %>%
    filter(climate_model_name != "observed")
}