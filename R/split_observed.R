split_observed <- function(x) {
  x %>%
    filter(climate_model_name == "observed")
}