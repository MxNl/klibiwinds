use_core_indicators_only <- function (x) {
  x %>%
    filter(core_indicator == TRUE)
}