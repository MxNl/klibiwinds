month_to_water_month <- function(month, start_month = 11) {
  # Year offset
  offset = ifelse(month < start_month, 2, -10)
  # Water month
  month + offset
}