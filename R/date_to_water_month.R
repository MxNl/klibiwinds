date_to_water_month <- function(dates, start_month = 11) {
  # Convert possible character vector into date
  d1 = as_date(dates)
  month_d1 <- month(d1)
  # Year offset
  offset = ifelse(month_d1 < start_month, 2, -10)
  # Water month
  month_d1 + offset
}