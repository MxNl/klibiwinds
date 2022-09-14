reference_periods <-
  tibble(
    Z1 = 1981:2010,
    Z2 = 2021:2050,
    Z3 = 2071:2100
  ) |>
    pivot_longer(
      names_to = "reference_period",
      values_to = "year",
      cols = everything()
    ) |>
    dplyr::arrange(reference_period)

usethis::use_data(reference_periods, overwrite = TRUE)
