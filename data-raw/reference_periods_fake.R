reference_periods_fake <-
  tibble(
    Z1 = 1981:1982,
    Z2 = 2021:2022
  ) |>
    pivot_longer(
      names_to = "reference_period",
      values_to = "year",
      cols = everything()
    ) |>
    dplyr::arrange(reference_period)

usethis::use_data(reference_periods_fake, overwrite = TRUE)
