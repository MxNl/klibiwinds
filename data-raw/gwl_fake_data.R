gwl_fake <- 1:2
climate_model_name_fake <- c("climate_model_1", "climate_model_2", "observed")
well_id_fake <- c(1000:1001) |> as.character()
years_fake <- 1980:2023
months_fake <- 1:12
day_fake <- 15
date_fake <-
  tidyr::expand_grid(
    years_fake,
    months_fake,
    day_fake
  ) |>
    dplyr::mutate(
      date = stringr::str_c(years_fake, months_fake, day_fake, sep = "-"),
    date = lubridate::as_date(date)) |>
    pull(date)

gwl_fake_data <-
  tidyr::expand_grid(
    well_id_fake,
    climate_model_name_fake,
    date_fake
  ) |>
    rename_all(stringr::str_remove_all, pattern = "_fake") |>
    group_by(well_id, climate_model_name) |>
    dplyr::mutate(
      gwl = rep_len(gwl_fake, length.out = dplyr::n()),
      gwl = gwl + cur_group_id() - 1
    ) |>
    ungroup()

usethis::use_data(gwl_fake_data, overwrite = TRUE)
