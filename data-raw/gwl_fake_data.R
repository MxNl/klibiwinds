gwl_fake <- 1:2
climate_model_name_fake <- c("climate_model_1", "climate_model_2", "observed")
well_id_fake <- c(
  "100000926", "100000930", "100003868", "200000624", "200000628",
  "200000876", "200001376", "200001392", "400080191", "40501871",
  "40502051", "40502062", "405030251", "405030392", "405031060",
  "405031310", "405031641", "405038092", "40504420", "40504710",
  "40507531", "40510500", "500000560", "500000615", "9610863",
  "9700005", "9700030", "9700050", "9700077", "9700094", "9700114",
  "9700165", "9700170", "9700195", "9700227", "9700231", "9700251",
  "9700253", "9700267", "9700277", "9842421", "9850661"
) |> as.character()
years_fake <- 1980:2100
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
    date = lubridate::as_date(date)
  ) |>
  pull(date)

set.seed(23452)
gwl_fake_data <-
  tidyr::expand_grid(
    well_id_fake,
    climate_model_name_fake,
    date_fake
  ) |>
  rename_all(stringr::str_remove_all, pattern = "_fake") |>
  group_by(well_id, climate_model_name) |>
  dplyr::mutate(
    gwl = (sin(seq_len(dplyr::n()) %% 12) / 11) + rnorm(dplyr::n(), mean = 0, sd = .1),
    gwl = gwl + cur_group_id() - 1
  ) |>
  ungroup()

usethis::use_data(gwl_fake_data, overwrite = TRUE)
