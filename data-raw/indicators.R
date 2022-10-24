indicators <-
  tibble(
    indicator_id = c(
      "1.1	Grundwassertiefststand",
      "1.2	Grundwasserhoechststand",
      "1.3	Mittlerer Grundwasserstand",
      "1.4	Standardabweichung",
      "1.5	Mittelwert der jaehrlichen Minima",
      "1.6	Mittelwert der jaehrlichen Maxima",
      "1.7	Mittlere Jahresamplitude",
      "1.8	Gesamtamplitude",
      "2.1	Gesamtsumme der Niedrigstandsmonate",
      "2.2	Gesamtsumme der Hochstandsmonate",
      "2.3	Anzahl der Jahre mit Hochstandssituation",
      "2.4	Anzahl der Jahre mit Niedrigstandssituation",
      "3.1	Mittl. Eintrittszeitpunkt des Jahresmaximums",
      "3.2	Mittl. Eintrittszeitpunkt des Jahresminimums",
      "3.3	Mittlerer Jahresgang"
    )
  ) |>
    separate(indicator_id, sep = "\\t", into = c("id", "name")) |>
    # dplyr::mutate(id = as.numeric(id)) |>
    dplyr::mutate(
      class = case_when(
        id < 2 ~ "allgemein",
        id < 3 ~ "niedrig und hoechststand",
        id < 4 ~ "saisonalitaet"
      )
    ) |>
    dplyr::mutate(
      indicator = str_glue("indicator_{id}"),
      indicator = str_remove(indicator, "\\."),
      .after = id
    ) |>
    dplyr::mutate(core_indicator = if_else(id == "1.3" | id == "1.8" | id == "2.4", TRUE, FALSE))

usethis::use_data(indicators, overwrite = TRUE)
