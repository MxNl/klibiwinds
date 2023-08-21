indicators <-
  dplyr::tibble(
    indicator_id = c(
      "1.1	Grundwasser- tiefststand",
      "1.2	Grundwasser- höchststand",
      "1.3	Mittlerer Grundwasserstand",
      "1.4	Standardabweichung",
      "1.5	Mittelwert der jährlichen Minima",
      "1.6	Mittelwert der jährlichen Maxima",
      "1.7	Mittlere Jahresamplitude",
      "1.8	Gesamtamplitude",
      "2.1	Gesamtsumme der Niedrigstandsmonate",
      "2.2	Gesamtsumme der Hochstandsmonate",
      "2.3	Anzahl der Jahre mit Hochstandssituation",
      "2.4	Anzahl der Jahre mit Niedrigstandssituation",
      "3.1	Mittl. Eintrittszeitpunkt des Jahresminimums",
      "3.2	Mittl. Eintrittszeitpunkt des Jahresmaximums",
      "3.3	Mittlerer Jahresgang"
    )
  ) |>
    tidyr::separate(indicator_id, sep = "\\t", into = c("id", "name")) |>
    # dplyr::mutate(id = as.numeric(id)) |>
    dplyr::mutate(
      class = dplyr::case_when(
        id < 2 ~ "allgemein",
        id < 3 ~ "niedrig und hoechststand",
        id < 4 ~ "saisonalitaet"
      )
    ) |>
    dplyr::mutate(
      indicator = stringr::str_glue("indicator_{id}"),
      indicator = stringr::str_remove(indicator, "\\."),
      .after = id
    ) |>
    dplyr::mutate(core_indicator = dplyr::if_else(id %in% c("1.5", "1.6", "2.1", "2.2", "1.7", "3.1", "3.2"), TRUE, FALSE)) |>
    dplyr::mutate(unit = c("m", "m", "m", "m", "m", "m", "m", "m", "Anzahl", "Anzahl", "Anzahl", "Anzahl", "Monat", "Monat", "m"))

usethis::use_data(indicators, overwrite = TRUE)
