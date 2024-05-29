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
      "2.3	Anzahl der Jahre mit Niedrigstandssituation",
      "2.4	Anzahl der Jahre mit Hochstandssituation",
      "3.1	Mittl. Eintrittszeitpunkt des Jahresminimums",
      "3.2	Mittl. Eintrittszeitpunkt des Jahresmaximums",
      "3.3	Mittlerer Jahresgang",
      "3.4	Standardabw. des Mittl. Eintrittszeitpunktes des Jahresminimums",
      "3.5	Standardabw. des Mittl. Eintrittszeitpunktes des Jahresmaximums",
      "4.2.1.1	Timing der Saisonalität als Colwell's Kontingenz",
      "6.1.3.1	Mittlere Dauer durchgehender Niedrigstände (20. Perzentil)",
      "6.1.3.2	Mittlere Dauer durchgehender Hochstände (80. Perzentil)"
    )
  ) |>
    tidyr::separate(indicator_id, sep = "\\t", into = c("id", "name")) |>
    # dplyr::mutate(id = as.numeric(id)) |>
    dplyr::mutate(
      class = dplyr::case_when(
        stringr::str_starts(id, "1") ~ "allgemein",
        stringr::str_starts(id, "2") ~ "niedrig und hoechststand",
        stringr::str_starts(id, "3") ~ "saisonalitaet",
        stringr::str_starts(id, "4") ~ "struktur",
        stringr::str_starts(id, "5") ~ "verteilung",
        stringr::str_starts(id, "6") ~ "form"
      )
    ) |>
    dplyr::mutate(
      indicator = stringr::str_glue("indicator_{id}"),
      indicator = stringr::str_remove_all(indicator, "\\."),
      .after = id
    ) |>
    dplyr::mutate(core_indicator = dplyr::if_else(id %in% c("1.5", "1.6", "2.1", "2.2", "1.7", "3.1", "3.2"), TRUE, FALSE)) |>
    dplyr::mutate(unit = c("m", "m", "m", "m", "m", "m", "m", "m", "Anzahl", "Anzahl", "Anzahl", "Anzahl", "Monat", "Monat", "m", "Monat", "Monat", "-", "Zeitschritte", "Zeitschritte"))

usethis::use_data(indicators, overwrite = TRUE)
