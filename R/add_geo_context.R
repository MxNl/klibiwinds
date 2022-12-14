#' add_geo_context
#'
#' @param x text...
#' @param filepath text...
#' @param as_sf text...
#'
#' @return
#' @export
#'

add_geo_context <- function(x, filepath, as_sf = FALSE) {
  regions_well <- sf::read_sf(filepath) |>
    janitor::clean_names() |>
    dplyr::rename(well_id = mest_id) |>
    dplyr::mutate(well_id = as.character(well_id)) |>
    dplyr::select(well_id, fok, region, klimareg_1, abst_avg) |>
    dplyr::rename(
      screen_top = fok,
      depth_to_gwl = abst_avg,
      region_natur = region,
      region_climate = klimareg_1
    ) |>
    dplyr::mutate(
      screen_top = screen_top |>
        quantile() |>
        purrr::map_dbl(round_nextsignif) |>
        cut(x = screen_top) |>
        stringr::str_remove_all("\\(|\\]") |>
        stringr::str_replace(",", " - ") |>
        stringr::str_c("m"),
      depth_to_gwl = depth_to_gwl |>
        quantile() |>
        purrr::map_dbl(round_nextsignif) |>
        cut(x = depth_to_gwl) |>
        stringr::str_remove_all("\\(|\\]") |>
        stringr::str_replace(",", " - ") |>
        stringr::str_c("m")
    )

  output <- x |>
    dplyr::left_join(regions_well, by = "well_id") |>
    dplyr::filter(!is.na(screen_top)) |>
    sf::st_as_sf()

  if(!as_sf) {
    output <- output |>
      sf::st_drop_geometry()
  }

  return(output)
}
