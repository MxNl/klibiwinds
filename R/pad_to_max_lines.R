pad_to_max_lines <- function(string, n_lines = 4) {
  n_spaces <- string |> stringr::str_count(" ")
  times_right <- ((n_lines - n_spaces) / 2) |> ceiling() - 1
  times_left <- n_lines - times_right - n_spaces - 1

  paste0(
    rep.int(" ", times = times_left) |> stringr::str_c(collapse = ""),
    string,
    rep.int(" ", times = times_right) |> stringr::str_c(collapse = "")
  ) |>
    stringr::str_replace_all(" ", "<br>")
}
