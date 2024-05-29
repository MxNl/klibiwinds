#' indicator_colwell
#'
#' @param data
#' @param date_col
#' @param value_col
#' @param fn
#' @param s
#' @param aggregation
#' @param out
#'
#' @return
#' @export
#'
#' @examples
indicator_colwell <- function(date, gwl, fn = "mean", s = 11, aggregation = "week", out = "contingency") {
  if (!out %in% c("constancy", "contingency", "predictability")) {
    stop("Error: Define valid output metric!")
  }

  date <- enquo(date)
  gwl <- enquo(gwl)

  fn <- match.fun(fn)
  flow_ts <- dplyr::tibble(Date = !!date, Head = !!gwl) |>
    tidytable::as_tidytable() |>
    tidytable::mutate(
      year = year(Date),
      month = month(Date),
      week = isoweek(Date)
    )

  aggregated_flow <- flow_ts |>
    tidytable::group_by(!!sym(aggregation), year) |>
    tidytable::summarize(Head = fn(Head, na.rm = TRUE), .groups = "drop") |>
    tidytable::mutate(flow_class = cut(Head, breaks = s, right = FALSE, include.lowest = TRUE))

  flow_table <- aggregated_flow |>
    tidytable::count(flow_class, !!sym(aggregation)) |>
    tidytable::pivot_wider(names_from = !!sym(aggregation), values_from = n, values_fill = list(n = 0))

  X <- colSums(flow_table[, -1], na.rm = TRUE)
  Y <- rowSums(flow_table[, -1], na.rm = TRUE)
  Z <- sum(flow_table[, -1], na.rm = TRUE)

  entropy <- function(prob) {
    -sum(prob * log(prob), na.rm = TRUE)
  }

  HX <- entropy(X / Z)
  HY <- entropy(Y / Z)
  HXY <- entropy(as.matrix(flow_table[, -1]) / Z)

  P <- round(1 - (HXY - HX) / log(s), 2)
  C <- round(1 - HY / log(s), 2)
  M <- round((HX + HY - HXY) / log(s), 2)

  result <- switch(out,
    constancy = C,
    contingency = M,
    predictability = P
  )

  return(result)
}

.datatable.aware <- TRUE
