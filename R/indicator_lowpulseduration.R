#' indicator_lowpulseduration
#'
#' @param date
#' @param thr
#' @param gwl
#'
#' @return
#' @export
#'
#' @examples
indicator_lowpulseduration <- function(date, gwl, thr = 0.2) {
  pulse <- ifelse(gwl < quantile(gwl, thr, na.rm = T),
                  1, 0)
  rlenc <- rle(pulse)
  mean(rlenc$lengths[rlenc$values == 1])
}
