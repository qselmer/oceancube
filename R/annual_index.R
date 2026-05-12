#' Compute annual indicators from an ocean cube
#'
#' @param x An `<ocean_cube>` object.
#' @param threshold_pos Optional positive threshold. If `NULL`, fraction above zero is returned.
#' @param threshold_neg Optional negative threshold. If `NULL`, fraction below zero is returned.
#'
#' @return A data frame of annual indicators by variable and depth layer.
#' @export
annual_index <- function(x, threshold_pos = NULL, threshold_neg = NULL) {
  .check_cube(x)

  years <- sort(unique(as.integer(format(x$time, "%Y"))))
  year_vec <- as.integer(format(x$time, "%Y"))
  d <- dim(x$data)

  rows <- vector("list", length(years) * d[3] * d[5])
  ii <- 0L

  for (k in seq_len(d[5])) {
    for (z in seq_len(d[3])) {
      for (yy in years) {
        idx <- which(year_vec == yy)
        vals <- as.vector(x$data[, , z, idx, k, drop = FALSE])
        vals <- vals[is.finite(vals)]

        pos_thr <- threshold_pos %||% 0
        neg_thr <- threshold_neg %||% 0

        ii <- ii + 1L
        rows[[ii]] <- data.frame(
          year = yy,
          var = x$vars[k],
          depth = x$depth[z],
          n = length(vals),
          mean_value = if (length(vals) == 0L) NA_real_ else mean(vals),
          median_value = if (length(vals) == 0L) NA_real_ else stats::median(vals),
          sd_value = if (length(vals) <= 1L) NA_real_ else stats::sd(vals),
          min_value = if (length(vals) == 0L) NA_real_ else min(vals),
          max_value = if (length(vals) == 0L) NA_real_ else max(vals),
          p10 = if (length(vals) == 0L) NA_real_ else as.numeric(stats::quantile(vals, 0.10, names = FALSE)),
          p90 = if (length(vals) == 0L) NA_real_ else as.numeric(stats::quantile(vals, 0.90, names = FALSE)),
          frac_positive = if (length(vals) == 0L) NA_real_ else mean(vals > pos_thr),
          frac_negative = if (length(vals) == 0L) NA_real_ else mean(vals < neg_thr),
          threshold_pos = pos_thr,
          threshold_neg = neg_thr,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  out <- do.call(rbind, rows)
  class(out) <- c("ocean_indicators", class(out))
  out
}
