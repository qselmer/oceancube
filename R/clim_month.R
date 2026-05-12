#' Compute monthly climatology from an ocean cube
#'
#' @param x An `<ocean_cube>` object.
#' @param period Optional base period `c(start, end)`.
#'
#' @return An object of class `<ocean_clim>`.
#' @export
clim_month <- function(x, period = NULL) {
  .check_cube(x)

  if (!is.null(period)) {
    if (length(period) != 2L) .abort_badarg("period", "must have length 2.")
    period <- as.Date(period)
    idx_base <- x$time >= period[1] & x$time <= period[2]
  } else {
    period <- range(x$time)
    idx_base <- rep(TRUE, length(x$time))
  }

  if (!any(idx_base)) {
    rlang::abort("Base period has no observations.")
  }

  mon <- as.integer(format(x$time, "%m"))
  d <- dim(x$data)

  clim_mean <- array(NA_real_, dim = c(d[1], d[2], d[3], 12L, d[5]))
  clim_sd <- array(NA_real_, dim = c(d[1], d[2], d[3], 12L, d[5]))
  clim_n <- array(0L, dim = c(d[1], d[2], d[3], 12L, d[5]))

  for (m in 1:12) {
    idx <- which(mon == m & idx_base)
    if (length(idx) == 0L) next
    sub <- x$data[, , , idx, , drop = FALSE]
    clim_mean[, , , m, ] <- apply(sub, c(1, 2, 3, 5), .safe_mean)
    clim_sd[, , , m, ] <- apply(sub, c(1, 2, 3, 5), .safe_sd)
    clim_n[, , , m, ] <- apply(sub, c(1, 2, 3, 5), function(z) sum(is.finite(z)))
  }

  dimnames(clim_mean) <- list(
    lon = as.character(x$lon),
    lat = as.character(x$lat),
    depth = as.character(x$depth),
    month = sprintf("%02d", 1:12),
    var = x$vars
  )
  dimnames(clim_sd) <- dimnames(clim_mean)
  dimnames(clim_n) <- dimnames(clim_mean)

  out <- list(
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    vars = x$vars,
    period = period,
    mean = clim_mean,
    sd = clim_sd,
    n = clim_n,
    units = x$units,
    source = x$source,
    dataset_id = x$dataset_id,
    provenance = .make_provenance("clim_month", args = list(period = period), extra = list(parent = x$provenance))
  )

  class(out) <- c("ocean_clim", "list")
  out
}

#' @export
print.ocean_clim <- function(x, ...) {
  cat("<ocean_clim> monthly climatology\n")
  cat("  period    : ", paste(x$period, collapse = " to "), "\n", sep = "")
  cat("  variables : ", paste(x$vars, collapse = ", "), "\n", sep = "")
  cat("  dimensions: ", paste(dim(x$mean), collapse = " x "), " [lon x lat x depth x month x var]\n", sep = "")
  invisible(x)
}
