#' Compute daily climatology from an ocean cube
#'
#' `clim_day()` computes a day-of-year climatology from an `<ocean_cube>`.
#' It is intended for daily NetCDF products and event-level linkage, for example
#' matching daily temperature anomalies to fishing sets or survey stations.
#'
#' @param x An `<ocean_cube>` object.
#' @param period Optional base period `c(start, end)` used to compute the climatology.
#'   Values are coerced to `Date`.
#' @param leap How to handle February 29. `"feb28"` maps February 29 to February 28;
#'   `"drop"` removes February 29 from the base climatology and returns `NA` for
#'   February 29 anomalies unless handled externally; `"keep"` keeps February 29
#'   as an additional climatological day.
#' @param min_n Minimum number of finite observations required to compute the
#'   climatological mean and standard deviation for each cell/day/variable.
#'
#' @return An object of class `<ocean_clim>`.
#' @export
clim_day <- function(x, period = NULL, leap = c("feb28", "drop", "keep"), min_n = 1L) {
  .check_cube(x)

  leap <- match.arg(leap)
  min_n <- as.integer(min_n)
  if (length(min_n) != 1L || is.na(min_n) || min_n < 1L) {
    .abort_badarg("min_n", "must be a positive integer.")
  }

  time <- as.Date(x$time)

  if (!is.null(period)) {
    if (length(period) != 2L) .abort_badarg("period", "must have length 2.")
    period <- as.Date(period)
    idx_base <- time >= period[1] & time <= period[2]
  } else {
    period <- range(time, na.rm = TRUE)
    idx_base <- rep(TRUE, length(time))
  }

  if (!any(idx_base)) {
    rlang::abort("Base period has no observations.")
  }

  key <- .date_key(time, leap = leap)
  idx_base <- idx_base & !is.na(key)

  if (!any(idx_base)) {
    rlang::abort("Base period has no valid daily keys after leap-day handling.")
  }

  days <- .daily_keys(leap = leap)
  d <- dim(x$data)

  clim_mean <- array(NA_real_, dim = c(d[1], d[2], d[3], length(days), d[5]))
  clim_sd   <- array(NA_real_, dim = c(d[1], d[2], d[3], length(days), d[5]))
  clim_n    <- array(0L,       dim = c(d[1], d[2], d[3], length(days), d[5]))

  for (j in seq_along(days)) {
    idx <- which(idx_base & key == days[j])
    if (length(idx) == 0L) next

    sub <- x$data[, , , idx, , drop = FALSE]
    n_j <- apply(sub, c(1, 2, 3, 5), function(z) sum(is.finite(z)))

    clim_n[, , , j, ] <- n_j

    mean_j <- apply(sub, c(1, 2, 3, 5), .safe_mean)
    sd_j   <- apply(sub, c(1, 2, 3, 5), .safe_sd)

    mean_j[n_j < min_n] <- NA_real_
    sd_j[n_j < max(2L, min_n)] <- NA_real_

    clim_mean[, , , j, ] <- mean_j
    clim_sd[, , , j, ] <- sd_j
  }

  dn <- list(
    lon = as.character(x$lon),
    lat = as.character(x$lat),
    depth = as.character(x$depth),
    day = days,
    var = x$vars
  )

  dimnames(clim_mean) <- dn
  dimnames(clim_sd) <- dn
  dimnames(clim_n) <- dn

  out <- list(
    scale = "day",
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    vars = x$vars,
    period = period,
    day = days,
    leap = leap,
    min_n = min_n,
    mean = clim_mean,
    sd = clim_sd,
    n = clim_n,
    units = x$units,
    source = x$source,
    dataset_id = x$dataset_id,
    provenance = .make_provenance(
      "clim_day",
      args = list(period = period, leap = leap, min_n = min_n),
      extra = list(parent = x$provenance)
    )
  )

  class(out) <- c("ocean_clim", "list")
  out
}

#' @export
print.ocean_clim <- function(x, ...) {
  scale <- x$scale %||% if (dim(x$mean)[4] == 12L) "month" else "day"
  cat("<ocean_clim> ", scale, " climatology\n", sep = "")
  cat("  period    : ", paste(x$period, collapse = " to "), "\n", sep = "")
  cat("  variables : ", paste(x$vars, collapse = ", "), "\n", sep = "")
  cat("  dimensions: ", paste(dim(x$mean), collapse = " x "),
      " [lon x lat x depth x ", scale, " x var]\n", sep = "")
  invisible(x)
}

.daily_keys <- function(leap = c("feb28", "drop", "keep")) {
  leap <- match.arg(leap)
  if (leap == "keep") {
    format(seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day"), "%m-%d")
  } else {
    format(seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by = "day"), "%m-%d")
  }
}

.date_key <- function(time, leap = c("feb28", "drop", "keep")) {
  leap <- match.arg(leap)
  key <- format(as.Date(time), "%m-%d")

  if (leap == "feb28") {
    key[key == "02-29"] <- "02-28"
  }

  if (leap == "drop") {
    key[key == "02-29"] <- NA_character_
  }

  key
}
