#' Compute absolute anomalies from a climatology
#'
#' @param x An `<ocean_cube>` object.
#' @param clim An `<ocean_clim>` object created by `clim_month()` or `clim_day()`.
#'
#' @return An `<ocean_cube>` with anomaly values `x - climatology`.
#' @export
anom_diff <- function(x, clim) {
  .anom_from_clim(x, clim, method = "difference", signed = TRUE)
}

#' Compute standardized anomalies from a climatology
#'
#' @param x An `<ocean_cube>` object.
#' @param clim An `<ocean_clim>` object created by `clim_month()` or `clim_day()`.
#'
#' @return An `<ocean_cube>` with standardized anomaly values `(x - climatology) / sd_clim`.
#' @export
anom_z <- function(x, clim) {
  .anom_from_clim(x, clim, method = "z_score", signed = TRUE)
}

#' Compute signal-to-noise anomaly magnitude
#'
#' @param x An `<ocean_cube>` object.
#' @param clim An `<ocean_clim>` object created by `clim_month()` or `clim_day()`.
#' @param signed Logical. If `FALSE`, returns absolute signal-to-noise magnitude.
#'
#' @return An `<ocean_cube>` with signal-to-noise values.
#' @export
signal_noise <- function(x, clim, signed = FALSE) {
  .anom_from_clim(x, clim, method = "signal_to_noise", signed = signed)
}

.anom_from_clim <- function(x, clim, method = c("difference", "z_score", "signal_to_noise"),
                            signed = TRUE) {
  .check_cube(x)

  if (!inherits(clim, "ocean_clim")) {
    rlang::abort("`clim` must be an <ocean_clim> object.")
  }

  method <- match.arg(method)

  if (!identical(x$vars, clim$vars)) {
    rlang::abort("`x$vars` and `clim$vars` must be identical and in the same order.")
  }

  if (!identical(length(x$lon), length(clim$lon)) ||
      !identical(length(x$lat), length(clim$lat)) ||
      !identical(length(x$depth), length(clim$depth))) {
    rlang::abort("`x` and `clim` must have compatible lon, lat, and depth dimensions.")
  }

  d <- dim(x$data)
  scale <- clim$scale %||% if (dim(clim$mean)[4] == 12L) "month" else "day"
  idx <- .clim_index(x$time, clim, scale = scale)

  out <- array(NA_real_, dim = d, dimnames = dimnames(x$data))

  for (i in seq_along(x$time)) {
    j <- idx[i]
    if (is.na(j)) next

    obs <- array(
      x$data[, , , i, , drop = FALSE],
      dim = c(d[1], d[2], d[3], d[5])
    )

    mu <- array(
      clim$mean[, , , j, , drop = FALSE],
      dim = c(d[1], d[2], d[3], d[5])
    )

    if (method == "difference") {
      res <- obs - mu
    } else {
      den <- array(
        clim$sd[, , , j, , drop = FALSE],
        dim = c(d[1], d[2], d[3], d[5])
      )
      den[!is.finite(den) | den == 0] <- NA_real_
      res <- (obs - mu) / den

      if (method == "signal_to_noise" && !isTRUE(signed)) {
        res <- abs(res)
      }
    }

    out[, , , i, ] <- res
  }

  anomaly_meta <- switch(
    method,
    difference = list(method = "difference", formula = "x - climatology"),
    z_score = list(method = "z_score", formula = "(x - climatology) / sd_clim"),
    signal_to_noise = if (isTRUE(signed)) {
      list(method = "signed_signal_to_noise", formula = "(x - climatology) / sd_clim")
    } else {
      list(method = "signal_to_noise", formula = "abs(x - climatology) / sd_clim")
    }
  )

  anomaly_meta$climatology_scale <- scale
  anomaly_meta$climatology_period <- clim$period

  ans <- ocean_cube(
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    time = x$time,
    vars = x$vars,
    data = out,
    units = if (method == "difference") x$units else NULL,
    source = x$source,
    dataset_id = x$dataset_id,
    spatial_extent = x$spatial_extent,
    temporal_extent = x$temporal_extent,
    depth_extent = x$depth_extent,
    mask = x$mask,
    dc = x$dc,
    climatology = clim,
    anomaly = anomaly_meta,
    provenance = .make_provenance(
      fun = paste0("anom_", method),
      args = list(method = method, signed = signed),
      extra = list(parent = x$provenance)
    )
  )

  class(ans) <- c("ocean_anom", class(ans))
  ans
}

.clim_index <- function(time, clim, scale = c("month", "day")) {
  scale <- match.arg(scale)

  clim_keys <- dimnames(clim$mean)[[4]]

  if (scale == "month") {
    key <- sprintf("%02d", as.integer(format(as.Date(time), "%m")))
  } else {
    key <- .date_key(as.Date(time), leap = clim$leap %||% "feb28")
  }

  match(key, clim_keys)
}
