#' Compute signed difference anomalies from a legacy climatology
#'
#' @param x An ocean_cube object.
#' @param clim An ocean_clim object created by [clim_month()] or [clim_day()].
#'
#' @return An ocean_anom compatibility object containing x minus climatology.
#'
#' @details This compatibility wrapper adapts a modern ocean_clim and delegates
#' to [cube_anomaly()]. Exact coordinate, variable, unit, calendar, and source
#' time-class validation is enforced. Historical climatologies without
#' sufficient canonical metadata must be recomputed.
#' @export
anom_diff <- function(x, clim) {
  .legacy_anomaly_wrapper(x, clim, type = "difference")
}

#' Compute standardized anomalies from a legacy climatology
#'
#' @param x An ocean_cube object.
#' @param clim An ocean_clim object created by [clim_month()] or [clim_day()].
#'
#' @return An ocean_anom compatibility object containing standardized anomaly
#' values and dimensionless unit "1".
#'
#' @details This compatibility wrapper delegates to [cube_anomaly()]. Zero or
#' non-finite SD produces NA; every finite positive SD is valid. Exact
#' canonical alignment is required, and historical climatologies without the
#' required metadata must be recomputed.
#' @export
anom_z <- function(x, clim) {
  .legacy_anomaly_wrapper(x, clim, type = "z")
}

#' Compute signal-to-noise anomaly magnitude
#'
#' @param x An ocean_cube object.
#' @param clim An ocean_clim object created by [clim_month()] or [clim_day()].
#' @param signed Logical. If FALSE, returns absolute signal-to-noise magnitude.
#'
#' @return An ocean_anom object with signal-to-noise values.
#' @export
signal_noise <- function(x, clim, signed = FALSE) {
  result <- .legacy_anomaly_wrapper(x, clim, type = "z")
  if (!isTRUE(signed)) result$data <- abs(result$data)
  result$anomaly$method <- if (isTRUE(signed)) {
    "signed_signal_to_noise"
  } else {
    "signal_to_noise"
  }
  result$anomaly$formula <- if (isTRUE(signed)) {
    "(x - climatology) / sd_clim"
  } else {
    "abs(x - climatology) / sd_clim"
  }
  result$provenance$signal_noise <- list(signed = isTRUE(signed))
  result
}

.legacy_anomaly_wrapper <- function(x, clim, type = c("difference", "z")) {
  type <- match.arg(type)
  canonical <- .anomaly_adapt_ocean_clim(clim)
  result <- cube_anomaly(x, canonical, type = type)
  result$climatology <- clim
  result$provenance$extra$parent <- x$provenance
  result$anomaly$method <- if (identical(type, "difference")) {
    "difference"
  } else {
    "z_score"
  }
  result$anomaly$formula <- if (identical(type, "difference")) {
    "x - climatology"
  } else {
    "(x - climatology) / sd_clim"
  }
  class(result) <- c("ocean_anom", class(result))
  result
}
