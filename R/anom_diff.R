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

#' Standardized climatological anomaly magnitude
#'
#' @param x An ocean_cube object.
#' @param clim An ocean_clim object created by [clim_month()] or [clim_day()].
#' @param signed A single non-missing logical. If `FALSE`, return the magnitude
#'   of the standardized climatological anomaly. If `TRUE`, retain its sign.
#'
#' @return An ocean_anom compatibility object with dimensionless unit `"1"`.
#'
#' @details Despite its historical name, `signal_noise()` is not a general
#' signal-to-noise ratio estimator. It first obtains the canonical signed
#' standardized anomaly
#' \deqn{z = (x - mean_clim) / sd_clim}
#' through [cube_anomaly()]. The numerator is the deviation from the
#' climatological mean. The denominator is the climatological sample standard
#' deviation across equally weighted day-year or month-year replicates. The
#' default returns `abs(z)`; `signed = TRUE` returns `z` unchanged.
#'
#' Only safe modern `ocean_clim` objects from [clim_day()] or [clim_month()] are
#' accepted. Canonical daily, monthly, or seasonal climatologies should use
#' `cube_anomaly(x, climatology, type = "z")` directly. Zero or non-finite SD
#' produces `NA`, negative finite SD is an error, and every positive finite SD
#' is valid. Exact alignment, leap handling, finite masking, and bounded lazy
#' NetCDF reads are inherited from the canonical anomaly engine.
#' @export
signal_noise <- function(x, clim, signed = FALSE) {
  if (!is.logical(signed) || length(signed) != 1L || is.na(signed)) {
    .abort_badarg("signed", "must be a single non-missing logical value.")
  }
  transformation <- if (signed) "identity" else "absolute_value"
  result <- .legacy_anomaly_wrapper(x, clim, type = "z")
  if (!signed) result$data <- abs(result$data)
  result$anomaly$method <- if (signed) {
    "standardized_anomaly"
  } else {
    "standardized_anomaly_magnitude"
  }
  result$anomaly$formula <- if (signed) {
    "(x - climatology) / sd_clim"
  } else {
    "abs((x - climatology) / sd_clim)"
  }
  signal_metadata <- list(
    base_operation = "standardized_anomaly",
    signed = signed,
    transformation = transformation
  )
  result$qa$signal_noise <- signal_metadata
  result$provenance$signal_noise <- c(
    list(operation = "signal_noise"),
    signal_metadata
  )
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
