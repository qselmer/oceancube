#' Compute monthly climatology from an ocean cube
#'
#' `clim_month()` is the compatibility wrapper for
#' `cube_climatology(x, by = "month")`. Since 0.2.0 it first computes one
#' month-year mean and then gives every valid yearly replicate equal weight,
#' rather than pooling every raw observation across years.
#'
#' @param x An `<ocean_cube>` object.
#' @param period Optional closed base period `c(start, end)`. Date and exact UTC
#'   POSIXct semantics follow the source cube.
#'
#' @return An object of class `<ocean_clim>`.
#' @export
clim_month <- function(x, period = NULL) {
  core <- cube_climatology(
    x,
    by = "month",
    period = period,
    min_n = 1L,
    diagnostics = TRUE
  )
  clim_mean <- core$data
  clim_sd <- core$climatology$sd
  clim_n <- core$qa$climatology$n_clim_valid
  monthly_dimnames <- dimnames(clim_mean)
  names(monthly_dimnames)[[4L]] <- "month"
  monthly_dimnames[[4L]] <- core$climatology$group_key
  dimnames(clim_mean) <- monthly_dimnames
  dimnames(clim_sd) <- dimnames(clim_mean)
  dimnames(clim_n) <- dimnames(clim_mean)

  out <- list(
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    vars = x$vars,
    period = core$climatology$effective_period,
    mean = clim_mean,
    sd = clim_sd,
    n = clim_n,
    units = x$units,
    source = x$source,
    dataset_id = x[["dataset_id"]],
    provenance = .make_provenance(
      "clim_month",
      args = list(period = core$climatology$effective_period),
      extra = list(parent = x$provenance, core = core$provenance$cube_climatology)
    )
  )

  class(out) <- c("ocean_clim", "list")
  out
}
