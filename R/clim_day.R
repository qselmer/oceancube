#' Compute daily climatology from an ocean cube
#'
#' `clim_day()` is the compatibility wrapper for
#' `cube_climatology(x, by = "day")`. Since 0.2.0 it first computes one daily
#' replicate per year and then gives every valid yearly replicate equal weight.
#' It is intended for daily NetCDF products and event-level linkage.
#'
#' @param x An `<ocean_cube>` object.
#' @param period Optional closed base period `c(start, end)` used to compute the
#'   climatology. Date and exact UTC POSIXct semantics follow the source cube.
#' @param leap How to handle February 29. `"feb28"` combines the February 28 and
#'   February 29 daily aggregates within each leap year before that year enters
#'   the climatology, preventing leap-year double weighting;
#'   `"drop"` removes February 29 from the base climatology and returns `NA` for
#'   February 29 anomalies unless handled externally; `"keep"` keeps February 29
#'   as an additional climatological day.
#' @param min_n Minimum number of finite yearly daily replicates required to
#'   compute the climatological mean for each cell/day/variable. SD requires at
#'   least two valid replicates.
#'
#' @return An object of class `<ocean_clim>`.
#' @export
clim_day <- function(x, period = NULL, leap = c("feb28", "drop", "keep"), min_n = 1L) {
  leap <- match.arg(leap)
  core <- cube_climatology(
    x,
    by = "day",
    period = period,
    leap = leap,
    min_n = min_n,
    diagnostics = TRUE
  )
  days <- core$climatology$group_key
  dn <- dimnames(core$data)
  names(dn)[[4L]] <- "day"
  dn[[4L]] <- days
  clim_mean <- core$data
  clim_sd <- core$climatology$sd
  clim_n <- core$qa$climatology$n_clim_valid
  dimnames(clim_mean) <- dn
  dimnames(clim_sd) <- dn
  dimnames(clim_n) <- dn

  out <- list(
    scale = "day",
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    vars = x$vars,
    period = core$climatology$effective_period,
    day = days,
    leap = leap,
    min_n = min_n,
    mean = clim_mean,
    sd = clim_sd,
    n = clim_n,
    units = x$units,
    source = x$source,
    dataset_id = x[["dataset_id"]],
    provenance = .make_provenance(
      "clim_day",
      args = list(
        period = core$climatology$effective_period,
        leap = leap,
        min_n = as.integer(min_n)
      ),
      extra = list(parent = x$provenance, core = core$provenance$cube_climatology)
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
