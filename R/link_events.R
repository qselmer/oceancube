#' Link gridded ocean-cube values to event points
#'
#' `link_events()` extracts nearest-neighbour ocean values from an `<ocean_cube>`
#' at event coordinates and dates. It is intended for linking daily anomalies to
#' fishing sets, lances, survey stations, biological samples, or occurrence records.
#'
#' @param x An `<ocean_cube>` object. This can be a raw variable cube, an absolute
#'   anomaly cube, a z-score cube, or a signal-to-noise cube.
#' @param events A data frame with event coordinates and dates.
#' @param lon_col Name of the longitude column in `events`.
#' @param lat_col Name of the latitude column in `events`.
#' @param date_col Name of the date or datetime column in `events`.
#' @param depth_col Optional name of the depth column in `events`. If `NULL`, the
#'   first depth level in `x` is used.
#' @param vars Optional variables to extract. Defaults to all variables in `x`.
#' @param prefix Optional prefix for extracted columns. If `NULL`, columns are named
#'   `<var>_value`.
#' @param time_tolerance Maximum allowed date mismatch in days. Use `0` for exact
#'   daily matching, or e.g. `1` to allow nearest day within ±1 day.
#' @param keep_grid Logical. If `TRUE`, appends matched grid lon, lat, depth, date,
#'   and time difference.
#'
#' @return The original `events` data frame with extracted ocean variables appended.
#' @export
link_events <- function(x, events, lon_col = "lon", lat_col = "lat", date_col = "date",
                        depth_col = NULL, vars = NULL, prefix = NULL,
                        time_tolerance = 0L, keep_grid = TRUE) {
  # Phase A: validate the cube, event structure, and requested variables.
  .check_cube(x)

  if (!is.data.frame(events)) {
    .abort_badarg("events", "must be a data frame.")
  }

  needed <- c(lon_col, lat_col, date_col)
  missing_cols <- setdiff(needed, names(events))
  if (length(missing_cols) > 0L) {
    rlang::abort(paste0("Missing columns in `events`: ", paste(missing_cols, collapse = ", ")))
  }

  if (!is.null(depth_col) && !depth_col %in% names(events)) {
    rlang::abort(paste0("Missing depth column in `events`: ", depth_col))
  }

  vars <- vars %||% x$vars
  vars <- as.character(vars)

  var_idx <- match(vars, x$vars)
  if (anyNA(var_idx)) {
    rlang::abort(paste0("Variables not found in `x`: ", paste(vars[is.na(var_idx)], collapse = ", ")))
  }

  # Phase B: localize event coordinates as array positions without reading data.
  n <- nrow(events)
  ev_lon <- as.numeric(events[[lon_col]])
  ev_lat <- as.numeric(events[[lat_col]])
  ev_date <- as.Date(events[[date_col]])

  ix <- .nearest_index(x$lon, ev_lon)
  iy <- .nearest_index(x$lat, ev_lat)
  it <- .nearest_date_index(x$time, ev_date, tolerance = time_tolerance)

  if (is.null(depth_col)) {
    iz <- rep(1L, n)
  } else {
    if (all(is.na(x$depth))) {
      rlang::abort("`depth_col` was supplied, but `x` has no valid depth coordinate.")
    }
    iz <- .nearest_index(x$depth, as.numeric(events[[depth_col]]))
  }

  # Phase C: read all requested variables once for each valid event position.
  ok_base <- is.finite(ix) & is.finite(iy) & is.finite(iz) & is.finite(it)
  values <- matrix(NA_real_, nrow = n, ncol = length(vars))

  if (length(vars) > 0L) {
    for (event_index in which(ok_base)) {
      values[event_index, ] <- .read_event_values(
        x,
        longitude_index = ix[event_index],
        latitude_index = iy[event_index],
        depth_index = iz[event_index],
        time_index = it[event_index],
        variable_index = var_idx
      )
    }
  }

  # Phase D: append values and diagnostics while retaining event row order.
  out <- events
  for (k in seq_along(vars)) {
    nm <- if (is.null(prefix)) paste0(vars[k], "_value") else paste(prefix, vars[k], sep = "_")
    out[[nm]] <- values[, k]
  }

  if (isTRUE(keep_grid)) {
    matched_dates <- as.Date(x$time[it])
    out$.oceancube_lon <- x$lon[ix]
    out$.oceancube_lat <- x$lat[iy]
    out$.oceancube_depth <- x$depth[iz]
    out$.oceancube_date <- matched_dates
    out$.oceancube_dt_days <- as.integer(abs(as.Date(ev_date) - matched_dates))
  }

  out
}

.read_event_values <- function(x, longitude_index, latitude_index, depth_index,
                               time_index, variable_index) {
  value_array <- .cube_read(
    x,
    index = list(
      longitude = longitude_index,
      latitude = latitude_index,
      depth = depth_index,
      time = time_index,
      variable = variable_index
    )
  )

  as.vector(value_array)
}

.nearest_index <- function(grid, values) {
  vapply(values, function(v) {
    if (!is.finite(v)) return(NA_integer_)
    which.min(abs(grid - v))
  }, integer(1))
}

.nearest_date_index <- function(time, dates, tolerance = 0L) {
  time <- as.Date(time)
  dates <- as.Date(dates)
  tolerance <- as.integer(tolerance)

  vapply(dates, function(d) {
    if (is.na(d)) return(NA_integer_)
    delta <- abs(as.integer(time - d))
    j <- which.min(delta)
    outside_tolerance <- delta[j] > tolerance
    if (length(outside_tolerance) > 1L) {
      warning(
        sprintf(
          "'length(x) = %d > 1' in coercion to 'logical(1)'",
          length(outside_tolerance)
        ),
        call. = FALSE
      )
      outside_tolerance <- outside_tolerance[[1L]]
    }
    if (length(j) == 0L || !is.finite(delta[j]) || outside_tolerance) {
      return(NA_integer_)
    }
    j
  }, integer(1))
}
