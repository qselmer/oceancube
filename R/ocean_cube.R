#' Construct an oceanographic data cube
#'
#' `ocean_cube()` stores gridded oceanographic data in a standard 5D array:
#' `[longitude, latitude, depth, time, variable]`. Surface fields can be stored
#' with a single depth level set to `NA_real_`.
#'
#' @param lon Non-empty finite numeric vector of longitudes, using either the
#'   `[-180, 180]` or `[0, 360]` convention.
#' @param lat Non-empty finite numeric vector of latitudes in `[-90, 90]`.
#' @param time Non-empty, unique, strictly increasing `Date`, `POSIXct`, or
#'   unambiguous ISO character vector. Civil dates remain `Date`; POSIXct
#'   instants retain sub-day precision and are normalized to UTC. Datetime
#'   strings must include `Z` or an explicit numeric UTC offset.
#' @param data Numeric array. Preferred shape is 5D: `[lon, lat, depth, time, var]`.
#'   A 4D array `[lon, lat, time, var]` is accepted and internally promoted to
#'   a single-depth cube.
#' @param depth Numeric vector of depth levels. If `NULL`, a single surface/no-depth
#'   level is used. A single `NA_real_` represents a surface cube without an
#'   explicit depth coordinate.
#' @param vars Non-empty character vector of unique variable names.
#' @param units Optional character vector or list with one unit per variable.
#'   If named, names must match `vars` uniquely.
#' @param source Optional data source label.
#' @param dataset_id Optional dataset identifier.
#' @param spatial_extent Optional finite
#'   `c(lon_min, lon_max, lat_min, lat_max)` covering the coordinates.
#' @param temporal_extent Optional ordered pair of `Date` or `POSIXct` values
#'   covering `time`.
#' @param depth_extent Optional finite ordered depth range covering `depth`, or
#'   `c(NA_real_, NA_real_)` for a surface cube.
#' @param mask Optional mask object.
#' @param dc Optional distance-to-coast matrix in nautical miles.
#' @param climatology Optional climatology object.
#' @param anomaly Optional anomaly metadata.
#' @param provenance Optional provenance metadata.
#' @param qa Optional QA/QC metadata.
#'
#' @return An object of class `<ocean_cube>`.
#' @export
#'
#' @details
#' The stable dimensional contract is
#' `[longitude, latitude, depth, time, variable]`. For current in-memory cubes:
#'
#' ```
#' dim(data) == c(length(lon), length(lat), length(depth),
#'                length(time), length(vars))
#' ```
#'
#' Coordinate values are preserved in the supplied order; the constructor does
#' not sort, deduplicate, or convert longitude conventions. Time is the
#' exception to otherwise permissive coordinate ordering: it must be unique and
#' strictly increasing so that temporal identity is unambiguous and aligned
#' data are never reordered silently. Positive and negative depth values are
#' accepted without inferring vertical direction. Variable names must be unique.
#'
#' Dimension lengths and axis order form the primary contract. Names attached to
#' `dim(data)` or `dimnames(data)` are descriptive and are not used to determine
#' axis meaning. The current `data` array is retained for compatibility, but the
#' physical storage mechanism is an internal concern that may be represented by
#' another backend in a future version.
#'
#' Canonical temporal provenance records class, timezone, source timezone or
#' offset when known, calendar, decoder, and normalization. Memory inputs use
#' proleptic-Gregorian R semantics. NetCDF calendar policy is applied by
#' [read_nc()] and the lazy backend before this constructor is reached.
#' Vertical positive-direction metadata are not inferred here.
#'
#' @examples
#' lon <- seq(-82, -80, length.out = 3)
#' lat <- seq(-12, -10, length.out = 4)
#' time <- as.Date("2000-01-01") + 0:2
#' data <- array(rnorm(3 * 4 * 1 * 3 * 1), dim = c(3, 4, 1, 3, 1))
#' cube <- ocean_cube(lon = lon, lat = lat, depth = 0, time = time, data = data, vars = "thetao")
#' cube
#' summary(cube)
ocean_cube <- function(lon, lat, time, data, depth = NULL, vars = NULL, units = NULL,
                       source = NULL, dataset_id = NULL, spatial_extent = NULL,
                       temporal_extent = NULL, depth_extent = NULL, mask = NULL,
                       dc = NULL, climatology = NULL, anomaly = NULL,
                       provenance = NULL, qa = NULL) {
  canonical_time <- .canonicalize_time(time)
  time <- canonical_time$values
  if (!is.null(temporal_extent)) {
    temporal_extent <- .canonicalize_time(
      temporal_extent,
      arg = "temporal_extent",
      validate_axis = FALSE
    )$values
    expected_class <- if (inherits(time, "Date")) "Date" else "POSIXct"
    if (!inherits(temporal_extent, expected_class)) {
      .abort_badarg(
        "temporal_extent",
        paste0("must use the same ", expected_class, " semantics as `time`.")
      )
    }
  }

  if (!is.array(data) || !is.numeric(data)) {
    .abort_badarg("data", "must be a numeric array.")
  }

  data_dim <- dim(data)

  if (length(data_dim) == 4L) {
    # Input convention: [lon, lat, time, var]
    data <- array(
      data,
      dim = c(data_dim[1], data_dim[2], 1L, data_dim[3], data_dim[4])
    )
    depth <- depth %||% NA_real_
    data_dim <- dim(data)
  }

  if (length(data_dim) != 5L) {
    .abort_badarg("data", "must have 5 dimensions [lon, lat, depth, time, var], or 4 dimensions [lon, lat, time, var].")
  }

  if (is.null(depth)) {
    depth <- if (data_dim[3] == 1L) NA_real_ else seq_len(data_dim[3])
  }

  if (is.null(vars)) {
    vars <- paste0("var", seq_len(data_dim[5]))
  }

  .check_cube_coordinates(lon, lat, depth, time, vars)
  .check_cube_dimensions(data, lon, lat, depth, time, vars)

  provenance_context <- .provenance_cube_context(
    source = source,
    dataset_id = dataset_id,
    time = time,
    shape = stats::setNames(as.integer(dim(data)), .cube_axis_names()),
    variables = vars,
    backend = "memory",
    provenance = provenance
  )
  if (.provenance_deferred_legacy(provenance)) {
    provenance <- .attach_time_provenance(provenance, canonical_time$provenance)
  } else {
    provenance <- .provenance_normalize(provenance, context = provenance_context)
    provenance <- .provenance_refresh_current(provenance, provenance_context)
  }

  dimnames(data) <- list(
    lon = as.character(lon),
    lat = as.character(lat),
    depth = as.character(depth),
    time = as.character(time),
    var = vars
  )

  spatial_extent <- spatial_extent %||% c(
    lon_min = min(lon, na.rm = TRUE),
    lon_max = max(lon, na.rm = TRUE),
    lat_min = min(lat, na.rm = TRUE),
    lat_max = max(lat, na.rm = TRUE)
  )

  temporal_extent <- temporal_extent %||% range(time, na.rm = TRUE)
  if (is.null(depth_extent)) {
    depth_extent <- if (all(is.na(depth))) {
      c(NA_real_, NA_real_)
    } else {
      range(depth, na.rm = TRUE)
    }
  }

  out <- list(
    lon = lon,
    lat = lat,
    depth = depth,
    time = time,
    vars = vars,
    data = data,
    units = units,
    source = source,
    dataset_id = dataset_id,
    spatial_extent = spatial_extent,
    temporal_extent = temporal_extent,
    depth_extent = depth_extent,
    mask = mask,
    dc = dc,
    climatology = climatology,
    anomaly = anomaly,
    provenance = provenance,
    qa = qa
  )

  class(out) <- c("ocean_cube", "list")
  .check_cube(out)
  out
}

#' @export
print.ocean_cube <- function(x, ...) {
  .check_cube(x)
  cube_shape <- .cube_shape(x)
  depth_range <- if (length(x$depth) == 0L || all(is.na(x$depth))) {
    c(NA_real_, NA_real_)
  } else {
    range(x$depth, na.rm = TRUE)
  }
  cat("<ocean_cube>\n")
  cat("  backend    : ", .cube_backend(x), "\n", sep = "")
  cat("  source     : ", x$source %||% "<unspecified>", "\n", sep = "")
  cat("  dimensions : ", paste(cube_shape, collapse = " x "), " [lon x lat x depth x time x var]\n", sep = "")
  cat("  lon        : ", min(x$lon), " to ", max(x$lon), " (n = ", length(x$lon), ")\n", sep = "")
  cat("  lat        : ", min(x$lat), " to ", max(x$lat), " (n = ", length(x$lat), ")\n", sep = "")
  cat("  depth      : ", paste(depth_range, collapse = " to "), " (n = ", length(x$depth), ")\n", sep = "")
  cat("  time       : ", paste(range(x$time), collapse = " to "), " (n = ", length(x$time), ")\n", sep = "")
  cat("  variables  : ", paste(x$vars, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @export
summary.ocean_cube <- function(object, ...) {
  .check_cube(object)
  depth_range <- if (length(object$depth) == 0L || all(is.na(object$depth))) {
    c(NA_real_, NA_real_)
  } else {
    range(object$depth, na.rm = TRUE)
  }
  out <- data.frame(
    field = c("longitude", "latitude", "depth", "time", "variable"),
    n = c(length(object$lon), length(object$lat), length(object$depth), length(object$time), length(object$vars)),
    min = c(min(object$lon), min(object$lat), depth_range[1], as.character(min(object$time)), NA_character_),
    max = c(max(object$lon), max(object$lat), depth_range[2], as.character(max(object$time)), NA_character_),
    stringsAsFactors = FALSE
  )
  out
}
