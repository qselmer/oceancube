`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.abort_badarg <- function(arg, message) {
  rlang::abort(
    message = paste0("Invalid `", arg, "`: ", message),
    class = "oceancube_bad_argument"
  )
}

.message_info <- function(...) {
  cli::cli_inform(paste0(...))
}

.message_done <- function(...) {
  cli::cli_inform(c("v" = paste0(...)))
}

.check_numeric_vector <- function(x, arg, allow_na = FALSE) {
  if (!is.numeric(x)) {
    .abort_badarg(arg, "must be numeric.")
  }
  if (!allow_na && anyNA(x)) {
    .abort_badarg(arg, "must not contain NA values.")
  }
  invisible(TRUE)
}

.check_range <- function(x, arg) {
  .check_numeric_vector(x, arg)
  if (length(x) != 2L) {
    .abort_badarg(arg, "must have length 2.")
  }
  if (x[1] > x[2]) {
    .abort_badarg(arg, "must be ordered as c(min, max).")
  }
  invisible(TRUE)
}

.check_cube_coordinates <- function(lon, lat, depth, time, vars) {
  .check_numeric_vector(lon, "lon")
  if (!is.null(dim(lon))) {
    .abort_badarg("lon", "must be a vector.")
  }
  if (length(lon) == 0L) {
    .abort_badarg("lon", "must not be empty.")
  }
  if (any(!is.finite(lon))) {
    .abort_badarg("lon", "must contain only finite values.")
  }
  valid_lon_convention <-
    all(lon >= -180 & lon <= 180) ||
    all(lon >= 0 & lon <= 360)
  if (!valid_lon_convention) {
    .abort_badarg("lon", "values must follow either the [-180, 180] or [0, 360] convention.")
  }

  .check_numeric_vector(lat, "lat")
  if (!is.null(dim(lat))) {
    .abort_badarg("lat", "must be a vector.")
  }
  if (length(lat) == 0L) {
    .abort_badarg("lat", "must not be empty.")
  }
  if (any(!is.finite(lat))) {
    .abort_badarg("lat", "must contain only finite values.")
  }
  if (any(lat < -90 | lat > 90)) {
    .abort_badarg("lat", "values must be between -90 and 90.")
  }

  .check_numeric_vector(depth, "depth", allow_na = TRUE)
  if (!is.null(dim(depth))) {
    .abort_badarg("depth", "must be a vector.")
  }
  if (length(depth) == 0L) {
    .abort_badarg("depth", "must not be empty.")
  }
  is_surface_depth <- length(depth) == 1L && is.na(depth) && !is.nan(depth)
  if (!is_surface_depth && any(!is.finite(depth))) {
    .abort_badarg("depth", "must contain finite values, or be a single NA for a surface cube.")
  }

  if (!inherits(time, c("Date", "POSIXct"))) {
    .abort_badarg("time", "must inherit from Date or POSIXct.")
  }
  if (length(time) == 0L) {
    .abort_badarg("time", "must not be empty.")
  }
  if (anyNA(time)) {
    .abort_badarg("time", "must not contain missing values.")
  }

  if (!is.character(vars) || !is.null(dim(vars))) {
    .abort_badarg("vars", "must be a character vector.")
  }
  if (length(vars) == 0L) {
    .abort_badarg("vars", "must contain at least one variable name.")
  }
  if (anyNA(vars) || any(!nzchar(vars))) {
    .abort_badarg("vars", "must contain non-empty, non-missing names.")
  }
  if (anyDuplicated(vars)) {
    .abort_badarg("vars", "must not contain duplicates.")
  }

  invisible(TRUE)
}

.check_cube_dimensions <- function(data, lon, lat, depth, time, vars) {
  if (!is.array(data) || !is.numeric(data)) {
    .abort_badarg("data", "must be a numeric array.")
  }

  actual <- dim(data)
  if (length(actual) != 5L) {
    .abort_badarg(
      "data",
      paste0(
        "must have 5 dimensions [longitude, latitude, depth, time, variable]; obtained ",
        length(actual), "."
      )
    )
  }

  expected <- c(
    length(lon),
    length(lat),
    length(depth),
    length(time),
    length(vars)
  )

  if (!identical(unname(actual), as.integer(expected))) {
    rlang::abort(
      paste0(
        "Invalid cube dimensions: coordinate length must match each data axis; expected [",
        paste(expected, collapse = " x "),
        "] from [longitude, latitude, depth, time, variable], obtained [",
        paste(actual, collapse = " x "),
        "]."
      ),
      class = "oceancube_bad_cube"
    )
  }

  invisible(TRUE)
}

.cube_shape <- function(x) {
  if (!inherits(x, "ocean_cube")) {
    rlang::abort("`x` must be an <ocean_cube> object.", class = "oceancube_bad_cube")
  }
  if (!is.list(x)) {
    rlang::abort("An <ocean_cube> must be a list.", class = "oceancube_bad_cube")
  }

  required <- c("lon", "lat", "depth", "time", "vars")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    rlang::abort(
      paste0("<ocean_cube> header is missing fields: ", paste(missing, collapse = ", ")),
      class = "oceancube_bad_cube"
    )
  }

  .check_cube_coordinates(x$lon, x$lat, x$depth, x$time, x$vars)
  shape <- stats::setNames(
    as.integer(c(
      length(x$lon),
      length(x$lat),
      length(x$depth),
      length(x$time),
      length(x$vars)
    )),
    .cube_axis_names()
  )
  is_netcdf_descriptor <- "storage" %in% names(x) &&
    !"data" %in% names(x)
  if (is_netcdf_descriptor) {
    .validate_netcdf_storage(x$storage, check_file = FALSE)
    if (!identical(shape, x$storage$dimensions$shape)) {
      rlang::abort(
        paste0(
          "Invalid NetCDF cube header shape: expected [",
          paste(x$storage$dimensions$shape, collapse = " x "),
          "] from storage, obtained [",
          paste(shape, collapse = " x "),
          "] from logical coordinates."
        ),
        class = "oceancube_bad_cube"
      )
    }
  }
  shape
}

.check_cube <- function(x) {
  if (!inherits(x, "ocean_cube")) {
    rlang::abort("`x` must be an <ocean_cube> object.", class = "oceancube_bad_cube")
  }
  if (!is.list(x)) {
    rlang::abort("An <ocean_cube> must be a list.", class = "oceancube_bad_cube")
  }
  required <- c("lon", "lat", "depth", "time", "vars")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    rlang::abort(
      paste0("<ocean_cube> is missing fields: ", paste(missing, collapse = ", ")),
      class = "oceancube_bad_cube"
    )
  }

  .check_cube_coordinates(x$lon, x$lat, x$depth, x$time, x$vars)
  backend <- .cube_backend(x)
  if (identical(backend, "memory")) {
    .validate_memory_backend(x)
  } else if (identical(backend, "netcdf")) {
    .validate_netcdf_storage(x$storage, check_file = FALSE)
    expected_shape <- stats::setNames(
      as.integer(c(
        length(x$lon),
        length(x$lat),
        length(x$depth),
        length(x$time),
        length(x$vars)
      )),
      .cube_axis_names()
    )
    if (!identical(expected_shape, x$storage$dimensions$shape)) {
      rlang::abort(
        "Invalid NetCDF cube: logical coordinates do not match the storage shape.",
        class = "oceancube_bad_cube"
      )
    }
  }

  if (!is.null(x$units)) {
    if (!is.character(x$units) && !is.list(x$units)) {
      .abort_badarg("units", "must be NULL, a character vector, or a list.")
    }
    if (length(x$units) != length(x$vars)) {
      .abort_badarg("units", "length must match `vars`.")
    }
    unit_names <- names(x$units)
    if (!is.null(unit_names)) {
      if (anyDuplicated(unit_names)) {
        .abort_badarg("units", "names must be unique.")
      }
      if (anyNA(unit_names) ||
          any(!nzchar(unit_names)) ||
          !setequal(unit_names, x$vars)) {
        .abort_badarg("units", "names must match `vars` exactly.")
      }
    }
  }

  if (!is.null(x$spatial_extent)) {
    extent <- x$spatial_extent
    if (!is.numeric(extent) || length(extent) != 4L || any(!is.finite(extent))) {
      .abort_badarg("spatial_extent", "must contain four finite numeric values.")
    }
    if (extent[1] > extent[2] || extent[3] > extent[4]) {
      .abort_badarg("spatial_extent", "must be ordered as c(lon_min, lon_max, lat_min, lat_max).")
    }
    if (min(x$lon) < extent[1] ||
        max(x$lon) > extent[2] ||
        min(x$lat) < extent[3] ||
        max(x$lat) > extent[4]) {
      .abort_badarg("spatial_extent", "must contain the longitude and latitude coordinates.")
    }
  }

  if (!is.null(x$temporal_extent)) {
    extent <- x$temporal_extent
    if (!inherits(extent, c("Date", "POSIXct")) ||
        length(extent) != 2L ||
        anyNA(extent)) {
      .abort_badarg("temporal_extent", "must contain two non-missing Date or POSIXct values.")
    }
    extent_date <- as.Date(extent)
    time_date <- as.Date(x$time)
    if (extent_date[1] > extent_date[2]) {
      .abort_badarg("temporal_extent", "must be ordered from start to end.")
    }
    if (min(time_date) < extent_date[1] || max(time_date) > extent_date[2]) {
      .abort_badarg("temporal_extent", "must contain the time coordinate.")
    }
  }

  if (!is.null(x$depth_extent)) {
    extent <- x$depth_extent
    is_surface_depth <- length(x$depth) == 1L && is.na(x$depth)
    if (is_surface_depth) {
      if (!is.numeric(extent) || length(extent) != 2L || !all(is.na(extent))) {
        .abort_badarg("depth_extent", "must be c(NA, NA) for a surface cube.")
      }
    } else {
      if (!is.numeric(extent) || length(extent) != 2L || any(!is.finite(extent))) {
        .abort_badarg("depth_extent", "must contain two finite numeric values.")
      }
      if (extent[1] > extent[2]) {
        .abort_badarg("depth_extent", "must be ordered as c(min, max).")
      }
      if (min(x$depth) < extent[1] || max(x$depth) > extent[2]) {
        .abort_badarg("depth_extent", "must contain the depth coordinate.")
      }
    }
  }

  invisible(TRUE)
}

.safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  mean(x)
}

.safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1L) return(NA_real_)
  stats::sd(x)
}

.weighted_mean <- function(x, w) {
  sel <- is.finite(x) & is.finite(w) & w > 0
  if (!any(sel)) return(NA_real_)
  sum(x[sel] * w[sel]) / sum(w[sel])
}

.depth_edges <- function(depth) {
  .check_numeric_vector(depth, "depth")
  if (length(depth) < 2L) {
    .abort_badarg("depth", "must contain at least two vertical levels.")
  }
  mid <- (depth[-1] + depth[-length(depth)]) / 2
  first <- max(0, depth[1] - (mid[1] - depth[1]))
  last <- depth[length(depth)] + (depth[length(depth)] - mid[length(mid)])
  c(first, mid, last)
}

.depth_weights <- function(zmin, zmax, z_edge) {
  pmax(0, pmin(z_edge[-1], zmax) - pmax(z_edge[-length(z_edge)], zmin))
}

.guess_dim <- function(dim_names, candidates, required = TRUE, arg = "dimension") {
  hit <- candidates[candidates %in% dim_names]
  if (length(hit) > 0L) return(hit[1])
  if (isTRUE(required)) {
    rlang::abort(
      paste0("Could not identify ", arg, ". Available dimensions: ", paste(dim_names, collapse = ", "))
    )
  }
  NULL
}

.read_cf_time <- function(time_raw, units, calendar = "gregorian") {
  if (is.null(units) || !grepl("since", units, fixed = TRUE)) {
    rlang::abort("NetCDF time units must follow a '<unit> since <origin>' pattern.")
  }

  origin_txt <- sub(".*since\\s+", "", units)
  origin <- as.POSIXct(origin_txt, tz = "UTC")

  if (is.na(origin)) {
    rlang::abort(paste0("Could not parse NetCDF time origin: ", origin_txt))
  }

  multiplier <- if (grepl("seconds since", units, ignore.case = TRUE)) {
    1
  } else if (grepl("minutes since", units, ignore.case = TRUE)) {
    60
  } else if (grepl("hours since", units, ignore.case = TRUE)) {
    3600
  } else if (grepl("days since", units, ignore.case = TRUE)) {
    86400
  } else {
    rlang::abort(paste0("Unsupported NetCDF time units: ", units))
  }

  if (!tolower(calendar) %in% c("gregorian", "standard", "proleptic_gregorian")) {
    cli::cli_warn("Calendar {.val {calendar}} is not fully supported; treating it as Gregorian.")
  }

  as.Date(origin + time_raw * multiplier)
}

.make_filename <- function(dataset_id, vars, lon = NULL, lat = NULL, time = NULL,
                           depth = NULL, ext = "nc") {
  clean <- function(x) gsub("[^A-Za-z0-9._-]+", "-", x)

  v_str <- paste(vars, collapse = "-")
  lon_str <- if (is.null(lon)) "fullLon" else sprintf("lon%.2f_%.2f", lon[1], lon[2])
  lat_str <- if (is.null(lat)) "fullLat" else sprintf("lat%.2f_%.2f", lat[1], lat[2])
  time_str <- if (is.null(time)) {
    "fullTime"
  } else {
    paste(format(as.Date(time[1]), "%Y%m%d"), format(as.Date(time[2]), "%Y%m%d"), sep = "_")
  }
  depth_str <- if (is.null(depth)) "fullDepth" else sprintf("z%.0f_%.0fm", depth[1], depth[2])

  paste0(clean(paste(dataset_id, v_str, lon_str, lat_str, depth_str, time_str, sep = "_")), ".", ext)
}

.make_provenance <- function(fun, args = list(), extra = list()) {
  list(
    package = "oceancube",
    package_version = tryCatch(as.character(utils::packageVersion("oceancube")), error = function(e) NA_character_),
    r_version = R.version.string,
    platform = R.version$platform,
    system = as.list(Sys.info()),
    date = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    function_name = fun,
    arguments = args,
    extra = extra
  )
}
