`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.abort_badarg <- function(arg, message) {
  rlang::abort(
    message = paste0("Invalid `", arg, "`: ", message),
    class = "oceancube_bad_argument"
  )
}

.time_timezone <- function(x) {
  zone <- attr(x, "tzone", exact = TRUE)
  if (is.null(zone) || length(zone) == 0L || is.na(zone[[1L]]) ||
      !nzchar(zone[[1L]])) {
    return(NA_character_)
  }
  as.character(zone[[1L]])
}

.as_utc_posixct <- function(x) {
  as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC")
}

.parse_explicit_datetime <- function(x, arg = "time") {
  pattern <- paste0(
    "^([0-9]{4}-[0-9]{2}-[0-9]{2})[T ]",
    "([0-9]{2}):([0-9]{2}):([0-9]{2}(?:\\.[0-9]+)?)",
    "(Z|[+-][0-9]{2}:?[0-9]{2})$"
  )
  parsed <- numeric(length(x))
  offsets <- character(length(x))
  for (i in seq_along(x)) {
    groups <- regmatches(x[[i]], regexec(pattern, x[[i]], perl = TRUE))[[1L]]
    if (length(groups) == 0L) {
      .abort_badarg(
        arg,
        paste(
          "datetime strings must include seconds and an explicit `Z` or",
          "numeric UTC offset, for example `2026-08-13T12:30:00Z`."
        )
      )
    }
    wall_text <- paste0(
      groups[[2L]], " ", groups[[3L]], ":", groups[[4L]], ":", groups[[5L]]
    )
    wall <- suppressWarnings(as.POSIXct(
      strptime(wall_text, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
    ))
    if (is.na(wall)) {
      .abort_badarg(arg, paste0("cannot parse datetime `", x[[i]], "`."))
    }
    offset <- groups[[6L]]
    offset_seconds <- 0
    if (!identical(offset, "Z")) {
      sign <- if (substr(offset, 1L, 1L) == "+") 1 else -1
      digits <- gsub(":", "", substring(offset, 2L), fixed = TRUE)
      hours <- as.integer(substr(digits, 1L, 2L))
      minutes <- as.integer(substr(digits, 3L, 4L))
      if (is.na(hours) || is.na(minutes) || hours > 23L || minutes > 59L) {
        .abort_badarg(arg, paste0("contains an invalid UTC offset in `", x[[i]], "`."))
      }
      offset_seconds <- sign * (hours * 3600 + minutes * 60)
    }
    parsed[[i]] <- as.numeric(wall) - offset_seconds
    offsets[[i]] <- offset
  }
  list(
    values = as.POSIXct(parsed, origin = "1970-01-01", tz = "UTC"),
    offsets = unique(offsets)
  )
}

.validate_time_axis <- function(time, arg = "time", require_canonical_utc = TRUE,
                                abort = TRUE) {
  valid_class <- inherits(time, "Date") || inherits(time, "POSIXct")
  nonempty <- valid_class && is.null(dim(time)) && length(time) > 0L
  numeric_time <- if (nonempty) as.numeric(time) else numeric()
  complete <- nonempty && !anyNA(time) && all(is.finite(numeric_time))
  duplicated_time <- complete && anyDuplicated(numeric_time) > 0L
  increasing <- complete && !duplicated_time &&
    (length(numeric_time) <= 1L || all(diff(numeric_time) > 0))
  timezone_utc <- !inherits(time, "POSIXct") ||
    identical(.time_timezone(time), "UTC")
  result <- list(
    valid_class = valid_class,
    nonempty = nonempty,
    complete = complete,
    duplicated = duplicated_time,
    strictly_increasing = increasing,
    timezone_utc = timezone_utc
  )
  if (isTRUE(abort)) {
    if (!valid_class || !is.null(dim(time))) {
      .abort_badarg(arg, "must be a Date or POSIXct vector.")
    }
    if (!nonempty) .abort_badarg(arg, "must not be empty.")
    if (!complete) .abort_badarg(arg, "must contain only finite, non-missing values.")
    if (duplicated_time) {
      .abort_badarg(
        arg,
        paste(
          "must contain unique values; resolve duplicate coordinates explicitly",
          "without dropping or aggregating aligned data silently."
        )
      )
    }
    if (!increasing) {
      .abort_badarg(
        arg,
        paste(
          "must be strictly increasing; reorder the time axis and aligned data",
          "explicitly rather than sorting coordinates alone."
        )
      )
    }
    if (isTRUE(require_canonical_utc) && !timezone_utc) {
      .abort_badarg(arg, "stored POSIXct values must use the canonical UTC timezone.")
    }
  }
  result
}

.canonicalize_time <- function(time, arg = "time", validate_axis = TRUE) {
  source_class <- if (inherits(time, "Date")) {
    "Date"
  } else if (inherits(time, "POSIXct")) {
    "POSIXct"
  } else if (is.character(time)) {
    "character"
  } else {
    NA_character_
  }
  if (is.na(source_class)) {
    .abort_badarg(arg, "must be Date, POSIXct, or unambiguous ISO character data.")
  }

  source_timezone <- if (inherits(time, "POSIXct")) .time_timezone(time) else NA_character_
  source_offset <- if (inherits(time, "POSIXct") && length(time) > 0L) {
    unique(format(time, "%z"))
  } else {
    character()
  }
  normalization <- "preserved"
  if (inherits(time, "Date")) {
    values <- time
  } else if (inherits(time, "POSIXct")) {
    values <- .as_utc_posixct(time)
    normalization <- "POSIXct instant normalized to UTC"
  } else {
    date_only <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", time)
    explicit_datetime <- grepl(
      "[T ][0-9]{2}:[0-9]{2}:[0-9]{2}(?:\\.[0-9]+)?(?:Z|[+-][0-9]{2}:?[0-9]{2})$",
      time,
      perl = TRUE
    )
    if (all(date_only)) {
      values <- suppressWarnings(as.Date(time, format = "%Y-%m-%d"))
      normalization <- "ISO date character decoded as Date"
    } else if (all(explicit_datetime)) {
      decoded <- .parse_explicit_datetime(time, arg = arg)
      values <- decoded$values
      source_offset <- decoded$offsets
      source_timezone <- "explicit character offset"
      normalization <- "explicit-offset datetime character decoded and normalized to UTC"
    } else if (any(date_only) || any(explicit_datetime)) {
      .abort_badarg(arg, "must not mix date-only and datetime character semantics.")
    } else {
      .abort_badarg(
        arg,
        paste(
          "datetime strings require an explicit `Z` or numeric UTC offset;",
          "supply a timezone/offset or a POSIXct value."
        )
      )
    }
  }
  if (anyNA(values) || any(!is.finite(as.numeric(values)))) {
    .abort_badarg(arg, "contains missing, non-finite, or undecodable values.")
  }
  if (isTRUE(validate_axis)) .validate_time_axis(values, arg = arg)
  list(
    values = values,
    provenance = list(
      canonical_class = if (inherits(values, "Date")) "Date" else "POSIXct",
      canonical_timezone = if (inherits(values, "POSIXct")) "UTC" else NA_character_,
      source_class = source_class,
      source_timezone = source_timezone,
      source_offset = source_offset,
      calendar = "proleptic_gregorian",
      calendar_defaulted = FALSE,
      decoder = "oceancube::.canonicalize_time",
      decode_status = "decoded",
      normalization = normalization
    )
  )
}

.attach_time_provenance <- function(provenance, time_provenance) {
  if (is.null(provenance)) return(list(time = time_provenance))
  if (!is.list(provenance)) {
    return(list(parent = provenance, time = time_provenance))
  }
  existing <- provenance$time %||% .find_time_provenance(provenance)
  provenance$time <- utils::modifyList(
    time_provenance,
    existing %||% list(),
    keep.null = TRUE
  )
  provenance
}

.find_time_provenance <- function(provenance) {
  if (!is.list(provenance)) return(NULL)
  if (!is.null(provenance$schema_version) &&
      is.list(provenance$time) && is.list(provenance$time$current)) {
    current <- provenance$time$current
    return(c(
      list(
        canonical_class = current$class,
        canonical_timezone = current$timezone,
        calendar = current$calendar
      ),
      provenance$time$source %||% list()
    ))
  }
  if (is.list(provenance$time)) return(provenance$time)
  for (name in names(provenance)) {
    found <- .find_time_provenance(provenance[[name]])
    if (!is.null(found)) return(found)
  }
  NULL
}

.parse_cf_origin <- function(origin_text) {
  origin_text <- trimws(origin_text)
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", origin_text)) {
    origin_text <- paste0(origin_text, "T00:00:00Z")
  } else if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}:[0-9]{2}(?:\\.[0-9]+)?$", origin_text, perl = TRUE)) {
    origin_text <- paste0(origin_text, "Z")
  }
  decoded <- tryCatch(
    .parse_explicit_datetime(origin_text, arg = "NetCDF time origin"),
    error = function(error) {
      rlang::abort(
        paste0("Cannot parse NetCDF time origin `", origin_text, "`: ", conditionMessage(error)),
        class = "oceancube_netcdf_schema_error",
        parent = error
      )
    }
  )
  list(value = decoded$values[[1L]], text = origin_text, offset = decoded$offsets[[1L]])
}

.decode_cf_time <- function(raw_values, units, calendar = NA_character_) {
  calendar_defaulted <- length(calendar) != 1L || is.na(calendar) || !nzchar(calendar)
  calendar <- if (calendar_defaulted) "standard" else tolower(trimws(calendar))
  supported <- c("standard", "gregorian", "proleptic_gregorian")
  if (!calendar %in% supported) {
    rlang::abort(
      paste0(
        "Calendar `", calendar, "` is unsupported because R Date/POSIXct cannot ",
        "represent it safely; reinterpretation as Gregorian is not performed."
      ),
      class = "oceancube_netcdf_schema_error"
    )
  }
  if (!is.numeric(raw_values) || anyNA(raw_values) || any(!is.finite(raw_values))) {
    rlang::abort(
      "NetCDF time coordinate must contain finite numeric values.",
      class = "oceancube_netcdf_schema_error"
    )
  }
  pattern <- "^[[:space:]]*(seconds|minutes|hours|days)[[:space:]]+since[[:space:]]+(.+?)[[:space:]]*$"
  if (!is.character(units) || length(units) != 1L || is.na(units)) {
    rlang::abort(
      "Time coordinate units must match '<seconds|minutes|hours|days> since <origin>'.",
      class = "oceancube_netcdf_schema_error"
    )
  }
  groups <- regmatches(units, regexec(pattern, units, ignore.case = TRUE, perl = TRUE))[[1L]]
  if (length(groups) == 0L) {
    rlang::abort(
      "Time coordinate units must match '<seconds|minutes|hours|days> since <origin>'.",
      class = "oceancube_netcdf_schema_error"
    )
  }
  unit <- tolower(groups[[2L]])
  origin <- .parse_cf_origin(groups[[3L]])
  multiplier <- switch(unit, seconds = 1, minutes = 60, hours = 3600, days = 86400)
  decoded <- as.POSIXct(
    as.numeric(origin$value) + as.numeric(raw_values) * multiplier,
    origin = "1970-01-01",
    tz = "UTC"
  )
  if (calendar %in% c("standard", "gregorian") &&
      any(decoded < as.POSIXct("1582-10-15 00:00:00", tz = "UTC"))) {
    rlang::abort(
      paste0(
        "Calendar `", calendar, "` before 1582-10-15 requires mixed ",
        "Julian/Gregorian semantics and is rejected rather than reinterpreted."
      ),
      class = "oceancube_netcdf_schema_error"
    )
  }
  .validate_time_axis(decoded, arg = "NetCDF time coordinate")
  list(
    raw_values = as.numeric(raw_values),
    units = units,
    unit = unit,
    calendar = calendar,
    calendar_defaulted = calendar_defaulted,
    origin = origin$value,
    origin_text = groups[[3L]],
    origin_offset = origin$offset,
    decoded_values = decoded,
    decoder = "oceancube::.decode_cf_time",
    decode_status = "decoded",
    normalization = "CF numeric offsets decoded as UTC POSIXct"
  )
}

.cf_time_provenance <- function(time_descriptor) {
  list(
    canonical_class = "POSIXct",
    canonical_timezone = "UTC",
    source_class = "CF numeric time",
    source_timezone = time_descriptor$origin_offset,
    source_offset = time_descriptor$origin_offset,
    calendar = time_descriptor$calendar,
    source_calendar = time_descriptor$calendar,
    calendar_defaulted = isTRUE(time_descriptor$calendar_defaulted),
    cf_units = time_descriptor$units,
    cf_origin = time_descriptor$origin_text,
    decoder = time_descriptor$decoder,
    decode_status = time_descriptor$decode_status,
    normalization = time_descriptor$normalization
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
  .validate_time_axis(time)

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
    expected_time_class <- if (inherits(x$time, "Date")) "Date" else "POSIXct"
    if (!inherits(extent, expected_time_class) ||
        length(extent) != 2L ||
        anyNA(extent) || any(!is.finite(as.numeric(extent)))) {
      .abort_badarg(
        "temporal_extent",
        paste0("must contain two finite ", expected_time_class, " values matching `time`.")
      )
    }
    if (inherits(extent, "POSIXct") && !identical(.time_timezone(extent), "UTC")) {
      .abort_badarg("temporal_extent", "POSIXct limits must use canonical UTC.")
    }
    if (extent[1] > extent[2]) {
      .abort_badarg("temporal_extent", "must be ordered from start to end.")
    }
    if (min(x$time) < extent[1] || max(x$time) > extent[2]) {
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

.read_cf_time <- function(time_raw, units, calendar = NA_character_) {
  .decode_cf_time(time_raw, units, calendar)$decoded_values
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
