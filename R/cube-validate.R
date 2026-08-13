#' Validate an ocean cube
#'
#' `cube_validate()` diagnoses the logical header, physical backend, metadata,
#' and coordinate contract of an ocean cube. Unlike [cube_inspect()], it
#' returns one row per validation check and never reads a complete NetCDF data
#' payload.
#'
#' @param x An object to validate as an `<ocean_cube>`.
#' @param strict A non-missing logical scalar. If `TRUE`, any failed check
#'   raises an error of class `oceancube_validation_error`; this includes
#'   duplicate or non-increasing time, non-UTC stored POSIXct, unsupported
#'   calendar metadata, and inconsistent temporal provenance. The condition
#'   includes the complete report in its `report` field.
#'
#' @return A data frame with class `oceancube_validation` and columns `check`,
#'   `status`, `severity`, `component`, `message`, `repairable`, and
#'   `suggested_action`.
#'
#' @details Both modes diagnose temporal class, missing/non-finite values,
#'   uniqueness, strict ordering, canonical UTC, supported calendar metadata,
#'   and provenance consistency. `strict = FALSE` returns those failures as
#'   rows; `strict = TRUE` raises after assembling the complete report. No
#'   validation path sorts, deduplicates, or otherwise repairs scientific data.
#' @export
#' @seealso [cube_inspect()], [ocean_cube()]
#'
#' @examples
#' values <- array(1:4, dim = c(2, 2, 1, 1, 1))
#' cube <- ocean_cube(
#'   lon = c(-80, -79), lat = c(-12, -11), depth = 0,
#'   time = as.Date("2020-01-01"), data = values, vars = "temperature"
#' )
#' cube_validate(cube)
cube_validate <- function(x, strict = FALSE) {
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict)) {
    rlang::abort(
      "`strict` must be a single non-missing logical value.",
      class = "oceancube_bad_argument"
    )
  }

  rows <- list()
  add <- function(check, ok, component, pass, fail,
                  warn = FALSE, repairable = FALSE,
                  suggested_action = "No action required.") {
    status <- if (isTRUE(ok)) {
      if (isTRUE(warn)) "WARN" else "PASS"
    } else {
      "FAIL"
    }
    rows[[length(rows) + 1L]] <<- data.frame(
      check = check,
      status = status,
      severity = switch(status, PASS = "info", WARN = "warning", FAIL = "error"),
      component = component,
      message = if (isTRUE(ok)) pass else fail,
      repairable = isTRUE(repairable),
      suggested_action = if (isTRUE(ok) && !isTRUE(warn)) {
        "No action required."
      } else {
        suggested_action
      },
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }
  has <- function(name) is.list(x) && name %in% names(x)
  value <- function(name) if (has(name)) x[[name]] else NULL
  scalar_label <- function(value, fallback) {
    if (is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)) {
      value
    } else {
      fallback
    }
  }

  class_ok <- inherits(x, "ocean_cube")
  list_ok <- is.list(x)
  add(
    "class", class_ok, "object",
    "Object inherits from <ocean_cube>.",
    "Object does not inherit from <ocean_cube>.",
    repairable = TRUE,
    suggested_action = "Construct the object with ocean_cube() or a supported backend constructor."
  )
  add(
    "list", list_ok, "object",
    "Object uses list storage.",
    "An <ocean_cube> must be a list.",
    repairable = FALSE,
    suggested_action = "Reconstruct the object as a valid ocean_cube."
  )

  required <- c("lon", "lat", "depth", "time", "vars")
  missing_required <- if (list_ok) setdiff(required, names(x)) else required
  required_ok <- list_ok && length(missing_required) == 0L
  add(
    "required_components", required_ok, "header",
    "All required logical components are present.",
    paste0("Missing required component(s): ", paste(missing_required, collapse = ", "), "."),
    repairable = TRUE,
    suggested_action = "Restore lon, lat, depth, time, and vars from a trustworthy source."
  )

  lon <- value("lon")
  lat <- value("lat")
  depth <- value("depth")
  time <- value("time")
  vars <- value("vars")
  coordinates_typed <- required_ok &&
    is.numeric(lon) && is.null(dim(lon)) &&
    is.numeric(lat) && is.null(dim(lat)) &&
    is.numeric(depth) && is.null(dim(depth)) &&
    inherits(time, c("Date", "POSIXct")) &&
    is.character(vars) && is.null(dim(vars))
  add(
    "coordinate_types", coordinates_typed, "coordinates",
    "Coordinate and variable vectors use supported types.",
    "One or more coordinate or variable vectors use an unsupported type.",
    repairable = TRUE,
    suggested_action = "Use numeric lon, lat, and depth vectors; Date/POSIXct time; and character vars."
  )

  axes_nonempty <- required_ok && all(vapply(
    list(lon, lat, depth, time, vars), length, integer(1)
  ) > 0L)
  add(
    "nonempty_axes", axes_nonempty, "coordinates",
    "All logical axes are non-empty.",
    "At least one logical axis is empty or absent.",
    repairable = TRUE,
    suggested_action = "Supply at least one value on every canonical axis."
  )

  lon_numeric <- is.numeric(lon) && is.null(dim(lon)) && length(lon) > 0L
  lon_finite <- lon_numeric && all(is.finite(lon))
  add(
    "longitude_finite", lon_finite, "lon",
    "Longitude values are finite.",
    "Longitude must be a non-empty finite numeric vector.",
    repairable = TRUE,
    suggested_action = "Remove or repair missing and non-finite longitude values."
  )
  lon_bounds <- lon_finite &&
    (all(lon >= -180 & lon <= 180) || all(lon >= 0 & lon <= 360))
  add(
    "longitude_bounds", lon_bounds, "lon",
    "Longitude follows a supported [-180, 180] or [0, 360] convention.",
    "Longitude values do not fit one supported domain convention.",
    repairable = TRUE,
    suggested_action = "Normalize all longitudes to one supported convention."
  )

  lat_numeric <- is.numeric(lat) && is.null(dim(lat)) && length(lat) > 0L
  lat_finite <- lat_numeric && all(is.finite(lat))
  add(
    "latitude_finite", lat_finite, "lat",
    "Latitude values are finite.",
    "Latitude must be a non-empty finite numeric vector.",
    repairable = TRUE,
    suggested_action = "Remove or repair missing and non-finite latitude values."
  )
  lat_bounds <- lat_finite && all(lat >= -90 & lat <= 90)
  add(
    "latitude_bounds", lat_bounds, "lat",
    "Latitude values are within [-90, 90].",
    "Latitude values fall outside [-90, 90].",
    repairable = TRUE,
    suggested_action = "Correct latitude values to the geographic domain."
  )

  time_class <- inherits(time, c("Date", "POSIXct")) && length(time) > 0L
  add(
    "time_class", time_class, "time",
    "Time inherits from Date or POSIXct.",
    "Time must be a non-empty Date or POSIXct vector.",
    repairable = TRUE,
    suggested_action = "Decode time explicitly to Date or POSIXct."
  )
  time_complete <- time_class && !anyNA(time)
  add(
    "time_missing", time_complete, "time",
    "Time contains no missing values.",
    "Time contains missing or undecodable values.",
    repairable = TRUE,
    suggested_action = "Repair or remove missing time coordinates."
  )
  time_numeric <- if (time_complete) as.numeric(time) else numeric()
  time_finite <- time_complete && all(is.finite(time_numeric))
  add(
    "time_finite", time_finite, "time",
    "Time contains only finite values.",
    "Time contains non-finite values.",
    repairable = TRUE,
    suggested_action = "Repair or remove non-finite time coordinates explicitly."
  )
  time_unique <- time_finite && anyDuplicated(time_numeric) == 0L
  add(
    "time_unique", time_unique, "time",
    "Time coordinates are unique.",
    "Time contains duplicate coordinates.",
    repairable = TRUE,
    suggested_action = paste(
      "Resolve duplicate coordinates and aligned observations explicitly;",
      "do not silently deduplicate or aggregate."
    )
  )
  time_increasing <- time_unique &&
    (length(time_numeric) <= 1L || all(diff(time_numeric) > 0))
  add(
    "time_strictly_increasing", time_increasing, "time",
    "Time coordinates are strictly increasing.",
    "Time coordinates are not strictly increasing.",
    repairable = TRUE,
    suggested_action = paste(
      "Reorder the time axis and aligned data explicitly;",
      "do not sort the coordinate alone."
    )
  )
  time_timezone_ok <- time_class &&
    (!inherits(time, "POSIXct") || identical(.time_timezone(time), "UTC"))
  add(
    "time_timezone", time_timezone_ok, "time",
    if (inherits(time, "POSIXct")) "Stored POSIXct time uses canonical UTC." else "Date time has no timezone semantics.",
    "Stored POSIXct time must use canonical UTC.",
    repairable = TRUE,
    suggested_action = "Normalize POSIXct instants to UTC without changing their numeric epoch values."
  )

  time_provenance <- .find_time_provenance(value("provenance"))
  supported_calendars <- c("standard", "gregorian", "proleptic_gregorian")
  time_calendar <- if (is.list(time_provenance)) time_provenance$calendar else NULL
  calendar_ok <- is.character(time_calendar) && length(time_calendar) == 1L &&
    !is.na(time_calendar) && time_calendar %in% supported_calendars
  add(
    "time_calendar", calendar_ok, "time",
    paste0("Time calendar `", time_calendar, "` is supported."),
    if (is.null(time_calendar)) {
      "Canonical time calendar metadata are missing."
    } else {
      paste0("Time calendar `", time_calendar, "` is unsupported or invalid.")
    },
    repairable = TRUE,
    suggested_action = "Restore supported calendar provenance; never reinterpret an unsupported calendar as Gregorian."
  )
  expected_class <- if (inherits(time, "Date")) "Date" else if (inherits(time, "POSIXct")) "POSIXct" else NA_character_
  provenance_consistent <- calendar_ok &&
    identical(time_provenance$canonical_class, expected_class) &&
    (!identical(expected_class, "POSIXct") ||
       identical(time_provenance$canonical_timezone, "UTC"))
  add(
    "time_provenance", provenance_consistent, "provenance",
    "Temporal provenance matches the canonical time class and timezone.",
    "Temporal provenance is missing or inconsistent with the stored time axis.",
    repairable = TRUE,
    suggested_action = "Reconstruct temporal provenance from a trustworthy source descriptor."
  )

  depth_numeric <- is.numeric(depth) && is.null(dim(depth)) && length(depth) > 0L
  surface_depth <- depth_numeric && length(depth) == 1L &&
    is.na(depth) && !is.nan(depth)
  depth_ok <- depth_numeric && (surface_depth || all(is.finite(depth)))
  add(
    "depth", depth_ok, "depth",
    if (surface_depth) "Depth is a valid surface sentinel." else "Depth values are finite.",
    "Depth must be finite or a single NA surface sentinel.",
    repairable = TRUE,
    suggested_action = "Use finite depth levels or one NA_real_ value for a surface cube."
  )

  vars_ok <- is.character(vars) && is.null(dim(vars)) && length(vars) > 0L &&
    !anyNA(vars) && all(nzchar(vars)) && !anyDuplicated(vars)
  add(
    "variables", vars_ok, "vars",
    "Variable names are non-empty and unique.",
    "Variable names must be non-empty, non-missing, and unique.",
    repairable = TRUE,
    suggested_action = "Provide a unique non-empty name for every variable."
  )

  backend <- NA_character_
  backend_detail <- ""
  if (list_ok && has("data") && is.array(value("data"))) {
    backend <- "memory"
  } else if (list_ok && !has("data") && is.list(value("storage")) &&
             identical(value("storage")$backend, "netcdf")) {
    backend <- "netcdf"
  } else if (list_ok && has("data")) {
    backend_detail <- "The data component is not an array."
  } else if (list_ok && has("storage")) {
    declared_backend <- if (is.list(value("storage"))) {
      value("storage")$backend
    } else {
      NULL
    }
    backend_detail <- paste0(
      "The storage descriptor declares backend '",
      scalar_label(declared_backend, "unknown"), "'."
    )
  } else {
    backend_detail <- "Neither data nor a storage descriptor is present."
  }
  backend_ok <- !is.na(backend) && backend %in% c("memory", "netcdf")
  add(
    "backend", backend_ok, "storage",
    paste0("Recognized ", backend, " backend."),
    paste0("No recognized backend. ", backend_detail),
    repairable = FALSE,
    suggested_action = "Reconstruct storage with the memory or NetCDF backend."
  )

  logical_shape <- NULL
  logical_shape_ok <- required_ok && axes_nonempty
  if (logical_shape_ok) {
    logical_shape <- stats::setNames(
      as.integer(c(length(lon), length(lat), length(depth), length(time), length(vars))),
      c("longitude", "latitude", "depth", "time", "variable")
    )
  }
  add(
    "logical_shape", logical_shape_ok, "header",
    paste0("Logical shape is [", paste(logical_shape, collapse = " x "), "]."),
    "Logical shape cannot be derived from the header.",
    repairable = TRUE,
    suggested_action = "Restore complete non-empty canonical coordinates."
  )

  storage_shape <- NULL
  physical_ok <- FALSE
  physical_message <- "Physical storage shape is unavailable."
  if (identical(backend, "memory")) {
    data <- value("data")
    physical_ok <- is.numeric(data) && length(dim(data)) == 5L
    if (physical_ok) {
      storage_shape <- stats::setNames(
        as.integer(dim(data)),
        c("longitude", "latitude", "depth", "time", "variable")
      )
      physical_message <- paste0(
        "Memory storage shape is [", paste(storage_shape, collapse = " x "), "]."
      )
    } else {
      physical_message <- "Memory storage must be a numeric five-dimensional array."
    }
  } else if (identical(backend, "netcdf")) {
    descriptor_error <- NULL
    descriptor_ok <- tryCatch(
      {
        .validate_netcdf_storage(value("storage"), check_file = FALSE)
        TRUE
      },
      error = function(e) {
        descriptor_error <<- conditionMessage(e)
        FALSE
      }
    )
    physical_ok <- descriptor_ok
    if (descriptor_ok) {
      storage_shape <- value("storage")$dimensions$shape
      physical_message <- paste0(
        "NetCDF descriptor shape is [", paste(storage_shape, collapse = " x "), "]."
      )
    } else {
      physical_message <- paste0("Invalid NetCDF descriptor: ", descriptor_error)
    }
  }
  add(
    "physical_shape", physical_ok, "storage",
    physical_message, physical_message,
    repairable = FALSE,
    suggested_action = "Rebuild physical storage from a trustworthy source."
  )

  dimensions_ok <- logical_shape_ok && physical_ok &&
    identical(unname(storage_shape), unname(logical_shape))
  add(
    "dimension_compatibility", dimensions_ok, "dimensions",
    "Logical coordinates and physical storage dimensions agree.",
    if (logical_shape_ok && physical_ok) {
      paste0(
        "Logical shape [", paste(logical_shape, collapse = " x "),
        "] differs from storage shape [", paste(storage_shape, collapse = " x "), "]."
      )
    } else {
      "Dimension compatibility cannot be established."
    },
    repairable = FALSE,
    suggested_action = "Rebuild the cube with storage matching all five logical axes."
  )

  units <- value("units")
  units_ok <- is.null(units)
  if (!is.null(units) && vars_ok) {
    unit_names <- names(units)
    units_ok <- (is.character(units) || is.list(units)) &&
      length(units) == length(vars) &&
      (is.null(unit_names) ||
         (!anyDuplicated(unit_names) && !anyNA(unit_names) &&
            all(nzchar(unit_names)) && setequal(unit_names, vars)))
  }
  add(
    "units", units_ok, "units",
    if (is.null(units)) "Units are not supplied; units are optional." else "Units map unambiguously to variables.",
    "Units must be NULL or map one-to-one to variable names.",
    repairable = TRUE,
    suggested_action = "Supply one unit per variable, with unique names matching vars when named."
  )

  spatial <- value("spatial_extent")
  spatial_ok <- lon_finite && lat_finite &&
    (is.null(spatial) ||
       (is.numeric(spatial) && length(spatial) == 4L && all(is.finite(spatial)) &&
          spatial[1L] <= spatial[2L] && spatial[3L] <= spatial[4L] &&
          min(lon) >= spatial[1L] && max(lon) <= spatial[2L] &&
          min(lat) >= spatial[3L] && max(lat) <= spatial[4L]))
  add(
    "spatial_extent", spatial_ok, "spatial_extent",
    if (is.null(spatial)) "Spatial extent is absent and can be derived from coordinates." else "Spatial extent is ordered and covers the coordinates.",
    "Spatial extent must be finite, ordered, and cover longitude and latitude.",
    repairable = TRUE,
    suggested_action = "Recalculate spatial_extent from the longitude and latitude ranges."
  )

  temporal <- value("temporal_extent")
  temporal_ok <- time_complete &&
    (is.null(temporal) ||
       (inherits(temporal, expected_class) && length(temporal) == 2L &&
          !anyNA(temporal) && all(is.finite(as.numeric(temporal))) &&
          (!inherits(temporal, "POSIXct") || identical(.time_timezone(temporal), "UTC")) &&
          temporal[1L] <= temporal[2L] &&
          min(time) >= temporal[1L] && max(time) <= temporal[2L]))
  add(
    "temporal_extent", temporal_ok, "temporal_extent",
    if (is.null(temporal)) "Temporal extent is absent and can be derived from time." else "Temporal extent is ordered and covers time.",
    "Temporal extent must be ordered and cover the time coordinate.",
    repairable = TRUE,
    suggested_action = "Recalculate temporal_extent from the time range."
  )

  depth_extent <- value("depth_extent")
  depth_extent_ok <- depth_ok &&
    (is.null(depth_extent) ||
       (surface_depth && is.numeric(depth_extent) && length(depth_extent) == 2L &&
          all(is.na(depth_extent))) ||
       (!surface_depth && is.numeric(depth_extent) && length(depth_extent) == 2L &&
          all(is.finite(depth_extent)) && depth_extent[1L] <= depth_extent[2L] &&
          min(depth) >= depth_extent[1L] && max(depth) <= depth_extent[2L]))
  add(
    "depth_extent", depth_extent_ok, "depth_extent",
    if (is.null(depth_extent)) "Depth extent is absent and can be derived from depth." else "Depth extent is compatible with the depth coordinate.",
    "Depth extent must be ordered and cover depth, or contain two NA values for a surface cube.",
    repairable = TRUE,
    suggested_action = "Recalculate depth_extent from depth or use c(NA_real_, NA_real_) for a surface cube."
  )

  file_ok <- TRUE
  file_message <- "No external backend file is required."
  if (identical(backend, "netcdf")) {
    path <- if (is.list(value("storage")$file)) {
      value("storage")$file$normalized_path
    } else {
      NULL
    }
    file_ok <- is.character(path) && length(path) == 1L && !is.na(path) && file.exists(path)
    file_message <- if (file_ok) {
      "The NetCDF source file exists."
    } else {
      "The NetCDF source file is missing or its path is invalid."
    }
  } else if (!backend_ok) {
    file_ok <- FALSE
    file_message <- "Backend file requirements cannot be determined."
  }
  add(
    "backend_file", file_ok, "storage",
    file_message, file_message,
    repairable = FALSE,
    suggested_action = "Restore the exact NetCDF source file or rebuild its descriptor."
  )

  provenance <- value("provenance")
  provenance_present <- !is.null(provenance)
  add(
    "provenance", TRUE, "provenance",
    if (provenance_present) "Provenance metadata are present." else "Provenance metadata are absent.",
    "",
    warn = !provenance_present,
    repairable = TRUE,
    suggested_action = "Record source, processing, and ownership metadata when available."
  )

  optional <- c("mask", "dc", "climatology", "anomaly", "qa")
  present_optional <- optional[vapply(optional, function(name) !is.null(value(name)), logical(1))]
  add(
    "optional_metadata", TRUE, "metadata",
    if (length(present_optional) == 0L) {
      "No optional analysis metadata are attached."
    } else {
      paste0("Optional metadata present: ", paste(present_optional, collapse = ", "), ".")
    },
    ""
  )

  report <- do.call(rbind, rows)
  rownames(report) <- NULL
  class(report) <- c("oceancube_validation", "data.frame")

  if (isTRUE(strict) && any(report$status == "FAIL")) {
    rlang::abort(
      paste0(
        "Ocean cube validation failed ",
        sum(report$status == "FAIL"), " check(s)."
      ),
      class = "oceancube_validation_error",
      report = report
    )
  }

  report
}

#' Print an ocean cube validation report
#'
#' @param x An `oceancube_validation` report.
#' @param ... Additional arguments, currently unused.
#'
#' @return `x`, invisibly.
#' @export
print.oceancube_validation <- function(x, ...) {
  counts <- table(factor(x$status, levels = c("PASS", "WARN", "FAIL")))
  cat("<oceancube_validation>\n")
  cat("PASS: ", counts[["PASS"]], "\n", sep = "")
  cat("WARN: ", counts[["WARN"]], "\n", sep = "")
  cat("FAIL: ", counts[["FAIL"]], "\n", sep = "")
  notable <- x[x$status %in% c("WARN", "FAIL"),
               c("check", "status", "component", "message"), drop = FALSE]
  if (nrow(notable) > 0L) {
    print.data.frame(notable, row.names = FALSE)
  }
  invisible(x)
}
