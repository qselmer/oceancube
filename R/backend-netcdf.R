# Internal NetCDF-backend descriptor operations ---------------------------

.netcdf_abort <- function(message, class = "oceancube_netcdf_schema_error") {
  rlang::abort(message, class = class)
}

.netcdf_scalar_string <- function(x, arg, allow_na = FALSE) {
  valid <- is.character(x) &&
    length(x) == 1L &&
    is.null(dim(x)) &&
    (isTRUE(allow_na) || (!is.na(x) && nzchar(x)))
  if (!valid) {
    .abort_badarg(
      arg,
      if (isTRUE(allow_na)) {
        "must be a character scalar or NA."
      } else {
        "must be a single non-empty, non-missing string."
      }
    )
  }
  x
}

.netcdf_attribute <- function(nc, variable, attribute, default = NA) {
  result <- tryCatch(
    ncdf4::ncatt_get(nc, variable, attribute),
    error = function(e) list(hasatt = FALSE)
  )
  if (isTRUE(result$hasatt)) result$value else default
}

.netcdf_optional_string <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(as.character(x))) {
    return(NA_character_)
  }
  as.character(x)
}

.netcdf_optional_number <- function(x, default = NA_real_) {
  if (length(x) != 1L || is.na(x) || !is.numeric(x)) {
    return(default)
  }
  as.numeric(x)
}

.netcdf_as_utc <- function(x) {
  as.POSIXct(
    as.numeric(x),
    origin = "1970-01-01",
    tz = "UTC"
  )
}

.with_netcdf_connection <- function(file, code) {
  .netcdf_scalar_string(file, "file")
  if (!is.function(code)) {
    .abort_badarg("code", "must be a function.")
  }

  nc <- tryCatch(
    ncdf4::nc_open(file),
    error = function(e) {
      .netcdf_abort(
        paste0("Cannot open NetCDF file `", file, "`: ", conditionMessage(e)),
        class = "oceancube_netcdf_file_error"
      )
    }
  )
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  code(nc)
}

.netcdf_file_identity <- function(file) {
  .netcdf_scalar_string(file, "file")
  if (grepl("^[[:alpha:]][[:alnum:]+.-]*://", file)) {
    .netcdf_abort(
      paste0(
        "The initial NetCDF backend supports local files only; received remote resource `",
        file,
        "`."
      ),
      class = "oceancube_netcdf_file_error"
    )
  }
  if (!file.exists(file)) {
    .netcdf_abort(
      paste0("NetCDF file does not exist: `", file, "`. Use a valid local path."),
      class = "oceancube_netcdf_file_error"
    )
  }

  info <- file.info(file)
  if (isTRUE(info$isdir[[1L]])) {
    .netcdf_abort(
      paste0("Expected a NetCDF file, but `", file, "` is a directory."),
      class = "oceancube_netcdf_file_error"
    )
  }

  normalized <- normalizePath(file, winslash = "/", mustWork = TRUE)
  info <- file.info(normalized)
  if (is.na(info$size[[1L]]) || is.na(info$mtime[[1L]])) {
    .netcdf_abort(
      paste0("Could not inspect the identity of NetCDF file `", normalized, "`."),
      class = "oceancube_netcdf_file_error"
    )
  }

  list(
    path = file,
    normalized_path = normalized,
    size_bytes = as.double(info$size[[1L]]),
    modified_utc = .netcdf_as_utc(info$mtime[[1L]]),
    identity_policy = "size_mtime_error"
  )
}

.validate_netcdf_file_identity <- function(file_descriptor) {
  required <- c(
    "path",
    "normalized_path",
    "size_bytes",
    "modified_utc",
    "identity_policy"
  )
  if (!is.list(file_descriptor) ||
      length(setdiff(required, names(file_descriptor))) > 0L) {
    .netcdf_abort(
      "Invalid NetCDF file descriptor: required identity fields are missing.",
      class = "oceancube_bad_storage"
    )
  }

  path <- file_descriptor$normalized_path
  .netcdf_scalar_string(path, "storage$file$normalized_path")
  if (!file.exists(path)) {
    .netcdf_abort(
      paste0(
        "NetCDF source no longer exists: `",
        path,
        "`. Reopen the file to rebuild its descriptor."
      ),
      class = "oceancube_netcdf_changed_file"
    )
  }
  info <- file.info(path)
  if (isTRUE(info$isdir[[1L]])) {
    .netcdf_abort(
      paste0("NetCDF source is now a directory: `", path, "`."),
      class = "oceancube_netcdf_changed_file"
    )
  }

  found_size <- as.double(info$size[[1L]])
  expected_size <- as.double(file_descriptor$size_bytes)
  found_modified <- .netcdf_as_utc(info$mtime[[1L]])
  expected_modified <- .netcdf_as_utc(file_descriptor$modified_utc)

  if (!identical(found_size, expected_size)) {
    .netcdf_abort(
      paste0(
        "NetCDF source changed after the descriptor was created: expected size ",
        format(expected_size, scientific = FALSE),
        " bytes, found ",
        format(found_size, scientific = FALSE),
        " bytes. Reopen the file to refresh its schema."
      ),
      class = "oceancube_netcdf_changed_file"
    )
  }
  if (!isTRUE(all.equal(
    as.numeric(found_modified),
    as.numeric(expected_modified),
    tolerance = 0
  ))) {
    .netcdf_abort(
      paste0(
        "NetCDF source changed after the descriptor was created: expected modified time ",
        format(expected_modified, tz = "UTC", usetz = TRUE),
        ", found ",
        format(found_modified, tz = "UTC", usetz = TRUE),
        ". Reopen the file to refresh its schema."
      ),
      class = "oceancube_netcdf_changed_file"
    )
  }

  invisible(TRUE)
}

.netcdf_dimension_metadata <- function(nc) {
  dimensions <- lapply(names(nc$dim), function(name) {
    dimension <- nc$dim[[name]]
    units <- .netcdf_attribute(nc, name, "units", default = dimension$units)
    list(
      name = name,
      length = as.integer(dimension$len),
      units = .netcdf_optional_string(units),
      standard_name = .netcdf_optional_string(
        .netcdf_attribute(nc, name, "standard_name")
      ),
      axis = toupper(.netcdf_optional_string(
        .netcdf_attribute(nc, name, "axis")
      )),
      positive = tolower(.netcdf_optional_string(
        .netcdf_attribute(nc, name, "positive")
      )),
      calendar = tolower(.netcdf_optional_string(
        .netcdf_attribute(nc, name, "calendar")
      ))
    )
  })
  names(dimensions) <- names(nc$dim)
  dimensions
}

.netcdf_dimension_evidence <- function(metadata, axis) {
  evidence <- character()
  if (identical(axis, "longitude")) {
    if (identical(metadata$axis, "X")) evidence <- c(evidence, "axis=X")
    if (identical(tolower(metadata$standard_name), "longitude")) {
      evidence <- c(evidence, "standard_name=longitude")
    }
    if (tolower(metadata$units) %in%
        c("degrees_east", "degree_east", "degrees_e", "degree_e")) {
      evidence <- c(evidence, paste0("units=", metadata$units))
    }
  } else if (identical(axis, "latitude")) {
    if (identical(metadata$axis, "Y")) evidence <- c(evidence, "axis=Y")
    if (identical(tolower(metadata$standard_name), "latitude")) {
      evidence <- c(evidence, "standard_name=latitude")
    }
    if (tolower(metadata$units) %in%
        c("degrees_north", "degree_north", "degrees_n", "degree_n")) {
      evidence <- c(evidence, paste0("units=", metadata$units))
    }
  } else if (identical(axis, "depth")) {
    if (identical(metadata$axis, "Z")) evidence <- c(evidence, "axis=Z")
    if (tolower(metadata$standard_name) %in%
        c("depth", "sea_floor_depth_below_geoid")) {
      evidence <- c(evidence, paste0("standard_name=", metadata$standard_name))
    }
    if (metadata$positive %in% c("up", "down")) {
      evidence <- c(evidence, paste0("positive=", metadata$positive))
    }
  } else if (identical(axis, "time")) {
    if (identical(metadata$axis, "T")) evidence <- c(evidence, "axis=T")
    if (identical(tolower(metadata$standard_name), "time")) {
      evidence <- c(evidence, "standard_name=time")
    }
    if (!is.na(metadata$units) &&
        grepl("^[[:alpha:]]+[[:space:]]+since[[:space:]]+", metadata$units,
          ignore.case = TRUE
        )) {
      evidence <- c(evidence, paste0("units=", metadata$units))
    }
    if (!is.na(metadata$calendar)) {
      evidence <- c(evidence, paste0("calendar=", metadata$calendar))
    }
  }
  evidence
}

.resolve_netcdf_dimension <- function(metadata, axis, explicit = NULL,
                                      required = TRUE) {
  known <- list(
    longitude = c("lon", "longitude", "x"),
    latitude = c("lat", "latitude", "y"),
    depth = c("depth", "deptht", "lev", "level", "z"),
    time = c("time", "t")
  )

  if (!is.null(explicit)) {
    .netcdf_scalar_string(explicit, paste0(axis, "_name"))
    if (!explicit %in% names(metadata)) {
      .netcdf_abort(
        paste0(
          "Cannot resolve the ",
          axis,
          " dimension: explicit dimension `",
          explicit,
          "` does not exist. Available dimensions: ",
          paste(names(metadata), collapse = ", "),
          "."
        )
      )
    }
    return(list(
      name = explicit,
      detection = list(method = "explicit", evidence = explicit)
    ))
  }

  evidence <- lapply(metadata, .netcdf_dimension_evidence, axis = axis)
  attribute_candidates <- names(evidence)[lengths(evidence) > 0L]
  if (length(attribute_candidates) > 1L) {
    details <- vapply(attribute_candidates, function(candidate) {
      paste0(candidate, " (", paste(evidence[[candidate]], collapse = ", "), ")")
    }, character(1))
    .netcdf_abort(
      paste0(
        "Cannot resolve the ",
        axis,
        " dimension because multiple CF candidates were found: ",
        paste(details, collapse = "; "),
        ". Specify the ",
        axis,
        " dimension explicitly."
      )
    )
  }
  if (length(attribute_candidates) == 1L) {
    candidate <- attribute_candidates[[1L]]
    return(list(
      name = candidate,
      detection = list(
        method = "cf_attributes",
        evidence = evidence[[candidate]]
      )
    ))
  }

  lower_names <- tolower(names(metadata))
  name_candidates <- names(metadata)[lower_names %in% known[[axis]]]
  if (length(name_candidates) > 1L) {
    .netcdf_abort(
      paste0(
        "Cannot resolve the ",
        axis,
        " dimension because multiple known-name candidates were found: ",
        paste(name_candidates, collapse = ", "),
        ". Specify it explicitly."
      )
    )
  }
  if (length(name_candidates) == 1L) {
    return(list(
      name = name_candidates[[1L]],
      detection = list(method = "known_name", evidence = name_candidates[[1L]])
    ))
  }

  if (!isTRUE(required)) return(NULL)
  .netcdf_abort(
    paste0(
      "Cannot resolve the ",
      axis,
      " dimension. Available dimensions: ",
      paste(names(metadata), collapse = ", "),
      ". No unique CF attribute or known name identified it; specify the dimension explicitly."
    )
  )
}

.read_netcdf_coordinate <- function(nc, variable) {
  as.vector(ncdf4::ncvar_get(
    nc,
    variable,
    collapse_degen = FALSE
  ))
}

.decode_netcdf_time <- function(raw_values, units, calendar = "standard") {
  .decode_cf_time(raw_values, units, calendar)
}

.validate_netcdf_variables_argument <- function(variables) {
  if (!is.character(variables) || !is.null(dim(variables))) {
    .abort_badarg("variables", "must be a character vector.")
  }
  if (length(variables) == 0L) {
    .abort_badarg("variables", "must contain at least one data-variable name.")
  }
  if (anyNA(variables) || any(!nzchar(variables))) {
    .abort_badarg("variables", "must contain non-empty, non-missing names.")
  }
  if (anyDuplicated(variables)) {
    .abort_badarg("variables", "must not contain duplicates.")
  }
  variables
}

.netcdf_variable_map <- function(nc, variable, dimension_names,
                                  coordinate_lengths) {
  var <- nc$var[[variable]]
  source_names <- vapply(var$dim, function(x) x$name, character(1))
  source_lengths <- stats::setNames(
    as.integer(vapply(var$dim, function(x) x$len, numeric(1))),
    source_names
  )
  axis_by_dimension <- stats::setNames(
    names(dimension_names),
    unlist(dimension_names, use.names = FALSE)
  )
  source_axes <- unname(axis_by_dimension[source_names])

  if (anyNA(source_axes)) {
    unsupported <- source_names[is.na(source_axes)]
    .netcdf_abort(
      paste0(
        "Variable `",
        variable,
        "` cannot use the initial rectilinear backend because dimension(s) ",
        paste0("`", unsupported, "`", collapse = ", "),
        " do not map to the resolved longitude, latitude, depth, or time axes."
      )
    )
  }
  required <- c("longitude", "latitude", "time")
  missing_required <- setdiff(required, source_axes)
  if (length(missing_required) > 0L) {
    .netcdf_abort(
      paste0(
        "Variable `",
        variable,
        "` is missing required canonical axis/axes: ",
        paste(missing_required, collapse = ", "),
        "."
      )
    )
  }
  if (anyDuplicated(source_axes)) {
    .netcdf_abort(
      paste0(
        "Variable `",
        variable,
        "` maps more than one physical dimension to the same canonical axis."
      )
    )
  }

  for (i in seq_along(source_names)) {
    expected <- coordinate_lengths[[source_axes[[i]]]]
    if (!identical(source_lengths[[i]], as.integer(expected))) {
      .netcdf_abort(
        paste0(
          "Variable `",
          variable,
          "` has incompatible ",
          source_axes[[i]],
          " length ",
          source_lengths[[i]],
          "; expected ",
          expected,
          "."
        )
      )
    }
  }

  canonical_order <- c("longitude", "latitude", "depth", "time")
  canonical_axes <- canonical_order[canonical_order %in% source_axes]
  source_to_canonical <- match(canonical_axes, source_axes)
  canonical_to_source <- match(source_axes, canonical_axes)
  coordinate_variables <- as.list(dimension_names[canonical_axes])
  names(coordinate_variables) <- canonical_axes

  units <- .netcdf_optional_string(
    .netcdf_attribute(nc, variable, "units", default = var$units)
  )
  long_name <- .netcdf_optional_string(
    .netcdf_attribute(nc, variable, "long_name", default = var$longname)
  )
  standard_name <- .netcdf_optional_string(
    .netcdf_attribute(nc, variable, "standard_name")
  )
  fill_value <- .netcdf_optional_number(
    .netcdf_attribute(nc, variable, "_FillValue")
  )
  missing_value <- .netcdf_optional_number(
    .netcdf_attribute(nc, variable, "missing_value")
  )
  scale_factor <- .netcdf_optional_number(
    .netcdf_attribute(nc, variable, "scale_factor"),
    default = 1
  )
  add_offset <- .netcdf_optional_number(
    .netcdf_attribute(nc, variable, "add_offset"),
    default = 0
  )

  list(
    logical_name = variable,
    source_name = variable,
    source_dimension_names = source_names,
    source_dimension_lengths = source_lengths,
    canonical_axes = canonical_axes,
    source_to_canonical_permutation = as.integer(source_to_canonical),
    canonical_to_source_permutation = as.integer(canonical_to_source),
    singleton_axes_inserted = setdiff(canonical_order, canonical_axes),
    coordinate_variables = coordinate_variables,
    source_type = as.character(var$prec),
    fill_value = fill_value,
    missing_value = missing_value,
    scale_factor = scale_factor,
    add_offset = add_offset,
    units = units,
    long_name = long_name,
    standard_name = standard_name,
    attributes = list(
      units = units,
      long_name = long_name,
      standard_name = standard_name,
      `_FillValue` = fill_value,
      missing_value = missing_value,
      scale_factor = scale_factor,
      add_offset = add_offset
    )
  )
}

.netcdf_canonical_dimension <- function(axis, metadata, values, detection) {
  list(
    axis = axis,
    source_dimension = metadata$name,
    coordinate_variable = metadata$name,
    length = as.integer(length(values)),
    values = values,
    source_type = typeof(values),
    units = metadata$units,
    standard_name = metadata$standard_name,
    axis_attribute = metadata$axis,
    positive = metadata$positive,
    calendar = metadata$calendar,
    detection = detection
  )
}

.new_netcdf_storage <- function(file, variables, lon_name = NULL,
                                 lat_name = NULL, depth_name = NULL,
                                 time_name = NULL, source = "netcdf",
                                 dataset_id = NULL) {
  variables <- .validate_netcdf_variables_argument(variables)
  .netcdf_scalar_string(source, "source")
  if (!is.null(dataset_id)) {
    .netcdf_scalar_string(dataset_id, "dataset_id")
  }
  identity <- .netcdf_file_identity(file)

  storage <- .with_netcdf_connection(identity$normalized_path, function(nc) {
    coordinate_variables <- names(nc$dim)
    requested_coordinates <- intersect(variables, coordinate_variables)
    if (length(requested_coordinates) > 0L) {
      .netcdf_abort(
        paste0(
          "Coordinate variable(s) cannot be selected as oceanographic data: ",
          paste0("`", requested_coordinates, "`", collapse = ", "),
          "."
        )
      )
    }

    available <- names(nc$var)
    missing <- setdiff(variables, available)
    if (length(missing) > 0L) {
      .netcdf_abort(
        paste0(
          "Variable(s) not present in the NetCDF file: ",
          paste0("`", missing, "`", collapse = ", "),
          ". Available data variables: ",
          paste(available, collapse = ", "),
          "."
        )
      )
    }

    selected_dimension_names <- unique(unlist(
      lapply(variables, function(variable) {
        vapply(nc$var[[variable]]$dim, function(x) x$name, character(1))
      }),
      use.names = FALSE
    ))
    metadata <- .netcdf_dimension_metadata(nc)
    metadata <- metadata[selected_dimension_names]
    resolved <- list(
      longitude = .resolve_netcdf_dimension(
        metadata, "longitude", lon_name, required = TRUE
      ),
      latitude = .resolve_netcdf_dimension(
        metadata, "latitude", lat_name, required = TRUE
      ),
      depth = .resolve_netcdf_dimension(
        metadata, "depth", depth_name, required = FALSE
      ),
      time = .resolve_netcdf_dimension(
        metadata, "time", time_name, required = TRUE
      )
    )
    resolved_names <- vapply(resolved[!vapply(resolved, is.null, logical(1))],
      `[[`,
      character(1),
      "name"
    )
    if (anyDuplicated(resolved_names)) {
      .netcdf_abort(
        paste0(
          "Canonical axes must map to distinct physical dimensions; resolved: ",
          paste(names(resolved_names), resolved_names, sep = "=", collapse = ", "),
          "."
        )
      )
    }

    coordinate_values <- lapply(resolved, function(item) {
      if (is.null(item)) return(NULL)
      .read_netcdf_coordinate(nc, item$name)
    })
    if (!is.numeric(coordinate_values$longitude) ||
        !is.numeric(coordinate_values$latitude) ||
        !is.numeric(coordinate_values$time) ||
        (!is.null(coordinate_values$depth) &&
          !is.numeric(coordinate_values$depth))) {
      .netcdf_abort("NetCDF coordinate variables must be numeric.")
    }

    time_metadata <- metadata[[resolved$time$name]]
    time <- .decode_netcdf_time(
      coordinate_values$time,
      units = time_metadata$units,
      calendar = time_metadata$calendar
    )

    dimension_names <- list(
      longitude = resolved$longitude$name,
      latitude = resolved$latitude$name,
      time = resolved$time$name
    )
    if (!is.null(resolved$depth)) {
      dimension_names$depth <- resolved$depth$name
      dimension_names <- dimension_names[
        c("longitude", "latitude", "depth", "time")
      ]
    }
    coordinate_lengths <- list(
      longitude = length(coordinate_values$longitude),
      latitude = length(coordinate_values$latitude),
      time = length(coordinate_values$time)
    )
    if (!is.null(coordinate_values$depth)) {
      coordinate_lengths$depth <- length(coordinate_values$depth)
    }

    variable_map <- lapply(variables, function(variable) {
      .netcdf_variable_map(
        nc,
        variable,
        dimension_names = dimension_names,
        coordinate_lengths = coordinate_lengths
      )
    })
    names(variable_map) <- variables

    has_depth <- vapply(variable_map, function(map) {
      "depth" %in% map$canonical_axes
    }, logical(1))
    if (any(has_depth) && !all(has_depth)) {
      depth_variables <- variables[has_depth]
      surface_variables <- variables[!has_depth]
      .netcdf_abort(
        paste0(
          "Variables cannot share one ocean_cube because their vertical axes are incompatible: ",
          paste0("`", depth_variables, "`", collapse = ", "),
          " use an explicit depth axis, while ",
          paste0("`", surface_variables, "`", collapse = ", "),
          " are surface variables. Open them as separate cubes."
        )
      )
    }
    if (all(has_depth) && is.null(resolved$depth)) {
      .netcdf_abort(
        "Selected variables use a vertical dimension, but no unique depth dimension was resolved."
      )
    }

    logical_depth <- if (all(has_depth)) {
      coordinate_values$depth
    } else {
      NA_real_
    }
    .check_cube_coordinates(
      coordinate_values$longitude,
      coordinate_values$latitude,
      logical_depth,
      time$decoded_values,
      variables
    )

    canonical <- list(
      longitude = .netcdf_canonical_dimension(
        "longitude",
        metadata[[resolved$longitude$name]],
        coordinate_values$longitude,
        resolved$longitude$detection
      ),
      latitude = .netcdf_canonical_dimension(
        "latitude",
        metadata[[resolved$latitude$name]],
        coordinate_values$latitude,
        resolved$latitude$detection
      ),
      depth = if (all(has_depth)) {
        .netcdf_canonical_dimension(
          "depth",
          metadata[[resolved$depth$name]],
          logical_depth,
          resolved$depth$detection
        )
      } else {
        list(
          axis = "depth",
          source_dimension = NA_character_,
          coordinate_variable = NA_character_,
          length = 1L,
          values = NA_real_,
          source_type = "double",
          units = NA_character_,
          standard_name = NA_character_,
          axis_attribute = NA_character_,
          positive = NA_character_,
          calendar = NA_character_,
          detection = list(
            method = "inserted_singleton",
            evidence = "all selected variables are surface variables"
          )
        )
      },
      time = .netcdf_canonical_dimension(
        "time",
        metadata[[resolved$time$name]],
        time$decoded_values,
        resolved$time$detection
      )
    )

    shape <- stats::setNames(
      as.integer(c(
        canonical$longitude$length,
        canonical$latitude$length,
        canonical$depth$length,
        canonical$time$length,
        length(variables)
      )),
      .cube_axis_names()
    )

    list(
      version = 1L,
      backend = "netcdf",
      read_only = TRUE,
      file = identity,
      dimensions = list(canonical = canonical, shape = shape),
      variables = list(order = variables, map = variable_map),
      time = time,
      decoding = list(
        raw_datavals = TRUE,
        missing_before_scale = TRUE,
        scale_once = TRUE
      ),
      options = list(
        local_only = TRUE,
        connection_policy = "open_per_operation",
        surface_policy = "separate_vertical_axes",
        noncontiguous_indices = "minimum_envelope",
        file_change = "error",
        source = source,
        dataset_id = dataset_id,
        created_utc = .netcdf_as_utc(Sys.time())
      )
    )
  })

  .validate_netcdf_storage(storage, check_file = TRUE)
  storage
}

.netcdf_contains_forbidden_object <- function(x) {
  if (inherits(x, "connection") ||
      inherits(x, "ncdf4") ||
      identical(typeof(x), "externalptr") ||
      is.array(x)) {
    return(TRUE)
  }
  if (!is.list(x)) return(FALSE)
  any(vapply(x, .netcdf_contains_forbidden_object, logical(1)))
}

.validate_netcdf_storage <- function(storage, check_file = TRUE) {
  if (!is.list(storage)) {
    .netcdf_abort(
      "Invalid NetCDF storage descriptor: expected a list.",
      class = "oceancube_bad_storage"
    )
  }
  required <- c(
    "version",
    "backend",
    "read_only",
    "file",
    "dimensions",
    "variables",
    "time",
    "decoding",
    "options"
  )
  missing <- setdiff(required, names(storage))
  if (length(missing) > 0L) {
    .netcdf_abort(
      paste0(
        "Invalid NetCDF storage descriptor: missing field(s) ",
        paste0("`", missing, "`", collapse = ", "),
        "."
      ),
      class = "oceancube_bad_storage"
    )
  }
  if (!identical(storage$version, 1L)) {
    .netcdf_abort(
      "Invalid NetCDF storage descriptor: unsupported `version`.",
      class = "oceancube_bad_storage"
    )
  }
  if (!identical(storage$backend, "netcdf")) {
    .netcdf_abort(
      "Invalid NetCDF storage descriptor: `backend` must be \"netcdf\".",
      class = "oceancube_bad_storage"
    )
  }
  if (!identical(storage$read_only, TRUE)) {
    .netcdf_abort(
      "Invalid NetCDF storage descriptor: `read_only` must be TRUE.",
      class = "oceancube_bad_storage"
    )
  }
  if (.netcdf_contains_forbidden_object(storage)) {
    .netcdf_abort(
      "Invalid NetCDF storage descriptor: connections, external pointers, ncdf4 handles, and arrays are forbidden.",
      class = "oceancube_bad_storage"
    )
  }
  tryCatch(
    serialize(storage, connection = NULL),
    error = function(e) {
      .netcdf_abort(
        paste0(
          "Invalid NetCDF storage descriptor: it is not serializable: ",
          conditionMessage(e)
        ),
        class = "oceancube_bad_storage"
      )
    }
  )

  if (!is.list(storage$dimensions) ||
      !is.list(storage$dimensions$canonical)) {
    .netcdf_abort(
      "Invalid NetCDF storage descriptor: `dimensions$canonical` must be a list.",
      class = "oceancube_bad_storage"
    )
  }
  axes <- c("longitude", "latitude", "depth", "time")
  if (!identical(names(storage$dimensions$canonical), axes)) {
    .netcdf_abort(
      paste0(
        "Invalid NetCDF storage descriptor: canonical dimensions must be exactly ",
        paste(axes, collapse = ", "),
        " in that order."
      ),
      class = "oceancube_bad_storage"
    )
  }
  for (axis in axes) {
    dimension <- storage$dimensions$canonical[[axis]]
    required_dimension <- c(
      "axis",
      "source_dimension",
      "coordinate_variable",
      "length",
      "values",
      "source_type",
      "units",
      "standard_name",
      "axis_attribute",
      "positive",
      "calendar",
      "detection"
    )
    if (!is.list(dimension) ||
        length(setdiff(required_dimension, names(dimension))) > 0L) {
      .netcdf_abort(
        paste0("Invalid NetCDF `", axis, "` dimension descriptor."),
        class = "oceancube_bad_storage"
      )
    }
    if (!identical(dimension$axis, axis) ||
        !identical(dimension$length, as.integer(length(dimension$values)))) {
      .netcdf_abort(
        paste0(
          "Invalid NetCDF `",
          axis,
          "` dimension: declared length does not match coordinate values."
        ),
        class = "oceancube_bad_storage"
      )
    }
  }

  shape <- storage$dimensions$shape
  expected_shape <- stats::setNames(
    as.integer(c(
      storage$dimensions$canonical$longitude$length,
      storage$dimensions$canonical$latitude$length,
      storage$dimensions$canonical$depth$length,
      storage$dimensions$canonical$time$length,
      length(storage$variables$order)
    )),
    .cube_axis_names()
  )
  if (!identical(shape, expected_shape)) {
    .netcdf_abort(
      paste0(
        "Invalid NetCDF storage shape: expected [",
        paste(expected_shape, collapse = " x "),
        "], obtained [",
        paste(shape, collapse = " x "),
        "]."
      ),
      class = "oceancube_bad_storage"
    )
  }

  variables <- storage$variables
  if (!is.list(variables) ||
      !is.character(variables$order) ||
      length(variables$order) == 0L ||
      anyNA(variables$order) ||
      any(!nzchar(variables$order)) ||
      anyDuplicated(variables$order) ||
      !is.list(variables$map) ||
      !identical(names(variables$map), variables$order)) {
    .netcdf_abort(
      "Invalid NetCDF variable descriptor: order and named map are inconsistent.",
      class = "oceancube_bad_storage"
    )
  }

  required_variable <- c(
    "logical_name",
    "source_name",
    "source_dimension_names",
    "source_dimension_lengths",
    "canonical_axes",
    "source_to_canonical_permutation",
    "canonical_to_source_permutation",
    "singleton_axes_inserted",
    "coordinate_variables",
    "source_type",
    "fill_value",
    "missing_value",
    "scale_factor",
    "add_offset",
    "units",
    "long_name",
    "standard_name",
    "attributes"
  )
  canonical_four <- c("longitude", "latitude", "depth", "time")
  for (variable in variables$order) {
    map <- variables$map[[variable]]
    if (!is.list(map) ||
        length(setdiff(required_variable, names(map))) > 0L) {
      .netcdf_abort(
        paste0("Invalid NetCDF variable map for `", variable, "`."),
        class = "oceancube_bad_storage"
      )
    }
    if (!identical(map$logical_name, variable) ||
        length(map$source_dimension_names) !=
          length(map$source_dimension_lengths) ||
        anyDuplicated(map$source_dimension_names) ||
        anyDuplicated(map$canonical_axes) ||
        !all(map$canonical_axes %in% canonical_four)) {
      .netcdf_abort(
        paste0("Invalid NetCDF dimensions in variable map `", variable, "`."),
        class = "oceancube_bad_storage"
      )
    }
    n_axes <- length(map$canonical_axes)
    valid_permutation <- function(x) {
      is.integer(x) &&
        length(x) == n_axes &&
        identical(sort(x), seq_len(n_axes))
    }
    if (!valid_permutation(map$source_to_canonical_permutation) ||
        !valid_permutation(map$canonical_to_source_permutation)) {
      .netcdf_abort(
        paste0("Invalid NetCDF permutation in variable map `", variable, "`."),
        class = "oceancube_bad_storage"
      )
    }
    if (!identical(
      map$canonical_to_source_permutation[
        map$source_to_canonical_permutation
      ],
      seq_len(n_axes)
    )) {
      .netcdf_abort(
        paste0(
          "Invalid NetCDF permutation in variable map `",
          variable,
          "`: physical-to-canonical and canonical-to-physical mappings ",
          "must be inverses."
        ),
        class = "oceancube_bad_storage"
      )
    }
    expected_singletons <- setdiff(canonical_four, map$canonical_axes)
    if (!identical(map$singleton_axes_inserted, expected_singletons)) {
      .netcdf_abort(
        paste0(
          "Invalid NetCDF singleton axes in variable map `",
          variable,
          "`."
        ),
        class = "oceancube_bad_storage"
      )
    }
  }

  required_time <- c(
    "raw_values",
    "units",
    "calendar",
    "calendar_defaulted",
    "origin",
    "origin_text",
    "origin_offset",
    "decoded_values",
    "decoder",
    "decode_status"
  )
  if (!is.list(storage$time) ||
      length(setdiff(required_time, names(storage$time))) > 0L ||
      !identical(storage$time$decode_status, "decoded") ||
      length(storage$time$raw_values) !=
        length(storage$time$decoded_values)) {
    .netcdf_abort(
      "Invalid NetCDF time descriptor.",
      class = "oceancube_bad_storage"
    )
  }
  .validate_time_axis(
    storage$time$decoded_values,
    arg = "storage$time$decoded_values"
  )
  if (!storage$time$calendar %in%
      c("standard", "gregorian", "proleptic_gregorian")) {
    .netcdf_abort(
      paste0("Invalid or unsupported NetCDF calendar `", storage$time$calendar, "`."),
      class = "oceancube_bad_storage"
    )
  }
  if (!is.list(storage$decoding) || !is.list(storage$options)) {
    .netcdf_abort(
      "Invalid NetCDF storage descriptor: `decoding` and `options` must be lists.",
      class = "oceancube_bad_storage"
    )
  }

  if (isTRUE(check_file)) {
    .validate_netcdf_file_identity(storage$file)
  }
  invisible(TRUE)
}

.translate_netcdf_block <- function(variable_descriptor, canonical_start,
                                     canonical_count) {
  canonical_four <- c("longitude", "latitude", "depth", "time")
  if (!is.list(variable_descriptor)) {
    .netcdf_abort(
      "Cannot translate a NetCDF block without a variable descriptor.",
      class = "oceancube_bad_storage"
    )
  }
  if (!is.numeric(canonical_start) ||
      !is.numeric(canonical_count) ||
      length(canonical_start) != 4L ||
      length(canonical_count) != 4L) {
    .netcdf_abort(
      "Canonical NetCDF block translation requires four start and count values.",
      class = "oceancube_bad_block"
    )
  }

  canonical_start <- stats::setNames(
    as.integer(canonical_start),
    canonical_four
  )
  canonical_count <- stats::setNames(
    as.integer(canonical_count),
    canonical_four
  )
  canonical_axes <- variable_descriptor$canonical_axes
  source_names <- variable_descriptor$source_dimension_names
  source_positions <- variable_descriptor$canonical_to_source_permutation

  if (!identical(canonical_axes, canonical_four[
    canonical_four %in% canonical_axes
  ]) ||
      length(source_names) != length(canonical_axes) ||
      !identical(sort(source_positions), seq_along(canonical_axes))) {
    .netcdf_abort(
      paste0(
        "Cannot translate NetCDF block for variable `",
        variable_descriptor$logical_name %||% "<unknown>",
        "` because its stored dimension map is invalid."
      ),
      class = "oceancube_bad_storage"
    )
  }

  present_start <- canonical_start[canonical_axes]
  present_count <- canonical_count[canonical_axes]
  source_start <- present_start[source_positions]
  source_count <- present_count[source_positions]
  names(source_start) <- source_names
  names(source_count) <- source_names

  list(
    source_start = source_start,
    source_count = source_count,
    source_axes = source_names,
    canonical_axes = canonical_axes,
    permutation =
      variable_descriptor$source_to_canonical_permutation,
    singleton_axes = variable_descriptor$singleton_axes_inserted
  )
}

.ncvar_get_block <- function(nc, variable, start, count) {
  tryCatch(
    ncdf4::ncvar_get(
      nc,
      variable,
      start = unname(start),
      count = unname(count),
      collapse_degen = FALSE,
      raw_datavals = TRUE
    ),
    error = function(e) {
      .netcdf_abort(
        paste0(
          "Failed to read physical NetCDF block for variable `",
          variable,
          "`: ",
          conditionMessage(e)
        ),
        class = "oceancube_netcdf_read_error"
      )
    }
  )
}

.decode_netcdf_block <- function(values, variable_descriptor) {
  decoded <- as.double(values)
  sentinels <- c(
    variable_descriptor$fill_value,
    variable_descriptor$missing_value
  )
  sentinels <- unique(sentinels[
    !is.na(sentinels) & is.finite(sentinels)
  ])
  missing <- rep(FALSE, length(decoded))
  for (sentinel in sentinels) {
    missing <- missing |
      (!is.na(decoded) & decoded == sentinel)
  }

  decoded <- decoded * variable_descriptor$scale_factor +
    variable_descriptor$add_offset
  decoded[missing] <- NA_real_
  decoded
}

.validate_netcdf_physical_schema <- function(nc, storage, variables) {
  for (variable in variables) {
    descriptor <- storage$variables$map[[variable]]
    source_name <- descriptor$source_name
    if (!source_name %in% names(nc$var)) {
      .netcdf_abort(
        paste0(
          "NetCDF variable `",
          source_name,
          "` recorded by the descriptor is no longer present."
        ),
        class = "oceancube_netcdf_missing_variable"
      )
    }

    physical <- nc$var[[source_name]]
    physical_names <- vapply(
      physical$dim,
      function(dimension) dimension$name,
      character(1)
    )
    physical_lengths <- as.integer(vapply(
      physical$dim,
      function(dimension) dimension$len,
      numeric(1)
    ))
    if (!identical(
      physical_names,
      descriptor$source_dimension_names
    ) ||
        !identical(
          physical_lengths,
          unname(descriptor$source_dimension_lengths)
        ) ||
        !identical(
          as.character(physical$prec),
          descriptor$source_type
        )) {
      .netcdf_abort(
        paste0(
          "NetCDF variable `",
          source_name,
          "` no longer matches the physical dimensions and type recorded by the descriptor."
        ),
        class = "oceancube_netcdf_schema_changed"
      )
    }
  }
  invisible(TRUE)
}

.read_netcdf_variable_block <- function(nc, variable_descriptor,
                                         canonical_start,
                                         canonical_count) {
  translated <- .translate_netcdf_block(
    variable_descriptor,
    canonical_start,
    canonical_count
  )
  raw <- .ncvar_get_block(
    nc,
    variable_descriptor$source_name,
    translated$source_start,
    translated$source_count
  )
  expected_values <- prod(as.double(translated$source_count))
  if (length(raw) != expected_values) {
    .netcdf_abort(
      paste0(
        "Physical NetCDF read for variable `",
        variable_descriptor$source_name,
        "` returned ",
        length(raw),
        " values; expected ",
        format(expected_values, scientific = FALSE),
        "."
      ),
      class = "oceancube_netcdf_read_error"
    )
  }

  physical <- array(
    .decode_netcdf_block(raw, variable_descriptor),
    dim = unname(translated$source_count)
  )
  canonical_present <- tryCatch(
    aperm(physical, translated$permutation),
    error = function(e) {
      .netcdf_abort(
        paste0(
          "Cannot permute NetCDF variable `",
          variable_descriptor$source_name,
          "` from physical to canonical order: ",
          conditionMessage(e)
        ),
        class = "oceancube_netcdf_permutation_error"
      )
    }
  )

  canonical <- array(
    canonical_present,
    dim = as.integer(canonical_count)
  )
  expected_shape <- as.integer(canonical_count)
  if (!identical(unname(dim(canonical)), expected_shape)) {
    .netcdf_abort(
      paste0(
        "Canonical NetCDF block for variable `",
        variable_descriptor$source_name,
        "` has an invalid shape."
      ),
      class = "oceancube_netcdf_permutation_error"
    )
  }
  canonical
}

.netcdf_block_dimnames <- function(x, index) {
  stats::setNames(
    list(
      as.character(x$lon[index$longitude]),
      as.character(x$lat[index$latitude]),
      as.character(x$depth[index$depth]),
      as.character(x$time[index$time]),
      x$vars[index$variable]
    ),
    .cube_axis_names()
  )
}

.read_netcdf_index_plan <- function(x, plan) {
  .validate_netcdf_storage(x$storage, check_file = TRUE)
  variables <- x$vars[plan$variable_index]
  envelope_index <- c(
    plan$physical_index,
    list(variable = plan$variable_index)
  )
  envelope <- array(
    NA_real_,
    dim = c(
      unname(plan$physical_count),
      length(plan$variable_index)
    ),
    dimnames = .netcdf_block_dimnames(x, envelope_index)
  )

  envelope <- .with_netcdf_connection(
    x$storage$file$normalized_path,
    function(nc) {
      .validate_netcdf_physical_schema(nc, x$storage, variables)
      for (output_position in seq_along(variables)) {
        variable <- variables[[output_position]]
        values <- .read_netcdf_variable_block(
          nc,
          x$storage$variables$map[[variable]],
          canonical_start = plan$physical_start,
          canonical_count = plan$physical_count
        )
        envelope[, , , , output_position] <- values
      }
      envelope
    }
  )

  local_index <- c(
    plan$local_index,
    list(variable = seq_along(plan$variable_index))
  )
  result <- do.call(
    `[`,
    c(
      list(envelope),
      unname(local_index),
      list(drop = FALSE)
    )
  )
  dimnames(result) <- .netcdf_block_dimnames(x, plan$requested)
  result
}

.cube_read_block_netcdf <- function(x, start, count) {
  .check_cube(x)
  block <- .validate_cube_block(
    start = start,
    count = count,
    shape = unname(.cube_storage_shape(x))
  )
  plan <- .plan_cube_index_read(x, block$index)
  .read_netcdf_index_plan(x, plan)
}

.cube_read_netcdf <- function(x, index = NULL, drop = FALSE) {
  .check_cube(x)
  if (!is.logical(drop) || length(drop) != 1L || is.na(drop)) {
    .abort_badarg("drop", "must be a single non-missing logical value.")
  }
  if (isTRUE(drop)) {
    .abort_badarg(
      "drop",
      "`drop = TRUE` is not supported; backend reads always preserve all 5 dimensions."
    )
  }

  plan <- .plan_cube_index_read(x, index)
  .read_netcdf_index_plan(x, plan)
}

.cube_read_spatial_pairs_netcdf <- function(x, longitude_index,
                                            latitude_index, depth_index,
                                            time_index, variable_index) {
  .check_cube(x)
  index <- .validate_spatial_pair_read(
    x, longitude_index, latitude_index, depth_index, time_index,
    variable_index
  )
  .validate_netcdf_storage(x$storage, check_file = TRUE)
  pairs <- .spatial_pair_map(index$longitude, index$latitude)
  unique_variable_index <- unique(index$variable)
  variable_to_unique <- match(index$variable, unique_variable_index)
  unique_variables <- x$vars[unique_variable_index]
  depth_envelope <- seq.int(min(index$depth), max(index$depth))
  depth_local <- match(index$depth, depth_envelope)

  unique_values <- array(
    NA_real_,
    dim = c(
      pair = nrow(pairs$unique_pairs),
      depth = length(index$depth),
      time = 1L,
      variable = length(unique_variable_index)
    )
  )
  unique_values <- .with_netcdf_connection(
    x$storage$file$normalized_path,
    function(nc) {
      .validate_netcdf_physical_schema(nc, x$storage, unique_variables)
      for (pair in seq_len(nrow(pairs$unique_pairs))) {
        for (variable in seq_along(unique_variable_index)) {
          block <- .read_netcdf_variable_block(
            nc,
            x$storage$variables$map[[unique_variables[[variable]]]],
            canonical_start = c(
              longitude = pairs$unique_pairs$longitude_index[[pair]],
              latitude = pairs$unique_pairs$latitude_index[[pair]],
              depth = min(depth_envelope),
              time = index$time[[1L]]
            ),
            canonical_count = c(
              longitude = 1L,
              latitude = 1L,
              depth = length(depth_envelope),
              time = 1L
            )
          )
          unique_values[pair, , 1L, variable] <-
            block[1L, 1L, depth_local, 1L, drop = TRUE]
        }
      }
      unique_values
    }
  )

  output <- unique_values[
    pairs$point_to_pair,
    seq_along(index$depth),
    1L,
    variable_to_unique,
    drop = FALSE
  ]
  dim(output) <- c(
    point = length(index$longitude),
    depth = length(index$depth),
    time = 1L,
    variable = length(index$variable)
  )

  n_paired <- nrow(pairs$unique_pairs) * length(index$depth) *
    length(unique_variable_index)
  rectangle_cells <-
    (diff(range(pairs$unique_pairs$longitude_index)) + 1) *
    (diff(range(pairs$unique_pairs$latitude_index)) + 1)
  attr(output, "oceancube_read_metrics") <- list(
    n_points = length(index$longitude),
    n_unique_pairs = nrow(pairs$unique_pairs),
    n_variables = length(index$variable),
    n_depth = length(index$depth),
    n_open = 1L,
    n_ncvar_get =
      nrow(pairs$unique_pairs) * length(unique_variable_index),
    n_values_requested = length(output),
    n_values_read =
      nrow(pairs$unique_pairs) * length(depth_envelope) *
      length(unique_variable_index),
    n_values_paired = n_paired,
    n_values_bounding_rectangle =
      rectangle_cells * length(index$depth) * length(unique_variable_index),
    read_amplification =
      rectangle_cells / nrow(pairs$unique_pairs)
  )
  output
}

.new_netcdf_cube <- function(storage, source = NULL, dataset_id = NULL,
                              provenance = NULL, qa = NULL) {
  .validate_netcdf_storage(storage, check_file = TRUE)
  canonical <- storage$dimensions$canonical
  source <- source %||% storage$options$source %||% "netcdf"
  dataset_id <- dataset_id %||% storage$options$dataset_id
  if (!is.null(source)) .netcdf_scalar_string(source, "source")
  if (!is.null(dataset_id)) .netcdf_scalar_string(dataset_id, "dataset_id")

  lon <- canonical$longitude$values
  lat <- canonical$latitude$values
  depth <- canonical$depth$values
  time <- storage$time$decoded_values
  vars <- storage$variables$order
  units <- stats::setNames(
    lapply(storage$variables$map, `[[`, "units"),
    vars
  )
  depth_extent <- if (all(is.na(depth))) {
    c(NA_real_, NA_real_)
  } else {
    range(depth, na.rm = TRUE)
  }

  provenance_context <- .provenance_cube_context(
    source = source,
    dataset_id = dataset_id,
    time = time,
    shape = stats::setNames(
      as.integer(c(length(lon), length(lat), length(depth), length(time),
                   length(vars))),
      .cube_axis_names()
    ),
    variables = vars,
    backend = "netcdf",
    provenance = provenance
  )
  provenance_context$calendar <- storage$time$calendar
  provenance <- .provenance_normalize(provenance, context = provenance_context)
  provenance$source$locator <- list(
    type = "file",
    value = storage$file$normalized_path,
    basename = basename(storage$file$normalized_path),
    portable = FALSE
  )
  if (is.null(provenance$time$source)) {
    provenance$time["source"] <- list(.provenance_time_source(list(
      time = .cf_time_provenance(storage$time)
    )))
  }
  provenance <- .provenance_append(
    provenance,
    operation = "read_nc",
    parameters = list(
      requested = list(variables = vars),
      resolved = list(
        variables = vars,
        dimension_mapping = lapply(
          storage$variables$map,
          `[[`,
          "source_dimension_names"
        )
      )
    ),
    output = .provenance_summary(provenance_context),
    scientific_method = .provenance_method("read_nc", list()),
    context = provenance_context
  )
  if (is.null(qa)) qa <- list()
  if (!is.list(qa)) qa <- list(previous = qa)
  qa$netcdf_source <- list(
    file_size_bytes = storage$file$size_bytes,
    file_modified_utc = storage$file$modified_utc
  )

  out <- list(
    lon = lon,
    lat = lat,
    depth = depth,
    time = time,
    vars = vars,
    units = units,
    source = source,
    dataset_id = dataset_id,
    spatial_extent = c(
      lon_min = min(lon),
      lon_max = max(lon),
      lat_min = min(lat),
      lat_max = max(lat)
    ),
    temporal_extent = range(time),
    depth_extent = depth_extent,
    mask = NULL,
    dc = NULL,
    climatology = NULL,
    anomaly = NULL,
    provenance = provenance,
    qa = qa,
    storage = storage
  )
  class(out) <- c("ocean_cube", "list")
  .check_cube(out)
  out
}
