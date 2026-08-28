# Internal CF metadata preservation engine ---------------------------------

.cf_metadata_abort <- function(message, class = "oceancube_cf_metadata_error") {
  rlang::abort(message, class = class)
}

.cf_attribute_record <- function(name, value, position, owner_path, scope) {
  list(
    name = name,
    source_order = as.integer(position),
    raw_value = value,
    r_type = typeof(value),
    r_class = class(value),
    length = as.integer(length(value)),
    owner_path = owner_path,
    scope = scope,
    source_primitive_type = NA_character_,
    primitive_type_status = "unavailable"
  )
}

.cf_scan_attributes <- function(nc, variable = 0, owner_path = "/",
                                scope = "global") {
  values <- tryCatch(
    ncdf4::ncatt_get(nc, variable),
    error = function(e) list()
  )
  if (!length(values)) return(list())
  Map(
    .cf_attribute_record,
    names(values),
    unname(values),
    seq_along(values),
    MoreArgs = list(owner_path = owner_path, scope = scope)
  )
}

.cf_attribute_value <- function(records, name, default = NULL) {
  if (!length(records)) return(default)
  record_names <- vapply(records, `[[`, character(1L), "name")
  hit <- which(record_names == name)
  if (!length(hit)) return(default)
  records[[hit[[1L]]]]$raw_value
}

.cf_group_path <- function(path) {
  if (!grepl("/", path, fixed = TRUE)) return("/")
  group <- sub("/[^/]+$", "", path)
  if (nzchar(group)) paste0("/", group) else "/"
}

.cf_basename <- function(path) {
  sub("^.*/", "", path)
}

.cf_split_names <- function(value) {
  normalized <- trimws(as.character(value))
  if (length(normalized) != 1L || !nzchar(normalized)) return(character())
  strsplit(normalized, "[[:space:]]+")[[1L]]
}

.cf_parse_pairs <- function(value) {
  tokens <- .cf_split_names(value)
  if (!length(tokens)) {
    return(list(status = "UNKNOWN_FORM", entries = list(), tokens = tokens))
  }
  odd <- seq.int(1L, length(tokens), by = 2L)
  even <- seq.int(2L, length(tokens), by = 2L)
  is_key <- grepl(":$", tokens)
  if (length(tokens) %% 2L || !all(is_key[odd]) || any(is_key[even])) {
    return(list(status = "UNKNOWN_FORM", entries = list(), tokens = tokens))
  }
  entries <- lapply(odd, function(i) {
    list(key = sub(":$", "", tokens[[i]]), target = tokens[[i + 1L]])
  })
  list(status = "PARSED", entries = entries, tokens = tokens)
}

.cf_parse_link <- function(attribute, value) {
  if (attribute %in% c("bounds", "climatology")) {
    tokens <- .cf_split_names(value)
    if (length(tokens) != 1L) {
      return(list(status = "UNKNOWN_FORM", entries = list(), tokens = tokens))
    }
    return(list(
      status = "PARSED",
      entries = list(list(key = NA_character_, target = tokens[[1L]])),
      tokens = tokens
    ))
  }
  if (attribute %in% c("coordinates", "ancillary_variables")) {
    tokens <- .cf_split_names(value)
    entries <- lapply(tokens, function(target) {
      list(key = NA_character_, target = target)
    })
    return(list(
      status = if (length(tokens)) "PARSED" else "UNKNOWN_FORM",
      entries = entries,
      tokens = tokens
    ))
  }
  if (attribute %in% c("cell_measures", "formula_terms")) {
    return(.cf_parse_pairs(value))
  }
  if (identical(attribute, "grid_mapping")) {
    tokens <- .cf_split_names(value)
    if (length(tokens) == 1L && !grepl(":", tokens, fixed = TRUE)) {
      return(list(
        status = "PARSED_SIMPLE",
        entries = list(list(key = NA_character_, target = tokens[[1L]])),
        tokens = tokens
      ))
    }
    return(list(
      status = "DEFERRED_EXTENDED",
      entries = list(),
      tokens = tokens
    ))
  }
  list(status = "UNKNOWN_FORM", entries = list(), tokens = character())
}

.cf_resolve_reference <- function(source_path, target_raw, variable_paths) {
  if (!is.character(target_raw) || length(target_raw) != 1L ||
      is.na(target_raw) || !nzchar(target_raw)) {
    return(list(status = "MISSING_TARGET", candidates = character()))
  }

  source_group <- .cf_group_path(source_path)
  local_target <- if (identical(source_group, "/")) {
    target_raw
  } else {
    paste0(sub("^/", "", source_group), "/", target_raw)
  }
  if (local_target %in% variable_paths) {
    status <- if (identical(local_target, source_path)) {
      "SELF_REFERENCE"
    } else {
      "RESOLVED"
    }
    return(list(status = status, candidates = local_target))
  }

  if (target_raw %in% variable_paths) {
    status <- if (identical(target_raw, source_path)) {
      "SELF_REFERENCE"
    } else {
      "RESOLVED"
    }
    return(list(status = status, candidates = target_raw))
  }

  basename_matches <- variable_paths[
    vapply(variable_paths, .cf_basename, character(1L)) ==
      .cf_basename(target_raw)
  ]
  if (length(basename_matches) > 1L) {
    return(list(status = "AMBIGUOUS", candidates = basename_matches))
  }
  if (length(basename_matches) == 1L) {
    status <- if (identical(basename_matches, source_path)) {
      "SELF_REFERENCE"
    } else {
      "RESOLVED"
    }
    return(list(status = status, candidates = basename_matches))
  }
  list(status = "MISSING_TARGET", candidates = character())
}

.cf_link_families <- function() {
  c(
    "coordinates", "bounds", "climatology", "ancillary_variables",
    "cell_measures", "grid_mapping", "formula_terms"
  )
}

.cf_scan_links <- function(variables) {
  variable_paths <- names(variables)
  links <- list()
  diagnostics <- list()

  for (source_path in variable_paths) {
    attributes <- variables[[source_path]]$attributes
    attribute_names <- if (length(attributes)) {
      vapply(attributes, `[[`, character(1L), "name")
    } else {
      character()
    }
    for (attribute in intersect(.cf_link_families(), attribute_names)) {
      raw_value <- .cf_attribute_value(attributes, attribute)
      parsed <- .cf_parse_link(attribute, raw_value)
      if (parsed$status %in% c("UNKNOWN_FORM", "DEFERRED_EXTENDED")) {
        links[[length(links) + 1L]] <- list(
          source_path = source_path,
          attribute = attribute,
          raw_value = raw_value,
          target_raw = NA_character_,
          key = NA_character_,
          candidate_target_paths = character(),
          resolved_path = NA_character_,
          status = parsed$status,
          parser_status = parsed$status
        )
        diagnostics[[length(diagnostics) + 1L]] <- list(
          severity = "INFO",
          code = parsed$status,
          source_path = source_path,
          attribute = attribute,
          raw_value = raw_value
        )
      }
      if (!length(parsed$entries)) next

      targets <- vapply(parsed$entries, `[[`, character(1L), "target")
      duplicates <- duplicated(targets)
      for (i in seq_along(parsed$entries)) {
        entry <- parsed$entries[[i]]
        resolution <- .cf_resolve_reference(
          source_path, entry$target, variable_paths
        )
        status <- if (duplicates[[i]]) {
          "DUPLICATE_REFERENCE"
        } else {
          resolution$status
        }
        resolved_path <- if (length(resolution$candidates) == 1L) {
          resolution$candidates[[1L]]
        } else {
          NA_character_
        }
        links[[length(links) + 1L]] <- list(
          source_path = source_path,
          attribute = attribute,
          raw_value = raw_value,
          target_raw = entry$target,
          key = entry$key,
          candidate_target_paths = resolution$candidates,
          resolved_path = resolved_path,
          status = status,
          parser_status = parsed$status
        )
        if (!identical(status, "RESOLVED") &&
            !(identical(attribute, "formula_terms") &&
              identical(status, "SELF_REFERENCE"))) {
          diagnostics[[length(diagnostics) + 1L]] <- list(
            severity = "WARNING",
            code = status,
            source_path = source_path,
            attribute = attribute,
            target_raw = entry$target
          )
        }
      }
    }
  }
  list(links = links, diagnostics = diagnostics)
}

.cf_role_for_link <- c(
  coordinates = "auxiliary_coordinate",
  bounds = "bounds",
  climatology = "climatology_bounds",
  ancillary_variables = "ancillary",
  cell_measures = "cell_measure",
  grid_mapping = "grid_mapping",
  formula_terms = "formula_term"
)

.cf_role_vocabulary <- function() {
  c(
    "data", "dimension_coordinate", "auxiliary_coordinate", "bounds",
    "climatology_bounds", "grid_mapping", "cell_measure", "ancillary",
    "formula_term", "quality_flag", "geometry", "unknown"
  )
}

.cf_classify_roles <- function(variables, links, dimension_paths) {
  roles <- stats::setNames(
    lapply(names(variables), function(path) {
      if (path %in% dimension_paths) "dimension_coordinate" else character()
    }),
    names(variables)
  )

  for (link in links) {
    if (!is.na(link$resolved_path) &&
        link$status %in% c("RESOLVED", "SELF_REFERENCE")) {
      roles[[link$resolved_path]] <- unique(c(
        roles[[link$resolved_path]],
        .cf_role_for_link[[link$attribute]]
      ))
    }
  }

  for (path in names(variables)) {
    attributes <- variables[[path]]$attributes
    if (!is.null(.cf_attribute_value(attributes, "flag_values")) ||
        !is.null(.cf_attribute_value(attributes, "flag_masks"))) {
      roles[[path]] <- unique(c(roles[[path]], "quality_flag"))
    }
    if (!is.null(.cf_attribute_value(attributes, "geometry_type")) ||
        !is.null(.cf_attribute_value(attributes, "cf_role"))) {
      roles[[path]] <- unique(c(roles[[path]], "geometry"))
    }
  }

  for (path in names(roles)) {
    if (!length(roles[[path]])) roles[[path]] <- "data"
  }
  roles
}

.cf_declaration <- function(global_attributes) {
  raw <- .cf_attribute_value(global_attributes, "Conventions")
  if (is.null(raw)) {
    return(list(raw = NULL, tokens = character(), declared_cf = NA_character_))
  }
  raw_text <- as.character(raw)
  tokens <- unlist(strsplit(raw_text, "[[:space:],]+"), use.names = FALSE)
  tokens <- tokens[nzchar(tokens)]
  cf_tokens <- tokens[grepl("^CF-[0-9]+(?:\\.[0-9]+)?$", tokens)]
  list(
    raw = raw,
    tokens = tokens,
    declared_cf = if (length(cf_tokens)) {
      sub("^CF-", "", cf_tokens[[1L]])
    } else {
      NA_character_
    }
  )
}

.cf_scan_ncdf4 <- function(nc) {
  if (!inherits(nc, "ncdf4")) {
    .cf_metadata_abort("CF scanning requires an open ncdf4 handle.")
  }

  dimension_order <- names(nc$dim)
  dimensions <- stats::setNames(lapply(seq_along(dimension_order), function(i) {
    path <- dimension_order[[i]]
    dimension <- nc$dim[[path]]
    list(
      source_id = path,
      source_path = path,
      basename = .cf_basename(path),
      group_path = .cf_group_path(path),
      source_order = as.integer(i),
      length = as.integer(dimension$len),
      unlimited = isTRUE(dimension$unlim),
      coordinate_variable = if (isTRUE(dimension$create_dimvar)) {
        path
      } else {
        NA_character_
      }
    )
  }), dimension_order)

  variables <- list()
  for (path in dimension_order) {
    dimension <- nc$dim[[path]]
    if (!isTRUE(dimension$create_dimvar)) next
    variables[[path]] <- list(
      source_id = path,
      source_path = path,
      basename = .cf_basename(path),
      group_path = .cf_group_path(path),
      source_order = NA_integer_,
      source_group_index = as.integer(dimension$group_index),
      source_variable_id = as.integer(dimension$dimvarid$id),
      source_primitive_type = NA_character_,
      primitive_type_status = "unavailable",
      source_dimensions = path,
      source_dimension_order = path,
      attributes = .cf_scan_attributes(
        nc, path, owner_path = path, scope = "variable"
      ),
      roles = character(),
      data_status = "metadata-container"
    )
  }

  for (path in names(nc$var)) {
    variable <- nc$var[[path]]
    variables[[path]] <- list(
      source_id = path,
      source_path = path,
      basename = .cf_basename(path),
      group_path = .cf_group_path(path),
      source_order = NA_integer_,
      source_group_index = as.integer(variable$group_index),
      source_variable_id = as.integer(variable$id$id),
      source_primitive_type = as.character(variable$prec),
      primitive_type_status = "available",
      source_dimensions = vapply(
        variable$dim, `[[`, character(1L), "name"
      ),
      source_dimension_order = vapply(
        variable$dim, `[[`, character(1L), "name"
      ),
      attributes = .cf_scan_attributes(
        nc, path, owner_path = path, scope = "variable"
      ),
      roles = character(),
      data_status = "data-bearing"
    )
  }

  variable_order <- names(variables)[order(
    vapply(variables, `[[`, integer(1L), "source_group_index"),
    vapply(variables, `[[`, integer(1L), "source_variable_id"),
    names(variables)
  )]
  variables <- variables[variable_order]
  for (i in seq_along(variables)) {
    variables[[i]]$source_order <- as.integer(i)
  }

  linked <- .cf_scan_links(variables)
  roles <- .cf_classify_roles(variables, linked$links, dimension_order)
  for (path in names(variables)) {
    variables[[path]]$roles <- roles[[path]]
    variables[[path]]$data_status <- if ("data" %in% roles[[path]]) {
      "data-bearing"
    } else {
      "metadata-container"
    }
  }

  global_attributes <- .cf_scan_attributes(
    nc, 0, owner_path = "/", scope = "global"
  )
  source <- list(
    format = "NetCDF",
    scanner = list(name = "oceancube_native_ncdf4", version = "1.0.0"),
    declaration = .cf_declaration(global_attributes),
    global_attributes = global_attributes,
    dimensions = list(order = dimension_order, map = dimensions),
    variables = list(order = variable_order, map = variables),
    links = linked$links
  )
  list(
    schema_name = "oceancube_cf_metadata",
    schema_version = "1.0.0",
    source = source,
    current = NULL,
    interpretation = list(
      status = "PRESERVED_SUPPORTED_SUBSET",
      link_families = .cf_link_families(),
      extended_grid_mapping = "preserved-raw-deferred"
    ),
    diagnostics = linked$diagnostics
  )
}

.cf_scan_netcdf <- function(file) {
  identity <- .netcdf_file_identity(file)
  nc <- tryCatch(
    ncdf4::nc_open(
      identity$normalized_path,
      readunlim = FALSE,
      suppress_dimvals = TRUE
    ),
    error = function(e) {
      .netcdf_abort(
        paste0(
          "Cannot open NetCDF file `", identity$normalized_path, "`: ",
          conditionMessage(e)
        ),
        class = "oceancube_netcdf_file_error"
      )
    }
  )
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  cf <- .cf_scan_ncdf4(nc)
  .cf_validate_cf(cf, require_current = FALSE)
  cf
}

.cf_axis_attribute_map <- function(source, dimension_id) {
  variable <- source$variables$map[[dimension_id]]
  attributes <- if (is.null(variable)) list() else variable$attributes
  list(
    axis = .cf_attribute_value(attributes, "axis"),
    standard_name = .cf_attribute_value(attributes, "standard_name"),
    units = .cf_attribute_value(attributes, "units"),
    positive = .cf_attribute_value(attributes, "positive"),
    formula_terms = .cf_attribute_value(attributes, "formula_terms")
  )
}

.cf_axis_evidence <- function(source, dimension_id) {
  attributes <- .cf_axis_attribute_map(source, dimension_id)
  evidence <- list(list(
    source = "coordinate_dimension_relationship",
    value = dimension_id,
    axis = NA_character_,
    strength = "structural"
  ))
  add <- function(source_name, value, axis) {
    if (is.null(value) || length(value) != 1L || is.na(value)) return()
    evidence[[length(evidence) + 1L]] <<- list(
      source = source_name,
      value = as.character(value),
      axis = axis,
      strength = "strong"
    )
  }

  axis_value <- toupper(as.character(attributes$axis %||% NA_character_))
  axis_map <- c(X = "longitude", Y = "latitude", Z = "depth", T = "time")
  if (!is.na(axis_value) && axis_value %in% names(axis_map)) {
    add("axis", attributes$axis, unname(axis_map[[axis_value]]))
  }

  standard_name <- tolower(as.character(
    attributes$standard_name %||% NA_character_
  ))
  standard_axis <- if (identical(standard_name, "longitude")) {
    "longitude"
  } else if (identical(standard_name, "latitude")) {
    "latitude"
  } else if (identical(standard_name, "time")) {
    "time"
  } else if (standard_name %in% c(
    "depth", "sea_floor_depth_below_geoid", "height", "altitude",
    "ocean_sigma_coordinate", "ocean_s_coordinate",
    "ocean_s_coordinate_g1", "ocean_s_coordinate_g2"
  )) {
    "depth"
  } else {
    NA_character_
  }
  if (!is.na(standard_axis)) {
    add("standard_name", attributes$standard_name, standard_axis)
  }

  units <- tolower(as.character(attributes$units %||% NA_character_))
  if (units %in% c("degrees_east", "degree_east", "degrees_e", "degree_e")) {
    add("units", attributes$units, "longitude")
  }
  if (units %in% c("degrees_north", "degree_north", "degrees_n", "degree_n")) {
    add("units", attributes$units, "latitude")
  }
  if (!is.na(units) && grepl(
    "^[[:alpha:]_]+[[:space:]]+since[[:space:]]+", units
  )) {
    add("units", attributes$units, "time")
  }

  positive <- tolower(as.character(attributes$positive %||% NA_character_))
  if (positive %in% c("up", "down")) {
    add("positive", attributes$positive, "depth")
  }
  if (!is.null(attributes$formula_terms)) {
    add("formula_terms", attributes$formula_terms, "depth")
  }
  evidence
}

.cf_axis_known_names <- function() {
  list(
    longitude = c("lon", "longitude", "x"),
    latitude = c("lat", "latitude", "y"),
    depth = c("depth", "deptht", "lev", "level", "z", "zlev"),
    time = c("time", "date", "t")
  )
}

.cf_selected_dimensions <- function(source, selected_variables) {
  selected_variables <- intersect(
    as.character(selected_variables), source$variables$order
  )
  unique(unlist(lapply(selected_variables, function(path) {
    source$variables$map[[path]]$source_dimensions
  }), use.names = FALSE))
}

.cf_match_dimension_override <- function(source, candidates, override) {
  exact <- candidates[candidates == override]
  if (length(exact) == 1L) return(exact)
  by_basename <- candidates[
    vapply(candidates, .cf_basename, character(1L)) == override
  ]
  if (length(by_basename) == 1L) return(by_basename)
  character()
}

.cf_resolve_one_axis <- function(source, candidates, evidence, axis,
                                 override = NULL) {
  strong_axes <- lapply(evidence, function(items) {
    unique(vapply(items, `[[`, character(1L), "axis")[
      vapply(items, `[[`, character(1L), "strength") == "strong" &
        !is.na(vapply(items, `[[`, character(1L), "axis"))
    ])
  })
  conflicts <- names(strong_axes)[lengths(strong_axes) > 1L]

  if (!is.null(override)) {
    matched <- .cf_match_dimension_override(source, candidates, override)
    if (length(matched) != 1L) {
      return(list(
        axis = axis, status = "UNRESOLVED", source_id = NA_character_,
        method = "explicit", evidence = list(), candidates = matched,
        diagnostic = paste0("explicit dimension `", override, "` does not exist")
      ))
    }
    candidate <- matched[[1L]]
    assigned <- strong_axes[[candidate]]
    if (candidate %in% conflicts ||
        (length(assigned) == 1L && !identical(assigned, axis))) {
      return(list(
        axis = axis, status = "CONFLICT", source_id = candidate,
        method = "explicit", evidence = evidence[[candidate]],
        candidates = candidate,
        diagnostic = "explicit override contradicts strong CF evidence"
      ))
    }
    method <- "explicit"
    diagnostic <- if (!length(assigned)) "OVERRIDDEN" else NA_character_
    return(list(
      axis = axis, status = "RESOLVED", source_id = candidate,
      method = method, evidence = evidence[[candidate]],
      candidates = candidate, diagnostic = diagnostic
    ))
  }

  valid <- candidates[vapply(candidates, function(candidate) {
    identical(strong_axes[[candidate]], axis)
  }, logical(1L))]
  if (length(valid) > 1L) {
    return(list(
      axis = axis, status = "AMBIGUOUS", source_id = NA_character_,
      method = "cf_attributes", evidence = unname(evidence[valid]),
      candidates = valid, diagnostic = "multiple CF candidates"
    ))
  }
  if (length(valid) == 1L) {
    candidate <- valid[[1L]]
    return(list(
      axis = axis, status = "RESOLVED", source_id = candidate,
      method = "cf_attributes", evidence = evidence[[candidate]],
      candidates = candidate, diagnostic = NA_character_
    ))
  }

  conflict_candidates <- intersect(conflicts, candidates)
  conflict_for_axis <- conflict_candidates[vapply(
    conflict_candidates,
    function(candidate) axis %in% strong_axes[[candidate]],
    logical(1L)
  )]
  if (length(conflict_for_axis)) {
    return(list(
      axis = axis, status = "CONFLICT", source_id = NA_character_,
      method = "cf_attributes", evidence = unname(evidence[conflict_for_axis]),
      candidates = conflict_for_axis,
      diagnostic = "incompatible strong CF evidence"
    ))
  }

  known <- .cf_axis_known_names()[[axis]]
  name_candidates <- candidates[
    tolower(vapply(candidates, .cf_basename, character(1L))) %in% known
  ]
  if (length(name_candidates) > 1L) {
    return(list(
      axis = axis, status = "AMBIGUOUS", source_id = NA_character_,
      method = "known_name", evidence = unname(evidence[name_candidates]),
      candidates = name_candidates, diagnostic = "multiple known-name candidates"
    ))
  }
  if (length(name_candidates) == 1L) {
    candidate <- name_candidates[[1L]]
    return(list(
      axis = axis, status = "RESOLVED", source_id = candidate,
      method = "known_name",
      evidence = c(evidence[[candidate]], list(list(
        source = "known_name", value = .cf_basename(candidate), axis = axis,
        strength = "weak"
      ))),
      candidates = candidate, diagnostic = NA_character_
    ))
  }

  list(
    axis = axis, status = "UNRESOLVED", source_id = NA_character_,
    method = "none", evidence = list(), candidates = character(),
    diagnostic = "no semantic or known-name evidence"
  )
}

.cf_resolve_axes <- function(cf, selected_variables, lon_name = NULL,
                             lat_name = NULL, depth_name = NULL,
                             time_name = NULL) {
  .cf_validate_cf(cf, require_current = FALSE)
  source <- cf$source
  candidates <- .cf_selected_dimensions(source, selected_variables)
  candidates <- source$dimensions$order[source$dimensions$order %in% candidates]
  evidence <- stats::setNames(lapply(candidates, function(id) {
    .cf_axis_evidence(source, id)
  }), candidates)
  overrides <- list(
    longitude = lon_name,
    latitude = lat_name,
    depth = depth_name,
    time = time_name
  )
  axes <- lapply(names(overrides), function(axis) {
    .cf_resolve_one_axis(
      source, candidates, evidence, axis, overrides[[axis]]
    )
  })
  names(axes) <- names(overrides)
  axes
}

.cf_resolution_evidence_labels <- function(resolution) {
  if (!length(resolution$evidence)) return(character())
  vapply(resolution$evidence, function(item) {
    if (is.na(item$axis)) {
      paste0(item$source, "=", item$value)
    } else {
      paste0(item$source, "=", item$value, "->", item$axis)
    }
  }, character(1L))
}

.cf_require_axis <- function(resolution, available, reader = "deferred",
                             required = TRUE) {
  if (identical(resolution$status, "RESOLVED")) return(resolution)
  if (!isTRUE(required) && identical(resolution$status, "UNRESOLVED")) {
    return(NULL)
  }
  axis <- resolution$axis
  if (identical(resolution$status, "AMBIGUOUS")) {
    .netcdf_abort(paste0(
      "Cannot resolve the ", axis,
      " dimension because multiple CF candidates were found: ",
      paste(resolution$candidates, collapse = ", "),
      ". Specify the ", axis, " dimension explicitly."
    ))
  }
  if (identical(resolution$status, "CONFLICT")) {
    .netcdf_abort(paste0(
      "Cannot resolve the ", axis,
      " dimension because strong CF evidence conflicts for: ",
      paste(resolution$candidates, collapse = ", "), "."
    ), class = "oceancube_cf_axis_conflict")
  }
  if (identical(resolution$method, "explicit")) {
    .netcdf_abort(paste0(
      "Cannot resolve the ", axis, " dimension: ", resolution$diagnostic,
      ". Available dimensions: ", paste(available, collapse = ", "), "."
    ))
  }
  if (identical(reader, "eager")) {
    rlang::abort(paste0(
      "Could not identify ", axis, ". Available dimensions: ",
      paste(available, collapse = ", ")
    ))
  }
  .netcdf_abort(paste0(
    "Cannot resolve the ", axis, " dimension. Available dimensions: ",
    paste(available, collapse = ", "),
    ". No unique CF attribute or known name identified it; specify the dimension explicitly."
  ))
}

.cf_resolution_for_storage <- function(resolution) {
  list(
    status = resolution$status,
    method = resolution$method,
    evidence = .cf_resolution_evidence_labels(resolution),
    diagnostic = resolution$diagnostic
  )
}

.cf_build_current <- function(cf, selected_variables, resolutions,
                              explicit_depth) {
  axes <- lapply(c("longitude", "latitude", "depth", "time"), function(axis) {
    resolution <- resolutions[[axis]]
    if (identical(axis, "depth") && !isTRUE(explicit_depth)) {
      return(list(
        source_id = NA_character_,
        status = "RESOLVED",
        method = "inserted_singleton",
        evidence = "no explicit vertical coordinate in selected variables",
        semantic = "inserted singleton / no explicit vertical coordinate"
      ))
    }
    list(
      source_id = resolution$source_id,
      status = resolution$status,
      method = resolution$method,
      evidence = .cf_resolution_evidence_labels(resolution),
      semantic = "source coordinate"
    )
  })
  names(axes) <- c("longitude", "latitude", "depth", "time")
  selected_variables <- as.character(selected_variables)
  related_links <- which(vapply(cf$source$links, function(link) {
    link$source_path %in% selected_variables ||
      (!is.na(link$resolved_path) && link$resolved_path %in% selected_variables)
  }, logical(1L)))
  cf$current <- list(
    variables = selected_variables,
    axes = axes,
    links = as.integer(related_links),
    semantic_status = "CURRENT_SUPPORTED_SUBSET"
  )
  cf
}

.cf_wrap_metadata <- function(cf) {
  metadata <- list(
    schema_name = "oceancube_metadata",
    schema_version = "1.0.0",
    cf = cf
  )
  .cf_metadata_validate(metadata)
  metadata
}

.cf_metadata_from_storage <- function(cf, storage) {
  .cf_validate_cf(cf, require_current = FALSE)
  .validate_netcdf_storage(storage, check_file = FALSE)
  canonical <- storage$dimensions$canonical
  axes <- lapply(c("longitude", "latitude", "depth", "time"), function(axis) {
    descriptor <- canonical[[axis]]
    inserted <- identical(descriptor$detection$method, "inserted_singleton")
    list(
      source_id = if (inserted) NA_character_ else descriptor$source_dimension,
      status = descriptor$detection$status %||% "RESOLVED",
      method = descriptor$detection$method,
      evidence = descriptor$detection$evidence,
      semantic = if (inserted) {
        "inserted singleton / no explicit vertical coordinate"
      } else {
        "source coordinate"
      }
    )
  })
  names(axes) <- c("longitude", "latitude", "depth", "time")
  selected <- storage$variables$order
  related_links <- which(vapply(cf$source$links, function(link) {
    link$source_path %in% selected ||
      (!is.na(link$resolved_path) && link$resolved_path %in% selected)
  }, logical(1L)))
  cf$current <- list(
    variables = selected,
    axes = axes,
    links = as.integer(related_links),
    semantic_status = "CURRENT_SUPPORTED_SUBSET"
  )
  .cf_wrap_metadata(cf)
}

.cf_contains_forbidden <- function(x) {
  if (is.function(x) || is.environment(x) || is.array(x) ||
      inherits(x, "connection") || inherits(x, "ncdf4") ||
      inherits(x, "R6") || identical(typeof(x), "externalptr")) {
    return(TRUE)
  }
  if (!is.list(x)) return(FALSE)
  any(vapply(x, .cf_contains_forbidden, logical(1L)))
}

.cf_validate_ordered_map <- function(x, label) {
  if (!is.list(x) || !is.character(x$order) || !is.list(x$map) ||
      anyNA(x$order) || any(!nzchar(x$order)) || anyDuplicated(x$order) ||
      !identical(names(x$map), x$order)) {
    .cf_metadata_abort(paste0(
      "Invalid CF metadata `", label, "`: order and map are inconsistent."
    ))
  }
  invisible(TRUE)
}

.cf_validate_attribute_records <- function(records, owner_path, scope) {
  if (!is.list(records)) {
    .cf_metadata_abort("Invalid CF metadata attribute registry.")
  }
  if (!length(records)) return(invisible(TRUE))

  required <- c(
    "name", "source_order", "raw_value", "r_type", "r_class", "length",
    "owner_path", "scope", "source_primitive_type",
    "primitive_type_status"
  )
  orders <- integer(length(records))
  names_seen <- character(length(records))
  for (i in seq_along(records)) {
    record <- records[[i]]
    if (!is.list(record) || length(setdiff(required, names(record))) > 0L ||
        !is.character(record$name) || length(record$name) != 1L ||
        is.na(record$name) || !nzchar(record$name) ||
        !is.integer(record$source_order) || length(record$source_order) != 1L ||
        !identical(record$owner_path, owner_path) ||
        !identical(record$scope, scope) ||
        !identical(record$r_type, typeof(record$raw_value)) ||
        !identical(record$r_class, class(record$raw_value)) ||
        !identical(record$length, as.integer(length(record$raw_value))) ||
        !is.character(record$primitive_type_status) ||
        length(record$primitive_type_status) != 1L ||
        !record$primitive_type_status %in% c("available", "unavailable")) {
      .cf_metadata_abort(paste0(
        "Invalid CF metadata attribute record for `", owner_path, "`."
      ))
    }
    orders[[i]] <- record$source_order
    names_seen[[i]] <- record$name
  }
  if (!identical(orders, seq_along(records)) || anyDuplicated(names_seen)) {
    .cf_metadata_abort(paste0(
      "Invalid CF metadata attribute order for `", owner_path, "`."
    ))
  }
  invisible(TRUE)
}

.cf_validate_cf <- function(cf, require_current = TRUE) {
  if (!is.list(cf) ||
      !identical(cf$schema_name, "oceancube_cf_metadata") ||
      !identical(cf$schema_version, "1.0.0")) {
    .cf_metadata_abort(
      "Invalid CF metadata schema; expected oceancube_cf_metadata 1.0.0."
    )
  }
  if (.cf_contains_forbidden(cf)) {
    .cf_metadata_abort(
      "Invalid CF metadata: canonical metadata must contain serializable plain-R descriptor values only."
    )
  }
  required_source <- c(
    "format", "scanner", "declaration", "global_attributes", "dimensions",
    "variables", "links"
  )
  if (!is.list(cf$source) ||
      length(setdiff(required_source, names(cf$source))) > 0L) {
    .cf_metadata_abort("Invalid CF metadata source tree.")
  }
  .cf_validate_ordered_map(cf$source$dimensions, "source$dimensions")
  .cf_validate_ordered_map(cf$source$variables, "source$variables")
  .cf_validate_attribute_records(
    cf$source$global_attributes, owner_path = "/", scope = "global"
  )

  for (i in seq_along(cf$source$dimensions$order)) {
    id <- cf$source$dimensions$order[[i]]
    descriptor <- cf$source$dimensions$map[[id]]
    if (!is.list(descriptor) || !identical(descriptor$source_id, id) ||
        !identical(descriptor$source_path, id) ||
        !is.integer(descriptor$source_order) ||
        length(descriptor$source_order) != 1L ||
        !identical(descriptor$source_order, as.integer(i))) {
      .cf_metadata_abort(paste0("Invalid CF dimension descriptor `", id, "`."))
    }
  }
  for (i in seq_along(cf$source$variables$order)) {
    id <- cf$source$variables$order[[i]]
    descriptor <- cf$source$variables$map[[id]]
    if (!is.list(descriptor) || !identical(descriptor$source_id, id) ||
        !identical(descriptor$source_path, id) ||
        !identical(descriptor$source_order, as.integer(i)) ||
        !is.character(descriptor$primitive_type_status) ||
        length(descriptor$primitive_type_status) != 1L ||
        !descriptor$primitive_type_status %in% c("available", "unavailable") ||
        !is.character(descriptor$roles) ||
        any(!descriptor$roles %in% .cf_role_vocabulary())) {
      .cf_metadata_abort(paste0("Invalid CF variable descriptor `", id, "`."))
    }
    .cf_validate_attribute_records(
      descriptor$attributes, owner_path = id, scope = "variable"
    )
  }

  allowed_link_status <- c(
    "RESOLVED", "MISSING_TARGET", "SELF_REFERENCE",
    "DUPLICATE_REFERENCE", "AMBIGUOUS", "UNKNOWN_FORM",
    "DEFERRED_EXTENDED"
  )
  required_link <- c(
    "source_path", "attribute", "raw_value", "target_raw", "key",
    "candidate_target_paths", "resolved_path", "status", "parser_status"
  )
  for (link in cf$source$links) {
    if (!is.list(link) || length(setdiff(required_link, names(link))) > 0L ||
        !link$status %in% allowed_link_status ||
        !link$source_path %in% cf$source$variables$order ||
        !is.character(link$candidate_target_paths) ||
        any(!link$candidate_target_paths %in% cf$source$variables$order) ||
        !is.character(link$resolved_path) || length(link$resolved_path) != 1L ||
        (!is.na(link$resolved_path) &&
          !link$resolved_path %in% cf$source$variables$order)) {
      .cf_metadata_abort("Invalid CF metadata link record.")
    }
  }

  if (isTRUE(require_current)) {
    if (!is.list(cf$current) ||
        !is.character(cf$current$variables) ||
        anyDuplicated(cf$current$variables) ||
        !all(cf$current$variables %in% cf$source$variables$order) ||
        !is.integer(cf$current$links) ||
        anyNA(cf$current$links) || anyDuplicated(cf$current$links) ||
        any(cf$current$links < 1L | cf$current$links > length(cf$source$links)) ||
        !is.list(cf$current$axes) ||
        !is.character(cf$current$semantic_status) ||
        length(cf$current$semantic_status) != 1L ||
        !cf$current$semantic_status %in% c(
          "CURRENT_SUPPORTED_SUBSET", "CURRENT_SELECTION",
          "DERIVATION_PENDING"
        ) ||
        !identical(names(cf$current$axes),
          c("longitude", "latitude", "depth", "time"))) {
      .cf_metadata_abort("Invalid CF current metadata view.")
    }
    for (axis in names(cf$current$axes)) {
      descriptor <- cf$current$axes[[axis]]
      source_id <- if (is.list(descriptor)) descriptor$source_id else NULL
      if (!is.list(descriptor) ||
          !is.character(descriptor$status) || length(descriptor$status) != 1L ||
          !descriptor$status %in% c(
            "RESOLVED", "AMBIGUOUS", "CONFLICT", "UNRESOLVED"
          ) ||
          !is.character(descriptor$method) || length(descriptor$method) != 1L ||
          !is.character(source_id) || length(source_id) != 1L ||
          (!is.na(source_id) && !source_id %in% cf$source$dimensions$order)) {
        .cf_metadata_abort(paste0(
          "Invalid current CF axis reference for `", axis, "`."
        ))
      }
    }
  }
  tryCatch(
    serialize(cf, connection = NULL),
    error = function(e) {
      .cf_metadata_abort(paste0(
        "Invalid CF metadata: serialization failed: ", conditionMessage(e)
      ))
    }
  )
  invisible(TRUE)
}

.cf_metadata_validate <- function(metadata) {
  if (is.null(metadata)) return(invisible(TRUE))
  if (!is.list(metadata) ||
      !identical(metadata$schema_name, "oceancube_metadata") ||
      !identical(metadata$schema_version, "1.0.0") ||
      !is.list(metadata$cf)) {
    .cf_metadata_abort(
      "Invalid metadata schema; expected oceancube_metadata 1.0.0."
    )
  }
  if (.cf_contains_forbidden(metadata)) {
    .cf_metadata_abort(
      "Invalid metadata: canonical metadata must contain serializable plain-R descriptor values only."
    )
  }
  .cf_validate_cf(metadata$cf, require_current = TRUE)
  invisible(TRUE)
}

.attach_cube_metadata <- function(x, metadata) {
  .cf_metadata_validate(metadata)
  out <- x
  out["metadata"] <- list(metadata)
  .check_cube(out)
  out
}

.cf_metadata_for_selection <- function(metadata, index, variables) {
  if (is.null(metadata)) return(NULL)
  .cf_metadata_validate(metadata)
  out <- metadata
  out$cf$current$variables <- as.character(variables)
  out$cf$current$semantic_status <- if (identical(
    metadata$cf$current$semantic_status,
    "DERIVATION_PENDING"
  )) {
    "DERIVATION_PENDING"
  } else {
    "CURRENT_SELECTION"
  }
  out$cf$current$selection <- list(
    axis_lengths = stats::setNames(
      as.integer(vapply(index, length, integer(1L))),
      names(index)
    ),
    variables = as.character(variables)
  )
  .cf_metadata_validate(out)
  out
}

.cf_metadata_for_transform <- function(metadata, operation) {
  if (is.null(metadata)) return(NULL)
  .cf_metadata_validate(metadata)
  out <- metadata
  out$cf$current$semantic_status <- "DERIVATION_PENDING"
  out$cf$current$derivation <- list(
    operation = as.character(operation),
    status = "current semantic derivation pending"
  )
  .cf_metadata_validate(out)
  out
}
