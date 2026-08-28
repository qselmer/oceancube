options(warn = 1)

if (!requireNamespace("ncdf4", quietly = TRUE)) {
  stop("The B1 prototype requires the existing ncdf4 package.")
}

cf_link_specs <- list(
  coordinates = "names",
  bounds = "single",
  climatology = "single",
  ancillary_variables = "names",
  cell_measures = "pairs",
  grid_mapping = "grid_mapping",
  formula_terms = "pairs"
)

attribute_record <- function(name, value, position) {
  list(
    name = name,
    source_order = as.integer(position),
    source_primitive_type = NA_character_,
    source_type_status = "unavailable-via-public-ncdf4",
    r_type = typeof(value),
    r_class = class(value),
    length = length(value),
    raw_value = value
  )
}

scan_attributes <- function(nc, variable = 0) {
  values <- ncdf4::ncatt_get(nc, variable)
  if (!length(values)) return(list())
  Map(attribute_record, names(values), unname(values), seq_along(values))
}

attribute_value <- function(records, name, default = NULL) {
  hit <- which(vapply(records, `[[`, character(1L), "name") == name)
  if (!length(hit)) return(default)
  records[[hit[[1L]]]]$raw_value
}

split_names <- function(value) {
  value <- trimws(as.character(value))
  if (!nzchar(value)) return(character())
  strsplit(value, "[[:space:]]+")[[1L]]
}

parse_pairs <- function(value) {
  raw <- trimws(as.character(value))
  if (!nzchar(raw)) {
    return(list(status = "EMPTY", entries = list(), tokens = character()))
  }
  tokens <- split_names(raw)
  is_key <- grepl(":$", tokens)
  odd <- seq.int(1L, length(tokens), by = 2L)
  even <- seq.int(2L, length(tokens), by = 2L)
  if (length(tokens) %% 2L || !all(is_key[odd]) || any(is_key[even])) {
    return(list(status = "UNKNOWN_FORM", entries = list(), tokens = tokens))
  }
  entries <- lapply(seq(1L, length(tokens), by = 2L), function(i) {
    list(key = sub(":$", "", tokens[[i]]), target = tokens[[i + 1L]])
  })
  list(status = "PARSED", entries = entries, tokens = tokens)
}

parse_grid_mapping <- function(value) {
  raw <- trimws(as.character(value))
  tokens <- split_names(raw)
  if (length(tokens) == 1L && !grepl(":", tokens)) {
    return(list(
      status = "PARSED_SIMPLE", entries = list(list(key = NA_character_, target = raw)),
      tokens = tokens
    ))
  }
  list(status = "DEFERRED_EXTENDED", entries = list(), tokens = tokens)
}

parse_link <- function(attribute, value) {
  kind <- cf_link_specs[[attribute]]
  if (identical(kind, "single")) {
    tokens <- split_names(value)
    status <- if (length(tokens) == 1L) "PARSED" else "UNKNOWN_FORM"
    entries <- if (status == "PARSED") {
      list(list(key = NA_character_, target = tokens[[1L]]))
    } else {
      list()
    }
    return(list(status = status, entries = entries, tokens = tokens))
  }
  if (identical(kind, "names")) {
    tokens <- split_names(value)
    entries <- lapply(tokens, function(x) list(key = NA_character_, target = x))
    return(list(status = "PARSED", entries = entries, tokens = tokens))
  }
  if (identical(kind, "pairs")) return(parse_pairs(value))
  if (identical(kind, "grid_mapping")) return(parse_grid_mapping(value))
  list(status = "UNSUPPORTED_ATTRIBUTE", entries = list(), tokens = character())
}

resolve_reference <- function(source, target, variable_names) {
  if (!nzchar(target)) {
    return(list(status = "MISSING_TARGET", matches = character()))
  }
  exact <- variable_names[variable_names == target]
  if (length(exact) == 1L) {
    return(list(
      status = if (identical(source, target)) "SELF_REFERENCE" else "RESOLVED",
      matches = exact
    ))
  }
  basename_matches <- variable_names[basename(variable_names) == basename(target)]
  if (length(basename_matches) > 1L) {
    return(list(status = "AMBIGUOUS", matches = basename_matches))
  }
  if (length(basename_matches) == 1L) {
    return(list(
      status = if (identical(source, basename_matches)) "SELF_REFERENCE" else "RESOLVED",
      matches = basename_matches
    ))
  }
  list(status = "MISSING_TARGET", matches = character())
}

scan_links <- function(variables) {
  variable_names <- names(variables)
  links <- list()
  diagnostics <- list()
  n <- 0L
  d <- 0L

  for (source in variable_names) {
    attrs <- variables[[source]]$attributes
    for (attribute in intersect(names(cf_link_specs), vapply(attrs, `[[`, character(1L), "name"))) {
      raw <- attribute_value(attrs, attribute)
      parsed <- parse_link(attribute, raw)
      if (parsed$status %in% c("UNKNOWN_FORM", "DEFERRED_EXTENDED")) {
        d <- d + 1L
        diagnostics[[d]] <- list(
          severity = "INFO", code = parsed$status, source = source,
          attribute = attribute, raw_value = raw
        )
      }
      if (!length(parsed$entries)) next
      targets <- vapply(parsed$entries, `[[`, character(1L), "target")
      duplicated_targets <- duplicated(targets)
      for (i in seq_along(parsed$entries)) {
        entry <- parsed$entries[[i]]
        resolution <- resolve_reference(source, entry$target, variable_names)
        status <- if (duplicated_targets[[i]]) "DUPLICATE_REFERENCE" else resolution$status
        n <- n + 1L
        links[[n]] <- list(
          source = source,
          attribute = attribute,
          key = entry$key,
          target_raw = entry$target,
          target = if (length(resolution$matches) == 1L) resolution$matches else NA_character_,
          status = status,
          candidates = resolution$matches,
          raw_value = raw,
          parser_status = parsed$status
        )
        if (status != "RESOLVED" &&
            !(attribute == "formula_terms" && status == "SELF_REFERENCE")) {
          d <- d + 1L
          diagnostics[[d]] <- list(
            severity = "WARNING", code = status, source = source,
            attribute = attribute, target = entry$target
          )
        }
      }
    }
  }
  list(links = links, diagnostics = diagnostics)
}

role_for_link <- c(
  coordinates = "auxiliary_coordinate",
  bounds = "bounds",
  climatology = "climatology_bounds",
  ancillary_variables = "ancillary",
  cell_measures = "cell_measure",
  grid_mapping = "grid_mapping",
  formula_terms = "formula_term"
)

classify_roles <- function(variables, links, dimension_names) {
  roles <- setNames(lapply(names(variables), function(x) {
    if (x %in% dimension_names) "dimension_coordinate" else "data"
  }), names(variables))
  for (link in links) {
    if (!is.na(link$target) && link$status == "RESOLVED") {
      roles[[link$target]] <- unique(c(roles[[link$target]], role_for_link[[link$attribute]]))
    }
  }
  for (name in names(variables)) {
    attrs <- variables[[name]]$attributes
    if (!is.null(attribute_value(attrs, "flag_values")) ||
        !is.null(attribute_value(attrs, "flag_masks"))) {
      roles[[name]] <- unique(c(roles[[name]], "quality_flag"))
    }
    if (!is.null(attribute_value(attrs, "geometry_type"))) {
      roles[[name]] <- unique(c(roles[[name]], "geometry"))
    }
  }
  roles
}

scan_cf_metadata <- function(file) {
  nc <- ncdf4::nc_open(file, readunlim = FALSE, suppress_dimvals = FALSE)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  dimension_order <- names(nc$dim)
  dimensions <- setNames(lapply(seq_along(nc$dim), function(i) {
    x <- nc$dim[[i]]
    list(
      source_name = x$name,
      source_path = x$name,
      source_order = as.integer(i),
      length = as.integer(x$len),
      unlimited = isTRUE(x$unlim),
      coordinate_variable = if (isTRUE(x$create_dimvar)) x$name else NA_character_
    )
  }), dimension_order)

  variables <- list()
  for (name in dimension_order) {
    dim <- nc$dim[[name]]
    if (!isTRUE(dim$create_dimvar)) next
    attrs <- scan_attributes(nc, name)
    variables[[name]] <- list(
      source_name = name,
      source_path = name,
      source_order = NA_integer_,
      source_group_index = as.integer(dim$group_index),
      source_variable_id = as.integer(dim$dimvarid$id),
      source_type = NA_character_,
      source_dimensions = name,
      source_dimension_order = name,
      attributes = attrs,
      coordinate_value_type = typeof(dim$vals),
      small_coordinate_values = if (length(dim$vals) <= 4L) as.vector(dim$vals) else NULL
    )
  }
  for (name in names(nc$var)) {
    var <- nc$var[[name]]
    variables[[name]] <- list(
      source_name = name,
      source_path = name,
      source_order = NA_integer_,
      source_group_index = as.integer(var$group_index),
      source_variable_id = as.integer(var$id$id),
      source_type = var$prec,
      source_dimensions = vapply(var$dim, `[[`, character(1L), "name"),
      source_dimension_order = vapply(var$dim, `[[`, character(1L), "name"),
      attributes = scan_attributes(nc, name),
      coordinate_value_type = NULL,
      small_coordinate_values = NULL
    )
  }

  variable_order <- names(variables)[order(
    vapply(variables, `[[`, integer(1L), "source_group_index"),
    vapply(variables, `[[`, integer(1L), "source_variable_id")
  )]
  variables <- variables[variable_order]
  for (i in seq_along(variables)) variables[[i]]$source_order <- as.integer(i)

  linked <- scan_links(variables)
  roles <- classify_roles(variables, linked$links, dimension_order)
  for (name in names(variables)) variables[[name]]$roles <- roles[[name]]
  globals <- scan_attributes(nc, 0)
  conventions <- attribute_value(globals, "Conventions")

  result <- list(
    schema_name = "oceancube_cf_metadata",
    schema_version = "1.0.0",
    source = list(format = "NetCDF", scanner = "B1 native ncdf4 prototype"),
    declaration = list(raw = conventions, cf_version = {
      if (is.null(conventions)) NA_character_ else {
        hit <- regmatches(conventions, regexpr("CF-[0-9]+(?:\\.[0-9]+)?", conventions))
        if (length(hit) && nzchar(hit)) hit else NA_character_
      }
    }),
    global_attributes = globals,
    dimensions = list(order = dimension_order, map = dimensions),
    variables = list(order = variable_order, map = variables),
    links = linked$links,
    interpretation = list(
      status = "PARTIAL",
      axis_resolution = "not implemented in B1 prototype",
      extended_grid_mapping = "preserved raw; interpretation deferred"
    ),
    diagnostics = linked$diagnostics
  )
  stopifnot(!any(vapply(result, inherits, logical(1L), what = "ncdf4")))
  result
}

make_cf_rich_fixture <- function(file = tempfile(fileext = ".nc"),
                                 include_formula_terms = TRUE) {
  lon <- ncdf4::ncdim_def("lon", "degrees_east", c(-80.5, -79.5))
  lat <- ncdf4::ncdim_def("lat", "degrees_north", c(-12.5, -11.5))
  sigma <- ncdf4::ncdim_def("sigma", "1", c(0.1, 0.9))
  time <- ncdf4::ncdim_def("time", "days since 2000-01-01", c(15, 45))
  nv <- ncdf4::ncdim_def("nv", "", 1:2, create_dimvar = FALSE)

  defs <- list(
    ncdf4::ncvar_def("lon_bnds", "degrees_east", list(nv, lon)),
    ncdf4::ncvar_def("lat_bnds", "degrees_north", list(nv, lat)),
    ncdf4::ncvar_def("time_clim", "days since 2000-01-01", list(nv, time)),
    ncdf4::ncvar_def("eta", "m", list(lon, lat)),
    ncdf4::ncvar_def("depth_ref", "m", list()),
    ncdf4::ncvar_def("crs", "", list(), prec = "integer"),
    ncdf4::ncvar_def("areacello", "m2", list(lon, lat)),
    ncdf4::ncvar_def("qc", "1", list(lon, lat, time), prec = "short"),
    ncdf4::ncvar_def("auxlat", "degrees_north", list(lon, lat)),
    ncdf4::ncvar_def(
      "temperature", "K", list(lon, lat, sigma, time),
      missval = -9999, prec = "float"
    )
  )
  nc <- ncdf4::nc_create(file, defs, force_v4 = TRUE)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.13")
  ncdf4::ncatt_put(nc, 0, "title", "Deterministic oceancube B1 CF-rich fixture")
  ncdf4::ncatt_put(nc, 0, "history", "B1 temporary prototype; not package provenance")
  ncdf4::ncatt_put(nc, "lon", "standard_name", "longitude")
  ncdf4::ncatt_put(nc, "lon", "axis", "X")
  ncdf4::ncatt_put(nc, "lon", "bounds", "lon_bnds")
  ncdf4::ncatt_put(nc, "lat", "standard_name", "latitude")
  ncdf4::ncatt_put(nc, "lat", "axis", "Y")
  ncdf4::ncatt_put(nc, "lat", "bounds", "lat_bnds")
  ncdf4::ncatt_put(nc, "sigma", "standard_name", "ocean_sigma_coordinate")
  ncdf4::ncatt_put(nc, "sigma", "axis", "Z")
  ncdf4::ncatt_put(nc, "sigma", "positive", "down")
  if (isTRUE(include_formula_terms)) {
    ncdf4::ncatt_put(
      nc, "sigma", "formula_terms", "sigma: sigma eta: eta depth: depth_ref"
    )
  }
  ncdf4::ncatt_put(nc, "time", "standard_name", "time")
  ncdf4::ncatt_put(nc, "time", "axis", "T")
  ncdf4::ncatt_put(nc, "time", "calendar", "standard")
  ncdf4::ncatt_put(nc, "time", "climatology", "time_clim")
  ncdf4::ncatt_put(nc, "crs", "grid_mapping_name", "latitude_longitude")
  ncdf4::ncatt_put(nc, "crs", "longitude_of_prime_meridian", 0)
  ncdf4::ncatt_put(nc, "auxlat", "standard_name", "latitude")
  ncdf4::ncatt_put(nc, "qc", "flag_values", c(0L, 1L), prec = "short")
  ncdf4::ncatt_put(nc, "qc", "flag_masks", c(1L, 1L), prec = "short")
  ncdf4::ncatt_put(nc, "qc", "flag_meanings", "good suspect")
  ncdf4::ncatt_put(nc, "temperature", "standard_name", "sea_water_temperature")
  ncdf4::ncatt_put(nc, "temperature", "coordinates", "auxlat")
  ncdf4::ncatt_put(nc, "temperature", "cell_methods", "time: mean within years time: mean over years")
  ncdf4::ncatt_put(nc, "temperature", "cell_measures", "area: areacello")
  ncdf4::ncatt_put(nc, "temperature", "ancillary_variables", "qc")
  ncdf4::ncatt_put(nc, "temperature", "grid_mapping", "crs")

  ncdf4::ncvar_put(nc, "lon_bnds", matrix(c(-81, -80, -80, -79), nrow = 2))
  ncdf4::ncvar_put(nc, "lat_bnds", matrix(c(-13, -12, -12, -11), nrow = 2))
  ncdf4::ncvar_put(nc, "time_clim", matrix(c(0, 30, 30, 60), nrow = 2))
  ncdf4::ncvar_put(nc, "depth_ref", 100)
  ncdf4::ncvar_put(nc, "crs", 1L)
  file
}

prototype_link_tests <- function() {
  vars <- setNames(lapply(c("v", "x", "g1/dup", "g2/dup"), function(x) {
    list(attributes = list())
  }), c("v", "x", "g1/dup", "g2/dup"))
  vars$v$attributes <- list(
    attribute_record("coordinates", "x x missing dup", 1L),
    attribute_record("bounds", "v", 2L),
    attribute_record("cell_measures", "area areacello", 3L),
    attribute_record("grid_mapping", "crs: x y", 4L)
  )
  found <- scan_links(vars)
  statuses <- vapply(found$links, `[[`, character(1L), "status")
  stopifnot(all(c(
    "RESOLVED", "DUPLICATE_REFERENCE", "MISSING_TARGET", "AMBIGUOUS",
    "SELF_REFERENCE"
  ) %in% statuses))
  codes <- vapply(found$diagnostics, `[[`, character(1L), "code")
  stopifnot(all(c("UNKNOWN_FORM", "DEFERRED_EXTENDED") %in% codes))
  invisible(TRUE)
}

oracle_summary <- function(file) {
  if (!requireNamespace("ncdfCF", quietly = TRUE)) {
    return(list(status = "NOT_INSTALLED"))
  }
  peek <- ncdfCF::peek_ncdf(file)
  dataset <- ncdfCF::open_ncdf(file)
  list(
    status = "EXECUTED",
    ncdfCF_version = as.character(utils::packageVersion("ncdfCF")),
    RNetCDF_version = as.character(utils::packageVersion("RNetCDF")),
    CFtime_version = as.character(utils::packageVersion("CFtime")),
    variables = dataset$var_names,
    axes = dataset$axis_names,
    conventions = dataset$conventions,
    peek = peek
  )
}

run_b1_prototype <- function() {
  prototype_link_tests()
  synthetic <- make_cf_rich_fixture()
  on.exit(unlink(synthetic), add = TRUE)
  model <- scan_cf_metadata(synthetic)
  roundtrip <- unserialize(serialize(model, NULL))
  stopifnot(identical(model, roundtrip))
  stopifnot(model$declaration$cf_version == "CF-1.13")
  attrs <- vapply(model$links, `[[`, character(1L), "attribute")
  stopifnot(all(names(cf_link_specs) %in% attrs))

  fixture_dir <- file.path("tests", "testthat", "fixtures", "real-data")
  fixtures <- c(
    OISST = "noaa-oisst21-surface-time-fv1.nc",
    ETOPO = "noaa-etopo2022-bathymetry-fv1.nc",
    WOA23 = "noaa-woa23-vertical-fv1.nc"
  )
  native <- lapply(file.path(fixture_dir, fixtures), scan_cf_metadata)
  oracle <- lapply(file.path(fixture_dir, fixtures), oracle_summary)

  cat("B1_NATIVE_PROTOTYPE: PASS\n")
  cat("schema:", model$schema_name, model$schema_version, "\n")
  cat("synthetic links:", length(model$links), "\n")
  cat("serialization: PASS\n")
  for (i in seq_along(fixtures)) {
    cat(
      names(fixtures)[[i]],
      "native variables=", length(native[[i]]$variables$order),
      "links=", length(native[[i]]$links),
      "oracle=", oracle[[i]]$status,
      "\n", sep = ""
    )
  }
  invisible(list(synthetic = model, native = native, oracle = oracle))
}

if (sys.nframe() == 0L) run_b1_prototype()
