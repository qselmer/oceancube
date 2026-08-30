# Internal CF vertical-coordinate semantics -------------------------------

.cf_vertical_parametric_names <- function() {
  c(
    "atmosphere_ln_pressure_coordinate",
    "atmosphere_sigma_coordinate",
    "atmosphere_hybrid_sigma_pressure_coordinate",
    "atmosphere_hybrid_height_coordinate",
    "atmosphere_sleve_coordinate",
    "ocean_sigma_coordinate",
    "ocean_s_coordinate",
    "ocean_s_coordinate_g1",
    "ocean_s_coordinate_g2",
    "ocean_sigma_z_coordinate",
    "ocean_double_sigma_coordinate"
  )
}

.cf_vertical_unit <- function(units) {
  raw <- if (is.null(units) || !length(units) || is.na(units[[1L]])) {
    NA_character_
  } else {
    trimws(as.character(units[[1L]]))
  }
  key <- tolower(raw)
  if (!nzchar(key) || is.na(key)) {
    return(list(raw = raw, family = "MISSING", normalized = NA_character_,
                scale_to_m = NA_real_))
  }
  if (key %in% c("m", "meter", "meters", "metre", "metres")) {
    return(list(raw = raw, family = "LENGTH", normalized = "m",
                scale_to_m = 1))
  }
  if (key %in% c("km", "kilometer", "kilometers", "kilometre", "kilometres")) {
    return(list(raw = raw, family = "LENGTH", normalized = "km",
                scale_to_m = 1000))
  }
  if (key %in% c("cm", "centimeter", "centimeters", "centimetre", "centimetres",
                 "mm", "millimeter", "millimeters", "millimetre", "millimetres",
                 "ft", "foot", "feet", "fathom", "fathoms")) {
    return(list(raw = raw, family = "LENGTH", normalized = NA_character_,
                scale_to_m = NA_real_))
  }
  pressure <- c(
    pa = "Pa", pascal = "Pa", pascals = "Pa",
    hpa = "hPa", kpa = "kPa", bar = "bar", bars = "bar",
    mbar = "mbar", millibar = "mbar", millibars = "mbar",
    dbar = "dbar", decibar = "dbar", decibars = "dbar",
    atm = "atm", atmosphere = "atm", atmospheres = "atm"
  )
  if (key %in% names(pressure)) {
    return(list(raw = raw, family = "PRESSURE",
                normalized = unname(pressure[[key]]), scale_to_m = NA_real_))
  }
  if (key %in% c("1", "level", "sigma_level")) {
    return(list(raw = raw, family = "DIMENSIONLESS", normalized = "1",
                scale_to_m = NA_real_))
  }
  list(raw = raw, family = "UNCLASSIFIED", normalized = NA_character_,
       scale_to_m = NA_real_)
}

.cf_vertical_source_order <- function(values) {
  if (length(values) <= 1L) return("SINGLETON")
  delta <- diff(values)
  if (all(delta > 0)) return("INCREASING")
  if (all(delta < 0)) return("DECREASING")
  if (any(delta == 0)) return("DUPLICATE")
  "NONMONOTONIC"
}

.vertical_geometry_tolerance <- function(...) {
  values <- unlist(list(...), recursive = TRUE, use.names = FALSE)
  values <- abs(as.numeric(values[is.finite(values)]))
  8 * sqrt(.Machine$double.eps) * max(c(1, values))
}

.cf_vertical_bounds_link <- function(cf, source_axis_id) {
  links <- Filter(function(link) {
    identical(link$source_path, source_axis_id) &&
      identical(link$attribute, "bounds")
  }, cf$source$links)
  if (!length(links)) return(NULL)
  links[[1L]]
}

.cf_vertical_read_bounds <- function(nc, cf, source_axis_id, centers,
                                     coordinate_unit) {
  link <- .cf_vertical_bounds_link(cf, source_axis_id)
  if (is.null(link)) {
    return(list(
      target = NA_character_, status = "BOUNDS_MISSING", units_raw = NA_character_,
      unit_family = "MISSING", normalized_unit = NA_character_, shape = integer(),
      values = list(), lower = numeric(), upper = numeric(),
      thickness = numeric(), coverage_contiguous = NA
    ))
  }
  if (!identical(link$status, "RESOLVED") || is.na(link$resolved_path)) {
    return(list(
      target = link$target_raw, status = paste0("BOUNDS_", link$status),
      units_raw = NA_character_, unit_family = "MISSING",
      normalized_unit = NA_character_, shape = integer(), values = list(),
      lower = numeric(), upper = numeric(), thickness = numeric(),
      coverage_contiguous = NA
    ))
  }
  target <- link$resolved_path
  variable <- nc$var[[target]]
  source_record <- cf$source$variables$map[[target]]
  if (is.null(variable) || is.null(source_record)) {
    return(list(
      target = target, status = "BOUNDS_TARGET_UNAVAILABLE",
      units_raw = NA_character_, unit_family = "MISSING",
      normalized_unit = NA_character_, shape = integer(), values = list(),
      lower = numeric(), upper = numeric(), thickness = numeric(),
      coverage_contiguous = NA
    ))
  }
  dims <- vapply(variable$dim, `[[`, character(1L), "name")
  lens <- as.integer(vapply(variable$dim, `[[`, numeric(1L), "len"))
  axis_at <- which(dims == source_axis_id)
  vertex_at <- which(lens == 2L & seq_along(lens) != axis_at)
  shape <- lens
  unit <- .cf_vertical_unit(.cf_attribute_value(source_record$attributes, "units"))
  if (length(axis_at) != 1L || length(vertex_at) != 1L || length(lens) != 2L ||
      lens[[axis_at]] != length(centers)) {
    return(list(
      target = target, status = "BOUNDS_INVALID_SHAPE", units_raw = unit$raw,
      unit_family = unit$family, normalized_unit = unit$normalized,
      shape = shape, values = list(), lower = numeric(), upper = numeric(),
      thickness = numeric(), coverage_contiguous = NA
    ))
  }
  raw <- ncdf4::ncvar_get(nc, target, collapse_degen = FALSE)
  paired <- if (axis_at == 1L) unname(raw) else t(unname(raw))
  if (!is.matrix(paired) || !identical(dim(paired), c(length(centers), 2L))) {
    return(list(
      target = target, status = "BOUNDS_INVALID_SHAPE", units_raw = unit$raw,
      unit_family = unit$family, normalized_unit = unit$normalized,
      shape = shape, values = list(), lower = numeric(), upper = numeric(),
      thickness = numeric(), coverage_contiguous = NA
    ))
  }
  coordinate_effective <- coordinate_unit
  bounds_effective <- unit
  if (identical(bounds_effective$family, "MISSING")) {
    bounds_effective <- coordinate_effective
  }
  status <- "BOUNDS_VALID"
  if (!identical(coordinate_effective$family, bounds_effective$family) ||
      (identical(coordinate_effective$family, "LENGTH") &&
       (is.na(coordinate_effective$normalized) || is.na(bounds_effective$normalized)))) {
    status <- "BOUNDS_UNIT_INCOMPATIBLE"
  }
  centers_for_bounds <- centers
  if (identical(status, "BOUNDS_VALID") &&
      identical(coordinate_effective$family, "LENGTH") &&
      !identical(coordinate_effective$normalized, bounds_effective$normalized)) {
    centers_for_bounds <- centers * coordinate_effective$scale_to_m /
      bounds_effective$scale_to_m
  }
  lower <- pmin(paired[, 1L], paired[, 2L])
  upper <- pmax(paired[, 1L], paired[, 2L])
  tolerance <- .vertical_geometry_tolerance(centers_for_bounds, paired)
  if (identical(status, "BOUNDS_VALID") &&
      (anyNA(paired) || any(!is.finite(paired)))) {
    status <- "BOUNDS_NONFINITE"
  }
  if (identical(status, "BOUNDS_VALID") && any(upper <= lower)) {
    status <- "BOUNDS_NONPOSITIVE"
  }
  if (identical(status, "BOUNDS_VALID") && any(
    centers_for_bounds < lower - tolerance | centers_for_bounds > upper + tolerance
  )) {
    status <- "BOUNDS_CENTRE_OUTSIDE"
  }
  ordered <- order(lower, upper)
  if (identical(status, "BOUNDS_VALID") && length(ordered) > 1L && any(
    lower[ordered][-1L] < upper[ordered][-length(ordered)] - tolerance
  )) {
    status <- "BOUNDS_OVERLAP"
  }
  contiguous <- if (!identical(status, "BOUNDS_VALID") || length(ordered) <= 1L) {
    if (identical(status, "BOUNDS_VALID")) TRUE else NA
  } else {
    all(abs(lower[ordered][-1L] - upper[ordered][-length(ordered)]) <= tolerance)
  }
  list(
    target = target,
    status = status,
    units_raw = unit$raw,
    unit_family = bounds_effective$family,
    normalized_unit = bounds_effective$normalized,
    shape = shape,
    values = lapply(seq_len(nrow(paired)), function(i) as.numeric(paired[i, ])),
    lower = as.numeric(lower),
    upper = as.numeric(upper),
    thickness = as.numeric(upper - lower),
    coverage_contiguous = contiguous
  )
}

.cf_vertical_formula_status <- function(cf, source_axis_id, standard_name) {
  record <- cf$source$variables$map[[source_axis_id]]
  raw <- .cf_attribute_value(record$attributes, "formula_terms")
  if (is.null(raw)) return("FORMULA_TERMS_ABSENT")
  if (!standard_name %in% .cf_vertical_parametric_names()) {
    return("FORMULA_TERMS_UNRECOGNIZED_STANDARD_NAME")
  }
  links <- Filter(function(link) {
    identical(link$source_path, source_axis_id) &&
      identical(link$attribute, "formula_terms")
  }, cf$source$links)
  coordinate_ok <- length(links) && all(vapply(links, function(link) {
    link$status %in% c("RESOLVED", "SELF_REFERENCE")
  }, logical(1L)))
  if (!coordinate_ok) return("PARAMETRIC_REFERENCES_UNRESOLVED")
  bounds_link <- .cf_vertical_bounds_link(cf, source_axis_id)
  if (is.null(bounds_link)) return("PARAMETRIC_STRUCTURALLY_RECOGNIZED")
  if (!identical(bounds_link$status, "RESOLVED") || is.na(bounds_link$resolved_path)) {
    return("PARAMETRIC_BOUNDS_UNRESOLVED")
  }
  boundary_links <- Filter(function(link) {
    identical(link$source_path, bounds_link$resolved_path) &&
      identical(link$attribute, "formula_terms")
  }, cf$source$links)
  if (!length(boundary_links)) return("PARAMETRIC_BOUNDS_FORMULA_TERMS_MISSING")
  if (!setequal(vapply(links, `[[`, character(1L), "key"),
                vapply(boundary_links, `[[`, character(1L), "key"))) {
    return("PARAMETRIC_BOUNDS_FORMULA_TERMS_MISMATCH")
  }
  if (!all(vapply(boundary_links, function(link) {
    link$status %in% c("RESOLVED", "SELF_REFERENCE")
  }, logical(1L)))) {
    return("PARAMETRIC_BOUNDS_REFERENCES_UNRESOLVED")
  }
  "PARAMETRIC_BOUNDS_STRUCTURALLY_RECOGNIZED"
}

.cf_vertical_diagnostic <- function(code, rule_kind, severity, message,
                                    blocking = FALSE) {
  list(
    code = code,
    rule_kind = rule_kind,
    severity = severity,
    message = message,
    blocking_for_geometry = isTRUE(blocking)
  )
}

.cf_vertical_descriptor <- function(nc, cf, source_axis_id = NULL,
                                    centers = NA_real_, explicit = TRUE) {
  if (!isTRUE(explicit) || is.null(source_axis_id)) {
    return(list(
      schema_name = "oceancube_cf_vertical",
      schema_version = "1.0.0",
      source_axis_id = NA_character_, source_coordinate = numeric(),
      kind = "SURFACE_SINGLETON",
      runtime_status = "VERTICAL_INTERPRETABLE_ONLY",
      geometry_status = "GEOMETRY_NO_BOUNDS",
      units_raw = NA_character_, unit_family = "NONE",
      normalized_unit = NA_character_, positive_raw = NA_character_,
      positive = NA_character_, standard_name = NA_character_,
      axis_attribute = NA_character_, source_order = "SINGLETON",
      orientation = "NO_EXPLICIT_VERTICAL_COORDINATE",
      bounds_target = NA_character_, bounds_status = "BOUNDS_MISSING",
      bounds_units_raw = NA_character_, bounds_unit = NA_character_,
      bounds_shape = integer(), bounds = list(), coverage_contiguous = NA,
      formula_terms_status = "FORMULA_TERMS_ABSENT",
      canonical_metric = FALSE, scale_to_m = NA_real_,
      sign_convention = "NOT_APPLICABLE", surface_status = "IMPLICIT_SURFACE",
      diagnostics = list(.cf_vertical_diagnostic(
        "VERTICAL_SURFACE_SINGLETON", "OCEANCUBE-SAFETY", "INFO",
        "No explicit vertical coordinate governs selected variables."
      ))
    ))
  }
  record <- cf$source$variables$map[[source_axis_id]]
  attrs <- record$attributes
  units_raw <- .cf_attribute_value(attrs, "units")
  unit <- .cf_vertical_unit(units_raw)
  standard_name <- tolower(as.character(
    .cf_attribute_value(attrs, "standard_name", NA_character_)
  )[[1L]])
  positive_raw <- .cf_attribute_value(attrs, "positive", NA_character_)
  positive <- tolower(trimws(as.character(positive_raw)[[1L]]))
  if (!positive %in% c("up", "down")) positive <- NA_character_
  axis_attribute <- toupper(as.character(
    .cf_attribute_value(attrs, "axis", NA_character_)
  )[[1L]])
  formula_status <- .cf_vertical_formula_status(cf, source_axis_id, standard_name)
  parametric <- standard_name %in% .cf_vertical_parametric_names()
  depth_name <- standard_name %in% c("depth", "sea_floor_depth_below_geoid")
  height_name <- standard_name %in% c("height", "altitude")
  conflict <- (depth_name && identical(positive, "up")) ||
    (height_name && identical(positive, "down"))
  kind <- if (parametric) {
    "PARAMETRIC"
  } else if (identical(unit$family, "PRESSURE")) {
    "PRESSURE"
  } else if (identical(unit$family, "DIMENSIONLESS")) {
    "DIMENSIONLESS_GENERIC"
  } else if (depth_name ||
             (identical(unit$family, "LENGTH") && identical(positive, "down"))) {
    "DEPTH_LENGTH"
  } else if (height_name ||
             (identical(unit$family, "LENGTH") && identical(positive, "up"))) {
    "HEIGHT_LENGTH"
  } else {
    "UNKNOWN_VERTICAL"
  }
  order <- .cf_vertical_source_order(centers)
  finite_monotonic <- is.numeric(centers) && !is.null(centers) && length(centers) &&
    !anyNA(centers) && all(is.finite(centers)) &&
    order %in% c("INCREASING", "DECREASING", "SINGLETON")
  effective_positive <- if (identical(kind, "PRESSURE") && is.na(positive)) {
    "down"
  } else {
    positive
  }
  diagnostics <- list()
  add_diagnostic <- function(code, rule_kind, severity, message, blocking = FALSE) {
    diagnostics[[length(diagnostics) + 1L]] <<- .cf_vertical_diagnostic(
      code, rule_kind, severity, message, blocking
    )
  }
  if (identical(unit$family, "MISSING")) {
    add_diagnostic(
      "VERTICAL_UNITS_MISSING", "REQUIREMENT", "ERROR",
      "Dimensional vertical coordinates require units.", TRUE
    )
  }
  if (!identical(kind, "PRESSURE") && !parametric && is.na(positive)) {
    add_diagnostic(
      "VERTICAL_POSITIVE_REQUIRED", "REQUIREMENT", "ERROR",
      "Non-pressure dimensional vertical coordinates require positive=up/down.", TRUE
    )
  }
  if (!is.na(as.character(positive_raw)[[1L]]) && is.na(positive)) {
    add_diagnostic(
      "VERTICAL_POSITIVE_INVALID", "REQUIREMENT", "ERROR",
      "The positive attribute is not up or down.", TRUE
    )
  }
  if (conflict) {
    add_diagnostic(
      "VERTICAL_STANDARD_NAME_POSITIVE_CONFLICT", "RECOMMENDATION", "WARNING",
      "The positive attribute should be consistent with the standard_name sign convention."
    )
    add_diagnostic(
      "VERTICAL_SEMANTIC_CONFLICT", "OCEANCUBE-SAFETY", "ERROR",
      "Conflicting standard_name and positive evidence cannot drive metric geometry.", TRUE
    )
  }
  if (!finite_monotonic) {
    add_diagnostic(
      "VERTICAL_COORDINATE_NOT_MONOTONIC", "OCEANCUBE-SAFETY", "ERROR",
      "Current vertical coordinates must be finite and strictly monotonic without duplicates.", TRUE
    )
  }
  runtime_status <- if (conflict) {
    "VERTICAL_CONFLICT"
  } else if (parametric) {
    "VERTICAL_PARAMETRIC_DEFERRED"
  } else if (identical(kind, "PRESSURE")) {
    "VERTICAL_INTERPRETABLE_ONLY"
  } else if (kind %in% c("HEIGHT_LENGTH", "DIMENSIONLESS_GENERIC")) {
    "VERTICAL_INTERPRETABLE_ONLY"
  } else if (!identical(kind, "DEPTH_LENGTH")) {
    "VERTICAL_UNRESOLVED"
  } else if (!identical(unit$family, "LENGTH") || is.na(unit$normalized)) {
    "VERTICAL_UNIT_UNSUPPORTED"
  } else if (is.na(effective_positive) || !finite_monotonic) {
    "VERTICAL_CONFLICT"
  } else {
    "VERTICAL_RUNTIME_SUPPORTED"
  }
  bounds <- .cf_vertical_read_bounds(nc, cf, source_axis_id, centers, unit)
  geometry_status <- if (identical(runtime_status, "VERTICAL_RUNTIME_SUPPORTED") &&
                         identical(bounds$status, "BOUNDS_VALID")) {
    "GEOMETRY_METRIC_BOUNDS_SUPPORTED"
  } else if (identical(kind, "PARAMETRIC")) {
    "GEOMETRY_PARAMETRIC_DEFERRED"
  } else if (!identical(kind, "DEPTH_LENGTH")) {
    "GEOMETRY_NON_LENGTH_VERTICAL"
  } else if (identical(bounds$status, "BOUNDS_MISSING")) {
    "GEOMETRY_NO_BOUNDS"
  } else if (runtime_status == "VERTICAL_UNIT_UNSUPPORTED" ||
             bounds$status == "BOUNDS_UNIT_INCOMPATIBLE") {
    "GEOMETRY_UNIT_UNSUPPORTED"
  } else {
    paste0("GEOMETRY_", sub("^BOUNDS_", "", bounds$status))
  }
  if (!identical(bounds$status, "BOUNDS_VALID") &&
      !identical(bounds$status, "BOUNDS_MISSING")) {
    add_diagnostic(
      paste0("VERTICAL_", bounds$status), "OCEANCUBE-SAFETY", "ERROR",
      paste0("Vertical bounds status: ", bounds$status, "."), TRUE
    )
  }
  list(
    schema_name = "oceancube_cf_vertical",
    schema_version = "1.0.0",
    source_axis_id = source_axis_id,
    source_coordinate = as.numeric(centers),
    kind = kind,
    runtime_status = runtime_status,
    geometry_status = geometry_status,
    units_raw = if (is.null(units_raw)) NA_character_ else as.character(units_raw)[[1L]],
    unit_family = unit$family,
    normalized_unit = unit$normalized,
    positive_raw = if (is.null(positive_raw)) NA_character_ else as.character(positive_raw)[[1L]],
    positive = effective_positive,
    standard_name = standard_name,
    axis_attribute = axis_attribute,
    source_order = order,
    orientation = if (is.na(effective_positive)) "UNKNOWN" else toupper(effective_positive),
    bounds_target = bounds$target,
    bounds_status = bounds$status,
    bounds_units_raw = bounds$units_raw,
    bounds_unit = bounds$normalized_unit,
    bounds_shape = if (length(bounds$values)) {
      as.integer(c(length(bounds$values), 2L))
    } else {
      as.integer(bounds$shape)
    },
    bounds = bounds$values,
    coverage_contiguous = bounds$coverage_contiguous,
    formula_terms_status = formula_status,
    canonical_metric = identical(runtime_status, "VERTICAL_RUNTIME_SUPPORTED"),
    scale_to_m = unit$scale_to_m,
    sign_convention = if (identical(effective_positive, "down")) {
      "SOURCE_VALUES_INCREASE_POSITIVE_DOWN"
    } else if (identical(effective_positive, "up")) {
      "SOURCE_VALUES_INCREASE_POSITIVE_UP"
    } else {
      "UNKNOWN"
    },
    surface_status = if (length(centers) == 1L) "EXPLICIT_SINGLETON" else "MULTI_LEVEL",
    diagnostics = unique(diagnostics)
  )
}

.cf_vertical_validate <- function(x) {
  required <- c(
    "schema_name", "schema_version", "source_axis_id", "source_coordinate",
    "kind", "runtime_status", "geometry_status", "units_raw", "unit_family",
    "normalized_unit", "positive_raw", "positive", "standard_name",
    "axis_attribute", "source_order", "orientation", "bounds_target",
    "bounds_status", "bounds_units_raw", "bounds_unit", "bounds_shape",
    "bounds", "coverage_contiguous", "formula_terms_status", "canonical_metric",
    "scale_to_m", "sign_convention", "surface_status", "diagnostics"
  )
  if (!is.list(x) || length(setdiff(required, names(x))) ||
      !identical(x$schema_name, "oceancube_cf_vertical") ||
      !identical(x$schema_version, "1.0.0") ||
      !is.numeric(x$source_coordinate) || !is.list(x$bounds) ||
      !is.logical(x$canonical_metric) || length(x$canonical_metric) != 1L ||
      !is.list(x$diagnostics) || any(vapply(x$diagnostics, function(item) {
        !is.list(item) || !all(c(
          "code", "rule_kind", "severity", "message", "blocking_for_geometry"
        ) %in% names(item))
      }, logical(1L)))) {
    .cf_metadata_abort("Invalid current CF vertical semantic descriptor.")
  }
  invisible(TRUE)
}

.cf_vertical_for_selection <- function(vertical, depth_index) {
  if (is.null(vertical)) return(NULL)
  .cf_vertical_validate(vertical)
  if (identical(vertical$kind, "SURFACE_SINGLETON")) return(vertical)
  out <- vertical
  out$source_coordinate <- vertical$source_coordinate[depth_index]
  if (length(vertical$bounds)) out$bounds <- vertical$bounds[depth_index]
  out$source_order <- .cf_vertical_source_order(out$source_coordinate)
  if (length(out$bounds)) {
    paired <- do.call(rbind, out$bounds)
    lower <- pmin(paired[, 1L], paired[, 2L])
    upper <- pmax(paired[, 1L], paired[, 2L])
    ordered <- order(lower, upper)
    out$coverage_contiguous <- if (length(ordered) <= 1L) TRUE else all(
      abs(lower[ordered][-1L] - upper[ordered][-length(ordered)]) <=
        .vertical_geometry_tolerance(lower, upper)
    )
    out$bounds_shape <- as.integer(c(length(depth_index), 2L))
  }
  out$surface_status <- if (length(depth_index) == 1L) "EXPLICIT_SINGLETON" else "MULTI_LEVEL"
  out
}

.cf_vertical_for_transform <- function(vertical) {
  if (is.null(vertical)) return(NULL)
  out <- vertical
  out$runtime_status <- "VERTICAL_DERIVATION_PENDING"
  out$geometry_status <- "GEOMETRY_DERIVATION_PENDING"
  out$canonical_metric <- FALSE
  out$diagnostics[[length(out$diagnostics) + 1L]] <- .cf_vertical_diagnostic(
    "VERTICAL_DERIVATION_PENDING", "OCEANCUBE-SAFETY", "INFO",
    "A meaning-changing transform requires a new vertical semantic derivation."
  )
  out
}

.cf_vertical_for_layer_mean <- function(vertical, bins, centers) {
  .cf_vertical_validate(vertical)
  out <- vertical
  out$source_coordinate <- as.numeric(centers)
  out$source_order <- .cf_vertical_source_order(out$source_coordinate)
  out$runtime_status <- "VERTICAL_RUNTIME_SUPPORTED"
  out$geometry_status <- "GEOMETRY_METRIC_BOUNDS_SUPPORTED"
  out$bounds_target <- "oceancube:derived_layer_bounds"
  out$bounds_status <- "BOUNDS_VALID"
  out$bounds_units_raw <- vertical$units_raw
  out$bounds_unit <- vertical$normalized_unit
  out$bounds_shape <- as.integer(c(length(bins), 2L))
  out$bounds <- lapply(bins, as.numeric)
  out$coverage_contiguous <- length(bins) <= 1L || all(vapply(
    seq_len(length(bins) - 1L),
    function(i) {
      abs(bins[[i]][[2L]] - bins[[i + 1L]][[1L]]) <=
        .vertical_geometry_tolerance(bins[[i]], bins[[i + 1L]])
    },
    logical(1L)
  ))
  out$canonical_metric <- TRUE
  out$surface_status <- if (length(centers) == 1L) {
    "EXPLICIT_SINGLETON"
  } else {
    "MULTI_LEVEL"
  }
  out$diagnostics <- Filter(function(item) {
    !identical(item$code, "VERTICAL_DERIVATION_PENDING")
  }, out$diagnostics)
  out$diagnostics[[length(out$diagnostics) + 1L]] <- .cf_vertical_diagnostic(
    "VERTICAL_LAYER_MEAN_DERIVED", "OCEANCUBE-SAFETY", "INFO",
    "Current metric-depth bounds were derived exactly from requested layer intervals."
  )
  .cf_vertical_validate(out)
  out
}
