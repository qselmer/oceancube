# Internal CF 1.13 supported-subset interpreter --------------------------

.cf_supported_subset_contract <- function() {
  list(
    reference = "CF-1.13",
    definition_version = "1.0.0",
    validator_version = "1.0.0",
    validation_scope = "oceancube_supported_subset",
    public_claim = "CF-aware; supports a documented subset of CF 1.13",
    constructs = c(
      Conventions = "INTERPRETS",
      dimension_structure = "STRUCTURALLY_VALIDATES",
      coordinate_variables = "INTERPRETS",
      auxiliary_coordinates = "STRUCTURALLY_VALIDATES",
      axis_semantics = "INTERPRETS",
      coordinates = "STRUCTURALLY_VALIDATES",
      bounds = "STRUCTURALLY_VALIDATES_VALUE_CHECKS_DEFERRED",
      climatology = "STRUCTURALLY_VALIDATES_VALUE_CHECKS_DEFERRED",
      ancillary_variables = "STRUCTURALLY_VALIDATES",
      cell_measures = "STRUCTURALLY_VALIDATES_UNIT_CHECKS_DEFERRED",
      grid_mapping_simple = "STRUCTURALLY_VALIDATES",
      grid_mapping_extended = "DEFERS",
      formula_terms = "STRUCTURALLY_VALIDATES_SEMANTICS_DEFERRED",
      cell_methods = "CLASSIFIES_BOUNDED_SUBSET",
      standard_name = "PRESENT_UNVALIDATED",
      units = "PRESERVES_AXIS_EVIDENCE_ONLY",
      fill_and_missing = "STRUCTURALLY_VALIDATES_WHEN_TYPE_KNOWN",
      valid_ranges = "STRUCTURALLY_VALIDATES_VALUE_CHECKS_DEFERRED",
      flags = "STRUCTURALLY_VALIDATES_NO_DATA_APPLICATION",
      groups_and_paths = "STRUCTURALLY_VALIDATES"
    ),
    bounds_attribute_inheritance =
      "units compared only when present on both variables"
  )
}

.cf_diagnostic <- function(code, severity, status, scope = "SOURCE",
                           source_id = NA_character_, attribute = NA_character_,
                           rule_kind = "REQUIREMENT", message,
                           cf_section, blocking_for_current_cube = FALSE,
                           requires_data_values = FALSE) {
  list(
    code = as.character(code),
    severity = as.character(severity),
    status = as.character(status),
    scope = as.character(scope),
    source_id = as.character(source_id),
    attribute = as.character(attribute),
    rule_id = as.character(code),
    rule_kind = as.character(rule_kind),
    message = as.character(message),
    cf_section = as.character(cf_section),
    blocking_for_current_cube = isTRUE(blocking_for_current_cube),
    requires_data_values = isTRUE(requires_data_values)
  )
}

.cf_semantic_dimensions <- function(variable) {
  rev(as.character(variable$source_dimension_order))
}

.cf_numeric_primitive <- function(variable) {
  if (!identical(variable$primitive_type_status, "available")) return(NA)
  tolower(variable$source_primitive_type) %in% c(
    "byte", "ubyte", "short", "ushort", "int", "uint", "int64",
    "uint64", "float", "double"
  )
}

.cf_attribute_names <- function(variable) {
  if (!length(variable$attributes)) return(character())
  vapply(variable$attributes, `[[`, character(1L), "name")
}

.cf_has_layout_exception <- function(source, parent, target) {
  exception_attributes <- c(
    "compress", "sample_dimension", "instance_dimension", "cf_role"
  )
  parent_names <- .cf_attribute_names(parent)
  target_names <- .cf_attribute_names(target)
  feature_type <- .cf_attribute_value(source$global_attributes, "featureType")
  target_type <- tolower(target$source_primitive_type %||% "")
  target_type %in% c("char", "string") ||
    any(exception_attributes %in% c(parent_names, target_names)) ||
    (!is.null(feature_type) && length(feature_type) == 1L &&
       !is.na(feature_type) && nzchar(as.character(feature_type)))
}

.cf_link_resolution_diagnostic <- function(link, code, section,
                                           allow_formula_self = FALSE) {
  status <- link$status
  pass <- identical(status, "RESOLVED") ||
    (isTRUE(allow_formula_self) && identical(status, "SELF_REFERENCE"))
  deferred <- identical(status, "DEFERRED_EXTENDED")
  if (pass) {
    return(.cf_diagnostic(
      code, "INFO", "PASS", source_id = link$source_path,
      attribute = link$attribute,
      message = paste0("`", link$attribute, "` target resolves uniquely."),
      cf_section = section
    ))
  }
  if (deferred) {
    return(.cf_diagnostic(
      code, "DEFERRED", "DEFERRED", source_id = link$source_path,
      attribute = link$attribute,
      message = paste0("`", link$attribute, "` form is preserved but deferred (", status, ")."),
      cf_section = section
    ))
  }
  .cf_diagnostic(
    code, "ERROR", "FAIL", source_id = link$source_path,
    attribute = link$attribute,
    message = paste0("`", link$attribute, "` reference is not uniquely resolved (", status, ")."),
    cf_section = section
  )
}

.cf_simple_cell_methods_class <- function(value) {
  if (is.null(value)) return("ABSENT")
  text <- as.character(value)
  if (length(text) != 1L || is.na(text) || !nzchar(text)) {
    return("PRESERVED_UNINTERPRETED")
  }
  simple <- paste0(
    "^[A-Za-z_][A-Za-z0-9_]*:[[:space:]]+",
    "(point|sum|maximum|median|mid_range|minimum|mean|mode|",
    "standard_deviation|variance)$"
  )
  if (grepl(simple, trimws(text))) "SIMPLE_RECOGNIZED" else "COMPLEX_DEFERRED"
}

.cf_flag_diagnostic <- function(variable) {
  values <- .cf_attribute_value(variable$attributes, "flag_values")
  masks <- .cf_attribute_value(variable$attributes, "flag_masks")
  meanings <- .cf_attribute_value(variable$attributes, "flag_meanings")
  if (is.null(values) && is.null(masks) && is.null(meanings)) return(NULL)
  tokens <- if (is.null(meanings)) character() else .cf_split_names(meanings)
  lengths <- c(if (!is.null(values)) length(values), if (!is.null(masks)) length(masks))
  ok <- (!is.null(values) || !is.null(masks)) && length(tokens) > 0L &&
    all(vapply(list(values, masks)[!vapply(list(values, masks), is.null, logical(1L))],
      is.numeric, logical(1L))) &&
    (length(lengths) < 2L || length(unique(lengths)) == 1L) &&
    length(tokens) == max(lengths)
  .cf_diagnostic(
    "CFB3_FLAGS_STRUCTURE",
    if (ok) "INFO" else "ERROR", if (ok) "PASS" else "FAIL",
    source_id = variable$source_path, attribute = "flag_values/flag_masks/flag_meanings",
    message = if (ok) {
      "Flag values or masks and flag meanings are structurally coherent; flags are not applied to data."
    } else {
      "Flag metadata are structurally incomplete or have incompatible lengths/types."
    },
    cf_section = "3.5 Flags"
  )
}

.cf_interpret_supported_subset <- function(source) {
  diagnostics <- list()
  add <- function(x) {
    if (!is.null(x)) diagnostics[[length(diagnostics) + 1L]] <<- x
  }
  contract <- .cf_supported_subset_contract()
  declaration <- source$declaration
  declaration_status <- if (is.null(declaration$raw)) {
    "NOT_DECLARED"
  } else if (is.na(declaration$declared_cf)) {
    "DECLARED_WITHOUT_CF_TOKEN"
  } else {
    "DECLARED"
  }
  add(.cf_diagnostic(
    "CFB3_DECLARATION", "INFO", "PASS", source_id = "/",
    attribute = "Conventions", rule_kind = "RECOMMENDATION",
    message = if (identical(declaration_status, "NOT_DECLARED")) {
      "No CF declaration is present; structural supported-subset interpretation remains available."
    } else {
      paste0("Source declaration preserved; declared CF version is `",
             declaration$declared_cf %||% "unresolved", "`.")
    },
    cf_section = "2.6.1 Identification of Conventions"
  ))

  identities_ok <- !anyDuplicated(source$dimensions$order) &&
    !anyDuplicated(source$variables$order) &&
    identical(names(source$dimensions$map), source$dimensions$order) &&
    identical(names(source$variables$map), source$variables$order)
  add(.cf_diagnostic(
    "CFB3_PATH_IDENTITIES", if (identities_ok) "INFO" else "ERROR",
    if (identities_ok) "PASS" else "FAIL", source_id = "/",
    message = if (identities_ok) {
      "Path-qualified dimension and variable identities are unique and ordered."
    } else {
      "Path-qualified dimension or variable identities are duplicated or inconsistent."
    },
    cf_section = "2.7 Groups"
  ))

  coordinate_map <- list()
  cell_methods_map <- list()
  standard_name_map <- list()
  units_map <- list()
  dimension_ids <- source$dimensions$order

  for (path in source$variables$order) {
    variable <- source$variables$map[[path]]
    dims <- as.character(variable$source_dimensions)
    duplicate_dims <- anyDuplicated(dims) > 0L
    add(.cf_diagnostic(
      "CFB3_VARIABLE_DIMENSIONS_UNIQUE",
      if (duplicate_dims) "ERROR" else "INFO",
      if (duplicate_dims) "FAIL" else "PASS", source_id = path,
      message = if (duplicate_dims) {
        "Variable repeats a source dimension name."
      } else {
        "Variable source dimension names are unique."
      },
      cf_section = "2.4 Dimensions"
    ))
    missing_dims <- setdiff(dims, dimension_ids)
    add(.cf_diagnostic(
      "CFB3_VARIABLE_DIMENSIONS_RESOLVE",
      if (length(missing_dims)) "ERROR" else "INFO",
      if (length(missing_dims)) "FAIL" else "PASS", source_id = path,
      message = if (length(missing_dims)) {
        paste0("Variable dimension reference(s) do not resolve: ",
               paste(missing_dims, collapse = ", "), ".")
      } else {
        "All variable dimension references resolve."
      },
      cf_section = "2.4 Dimensions"
    ))

    coordinate_class <- if (path %in% dimension_ids) {
      "DIMENSION_COORDINATE"
    } else if ("auxiliary_coordinate" %in% variable$roles) {
      if (length(dims)) "AUXILIARY_COORDINATE" else "SCALAR_COORDINATE"
    } else {
      "NOT_COORDINATE"
    }
    coordinate_map[[path]] <- list(
      classification = coordinate_class,
      roles = variable$roles
    )

    if (path %in% dimension_ids) {
      evidence <- .cf_axis_evidence(source, path)
      strong <- unique(vapply(evidence, `[[`, character(1L), "axis")[
        vapply(evidence, `[[`, character(1L), "strength") == "strong" &
          !is.na(vapply(evidence, `[[`, character(1L), "axis"))
      ])
      conflict <- length(strong) > 1L
      add(.cf_diagnostic(
        "CFB3_AXIS_EVIDENCE",
        if (conflict) "ERROR" else "INFO",
        if (conflict) "FAIL" else "PASS", source_id = path,
        attribute = "axis/standard_name/units/positive/formula_terms",
        message = if (conflict) {
          paste0("Strong axis evidence conflicts: ", paste(strong, collapse = ", "), ".")
        } else {
          "Strong axis evidence is coherent or absent."
        },
        cf_section = "4 Coordinate Types"
      ))
    }

    cell_methods <- .cf_attribute_value(variable$attributes, "cell_methods")
    cell_class <- .cf_simple_cell_methods_class(cell_methods)
    cell_methods_map[[path]] <- list(
      status = cell_class,
      raw = cell_methods
    )
    if (!identical(cell_class, "ABSENT")) {
      deferred <- cell_class %in% c("PRESERVED_UNINTERPRETED", "COMPLEX_DEFERRED")
      add(.cf_diagnostic(
        "CFB3_CELL_METHODS",
        if (deferred) "DEFERRED" else "INFO",
        if (deferred) "DEFERRED" else "PASS", source_id = path,
        attribute = "cell_methods",
        message = paste0("cell_methods classification: ", cell_class, "."),
        cf_section = "7.3 Cell Methods"
      ))
    }

    standard_name <- .cf_attribute_value(variable$attributes, "standard_name")
    standard_name_map[[path]] <- if (is.null(standard_name)) {
      list(status = "ABSENT", raw = NULL)
    } else {
      list(status = "PRESENT_UNVALIDATED", raw = standard_name)
    }
    if (!is.null(standard_name)) {
      add(.cf_diagnostic(
        "CFB3_STANDARD_NAME_TABLE", "DEFERRED", "DEFERRED",
        source_id = path, attribute = "standard_name",
        message = "standard_name is preserved exactly; legal-name lookup is not performed without a versioned offline table.",
        cf_section = "3.3 Standard Name"
      ))
    }

    units <- .cf_attribute_value(variable$attributes, "units")
    units_map[[path]] <- if (is.null(units)) {
      list(status = "ABSENT", raw = NULL)
    } else {
      list(status = "PRESERVED_AXIS_EVIDENCE_ONLY", raw = units)
    }
    if (!is.null(units)) {
      add(.cf_diagnostic(
        "CFB3_UNITS_SCOPE", "DEFERRED", "DEFERRED", source_id = path,
        attribute = "units", rule_kind = "RECOMMENDATION",
        message = "Units are preserved and may supply bounded axis evidence; general dimensional conformance is deferred.",
        cf_section = "3.1 Units"
      ))
      duration_month_or_year <- path %in% dimension_ids &&
        length(units) == 1L && !is.na(units) &&
        grepl(
          "^[[:space:]]*(months?|years?)[[:space:]]+since[[:space:]]+",
          as.character(units), ignore.case = TRUE
        )
      if (duration_month_or_year) {
        add(.cf_diagnostic(
          "CFB3_TIME_DURATION_UNIT_RECOMMENDATION", "WARNING", "FAIL",
          source_id = path, attribute = "units",
          rule_kind = "RECOMMENDATION",
          message = "CF 1.13 recommends that year and month duration units not be used for time coordinates; source text is preserved and decoding policy is unchanged.",
          cf_section = "4.4 Time Coordinate"
        ))
      }
    }

    for (attribute in c("_FillValue", "missing_value")) {
      value <- .cf_attribute_value(variable$attributes, attribute)
      if (is.null(value)) next
      numeric_type <- .cf_numeric_primitive(variable)
      if (is.na(numeric_type)) {
        add(.cf_diagnostic(
          "CFB3_MISSING_TYPE", "DEFERRED", "DEFERRED", source_id = path,
          attribute = attribute,
          message = paste0(attribute, " type compatibility is deferred because the source primitive type is unavailable."),
          cf_section = "2.5.1 Missing Data"
        ))
      } else {
        ok <- length(value) == 1L &&
          if (numeric_type) is.numeric(value) else is.character(value)
        add(.cf_diagnostic(
          "CFB3_MISSING_TYPE", if (ok) "INFO" else "ERROR",
          if (ok) "PASS" else "FAIL", source_id = path,
          attribute = attribute,
          message = if (ok) {
            paste0(attribute, " is structurally compatible with the known source primitive family.")
          } else {
            paste0(attribute, " is not structurally compatible with the known source primitive family.")
          },
          cf_section = "2.5.1 Missing Data"
        ))
      }
    }

    attributes <- .cf_attribute_names(variable)
    range_conflict <- "valid_range" %in% attributes &&
      any(c("valid_min", "valid_max") %in% attributes)
    if (any(c("valid_range", "valid_min", "valid_max") %in% attributes)) {
      add(.cf_diagnostic(
        "CFB3_VALID_RANGE_EXCLUSIVE",
        if (range_conflict) "ERROR" else "INFO",
        if (range_conflict) "FAIL" else "PASS", source_id = path,
        attribute = "valid_range/valid_min/valid_max",
        message = if (range_conflict) {
          "valid_range must not coexist with valid_min or valid_max in the supported subset."
        } else {
          "Valid-range attributes use one non-conflicting representation."
        },
        cf_section = "2.5.1 Missing Data"
      ))
    }
    if ("actual_range" %in% attributes) {
      add(.cf_diagnostic(
        "CFB3_ACTUAL_RANGE_VALUES", "DEFERRED", "DEFERRED",
        source_id = path, attribute = "actual_range",
        message = "actual_range comparison with scientific payload extrema is deferred.",
        cf_section = "2.5.1 Missing Data", requires_data_values = TRUE
      ))
    }
    add(.cf_flag_diagnostic(variable))
  }

  for (link in source$links) {
    parent <- source$variables$map[[link$source_path]]
    target <- if (!is.na(link$resolved_path)) {
      source$variables$map[[link$resolved_path]]
    } else {
      NULL
    }
    attribute <- link$attribute
    section <- switch(
      attribute,
      coordinates = "5 Coordinate Systems",
      ancillary_variables = "3.4 Ancillary Data",
      bounds = "7.1 Cell Boundaries",
      climatology = "7.4 Climatological Statistics",
      cell_measures = "7.2 Cell Measures",
      grid_mapping = "5.6 Horizontal Coordinate Reference Systems",
      formula_terms = "4.3.3 Parametric Vertical Coordinate",
      "CF-1.13"
    )
    resolution_code <- paste0("CFB3_", toupper(attribute), "_RESOLUTION")
    add(.cf_link_resolution_diagnostic(
      link, resolution_code, section,
      allow_formula_self = identical(attribute, "formula_terms")
    ))
    resolved <- !is.null(target) &&
      (identical(link$status, "RESOLVED") ||
       (identical(attribute, "formula_terms") && identical(link$status, "SELF_REFERENCE")))
    if (!resolved) next

    parent_dims <- .cf_semantic_dimensions(parent)
    target_dims <- .cf_semantic_dimensions(target)
    if (attribute %in% c("coordinates", "ancillary_variables")) {
      subset_ok <- all(target_dims %in% parent_dims)
      exception <- !subset_ok && .cf_has_layout_exception(source, parent, target)
      add(.cf_diagnostic(
        paste0("CFB3_", toupper(attribute), "_DIMENSIONS"),
        if (exception) "DEFERRED" else if (subset_ok) "INFO" else "ERROR",
        if (exception) "DEFERRED" else if (subset_ok) "PASS" else "FAIL",
        source_id = link$source_path, attribute = attribute,
        message = if (exception) {
          "Dimension compatibility is deferred for a character/compressed/ragged/DSG layout exception."
        } else if (subset_ok) {
          "Referenced variable dimensions are a subset of parent dimensions."
        } else {
          "Referenced variable dimensions are not a subset of parent dimensions."
        },
        cf_section = section
      ))
    }

    if (identical(attribute, "bounds")) {
      numeric <- .cf_numeric_primitive(target)
      add(.cf_diagnostic(
        "CFB3_BOUNDS_NUMERIC",
        if (is.na(numeric)) "DEFERRED" else if (numeric) "INFO" else "ERROR",
        if (is.na(numeric)) "DEFERRED" else if (numeric) "PASS" else "FAIL",
        source_id = link$source_path, attribute = attribute,
        message = if (is.na(numeric)) {
          "Bounds numeric type check is deferred because the primitive type is unavailable."
        } else if (numeric) {
          "Bounds target has a known numeric primitive type."
        } else {
          "Bounds target must have a numeric primitive type."
        },
        cf_section = section
      ))
      dims_ok <- length(target_dims) == length(parent_dims) + 1L &&
        identical(target_dims[seq_along(parent_dims)], parent_dims)
      vertex_ok <- FALSE
      if (dims_ok) {
        vertex <- target_dims[[length(target_dims)]]
        vertex_descriptor <- source$dimensions$map[[vertex]]
        vertex_ok <- length(parent_dims) != 1L ||
          (!is.null(vertex_descriptor) && identical(vertex_descriptor$length, 2L))
      }
      add(.cf_diagnostic(
        "CFB3_BOUNDS_DIMENSIONS",
        if (dims_ok && vertex_ok) "INFO" else "ERROR",
        if (dims_ok && vertex_ok) "PASS" else "FAIL",
        source_id = link$source_path, attribute = attribute,
        message = if (dims_ok && vertex_ok) {
          "Bounds dimensions equal associated dimensions plus one trailing vertex dimension."
        } else {
          "Bounds dimensions do not match the supported associated-dimensions plus trailing-vertex rule."
        },
        cf_section = section
      ))
      parent_units <- .cf_attribute_value(parent$attributes, "units")
      target_units <- .cf_attribute_value(target$attributes, "units")
      if (!is.null(parent_units) && !is.null(target_units)) {
        same_units <- identical(parent_units, target_units)
        add(.cf_diagnostic(
          "CFB3_BOUNDS_UNITS_INHERITANCE",
          if (same_units) "INFO" else "ERROR",
          if (same_units) "PASS" else "FAIL", source_id = link$source_path,
          attribute = attribute,
          message = if (same_units) {
            "Coordinate and bounds units are identical."
          } else {
            "Coordinate and bounds units differ."
          },
          cf_section = section
        ))
      }
      add(.cf_diagnostic(
        "CFB3_BOUNDS_VALUE_CHECK", "DEFERRED", "DEFERRED",
        source_id = link$source_path, attribute = attribute,
        message = "Bounds containment, ordering, and missing-value placement require data values and are deferred.",
        cf_section = section, requires_data_values = TRUE
      ))
    }

    if (identical(attribute, "climatology")) {
      numeric <- .cf_numeric_primitive(target)
      dims_ok <- length(parent_dims) == 1L && length(target_dims) == 2L &&
        identical(target_dims[[1L]], parent_dims[[1L]]) &&
        !is.null(source$dimensions$map[[target_dims[[2L]]]]) &&
        identical(source$dimensions$map[[target_dims[[2L]]]]$length, 2L)
      ok <- (is.na(numeric) || numeric) && dims_ok
      add(.cf_diagnostic(
        "CFB3_CLIMATOLOGY_STRUCTURE",
        if (ok && !is.na(numeric)) "INFO" else if (ok) "DEFERRED" else "ERROR",
        if (ok && !is.na(numeric)) "PASS" else if (ok) "DEFERRED" else "FAIL",
        source_id = link$source_path, attribute = attribute,
        message = if (ok && !is.na(numeric)) {
          "Climatology target is numeric with dimensions (time, 2)."
        } else if (ok) {
          "Climatology dimensions are valid; numeric type check is deferred."
        } else {
          "Climatology target must be numeric with dimensions (time, 2)."
        },
        cf_section = section
      ))
      add(.cf_diagnostic(
        "CFB3_CLIMATOLOGY_VALUES", "DEFERRED", "DEFERRED",
        source_id = link$source_path, attribute = attribute,
        message = "Climatology interval values and time decoding are deferred.",
        cf_section = section, requires_data_values = TRUE
      ))
    }

    if (identical(attribute, "cell_measures")) {
      recognized <- link$key %in% c("area", "volume")
      add(.cf_diagnostic(
        "CFB3_CELL_MEASURE_KEYWORD",
        if (recognized) "INFO" else "ERROR",
        if (recognized) "PASS" else "FAIL", source_id = link$source_path,
        attribute = attribute,
        message = if (recognized) {
          paste0("Cell-measure keyword `", link$key, "` is recognized.")
        } else {
          paste0("Cell-measure keyword `", link$key, "` is not in the supported simple subset.")
        },
        cf_section = section
      ))
      subset_ok <- all(target_dims %in% parent_dims)
      add(.cf_diagnostic(
        "CFB3_CELL_MEASURE_DIMENSIONS",
        if (subset_ok) "INFO" else "ERROR",
        if (subset_ok) "PASS" else "FAIL", source_id = link$source_path,
        attribute = attribute,
        message = if (subset_ok) {
          "Cell-measure dimensions are a subset of parent dimensions."
        } else {
          "Cell-measure dimensions are not a subset of parent dimensions."
        },
        cf_section = section
      ))
      add(.cf_diagnostic(
        "CFB3_CELL_MEASURE_UNITS", "DEFERRED", "DEFERRED",
        source_id = link$source_path, attribute = attribute,
        message = "Cell-measure units are preserved; dimensional-unit conformance is deferred without UDUNITS.",
        cf_section = section
      ))
    }

    if (identical(attribute, "grid_mapping")) {
      role_ok <- "grid_mapping" %in% target$roles
      name <- .cf_attribute_value(target$attributes, "grid_mapping_name")
      name_ok <- is.character(name) && length(name) == 1L &&
        !is.na(name) && nzchar(name)
      add(.cf_diagnostic(
        "CFB3_GRID_MAPPING_CONTAINER",
        if (role_ok && name_ok) "INFO" else "ERROR",
        if (role_ok && name_ok) "PASS" else "FAIL",
        source_id = link$source_path, attribute = attribute,
        message = if (role_ok && name_ok) {
          "Simple grid-mapping target has the grid_mapping role and grid_mapping_name."
        } else {
          "Simple grid-mapping target lacks the required role or grid_mapping_name."
        },
        cf_section = section
      ))
      add(.cf_diagnostic(
        "CFB3_GRID_MAPPING_NAME_TABLE", "DEFERRED", "DEFERRED",
        source_id = link$source_path, attribute = attribute,
        message = "grid_mapping_name legal-value validation is deferred without a versioned Appendix-F table.",
        cf_section = section
      ))
    }

    if (identical(attribute, "formula_terms")) {
      add(.cf_diagnostic(
        "CFB3_FORMULA_SEMANTICS", "DEFERRED", "DEFERRED",
        source_id = link$source_path, attribute = attribute,
        message = "Formula-term paths are resolved structurally; term vocabulary and formula evaluation are deferred.",
        cf_section = section
      ))
    }
  }

  source_diagnostics <- diagnostics
  status <- if (any(vapply(source_diagnostics, function(x) {
    identical(x$status, "FAIL") && identical(x$severity, "ERROR")
  }, logical(1L)))) "FAIL" else "PASS"
  summary <- list(
    reference = contract$reference,
    definition_version = contract$definition_version,
    validator_version = contract$validator_version,
    validation_scope = contract$validation_scope,
    status = status,
    rules_checked = as.integer(length(source_diagnostics)),
    pass = as.integer(sum(vapply(source_diagnostics, `[[`, character(1L), "status") == "PASS")),
    fail = as.integer(sum(vapply(source_diagnostics, `[[`, character(1L), "status") == "FAIL")),
    warning = as.integer(sum(vapply(source_diagnostics, `[[`, character(1L), "severity") == "WARNING")),
    deferred = as.integer(sum(vapply(source_diagnostics, `[[`, character(1L), "status") == "DEFERRED")),
    not_applicable = as.integer(sum(vapply(source_diagnostics, `[[`, character(1L), "status") == "NOT_APPLICABLE")),
    declaration_status = declaration_status,
    declared_cf_version = declaration$declared_cf,
    reference_cf_version = "1.13"
  )
  list(
    interpretation = list(
      status = "PRESERVED_SUPPORTED_SUBSET",
      link_families = .cf_link_families(),
      extended_grid_mapping = "preserved-raw-deferred",
      supported_subset = summary,
      contract = contract,
      declaration = list(
        status = declaration_status,
        declared_cf_version = declaration$declared_cf,
        reference_cf_version = "1.13",
        validation_scope = contract$validation_scope
      ),
      coordinates = list(order = source$variables$order, map = coordinate_map),
      cell_methods = list(order = source$variables$order, map = cell_methods_map),
      standard_names = list(order = source$variables$order, map = standard_name_map),
      units = list(order = source$variables$order, map = units_map),
      current = list(status = "NOT_RESOLVED", blocking_diagnostics = 0L)
    ),
    diagnostics = source_diagnostics
  )
}

.cf_add_current_interpretation <- function(cf, resolutions, selected_variables,
                                           semantic_status) {
  cf$diagnostics <- cf$diagnostics[vapply(
    cf$diagnostics, function(x) !identical(x$scope, "CURRENT"), logical(1L)
  )]
  current_diagnostics <- lapply(names(resolutions), function(axis) {
    resolution <- resolutions[[axis]]
    if (is.null(resolution)) {
      resolution <- list(status = "UNRESOLVED", source_id = NA_character_)
    }
    required <- axis %in% c("longitude", "latitude", "time")
    ok <- identical(resolution$status, "RESOLVED") ||
      (!required && identical(resolution$status, "UNRESOLVED"))
    .cf_diagnostic(
      paste0("CFB3_CURRENT_AXIS_", toupper(axis)),
      if (ok) "INFO" else "ERROR", if (ok) "PASS" else "FAIL",
      scope = "CURRENT", source_id = resolution$source_id %||% NA_character_,
      attribute = "axis/standard_name/units/positive/formula_terms",
      rule_kind = "OCEANCUBE-SAFETY",
      message = if (ok) {
        paste0("Current ", axis, " axis is safely resolved for the cube contract.")
      } else {
        paste0("Current required ", axis, " axis is ", resolution$status, ".")
      },
      cf_section = "oceancube current cube schema",
      blocking_for_current_cube = required && !ok
    )
  })
  cf$diagnostics <- c(cf$diagnostics, current_diagnostics)
  blocking <- sum(vapply(current_diagnostics, `[[`, logical(1L),
                         "blocking_for_current_cube"))
  cf$interpretation$current <- list(
    status = semantic_status,
    variables = as.character(selected_variables),
    axes = stats::setNames(
      vapply(resolutions, function(x) if (is.null(x)) "UNRESOLVED" else x$status,
             character(1L)),
      names(resolutions)
    ),
    blocking_diagnostics = as.integer(blocking)
  )
  cf
}

.cf_mark_current_interpretation <- function(metadata, status, variables = NULL) {
  if (is.null(metadata)) return(metadata)
  metadata$cf$interpretation$current$status <- status
  if (!is.null(variables)) {
    metadata$cf$interpretation$current$variables <- as.character(variables)
  }
  metadata
}
