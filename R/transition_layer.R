#' Diagnose a variable-aware vertical transition-layer candidate
#'
#' @param x A direct certified source-profile `<ocean_cube>` or an already
#'   computed certified C4 vertical-gradient `<ocean_cube>`. C1, C2, and C3
#'   derived profiles are outside the initial certified subset.
#' @param diagnostic Required diagnostic name. C6 supports exactly
#'   `"thermocline"` and `"halocline"`.
#' @param variable Optional exact current variable name. `NULL` automatically
#'   selects the sole variable whose preserved source CF `standard_name` is
#'   eligible; zero or multiple eligible variables are errors.
#' @param support C5 support policy. `"local"` excludes explicitly gapped
#'   secants; `"all"` may return a truthfully labelled gapped candidate.
#'
#' @return A base `data.frame` with one row per longitude, latitude, and time.
#'   C5 feature diagnostics are retained and augmented with the physical
#'   variable identity, diagnostic definition, basis, direction, strength,
#'   input mode, diagnostic status, Provenance V1, and bounded QA.
#'
#' @details
#' Physical eligibility is determined only from the immutable preserved source
#' CF `standard_name` and a bounded unit contract. Variable names and
#' `long_name` are never semantic evidence. A thermocline candidate is the
#' strongest eligible negative temperature gradient with depth; a halocline
#' candidate is the strongest eligible absolute salinity gradient. Neither is
#' thresholded, smoothed, or a claim that a physically strong layer exists.
#'
#' The function estimates no layer top, bottom, width, thickness, statistical
#' uncertainty, oxycline, mixed-layer depth, pycnocline, or density diagnostic.
#' `localization_half_span_m` remains a vertical-resolution scale inherited
#' from C5, not statistical uncertainty.
#'
#' @examples
#' \dontrun{
#' transition_layer(cube, diagnostic = "thermocline")
#' transition_layer(cube, diagnostic = "halocline", support = "all")
#'
#' gradient <- depth_gradient(cube)
#' transition_layer(gradient, diagnostic = "thermocline")
#' }
#' @export
transition_layer <- function(
    x,
    diagnostic,
    variable = NULL,
    support = c("local", "all")) {
  if (missing(diagnostic)) {
    rlang::abort(
      "`diagnostic` is required and must be thermocline or halocline.",
      class = "oceancube_transition_diagnostic"
    )
  }
  definition <- .transition_definition(diagnostic)
  support <- match.arg(support)
  .check_cube(x)

  current <- x$metadata$cf$current %||% NULL
  if (is.null(current)) {
    rlang::abort(
      "transition_layer requires current preserved CF metadata.",
      class = "oceancube_transition_input"
    )
  }
  input_mode <- if (!is.null(current$vertical_gradient)) {
    "CERTIFIED_GRADIENT"
  } else {
    "SOURCE_PROFILE"
  }
  source_metadata <- .transition_resolve_variable(
    x, definition, variable = variable
  )
  unit <- .transition_unit_contract(
    source_metadata$standard_name,
    source_metadata$unit,
    definition$family
  )

  if (identical(input_mode, "SOURCE_PROFILE")) {
    .transition_validate_source_profile(x)
    selected <- cube_slice(x, variable = source_metadata$current_variable)
    read_diagnostics <- selected$qa$selection$netcdf_read %||% NULL
    gradient <- depth_gradient(selected, method = "auto")
    gradient_input_method <- "computed"
    netcdf_reads <- as.integer(!is.null(read_diagnostics))
    variables_read <- read_diagnostics$variables %||% character()
    levels_read <- if (is.null(read_diagnostics)) 0L else {
      unname(read_diagnostics$physical_count[["depth"]])
    }
  } else {
    .transition_validate_gradient_origin(x, source_metadata$current_variable)
    gradient <- cube_slice(x, variable = source_metadata$current_variable)
    gradient_input_method <- "supplied"
    netcdf_reads <- 0L
    variables_read <- character()
    levels_read <- 0L
  }

  descriptor <- gradient$metadata$cf$current$vertical_gradient
  descriptor_item <- .transition_gradient_item(
    descriptor, source_metadata$current_variable
  )
  feature <- depth_feature(
    gradient,
    polarity = definition$polarity,
    support = support
  )
  names(feature)[names(feature) == "status"] <- "feature_status"
  names(feature)[names(feature) == "certification_status"] <-
    "feature_certification_status"

  resolved <- !is.na(feature$gradient_index)
  incomplete <- resolved & grepl(
    "INCOMPLETE_PROFILE$", feature$feature_certification_status
  )
  gapped <- resolved & feature$support_relation == "GAPPED_SUPPORT"
  diagnostic_label <- toupper(diagnostic)
  diagnostic_status <- feature$feature_status
  diagnostic_status[resolved & !gapped & !incomplete] <- paste0(
    diagnostic_label, "_GRADIENT_CANDIDATE"
  )
  diagnostic_status[gapped & !incomplete] <- paste0(
    "GAPPED_", diagnostic_label, "_GRADIENT_CANDIDATE"
  )
  diagnostic_status[incomplete & !gapped] <- paste0(
    "OBSERVED_", diagnostic_label, "_CANDIDATE_INCOMPLETE_PROFILE"
  )
  diagnostic_status[incomplete & gapped] <- paste0(
    "OBSERVED_GAPPED_", diagnostic_label,
    "_CANDIDATE_INCOMPLETE_PROFILE"
  )
  diagnostic_certification <- feature$feature_certification_status
  diagnostic_certification[resolved & !gapped & !incomplete] <-
    "CERTIFIED_UNTHRESHOLDED_LOCAL_CANDIDATE"
  diagnostic_certification[gapped & !incomplete] <-
    "CERTIFIED_UNTHRESHOLDED_GAPPED_CANDIDATE"
  diagnostic_certification[incomplete & !gapped] <-
    "OBSERVED_UNTHRESHOLDED_CANDIDATE_INCOMPLETE_PROFILE"
  diagnostic_certification[incomplete & gapped] <-
    "OBSERVED_UNTHRESHOLDED_GAPPED_CANDIDATE_INCOMPLETE_PROFILE"

  direction <- rep(NA_character_, nrow(feature))
  direction[resolved & feature$gradient > 0] <- "INCREASING_WITH_DEPTH"
  direction[resolved & feature$gradient < 0] <- "DECREASING_WITH_DEPTH"
  feature$diagnostic <- rep(diagnostic, nrow(feature))
  feature$diagnostic_definition <- rep(definition$id, nrow(feature))
  feature$variable_standard_name <- rep(
    source_metadata$standard_name, nrow(feature)
  )
  feature$variable_semantic_family <- rep(definition$family, nrow(feature))
  feature$temperature_or_salinity_basis <- rep(
    definition$basis[[source_metadata$standard_name]], nrow(feature)
  )
  feature$input_mode <- rep(input_mode, nrow(feature))
  feature$gradient_value_semantics <- rep(
    descriptor_item$output_value_semantics, nrow(feature)
  )
  feature$gradient_method <- rep(
    descriptor_item$resolved_method, nrow(feature)
  )
  feature$gradient_direction <- direction
  feature$diagnostic_strength <- feature$gradient_magnitude
  feature$diagnostic_strength_unit <- feature$gradient_unit
  feature$threshold_applied <- rep(FALSE, nrow(feature))
  feature$diagnostic_status <- diagnostic_status
  feature$diagnostic_certification_status <- diagnostic_certification

  status_counts <- as.list(as.integer(table(diagnostic_status)))
  names(status_counts) <- names(table(diagnostic_status))
  transition_qa <- list(
    diagnostic = diagnostic,
    input_mode = input_mode,
    selected_variable = source_metadata$current_variable,
    source_variable_path = source_metadata$source_path,
    standard_name = source_metadata$standard_name,
    source_unit = source_metadata$unit,
    unit_family = unit$family,
    unit_status = unit$status,
    profiles_total = as.integer(nrow(feature)),
    diagnostic_candidates = as.integer(sum(resolved)),
    gapped_candidates = as.integer(sum(gapped)),
    incomplete_candidates = as.integer(sum(incomplete)),
    ambiguous_profiles = as.integer(sum(
      feature$feature_status == "AMBIGUOUS_TIE"
    )),
    no_candidate_profiles = as.integer(sum(!resolved)),
    netcdf_scientific_payload_reads = netcdf_reads,
    variables_read = variables_read,
    levels_read = as.integer(levels_read),
    diagnostic_status_counts = status_counts
  )

  feature_provenance <- attr(
    feature, "oceancube_provenance", exact = TRUE
  )
  feature_qa <- attr(feature, "oceancube_qa", exact = TRUE)
  output_shape <- c(rows = nrow(feature), columns = ncol(feature))
  context <- .provenance_cube_context(
    source = gradient$source,
    dataset_id = gradient$dataset_id,
    time = gradient$time,
    shape = stats::setNames(as.integer(.cube_shape(gradient)), .cube_axis_names()),
    variables = gradient$vars,
    backend = "memory",
    provenance = feature_provenance
  )
  provenance <- .provenance_append(
    feature_provenance,
    operation = "transition_layer",
    parameters = list(
      requested = list(
        diagnostic = diagnostic,
        variable = variable,
        support = support
      ),
      resolved = list(
        definition_id = definition$id,
        input_mode = input_mode,
        selected_variable = source_metadata$current_variable,
        source_variable_path = source_metadata$source_path,
        source_standard_name = source_metadata$standard_name,
        source_unit = source_metadata$unit,
        semantic_family = definition$family,
        physical_quantity_basis =
          definition$basis[[source_metadata$standard_name]],
        unit_family = unit$family,
        unit_recognition_status = unit$status,
        gradient_polarity = definition$polarity,
        support_policy = support,
        gradient_input_method = gradient_input_method,
        depth_gradient_method = if (identical(
          gradient_input_method, "computed"
        )) "auto" else "supplied certified C4 descriptor",
        depth_feature_polarity = definition$polarity,
        threshold_applied = FALSE,
        candidate_count = as.integer(sum(resolved)),
        diagnostic_status_counts = status_counts,
        netcdf_scientific_payload_reads = netcdf_reads,
        variables_read = variables_read,
        levels_read = as.integer(levels_read),
        output_row_count = as.integer(nrow(feature))
      )
    ),
    output = list(
      backend = "memory",
      shape = stats::setNames(as.integer(output_shape), names(output_shape)),
      variables = source_metadata$current_variable,
      time_kind = context$time_kind
    ),
    scientific_method = .provenance_method(
      "transition_layer", list(diagnostic = diagnostic)
    ),
    context = context
  )
  attr(feature, "oceancube_provenance") <- provenance
  if (is.null(feature_qa)) feature_qa <- list()
  feature_qa$transition_layer <- transition_qa
  attr(feature, "oceancube_qa") <- feature_qa
  feature
}

.transition_definition <- function(diagnostic) {
  if (!is.character(diagnostic) || length(diagnostic) != 1L ||
      is.na(diagnostic) || !nzchar(diagnostic)) {
    rlang::abort(
      "`diagnostic` must be exactly one of: thermocline, halocline.",
      class = "oceancube_transition_diagnostic"
    )
  }
  definitions <- list(
    thermocline = list(
      id = "THERMOCLINE_MAX_NEGATIVE_TEMPERATURE_GRADIENT",
      family = "temperature",
      polarity = "negative",
      basis = c(
        sea_water_temperature = "IN_SITU_SEA_WATER_TEMPERATURE",
        sea_water_potential_temperature = "POTENTIAL_TEMPERATURE",
        sea_water_conservative_temperature = "CONSERVATIVE_TEMPERATURE"
      )
    ),
    halocline = list(
      id = "HALOCLINE_MAX_ABSOLUTE_SALINITY_GRADIENT",
      family = "salinity",
      polarity = "absolute",
      basis = c(
        sea_water_salinity = "GENERIC_SALINITY",
        sea_water_practical_salinity = "PRACTICAL_SALINITY",
        sea_water_absolute_salinity = "ABSOLUTE_SALINITY",
        sea_water_reference_salinity = "REFERENCE_SALINITY",
        sea_water_cox_salinity = "COX_SALINITY",
        sea_water_knudsen_salinity = "KNUDSEN_SALINITY"
      )
    )
  )
  definition <- definitions[[diagnostic]]
  if (is.null(definition)) {
    rlang::abort(
      paste0(
        "Unsupported transition diagnostic `", diagnostic,
        "`; C6 supports only thermocline and halocline."
      ),
      class = "oceancube_transition_diagnostic"
    )
  }
  definition
}

.transition_source_item <- function(x, variable) {
  source <- x$metadata$cf$source %||% NULL
  map <- source$variables$map %||% NULL
  if (!is.list(map) || !length(map)) {
    rlang::abort(
      "transition_layer requires immutable preserved source-variable metadata.",
      class = "oceancube_transition_metadata"
    )
  }
  paths <- names(map)
  exact <- paths == variable
  keep <- if (sum(exact) == 1L) exact else {
    vapply(paths, .cf_basename, character(1L)) == .cf_basename(variable)
  }
  if (sum(keep) != 1L) {
    rlang::abort(
      paste0(
        "Current variable `", variable,
        "` does not resolve uniquely to preserved source metadata."
      ),
      class = "oceancube_transition_metadata"
    )
  }
  item <- map[[which(keep)]]
  standard_name <- .cf_attribute_value(
    item$attributes, "standard_name", default = NULL
  )
  unit <- .cf_attribute_value(item$attributes, "units", default = NULL)
  scalar <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  }
  list(
    current_variable = variable,
    source_path = item$source_path,
    standard_name = if (scalar(standard_name)) trimws(standard_name) else NA_character_,
    unit = if (scalar(unit)) trimws(unit) else NA_character_
  )
}

.transition_resolve_variable <- function(x, definition, variable = NULL) {
  if (!is.null(variable) && (
    !is.character(variable) || length(variable) != 1L ||
      is.na(variable) || !nzchar(variable))) {
    rlang::abort(
      "`variable` must be NULL or one exact non-empty current variable name.",
      class = "oceancube_transition_variable"
    )
  }
  if (!is.null(variable) && !variable %in% x$vars) {
    rlang::abort(
      paste0("Variable `", variable, "` is not present in the cube."),
      class = "oceancube_transition_variable"
    )
  }
  metadata <- lapply(x$vars, function(item) {
    .transition_source_item(x, item)
  })
  names(metadata) <- x$vars
  eligible <- vapply(metadata, function(item) {
    !is.na(item$standard_name) && item$standard_name %in% names(definition$basis)
  }, logical(1L))
  if (is.null(variable)) {
    candidates <- x$vars[eligible]
    if (!length(candidates)) {
      rlang::abort(
        paste0(
          "No variable has a preserved source CF standard_name eligible for ",
          definition$family, "."
        ),
        class = "oceancube_transition_variable"
      )
    }
    if (length(candidates) > 1L) {
      rlang::abort(
        paste0(
          "Multiple variables are eligible (", paste(candidates, collapse = ", "),
          "); supply one exact `variable=`."
        ),
        class = "oceancube_transition_variable_ambiguous"
      )
    }
    return(metadata[[candidates]])
  }
  if (!eligible[[variable]]) {
    standard_name <- metadata[[variable]]$standard_name
    detail <- if (is.na(standard_name)) {
      "has no usable preserved source CF standard_name"
    } else {
      paste0("declares incompatible standard_name `", standard_name, "`")
    }
    rlang::abort(
      paste0("Variable `", variable, "` ", detail, "."),
      class = "oceancube_transition_variable"
    )
  }
  metadata[[variable]]
}

.transition_unit_key <- function(unit) {
  if (!is.character(unit) || length(unit) != 1L || is.na(unit) || !nzchar(unit)) {
    return(NA_character_)
  }
  tolower(gsub("[[:space:]]+", " ", trimws(unit)))
}

.transition_unit_contract <- function(standard_name, unit, family) {
  key <- .transition_unit_key(unit)
  temperature <- c(
    "k", "kelvin", "degree_celsius", "degrees_celsius",
    "degree_c", "degrees_c"
  )
  dimensionless <- c("1", "1e-3", "1e-03", "0.001")
  mass_fraction <- c("g kg-1", "g kg^-1", "g kg**-1", "g/kg")
  if (identical(family, "temperature") && key %in% temperature) {
    return(list(
      family = "TEMPERATURE_K_OR_C_INTERVAL",
      status = "RECOGNIZED_COMPATIBLE"
    ))
  }
  dimensionless_names <- c(
    "sea_water_salinity", "sea_water_practical_salinity",
    "sea_water_cox_salinity", "sea_water_knudsen_salinity"
  )
  mass_names <- c("sea_water_absolute_salinity", "sea_water_reference_salinity")
  if (standard_name %in% dimensionless_names && key %in% dimensionless) {
    return(list(
      family = "DIMENSIONLESS_SALINITY_SCALE",
      status = "RECOGNIZED_COMPATIBLE"
    ))
  }
  if (standard_name %in% mass_names && key %in% mass_fraction) {
    return(list(
      family = "SALINITY_MASS_FRACTION_G_PER_KG",
      status = "RECOGNIZED_COMPATIBLE"
    ))
  }
  rlang::abort(
    paste0(
      "Source unit `", if (is.na(unit)) "<missing>" else unit,
      "` is incompatible with transition diagnostic standard_name `",
      standard_name, "`."
    ),
    class = "oceancube_transition_unit_unsupported"
  )
}

.transition_validate_source_profile <- function(x) {
  current <- x$metadata$cf$current
  derived <- c("vertical_sampling", "vertical_reduction", "vertical_gradient")
  present <- derived[vapply(derived, function(name) {
    !is.null(current[[name]])
  }, logical(1L))]
  if (length(present)) {
    rlang::abort(
      paste0(
        "Direct transition diagnostics require original source-profile semantics; ",
        "found current ", paste(present, collapse = ", "), "."
      ),
      class = "oceancube_transition_derived_input"
    )
  }
  vertical <- current$vertical %||% NULL
  if (is.null(vertical)) {
    rlang::abort(
      "Direct transition diagnostics require current metric depth metadata.",
      class = "oceancube_transition_input"
    )
  }
  .cf_vertical_validate(vertical)
  if (!identical(vertical$kind, "DEPTH_LENGTH") ||
      !identical(vertical$runtime_status, "VERTICAL_RUNTIME_SUPPORTED")) {
    rlang::abort(
      "Direct transition diagnostics require supported metric DEPTH_LENGTH.",
      class = "oceancube_transition_input"
    )
  }
  invisible(TRUE)
}

.transition_gradient_item <- function(descriptor, variable) {
  .cf_vertical_gradient_validate(descriptor)
  keep <- vapply(descriptor$variables, function(item) {
    identical(item$variable, variable) ||
      identical(.cf_basename(item$variable), .cf_basename(variable))
  }, logical(1L))
  if (sum(keep) != 1L) {
    rlang::abort(
      "C4 gradient variable metadata do not align uniquely with the diagnostic variable.",
      class = "oceancube_transition_gradient"
    )
  }
  descriptor$variables[[which(keep)]]
}

.transition_validate_gradient_origin <- function(x, variable) {
  descriptor <- x$metadata$cf$current$vertical_gradient %||% NULL
  if (is.null(descriptor)) {
    rlang::abort(
      "Certified-gradient input requires a current C4 descriptor.",
      class = "oceancube_transition_gradient"
    )
  }
  .vertical_feature_plan(x, "absolute", "local")
  item <- .transition_gradient_item(descriptor, variable)
  if (!identical(item$semantic_source, "current/source CF classifier") ||
      !item$input_value_semantics %in% c("VERTICAL_POINT", "VERTICAL_CELL_MEAN")) {
    rlang::abort(
      paste0(
        "C6 accepts only gradients originating directly from source/current CF ",
        "profile semantics; found `", item$semantic_source, "`."
      ),
      class = "oceancube_transition_derived_gradient"
    )
  }
  invisible(TRUE)
}
