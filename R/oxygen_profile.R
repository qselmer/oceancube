.oxygen_standard_name_contract <- function() {
  c(
    moles_of_oxygen_per_unit_mass_in_sea_water = "AMOUNT_PER_MASS",
    mole_concentration_of_dissolved_molecular_oxygen_in_sea_water =
      "AMOUNT_PER_VOLUME",
    mass_concentration_of_oxygen_in_sea_water = "MASS_PER_VOLUME"
  )
}

.oxygen_unit_key <- function(unit) {
  if (!is.character(unit) || length(unit) != 1L || is.na(unit) || !nzchar(unit)) {
    return(NA_character_)
  }
  key <- enc2utf8(trimws(unit))
  key <- gsub("[\u00b5\u03bc]", "u", key)
  key <- gsub("\u2212", "-", key, fixed = TRUE)
  key <- tolower(gsub("[[:space:]]+", " ", key))
  key
}

.oxygen_unit_any <- function(unit) {
  key <- .oxygen_unit_key(unit)
  contracts <- list(
    AMOUNT_PER_MASS = list(
      canonical_unit = "mol kg-1",
      scales = c(
        "mol kg-1" = 1, "mol kg^-1" = 1, "mol kg**-1" = 1,
        "mol/kg" = 1,
        "mmol kg-1" = 1e-3, "mmol kg^-1" = 1e-3,
        "mmol kg**-1" = 1e-3, "mmol/kg" = 1e-3,
        "umol kg-1" = 1e-6, "umol kg^-1" = 1e-6,
        "umol kg**-1" = 1e-6, "umol/kg" = 1e-6,
        "micromoles_per_kilogram" = 1e-6
      )
    ),
    AMOUNT_PER_VOLUME = list(
      canonical_unit = "mol m-3",
      scales = c(
        "mol m-3" = 1, "mol m^-3" = 1, "mol m**-3" = 1,
        "mol/m3" = 1,
        "mmol m-3" = 1e-3, "mmol m^-3" = 1e-3,
        "mmol m**-3" = 1e-3, "mmol/m3" = 1e-3,
        "umol m-3" = 1e-6, "umol m^-3" = 1e-6,
        "umol m**-3" = 1e-6, "umol/m3" = 1e-6,
        "mol l-1" = 1e3, "mol l^-1" = 1e3, "mol/l" = 1e3,
        "mmol l-1" = 1, "mmol l^-1" = 1, "mmol/l" = 1,
        "umol l-1" = 1e-3, "umol l^-1" = 1e-3, "umol/l" = 1e-3
      )
    ),
    MASS_PER_VOLUME = list(
      canonical_unit = "kg m-3",
      scales = c(
        "kg m-3" = 1, "kg m^-3" = 1, "kg m**-3" = 1,
        "kg/m3" = 1,
        "g m-3" = 1e-3, "g m^-3" = 1e-3,
        "g m**-3" = 1e-3, "g/m3" = 1e-3,
        "mg m-3" = 1e-6, "mg m^-3" = 1e-6,
        "mg m**-3" = 1e-6, "mg/m3" = 1e-6,
        "mg l-1" = 1e-3, "mg l^-1" = 1e-3, "mg/l" = 1e-3
      )
    )
  )
  for (family in names(contracts)) {
    at <- match(key, names(contracts[[family]]$scales))
    if (!is.na(at)) {
      return(list(
        source_unit = unit,
        normalized_unit = key,
        family = family,
        canonical_unit = contracts[[family]]$canonical_unit,
        scale_to_canonical = unname(contracts[[family]]$scales[[at]]),
        status = "RECOGNIZED_EXACT_MULTIPLICATIVE"
      ))
    }
  }
  rlang::abort(
    paste0("Unsupported bounded C7 oxygen unit `", unit, "`."),
    class = "oceancube_oxygen_unit_unsupported"
  )
}

.oxygen_unit_contract <- function(standard_name, unit) {
  expected <- .oxygen_standard_name_contract()[[standard_name]] %||% NULL
  if (is.null(expected)) {
    rlang::abort(
      paste0("Unsupported oxygen concentration standard_name `", standard_name, "`."),
      class = "oceancube_oxygen_variable"
    )
  }
  resolved <- .oxygen_unit_any(unit)
  if (!identical(resolved$family, expected)) {
    rlang::abort(
      paste0(
        "Oxygen source unit `", unit, "` belongs to ", resolved$family,
        " but standard_name `", standard_name, "` requires ", expected, "."
      ),
      class = "oceancube_oxygen_unit_cross_basis"
    )
  }
  resolved
}

.oxygen_resolve_variable <- function(x, variable = NULL) {
  if (!is.null(variable) && (
    !is.character(variable) || length(variable) != 1L ||
      is.na(variable) || !nzchar(variable))) {
    rlang::abort(
      "`variable` must be NULL or one exact non-empty current variable name.",
      class = "oceancube_oxygen_variable"
    )
  }
  if (!is.null(variable) && !variable %in% x$vars) {
    rlang::abort(
      paste0("Variable `", variable, "` is not present in the cube."),
      class = "oceancube_oxygen_variable"
    )
  }
  metadata <- lapply(x$vars, function(item) .transition_source_item(x, item))
  names(metadata) <- x$vars
  supported <- names(.oxygen_standard_name_contract())
  eligible <- vapply(metadata, function(item) {
    !is.na(item$standard_name) && item$standard_name %in% supported
  }, logical(1L))
  if (is.null(variable)) {
    candidates <- x$vars[eligible]
    if (!length(candidates)) {
      rlang::abort(
        "No variable has a preserved source CF oxygen concentration standard_name.",
        class = "oceancube_oxygen_variable"
      )
    }
    if (length(candidates) > 1L) {
      rlang::abort(
        paste0(
          "Multiple oxygen variables are eligible (",
          paste(candidates, collapse = ", "), "); supply exact `variable=`."
        ),
        class = "oceancube_oxygen_variable_ambiguous"
      )
    }
    variable <- candidates[[1L]]
  }
  if (!eligible[[variable]]) {
    rlang::abort(
      paste0(
        "Variable `", variable,
        "` lacks an eligible preserved source CF oxygen concentration standard_name."
      ),
      class = "oceancube_oxygen_variable"
    )
  }
  metadata[[variable]]
}

.oxygen_tolerance <- function(values) {
  finite <- values[is.finite(values)]
  if (!length(finite)) return(NA_real_)
  8 * sqrt(.Machine$double.eps) * max(1, abs(finite))
}

.oxygen_private_provenance <- function(provenance) {
  out <- provenance
  locator <- out$source$locator %||% NULL
  if (is.list(locator) && !isTRUE(locator$portable)) {
    base <- locator$basename %||% basename(locator$value)
    out$source$locator$value <- base
    out$source$locator$basename <- base
  }
  out
}

.oxygen_core <- function(source_values, canonical_values, depth_m) {
  finite <- is.finite(canonical_values)
  completeness <- sum(finite) / length(canonical_values)
  blank <- list(
    type = "INCOMPLETE_OXYGEN_PROFILE",
    source_minimum = NA_real_, canonical_minimum = NA_real_,
    tolerance = if (any(finite)) .oxygen_tolerance(canonical_values[finite]) else NA_real_,
    members = integer(), depth_m = NA_real_, shallow_depth_m = NA_real_,
    deep_depth_m = NA_real_, completeness = completeness
  )
  if (!all(finite)) return(blank)
  tolerance <- .oxygen_tolerance(canonical_values)
  minimum <- min(canonical_values)
  if (max(canonical_values) - minimum <= tolerance) {
    blank$type <- "FLAT_OXYGEN_PROFILE"
    blank$source_minimum <- min(source_values)
    blank$canonical_minimum <- minimum
    blank$tolerance <- tolerance
    blank$completeness <- 1
    return(blank)
  }
  members <- which(abs(canonical_values - minimum) <= tolerance)
  physical <- order(depth_m)
  positions <- sort(match(members, physical))
  components <- cumsum(c(TRUE, diff(positions) > 1L))
  if (length(unique(components)) > 1L) {
    blank$type <- "AMBIGUOUS_DISJOINT_MINIMA"
    blank$source_minimum <- min(source_values[members])
    blank$canonical_minimum <- minimum
    blank$tolerance <- tolerance
    blank$members <- as.integer(members)
    blank$completeness <- 1
    return(blank)
  }
  shallow <- min(depth_m[members])
  deep <- max(depth_m[members])
  list(
    type = if (length(members) == 1L) {
      "UNIQUE_MINIMUM"
    } else {
      "CONTIGUOUS_MINIMUM_PLATEAU"
    },
    source_minimum = min(source_values[members]),
    canonical_minimum = minimum,
    tolerance = tolerance,
    members = as.integer(members),
    depth_m = if (length(members) == 1L) shallow else (shallow + deep) / 2,
    shallow_depth_m = shallow,
    deep_depth_m = deep,
    completeness = 1
  )
}

.oxygen_prepare <- function(x, variable = NULL) {
  .check_cube(x)
  .transition_validate_source_profile(x)
  metadata <- .oxygen_resolve_variable(x, variable)
  unit <- .oxygen_unit_contract(metadata$standard_name, metadata$unit)
  selected <- cube_slice(x, variable = metadata$current_variable)
  read_diagnostics <- selected$qa$selection$netcdf_read %||% NULL
  plan <- .vertical_gradient_plan(selected, "auto")
  gradient <- depth_gradient(selected, method = "auto")
  descriptor <- gradient$metadata$cf$current$vertical_gradient
  descriptor_item <- .transition_gradient_item(
    descriptor, metadata$current_variable
  )
  if (!descriptor_item$input_value_semantics %in%
      c("VERTICAL_POINT", "VERTICAL_CELL_MEAN")) {
    rlang::abort(
      "C7 oxygen diagnostics require original point or certified cell-mean profiles.",
      class = "oceancube_oxygen_input"
    )
  }
  values <- .cube_read(selected)
  d <- unname(.cube_shape(selected))
  profile_dimensions <- as.integer(d[c(1L, 2L, 4L)])
  profile_count <- prod(profile_dimensions)
  profile_index <- arrayInd(seq_len(profile_count), .dim = profile_dimensions)
  colnames(profile_index) <- c("longitude", "latitude", "time")
  source_matrix <- matrix(
    aperm(values, c(3L, 1L, 2L, 4L, 5L)),
    nrow = d[[3L]], ncol = profile_count
  )
  canonical_matrix <- source_matrix * unit$scale_to_canonical
  depth_m <- as.numeric(plan$metric$canonical_m)
  cores <- lapply(seq_len(profile_count), function(i) {
    .oxygen_core(source_matrix[, i], canonical_matrix[, i], depth_m)
  })
  list(
    selected = selected,
    gradient = gradient,
    descriptor = descriptor,
    descriptor_item = descriptor_item,
    plan = plan,
    metadata = metadata,
    unit = unit,
    source_matrix = source_matrix,
    canonical_matrix = canonical_matrix,
    depth_m = depth_m,
    profile_index = profile_index,
    profile_count = as.integer(profile_count),
    cores = cores,
    netcdf_reads = as.integer(!is.null(read_diagnostics)),
    variables_read = read_diagnostics$variables %||% character(),
    levels_read = if (is.null(read_diagnostics)) 0L else {
      as.integer(unname(read_diagnostics$physical_count[["depth"]]))
    }
  )
}

.oxygen_branch_pairs <- function(prepared, core, branch) {
  if (!core$type %in% c("UNIQUE_MINIMUM", "CONTIGUOUS_MINIMUM_PLATEAU")) {
    return(integer())
  }
  pairs <- prepared$descriptor$source_pair_indices
  tolerance <- 8 * sqrt(.Machine$double.eps) *
    max(1, abs(prepared$depth_m))
  keep <- vapply(pairs, function(pair) {
    pair_depth <- prepared$depth_m[as.integer(pair)]
    if (identical(branch, "UPPER")) {
      max(pair_depth) <= core$shallow_depth_m + tolerance
    } else {
      min(pair_depth) >= core$deep_depth_m - tolerance
    }
  }, logical(1L))
  which(keep)
}

.oxygen_blank_feature <- function(prepared, profile, polarity, support, status) {
  at <- prepared$profile_index[profile, ]
  item <- prepared$descriptor_item
  data.frame(
    longitude = prepared$selected$lon[at[["longitude"]]],
    latitude = prepared$selected$lat[at[["latitude"]]],
    time = prepared$selected$time[at[["time"]]],
    variable = prepared$metadata$current_variable,
    variable_unit = item$source_unit,
    polarity = polarity,
    support_policy = support,
    feature_depth = NA_real_,
    depth_unit = prepared$gradient$metadata$cf$current$vertical$units_raw,
    feature_depth_m = NA_real_, source_depth_1 = NA_real_,
    source_depth_2 = NA_real_, source_depth_1_m = NA_real_,
    source_depth_2_m = NA_real_, gradient = NA_real_,
    gradient_magnitude = NA_real_, gradient_unit = item$output_unit,
    gradient_index = NA_integer_, spacing_m = NA_real_,
    support_relation = NA_character_, support_gap_m = NA_real_,
    localization_half_span_m = NA_real_, n_support_eligible = 0L,
    n_finite_gradient = 0L, gradient_completeness = NA_real_,
    n_matching_candidates = 0L, n_tied = 0L,
    feature_tolerance = NA_real_, feature_status = status,
    feature_certification_status = "NO_CANDIDATE",
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

.oxygen_branch_feature <- function(prepared, profile, branch, support) {
  core <- prepared$cores[[profile]]
  polarity <- if (identical(branch, "UPPER")) "negative" else "positive"
  invalid <- c(
    INCOMPLETE_OXYGEN_PROFILE = "INCOMPLETE_OXYGEN_PROFILE",
    AMBIGUOUS_DISJOINT_MINIMA = "AMBIGUOUS_DISJOINT_MINIMA",
    FLAT_OXYGEN_PROFILE = "FLAT_OXYGEN_PROFILE"
  )
  if (core$type %in% names(invalid)) {
    return(list(
      feature = .oxygen_blank_feature(
        prepared, profile, polarity, support, invalid[[core$type]]
      ),
      pairs = integer()
    ))
  }
  pairs <- .oxygen_branch_pairs(prepared, core, branch)
  if (!length(pairs)) {
    status <- if (identical(branch, "UPPER")) {
      "NO_UPPER_BRANCH"
    } else {
      "NO_LOWER_BRANCH"
    }
    return(list(
      feature = .oxygen_blank_feature(
        prepared, profile, polarity, support, status
      ),
      pairs = pairs
    ))
  }
  at <- prepared$profile_index[profile, ]
  branch_cube <- cube_slice(
    prepared$gradient,
    longitude = at[["longitude"]], latitude = at[["latitude"]],
    depth = pairs, time = at[["time"]], variable = 1L,
    by = "index"
  )
  feature <- depth_feature(branch_cube, polarity = polarity, support = support)
  names(feature)[names(feature) == "status"] <- "feature_status"
  names(feature)[names(feature) == "certification_status"] <-
    "feature_certification_status"
  list(feature = feature, pairs = pairs)
}

.transition_oxygen <- function(x, diagnostic, variable, support) {
  branch <- if (identical(diagnostic, "upper_oxycline")) "UPPER" else "LOWER"
  prepared <- .oxygen_prepare(x, variable)
  resolved <- lapply(seq_len(prepared$profile_count), function(profile) {
    .oxygen_branch_feature(prepared, profile, branch, support)
  })
  feature <- do.call(rbind, lapply(resolved, `[[`, "feature"))
  rownames(feature) <- NULL
  cores <- prepared$cores
  core_field <- function(name, default = NA) {
    vapply(cores, function(core) core[[name]] %||% default, default)
  }
  candidate <- !is.na(feature$gradient_index)
  gapped <- candidate & feature$support_relation == "GAPPED_SUPPORT"
  label <- paste0(branch, "_OXYCLINE_GRADIENT_CANDIDATE")
  diagnostic_status <- feature$feature_status
  diagnostic_status[candidate & !gapped] <- label
  diagnostic_status[candidate & gapped] <- paste0("GAPPED_", label)
  diagnostic_certification <- feature$feature_certification_status
  diagnostic_certification[candidate & !gapped] <-
    "CERTIFIED_UNTHRESHOLDED_LOCAL_BRANCH_CANDIDATE"
  diagnostic_certification[candidate & gapped] <-
    "CERTIFIED_UNTHRESHOLDED_GAPPED_BRANCH_CANDIDATE"
  direction <- rep(NA_character_, nrow(feature))
  direction[candidate & feature$gradient > 0] <- "INCREASING_WITH_DEPTH"
  direction[candidate & feature$gradient < 0] <- "DECREASING_WITH_DEPTH"
  definition <- paste0(
    branch,
    if (identical(branch, "UPPER")) {
      "_OXYCLINE_BRANCH_MAX_NEGATIVE_OXYGEN_GRADIENT"
    } else {
      "_OXYCLINE_BRANCH_MAX_POSITIVE_OXYGEN_GRADIENT"
    }
  )
  feature$diagnostic <- diagnostic
  feature$diagnostic_definition <- definition
  feature$variable_standard_name <- prepared$metadata$standard_name
  feature$variable_semantic_family <- "oxygen"
  feature$temperature_or_salinity_basis <- prepared$unit$family
  feature$input_mode <- "SOURCE_PROFILE"
  feature$gradient_value_semantics <-
    prepared$descriptor_item$output_value_semantics
  feature$gradient_method <- prepared$descriptor_item$resolved_method
  feature$gradient_direction <- direction
  feature$diagnostic_strength <- feature$gradient_magnitude
  feature$diagnostic_strength_unit <- feature$gradient_unit
  feature$threshold_applied <- FALSE
  feature$diagnostic_status <- diagnostic_status
  feature$diagnostic_certification_status <- diagnostic_certification
  feature$oxygen_standard_name <- prepared$metadata$standard_name
  feature$oxygen_quantity_family <- prepared$unit$family
  feature$oxygen_source_unit <- prepared$metadata$unit
  feature$oxygen_canonical_unit <- prepared$unit$canonical_unit
  feature$core_type <- core_field("type", "")
  feature$core_minimum_value <- core_field("source_minimum", NA_real_)
  feature$core_minimum_value_canonical <-
    core_field("canonical_minimum", NA_real_)
  feature$core_depth_m <- core_field("depth_m", NA_real_)
  feature$core_shallow_depth_m <- core_field("shallow_depth_m", NA_real_)
  feature$core_deep_depth_m <- core_field("deep_depth_m", NA_real_)
  feature$core_value_semantics <- if (identical(
    prepared$descriptor_item$input_value_semantics, "VERTICAL_POINT"
  )) "POINT_VALUE_MINIMUM" else "CELL_MEAN_MINIMUM"
  feature$branch <- branch
  feature$branch_gradient_count <- as.integer(vapply(
    resolved, function(item) length(item$pairs), integer(1L)
  ))
  feature$oxygen_profile_completeness <-
    core_field("completeness", NA_real_)

  status_counts <- as.list(as.integer(table(diagnostic_status)))
  names(status_counts) <- names(table(diagnostic_status))
  base_provenance <- .oxygen_private_provenance(prepared$gradient$provenance)
  context <- .provenance_cube_context(
    source = prepared$gradient$source,
    dataset_id = prepared$gradient$dataset_id,
    time = prepared$gradient$time,
    shape = stats::setNames(
      as.integer(.cube_shape(prepared$gradient)), .cube_axis_names()
    ),
    variables = prepared$gradient$vars,
    backend = "memory",
    provenance = base_provenance
  )
  provenance <- .provenance_append(
    base_provenance,
    operation = "depth_feature",
    parameters = list(
      requested = list(
        polarity = if (identical(branch, "UPPER")) "negative" else "positive",
        support = support
      ),
      resolved = list(
        branch = branch,
        branch_pair_indices = lapply(resolved, `[[`, "pairs"),
        per_profile_branch_ranking = TRUE,
        core_required = TRUE
      )
    ),
    output = list(
      backend = "memory",
      shape = c(rows = nrow(feature), columns = ncol(feature)),
      variables = prepared$metadata$current_variable,
      time_kind = context$time_kind
    ),
    scientific_method = .provenance_method(
      "depth_feature",
      list(polarity = if (identical(branch, "UPPER")) "negative" else "positive")
    ),
    context = context
  )
  provenance <- .provenance_append(
    provenance,
    operation = "transition_layer",
    parameters = list(
      requested = list(
        diagnostic = diagnostic, variable = variable, support = support
      ),
      resolved = list(
        oxygen_standard_name = prepared$metadata$standard_name,
        oxygen_source_unit = prepared$metadata$unit,
        oxygen_quantity_family = prepared$unit$family,
        oxygen_canonical_unit = prepared$unit$canonical_unit,
        core_tolerance_rule =
          "8*sqrt(.Machine$double.eps)*max(1,abs(finite canonical oxygen))",
        core_types = as.list(table(feature$core_type)),
        core_member_indices = lapply(cores, `[[`, "members"),
        core_depths_m = lapply(cores, function(core) list(
          representative = core$depth_m,
          shallow = core$shallow_depth_m,
          deep = core$deep_depth_m
        )),
        branch = branch,
        branch_pair_indices = lapply(resolved, `[[`, "pairs"),
        c4_descriptor_version = prepared$descriptor$schema_version,
        c5_polarity = if (identical(branch, "UPPER")) "negative" else "positive",
        support_policy = support,
        candidate_status = as.character(diagnostic_status),
        diagnostic_status_counts = status_counts,
        netcdf_scientific_payload_reads = prepared$netcdf_reads,
        variables_read = prepared$variables_read,
        levels_read = prepared$levels_read
      )
    ),
    output = list(
      backend = "memory",
      shape = c(rows = nrow(feature), columns = ncol(feature)),
      variables = prepared$metadata$current_variable,
      time_kind = context$time_kind
    ),
    scientific_method = .provenance_method(
      "transition_layer", list(diagnostic = diagnostic)
    ),
    context = context
  )
  qa <- list(
    profiles_total = prepared$profile_count,
    profiles_complete = as.integer(sum(feature$oxygen_profile_completeness == 1)),
    profiles_incomplete = as.integer(sum(feature$oxygen_profile_completeness < 1)),
    unique_minima = as.integer(sum(feature$core_type == "UNIQUE_MINIMUM")),
    plateau_cores = as.integer(sum(
      feature$core_type == "CONTIGUOUS_MINIMUM_PLATEAU"
    )),
    ambiguous_disjoint_minima = as.integer(sum(
      feature$core_type == "AMBIGUOUS_DISJOINT_MINIMA"
    )),
    flat_profiles = as.integer(sum(feature$core_type == "FLAT_OXYGEN_PROFILE")),
    upper_candidates = if (identical(branch, "UPPER")) as.integer(sum(candidate)) else 0L,
    lower_candidates = if (identical(branch, "LOWER")) as.integer(sum(candidate)) else 0L,
    gapped_upper_candidates = if (identical(branch, "UPPER")) as.integer(sum(gapped)) else 0L,
    gapped_lower_candidates = if (identical(branch, "LOWER")) as.integer(sum(gapped)) else 0L,
    netcdf_scientific_payload_reads = prepared$netcdf_reads,
    variables_read = prepared$variables_read,
    levels_read = prepared$levels_read,
    diagnostic_status_counts = status_counts
  )
  attr(feature, "oceancube_provenance") <- provenance
  attr(feature, "oceancube_qa") <- list(oxygen_profile = qa)
  feature
}
