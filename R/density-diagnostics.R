.c10_state_require <- function(x, variables, reference_zero = FALSE) {
  .check_cube(x)
  if (!identical(.cube_backend(x), "memory")) {
    rlang::abort(
      "C10 density diagnostics require a memory-backed certified C9 state.",
      class = "oceancube_c10_state_input"
    )
  }
  descriptor <- x$metadata$cf$current$thermodynamic_state %||% NULL
  if (is.null(descriptor)) {
    rlang::abort(
      "C10 density diagnostics require the current certified C9 thermodynamic descriptor.",
      class = "oceancube_c10_state_input"
    )
  }
  .cf_thermodynamic_state_validate(descriptor)
  certified <- identical(descriptor$schema_name, "oceancube_thermodynamic_state") &&
    identical(descriptor$schema_version, "1.0.0") &&
    identical(descriptor$method, "TEOS-10") &&
    identical(descriptor$certification_status, "CERTIFIED_C9_TEOS10_STATE")
  if (!certified || (reference_zero && !identical(
    as.numeric(descriptor$reference_pressure_dbar), 0
  ))) {
    rlang::abort(
      paste0(
        "C10 requires certified C9 TEOS-10 state",
        if (reference_zero) " referenced to exactly 0 dbar" else "", "."
      ),
      class = "oceancube_c10_state_input"
    )
  }
  if (!all(variables %in% x$vars)) {
    rlang::abort(
      paste0("Certified C9 state is missing: ",
             paste(setdiff(variables, x$vars), collapse = ", "), "."),
      class = "oceancube_c10_state_variable"
    )
  }
  definitions <- stats::setNames(descriptor$output_variables, vapply(
    descriptor$output_variables, `[[`, character(1L), "variable"
  ))
  valid <- vapply(variables, function(variable) {
    item <- definitions[[variable]]
    !is.null(item) && identical(item$variable, variable) &&
      identical(
        item$value_semantics,
        "TEOS10_POINT_STATE_FROM_REPRESENTATIVE_SOURCE_VALUES"
      )
  }, logical(1L))
  if (!all(valid)) {
    rlang::abort(
      "C10 state variables are not exact certified C9 point-state outputs.",
      class = "oceancube_c10_state_variable"
    )
  }
  list(descriptor = descriptor, definitions = definitions)
}

.density_mld <- function(
    x, variable, reference_depth_m, threshold, support, threshold_source) {
  if (!is.numeric(reference_depth_m) || length(reference_depth_m) != 1L ||
      is.na(reference_depth_m) || !is.finite(reference_depth_m) ||
      reference_depth_m < 0) {
    rlang::abort(
      "`reference_depth_m` must be one finite non-negative numeric value.",
      class = "oceancube_mld_reference_depth"
    )
  }
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) ||
      !is.finite(threshold) || threshold <= 0) {
    rlang::abort(
      "`threshold` must be one finite positive numeric value.",
      class = "oceancube_mld_threshold"
    )
  }
  required <- "sea_water_potential_density"
  if (!is.null(variable) && (!is.character(variable) || length(variable) != 1L ||
      is.na(variable) || !identical(variable, required))) {
    rlang::abort(
      "Density MLD `variable` must be exactly `sea_water_potential_density`.",
      class = "oceancube_mld_density_variable"
    )
  }
  state <- .c10_state_require(x, required, reference_zero = TRUE)
  item <- state$definitions[[required]]
  if (!identical(item$standard_name, "sea_water_potential_density") ||
      !identical(item$unit, "kg m-3") ||
      !identical(as.numeric(item$reference_pressure_dbar), 0)) {
    rlang::abort(
      "Density MLD requires the exact C9 potential-density output at 0 dbar.",
      class = "oceancube_mld_density_basis"
    )
  }
  selected <- if (identical(x$vars, required)) x else cube_slice(x, variable = required)
  metric <- .vertical_metric_depth(selected)
  support_plan <- .vertical_gradient_support(metric, require_cell = FALSE)
  values <- .cube_read(selected)
  d <- unname(.cube_shape(selected))
  profile_dims <- as.integer(d[c(1L, 2L, 4L)])
  n_profiles <- as.integer(prod(profile_dims))
  indices <- arrayInd(seq_len(n_profiles), profile_dims)
  colnames(indices) <- c("longitude", "latitude", "time")
  matrix_values <- matrix(
    aperm(values, c(3L, 1L, 2L, 4L, 5L)), nrow = d[[3L]], ncol = n_profiles
  )
  physical <- order(metric$canonical_m)
  pair_map <- vapply(seq_len(length(physical) - 1L), function(i) {
    pair <- sort(physical[c(i, i + 1L)])
    which(vapply(metric$source_pair_indices, function(candidate) {
      identical(sort(as.integer(candidate)), as.integer(pair))
    }, logical(1L)))[[1L]]
  }, integer(1L))
  prepared <- list(
    selected = selected, metric = metric, profile_index = indices,
    profile_count = n_profiles, physical = physical,
    depth_m = metric$canonical_m[physical],
    source_depth = metric$source_depths[physical],
    source_matrix = matrix_values,
    pair_relation = support_plan$relation[pair_map],
    pair_gap_m = support_plan$gap_m[pair_map],
    depth_tolerance_m = .vertical_geometry_tolerance(metric$canonical_m)
  )
  resolved <- lapply(seq_len(n_profiles), function(profile) {
    .density_mld_profile(
      prepared, profile, reference_depth_m, threshold, support,
      threshold_source
    )
  })
  result <- do.call(rbind, lapply(resolved, `[[`, "row"))
  rownames(result) <- NULL
  counts <- table(result$status)
  status_counts <- as.list(as.integer(counts)); names(status_counts) <- names(counts)
  context <- .provenance_cube_context(
    source = selected$source, dataset_id = selected$dataset_id,
    time = selected$time,
    shape = stats::setNames(as.integer(.cube_shape(selected)), .cube_axis_names()),
    variables = required, backend = "memory", provenance = selected$provenance
  )
  provenance <- .provenance_append(
    selected$provenance, operation = "mixed_layer_depth",
    parameters = list(
      requested = list(
        method = "density_threshold", variable = variable,
        reference_depth_m = reference_depth_m,
        threshold = threshold, support = support
      ),
      resolved = list(
        c9_schema_version = state$descriptor$schema_version,
        c9_reference_pressure_dbar = state$descriptor$reference_pressure_dbar,
        selected_density_variable = required,
        density_basis = "POTENTIAL_DENSITY_REF_0_DBAR",
        reference_depth_m = reference_depth_m,
        reference_method = as.character(result$reference_status),
        reference_density = .provenance_compact(result$reference_density),
        threshold_requested = if (identical(threshold_source, "EXPLICIT")) threshold else NULL,
        threshold_effective = threshold,
        threshold_source = threshold_source,
        departure_rule = "rho(z)-rho(reference); positive departure only",
        first_crossing_rule = "FIRST_POSITIVE_DEPARTURE_AT_OR_ABOVE_THRESHOLD",
        gap_policy = support,
        missingness_policy = "NO_BRIDGING_BEFORE_FIRST_CROSSING",
        crossing_brackets = lapply(resolved, `[[`, "evidence"),
        status_counts = status_counts,
        netcdf_scientific_payload_reads = 0L
      )
    ),
    output = list(
      backend = "memory", shape = c(rows = nrow(result), columns = ncol(result)),
      variables = required, time_kind = context$time_kind
    ),
    scientific_method = .provenance_method(
      "mixed_layer_depth", list(method = "density_threshold")
    ), context = context
  )
  qa <- list(
    profiles_total = n_profiles,
    resolved_exact = as.integer(sum(result$status == "MLD_EXACT_THRESHOLD_POINT")),
    resolved_interpolated = as.integer(sum(
      result$status == "MLD_INTERPOLATED_THRESHOLD_CROSSING"
    )),
    open_bottom = as.integer(sum(result$status == "MLD_OPEN_AT_PROFILE_BOTTOM")),
    reference_failures = as.integer(sum(grepl("^REFERENCE_", result$status))),
    gap_failures = as.integer(sum(grepl("GAP", result$status))),
    missing_path_failures = as.integer(sum(
      result$status == "MLD_UNRESOLVED_INCOMPLETE_PATH"
    )),
    density_inversion_profiles = as.integer(sum(
      result$density_inversion_encountered, na.rm = TRUE
    )),
    netcdf_scientific_payload_reads = 0L,
    memory_cube_reads = 1L,
    status_counts = status_counts
  )
  attr(result, "oceancube_provenance") <- provenance
  attr(result, "oceancube_qa") <- list(mixed_layer_depth = qa)
  result
}

.density_mld_profile <- function(
    prepared, profile, reference_depth_m, threshold, support, threshold_source) {
  z <- prepared$depth_m
  source_z <- prepared$source_depth
  rho <- prepared$source_matrix[prepared$physical, profile]
  tolerance_z <- prepared$depth_tolerance_m
  reference <- .density_mld_reference(
    z, source_z, rho, prepared$pair_relation, prepared$pair_gap_m,
    reference_depth_m, tolerance_z
  )
  crossing <- NULL
  inversion <- FALSE
  n_points <- 0L
  n_finite <- 0L
  if (reference$resolved) {
    previous <- list(
      depth_m = reference$depth_m, source_depth = reference$source_depth,
      density = reference$density, departure = 0,
      position = reference$anchor_position
    )
    positions <- which(z > reference_depth_m + tolerance_z)
    for (position in positions) {
      relation_index <- if (is.na(previous$position)) {
        reference$deep_pair_position
      } else min(previous$position, position)
      relation <- prepared$pair_relation[[relation_index]]
      gap <- prepared$pair_gap_m[[relation_index]]
      current <- list(
        depth_m = z[[position]], source_depth = source_z[[position]],
        density = rho[[position]], position = position,
        relation = relation, gap_m = gap
      )
      n_points <- n_points + 1L
      if (!is.finite(current$density)) {
        crossing <- list(
          status = "MLD_UNRESOLVED_INCOMPLETE_PATH",
          certification = "UNRESOLVED_MISSING_PATH",
          path = "INCOMPLETE_BEFORE_FIRST_CROSSING", previous = previous,
          current = current, relation = relation, gap_m = gap
        )
        break
      }
      n_finite <- n_finite + 1L
      current$departure <- current$density - reference$density
      inversion <- inversion || current$departure < 0
      is_gap <- identical(relation, "GAPPED_SUPPORT")
      if (is_gap && identical(support, "local")) {
        crossing <- list(
          status = "MLD_UNRESOLVED_BEFORE_SUPPORT_GAP",
          certification = "UNRESOLVED_LOCAL_SUPPORT_GAP",
          path = "TERMINATED_AT_SUPPORT_GAP", previous = previous,
          current = current, relation = relation, gap_m = gap
        )
        break
      }
      tolerance <- 8 * sqrt(.Machine$double.eps) *
        max(1, abs(c(reference$density, previous$density, current$density, threshold)))
      if (current$departure >= threshold - tolerance) {
        if (is_gap) {
          crossing <- list(
            status = "GAPPED_MLD_THRESHOLD_BRACKET",
            certification = "UNRESOLVED_GAPPED_THRESHOLD_PATH",
            path = "GAPPED_THRESHOLD_BRACKET", previous = previous,
            current = current, relation = relation, gap_m = gap
          )
          break
        }
        if (abs(current$departure - threshold) <= tolerance) {
          crossing <- list(
            status = "MLD_EXACT_THRESHOLD_POINT",
            certification = "CERTIFIED_EXACT_POINT_MLD",
            path = "COMPLETE_THROUGH_MLD", previous = previous,
            current = current, relation = relation, gap_m = gap,
            depth_m = current$depth_m, source_depth = current$source_depth
          )
        } else {
          denominator <- current$departure - previous$departure
          fraction <- (threshold - previous$departure) / denominator
          if (!is.finite(fraction) || fraction < 0 || fraction > 1) {
            crossing <- list(
              status = "MLD_UNRESOLVED_INVALID_INTERPOLATION",
              certification = "UNRESOLVED_NUMERICAL_BRACKET",
              path = "INVALID_THRESHOLD_INTERPOLATION", previous = previous,
              current = current, relation = relation, gap_m = gap
            )
          } else {
            depth_m <- previous$depth_m +
              fraction * (current$depth_m - previous$depth_m)
            crossing <- list(
              status = "MLD_INTERPOLATED_THRESHOLD_CROSSING",
              certification = "CERTIFIED_INTERPOLATED_LOCAL_MLD",
              path = "COMPLETE_THROUGH_MLD", previous = previous,
              current = current, relation = relation, gap_m = gap,
              depth_m = depth_m,
              source_depth = .mld_source_depth(prepared, depth_m),
              fraction = fraction
            )
          }
        }
        break
      }
      previous <- current
    }
    if (is.null(crossing)) {
      crossing <- list(
        status = "MLD_OPEN_AT_PROFILE_BOTTOM",
        certification = "CERTIFIED_OPEN_BOTTOM",
        path = "COMPLETE_TO_PROFILE_BOTTOM", previous = previous,
        current = NULL, relation = NA_character_, gap_m = NA_real_
      )
    }
  } else {
    crossing <- list(
      status = reference$status, certification = "UNRESOLVED_REFERENCE",
      path = "REFERENCE_UNRESOLVED", previous = NULL, current = NULL,
      relation = NA_character_, gap_m = NA_real_
    )
  }
  crossing$n_path_points <- n_points
  crossing$n_path_finite <- n_finite
  previous <- crossing$previous %||% NULL
  current <- crossing$current %||% NULL
  at <- prepared$profile_index[profile, ]
  row <- data.frame(
    longitude = prepared$selected$lon[at[["longitude"]]],
    latitude = prepared$selected$lat[at[["latitude"]]],
    time = prepared$selected$time[at[["time"]]],
    variable = "sea_water_potential_density",
    density_standard_name = "sea_water_potential_density",
    density_basis = "POTENTIAL_DENSITY_REF_0_DBAR",
    density_unit = "kg m-3",
    potential_density_reference_pressure_dbar = 0,
    method = "density_threshold",
    reference_depth_requested_m = reference_depth_m,
    reference_depth_resolved_m = reference$depth_m,
    reference_density = reference$density,
    reference_status = reference$status,
    threshold = threshold,
    threshold_effective = threshold,
    threshold_source = threshold_source,
    threshold_unit = "kg m-3",
    mld_depth = crossing$source_depth %||% NA_real_,
    depth_unit = prepared$metric$source_unit,
    mld_depth_m = crossing$depth_m %||% NA_real_,
    bracket_shallow_depth = previous$source_depth %||% NA_real_,
    bracket_deep_depth = current$source_depth %||% NA_real_,
    bracket_shallow_depth_m = previous$depth_m %||% NA_real_,
    bracket_deep_depth_m = current$depth_m %||% NA_real_,
    bracket_shallow_density = previous$density %||% NA_real_,
    bracket_deep_density = current$density %||% NA_real_,
    density_departure_at_shallow = previous$departure %||% NA_real_,
    density_departure_at_deep = current$departure %||% NA_real_,
    density_inversion_encountered = inversion,
    support_relation = crossing$relation %||% NA_character_,
    support_gap_m = crossing$gap_m %||% NA_real_,
    n_path_points = crossing$n_path_points,
    n_path_finite = crossing$n_path_finite,
    path_completeness = crossing$path,
    status = crossing$status,
    certification_status = crossing$certification,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  list(row = row, evidence = list(
    status = crossing$status,
    shallow_depth_m = previous$depth_m %||% NA_real_,
    deep_depth_m = current$depth_m %||% NA_real_,
    shallow_density = previous$density %||% NA_real_,
    deep_density = current$density %||% NA_real_,
    interpolation_fraction = crossing$fraction %||% NA_real_
  ))
}

.density_mld_reference <- function(
    z, source_z, rho, relation, gap, requested, tolerance) {
  failure <- function(status) list(
    resolved = FALSE, status = status, depth_m = NA_real_,
    source_depth = NA_real_, density = NA_real_, anchor_position = NA_integer_,
    deep_pair_position = NA_integer_
  )
  if (requested < min(z) - tolerance || requested > max(z) + tolerance) {
    return(failure("REFERENCE_DEPTH_OUTSIDE_PROFILE"))
  }
  exact <- which(abs(z - requested) <= tolerance)
  if (length(exact) == 1L) {
    i <- exact[[1L]]
    if (!is.finite(rho[[i]])) return(failure("REFERENCE_DENSITY_UNRESOLVED"))
    return(list(
      resolved = TRUE, status = "REFERENCE_EXACT_POINT", depth_m = z[[i]],
      source_depth = source_z[[i]], density = rho[[i]],
      anchor_position = i, deep_pair_position = NA_integer_
    ))
  }
  shallow <- max(which(z < requested)); deep <- min(which(z > requested))
  if (identical(relation[[shallow]], "GAPPED_SUPPORT")) {
    return(failure("REFERENCE_GAPPED_BRACKET"))
  }
  if (!is.finite(rho[[shallow]]) || !is.finite(rho[[deep]])) {
    return(failure("REFERENCE_DENSITY_UNRESOLVED"))
  }
  fraction <- (requested - z[[shallow]]) / (z[[deep]] - z[[shallow]])
  list(
    resolved = TRUE, status = "REFERENCE_INTERPOLATED_POINT",
    depth_m = requested,
    source_depth = source_z[[shallow]] + fraction *
      (source_z[[deep]] - source_z[[shallow]]),
    density = rho[[shallow]] + fraction * (rho[[deep]] - rho[[shallow]]),
    anchor_position = NA_integer_, deep_pair_position = shallow
  )
}

.transition_pycnocline <- function(x, variable, support) {
  required <- "sea_water_potential_density"
  if (!is.null(variable) && (!is.character(variable) || length(variable) != 1L ||
      is.na(variable) || !identical(variable, required))) {
    rlang::abort(
      "Pycnocline `variable` must be exactly `sea_water_potential_density`.",
      class = "oceancube_transition_variable"
    )
  }
  state <- .c10_state_require(x, required, reference_zero = TRUE)
  selected <- if (identical(x$vars, required)) x else cube_slice(x, variable = required)
  gradient <- depth_gradient(selected, method = "auto")
  feature <- depth_feature(gradient, polarity = "positive", support = support)
  names(feature)[names(feature) == "status"] <- "feature_status"
  names(feature)[names(feature) == "certification_status"] <-
    "feature_certification_status"
  resolved <- !is.na(feature$gradient_index)
  gapped <- resolved & feature$support_relation == "GAPPED_SUPPORT"
  incomplete <- resolved & grepl(
    "INCOMPLETE_PROFILE$", feature$feature_certification_status
  )
  status <- feature$feature_status
  status[resolved & !gapped & !incomplete] <- "PYCNOCLINE_GRADIENT_CANDIDATE"
  status[gapped & !incomplete] <- "GAPPED_PYCNOCLINE_GRADIENT_CANDIDATE"
  status[incomplete & !gapped] <- "OBSERVED_PYCNOCLINE_CANDIDATE_INCOMPLETE_PROFILE"
  status[incomplete & gapped] <- "OBSERVED_GAPPED_PYCNOCLINE_CANDIDATE_INCOMPLETE_PROFILE"
  direction <- rep(NA_character_, nrow(feature))
  direction[resolved] <- "INCREASING_WITH_DEPTH"
  feature$diagnostic <- "pycnocline"
  feature$diagnostic_definition <- "PYCNOCLINE_MAX_POSITIVE_POTENTIAL_DENSITY_GRADIENT"
  feature$variable_standard_name <- "sea_water_potential_density"
  feature$variable_semantic_family <- "potential_density"
  feature$density_basis <- "POTENTIAL_DENSITY_REF_0_DBAR"
  feature$potential_density_reference_pressure_dbar <- 0
  feature$input_mode <- "CERTIFIED_THERMODYNAMIC_STATE"
  feature$gradient_value_semantics <- "DERIVED_POINT_SECANT_GRADIENT"
  feature$gradient_method <- "point"
  feature$gradient_direction <- direction
  feature$diagnostic_strength <- feature$gradient_magnitude
  feature$diagnostic_strength_unit <- feature$gradient_unit
  feature$threshold_applied <- FALSE
  feature$diagnostic_status <- status
  feature$diagnostic_certification_status <- ifelse(
    resolved, ifelse(gapped,
      "CERTIFIED_UNTHRESHOLDED_GAPPED_CANDIDATE",
      "CERTIFIED_UNTHRESHOLDED_LOCAL_CANDIDATE"
    ), feature$feature_certification_status
  )
  old_provenance <- attr(feature, "oceancube_provenance", exact = TRUE)
  old_qa <- attr(feature, "oceancube_qa", exact = TRUE) %||% list()
  context <- .provenance_cube_context(
    source = selected$source, dataset_id = selected$dataset_id,
    time = selected$time,
    shape = stats::setNames(as.integer(.cube_shape(gradient)), .cube_axis_names()),
    variables = required, backend = "memory", provenance = old_provenance
  )
  provenance <- .provenance_append(
    old_provenance, operation = "transition_layer",
    parameters = list(
      requested = list(diagnostic = "pycnocline", variable = variable, support = support),
      resolved = list(
        definition_id = "PYCNOCLINE_MAX_POSITIVE_POTENTIAL_DENSITY_GRADIENT",
        input_mode = "CERTIFIED_THERMODYNAMIC_STATE",
        c9_schema_version = state$descriptor$schema_version,
        reference_pressure_dbar = 0,
        selected_variable = required,
        depth_gradient_method = "point",
        depth_feature_polarity = "positive",
        threshold_applied = FALSE,
        candidate_count = as.integer(sum(resolved)),
        netcdf_scientific_payload_reads = 0L
      )
    ),
    output = list(
      backend = "memory", shape = c(rows = nrow(feature), columns = ncol(feature)),
      variables = required, time_kind = context$time_kind
    ),
    scientific_method = .provenance_method(
      "transition_layer", list(diagnostic = "pycnocline")
    ), context = context
  )
  old_qa$transition_layer <- list(
    diagnostic = "pycnocline", input_mode = "CERTIFIED_THERMODYNAMIC_STATE",
    selected_variable = required, profiles_total = as.integer(nrow(feature)),
    diagnostic_candidates = as.integer(sum(resolved)),
    gapped_candidates = as.integer(sum(gapped)),
    ambiguous_profiles = as.integer(sum(feature$feature_status == "AMBIGUOUS_TIE")),
    no_candidate_profiles = as.integer(sum(!resolved)),
    netcdf_scientific_payload_reads = 0L
  )
  attr(feature, "oceancube_provenance") <- provenance
  attr(feature, "oceancube_qa") <- old_qa
  feature
}
