#' Estimate temperature-threshold mixed-layer depth
#'
#' @param x A direct source-profile `<ocean_cube>` with one eligible preserved
#'   CF temperature variable and certified metric-depth point semantics.
#' @param method Mixed-layer method. C8 supports only
#'   `"temperature_threshold"`.
#' @param variable Optional exact eligible current temperature variable name.
#' @param reference_depth_m Finite non-negative reference depth in canonical
#'   physical metres, positive downward.
#' @param threshold Finite positive absolute temperature-departure threshold in
#'   K/degree-Celsius-equivalent magnitude.
#' @param support Vertical connectivity policy. `"local"` stops at the first
#'   explicit support gap; `"all"` may inspect beyond it but never localizes an
#'   exact MLD through a gapped path.
#'
#' @return A base `data.frame` with one row per longitude, latitude, and time.
#'   Resolved rows contain the first physical threshold crossing below the
#'   reference depth. Unresolved rows retain the governing reference, gap,
#'   missingness, or open-bottom status.
#'
#' @details
#' C8 implements only a point-profile temperature threshold. The reference
#' temperature is observed exactly or linearly interpolated from a locally
#' adjacent supported pair. The first deeper point whose absolute departure
#' from that reference is at least `threshold` is selected; a locally supported
#' bracket may be linearly interpolated in departure magnitude. Missing values
#' and explicit gaps are never bridged, and no crossing before the observed
#' bottom is reported as open rather than replaced by the deepest level.
#'
#' Cell-mean profiles, density thresholds, gradient and hybrid definitions,
#' pycnoclines, stratification, and pressure or TEOS-10 conversions are outside
#' the C8 runtime contract.
#'
#' @references
#' de Boyer Montegut, C. et al. (2004). Mixed layer depth over the global
#' ocean: An examination of profile data and a profile-based climatology.
#' Journal of Geophysical Research: Oceans, 109, C12003.
#' \doi{10.1029/2004JC002378}
#'
#' @examples
#' \dontrun{
#' mixed_layer_depth(cube)
#' mixed_layer_depth(cube, reference_depth_m = 10, threshold = 0.2)
#' }
#' @export
mixed_layer_depth <- function(
    x,
    method = "temperature_threshold",
    variable = NULL,
    reference_depth_m = 10,
    threshold = 0.2,
    support = c("local", "all")) {
  if (!is.character(method) || length(method) != 1L || is.na(method) ||
      !identical(method, "temperature_threshold")) {
    rlang::abort(
      paste0(
        "Unsupported mixed-layer method `", paste(method, collapse = ", "),
        "`; C8 supports only `temperature_threshold`."
      ),
      class = "oceancube_mld_method_unsupported"
    )
  }
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
  support <- match.arg(support)
  prepared <- .mld_prepare(x, variable)
  resolved <- lapply(seq_len(prepared$profile_count), function(profile) {
    .mld_profile(
      prepared, profile,
      reference_depth_m = as.numeric(reference_depth_m),
      threshold = as.numeric(threshold), support = support
    )
  })
  result <- do.call(rbind, lapply(resolved, `[[`, "row"))
  rownames(result) <- NULL

  status_table <- table(result$status)
  status_counts <- as.list(as.integer(status_table))
  names(status_counts) <- names(status_table)
  base_provenance <- .mld_private_provenance(prepared$selected$provenance)
  context <- .provenance_cube_context(
    source = prepared$selected$source,
    dataset_id = prepared$selected$dataset_id,
    time = prepared$selected$time,
    shape = stats::setNames(
      as.integer(.cube_shape(prepared$selected)), .cube_axis_names()
    ),
    variables = prepared$selected$vars,
    backend = "memory",
    provenance = base_provenance
  )
  provenance <- .provenance_append(
    base_provenance,
    operation = "mixed_layer_depth",
    parameters = list(
      requested = list(
        method = method, variable = variable,
        reference_depth_m = as.numeric(reference_depth_m),
        threshold = as.numeric(threshold), support = support
      ),
      resolved = list(
        temperature_standard_name = prepared$metadata$standard_name,
        temperature_basis = prepared$temperature_basis,
        source_temperature_unit = prepared$metadata$unit,
        temperature_interval_family = prepared$unit$family,
        reference_method = as.character(result$reference_status),
        reference_brackets = lapply(resolved, function(item) {
          .mld_provenance_evidence(item$reference_evidence)
        }),
        threshold = as.numeric(threshold),
        threshold_unit = "K_or_degree_Celsius_interval",
        search_direction = "PHYSICALLY_DEEPER_POSITIVE_DOWN",
        first_crossing_policy = "FIRST_ABSOLUTE_DEPARTURE_AT_OR_ABOVE_THRESHOLD",
        support_policy = support,
        missingness_policy = "NO_BRIDGING_BEFORE_FIRST_CROSSING",
        crossing_method = as.character(result$status),
        crossing_brackets = lapply(resolved, function(item) {
          .mld_provenance_evidence(item$crossing_evidence)
        }),
        status_counts = status_counts,
        output_rows = nrow(result),
        netcdf_scientific_payload_reads = prepared$netcdf_reads,
        variables_read = prepared$variables_read,
        levels_read = prepared$levels_read
      )
    ),
    output = list(
      backend = "memory",
      shape = c(rows = nrow(result), columns = ncol(result)),
      variables = prepared$metadata$current_variable,
      time_kind = context$time_kind
    ),
    scientific_method = .provenance_method("mixed_layer_depth", list()),
    context = context
  )

  qa <- list(
    profiles_total = prepared$profile_count,
    resolved_exact = as.integer(sum(
      result$status == "MLD_EXACT_THRESHOLD_POINT"
    )),
    resolved_interpolated = as.integer(sum(
      result$status == "MLD_INTERPOLATED_THRESHOLD_CROSSING"
    )),
    open_bottom = as.integer(sum(
      result$status == "MLD_OPEN_AT_PROFILE_BOTTOM"
    )),
    reference_failures = as.integer(sum(grepl(
      "^REFERENCE_", result$status
    ))),
    gap_failures = as.integer(sum(grepl(
      "GAP", result$status
    ))),
    missing_path_failures = as.integer(sum(
      result$status == "MLD_UNRESOLVED_INCOMPLETE_PATH"
    )),
    temperature_inversions = as.integer(sum(
      result$crossing_direction == "WARMER_WITH_DEPTH", na.rm = TRUE
    )),
    netcdf_scientific_payload_reads = prepared$netcdf_reads,
    variables_read = prepared$variables_read,
    levels_read = prepared$levels_read,
    status_counts = status_counts
  )
  attr(result, "oceancube_provenance") <- provenance
  attr(result, "oceancube_qa") <- list(mixed_layer_depth = qa)
  result
}

.mld_temperature_definition <- function() {
  list(
    family = "temperature",
    basis = c(
      sea_water_temperature = "IN_SITU_SEA_WATER_TEMPERATURE",
      sea_water_potential_temperature = "POTENTIAL_TEMPERATURE",
      sea_water_conservative_temperature = "CONSERVATIVE_TEMPERATURE"
    )
  )
}

.mld_prepare <- function(x, variable = NULL) {
  .check_cube(x)
  .transition_validate_source_profile(x)
  definition <- .mld_temperature_definition()
  metadata <- .transition_resolve_variable(x, definition, variable)
  unit <- .transition_unit_contract(
    metadata$standard_name, metadata$unit, definition$family
  )
  selected <- cube_slice(x, variable = metadata$current_variable)
  semantics <- .vertical_value_semantics(selected)[[metadata$current_variable]]
  if (!identical(semantics$status, "VERTICAL_POINT")) {
    rlang::abort(
      paste0(
        "C8 temperature MLD requires direct VERTICAL_POINT semantics; found `",
        semantics$status, "`. Cell means cannot be center-interpolated."
      ),
      class = "oceancube_mld_value_semantics_unsupported"
    )
  }
  metric <- .vertical_metric_depth(selected)
  support_plan <- .vertical_gradient_support(metric, require_cell = FALSE)
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
  physical <- order(metric$canonical_m)
  physical_pair_index <- vapply(seq_len(length(physical) - 1L), function(i) {
    pair <- sort(physical[c(i, i + 1L)])
    matched <- which(vapply(metric$source_pair_indices, function(source_pair) {
      identical(sort(as.integer(source_pair)), as.integer(pair))
    }, logical(1L)))
    if (length(matched) != 1L) {
      rlang::abort(
        "Physical depth adjacency does not map uniquely to source support.",
        class = "oceancube_mld_geometry_unsupported"
      )
    }
    matched[[1L]]
  }, integer(1L))
  read_diagnostics <- selected$qa$selection$netcdf_read %||% NULL
  list(
    selected = selected,
    metadata = metadata,
    unit = unit,
    temperature_basis = unname(definition$basis[[metadata$standard_name]]),
    metric = metric,
    support = support_plan,
    source_matrix = source_matrix,
    profile_index = profile_index,
    profile_count = as.integer(profile_count),
    physical = physical,
    depth_m = metric$canonical_m[physical],
    source_depth = metric$source_depths[physical],
    pair_relation = support_plan$relation[physical_pair_index],
    pair_gap_m = support_plan$gap_m[physical_pair_index],
    depth_tolerance_m = .vertical_geometry_tolerance(metric$canonical_m),
    netcdf_reads = as.integer(!is.null(read_diagnostics)),
    variables_read = read_diagnostics$variables %||% character(),
    levels_read = if (is.null(read_diagnostics)) 0L else {
      as.integer(unname(read_diagnostics$physical_count[["depth"]]))
    }
  )
}

.mld_profile <- function(prepared, profile, reference_depth_m, threshold, support) {
  z <- prepared$depth_m
  source_z <- prepared$source_depth
  temperature <- prepared$source_matrix[prepared$physical, profile]
  ztol <- prepared$depth_tolerance_m
  ref <- .mld_reference(
    z, source_z, temperature, prepared$pair_relation, prepared$pair_gap_m,
    reference_depth_m, ztol
  )
  if (!isTRUE(ref$resolved)) {
    row <- .mld_row(
      prepared, profile, reference_depth_m, threshold,
      reference = ref, status = ref$status,
      certification_status = "UNRESOLVED_REFERENCE"
    )
    return(list(
      row = row, reference_evidence = ref$evidence,
      crossing_evidence = list(status = ref$status)
    ))
  }

  candidate_positions <- which(z > reference_depth_m + ztol)
  previous <- list(
    depth_m = ref$depth_m, source_depth = ref$source_depth,
    temperature = ref$temperature, departure = 0,
    position = ref$anchor_position
  )
  n_path_points <- as.integer(!is.na(ref$exact_position))
  n_path_finite <- n_path_points
  first_gap <- NULL
  crossing <- NULL
  if (length(candidate_positions)) {
    for (position in candidate_positions) {
      relation_index <- if (is.na(previous$position)) {
        ref$deep_pair_position
      } else {
        min(previous$position, position)
      }
      relation <- prepared$pair_relation[[relation_index]]
      gap_m <- prepared$pair_gap_m[[relation_index]]
      current <- list(
        depth_m = z[[position]], source_depth = source_z[[position]],
        temperature = temperature[[position]], position = position,
        relation = relation, gap_m = gap_m
      )
      n_path_points <- n_path_points + 1L
      if (!is.finite(current$temperature)) {
        crossing <- list(
          status = "MLD_UNRESOLVED_INCOMPLETE_PATH",
          certification = "UNRESOLVED_MISSING_PATH",
          path = "INCOMPLETE_BEFORE_FIRST_CROSSING",
          previous = previous, current = current,
          relation = relation, gap_m = gap_m
        )
        break
      }
      n_path_finite <- n_path_finite + 1L
      current$departure <- abs(current$temperature - ref$temperature)
      tolerance <- .mld_temperature_tolerance(c(
        ref$temperature, previous$temperature, current$temperature, threshold
      ))
      is_gap <- identical(relation, "GAPPED_SUPPORT")
      if (is_gap && is.null(first_gap)) {
        first_gap <- list(
          previous = previous, current = current,
          relation = relation, gap_m = gap_m
        )
      }
      if (is_gap && identical(support, "local")) {
        crossing <- c(first_gap, list(
          status = "MLD_UNRESOLVED_BEFORE_SUPPORT_GAP",
          certification = "UNRESOLVED_LOCAL_SUPPORT_GAP",
          path = "TERMINATED_AT_SUPPORT_GAP"
        ))
        break
      }
      reaches_threshold <- current$departure >= threshold - tolerance
      if (!is.null(first_gap) && reaches_threshold) {
        crossing <- c(first_gap, list(
          status = "GAPPED_MLD_THRESHOLD_BRACKET",
          certification = "UNRESOLVED_GAPPED_THRESHOLD_PATH",
          path = "GAPPED_BEFORE_FIRST_APPARENT_CROSSING"
        ))
        break
      }
      if (reaches_threshold) {
        direction <- if (current$temperature - ref$temperature >= 0) {
          "WARMER_WITH_DEPTH"
        } else {
          "COOLER_WITH_DEPTH"
        }
        if (abs(current$departure - threshold) <= tolerance) {
          crossing <- list(
            status = "MLD_EXACT_THRESHOLD_POINT",
            certification = "CERTIFIED_EXACT_POINT_MLD",
            path = "COMPLETE_THROUGH_MLD",
            previous = previous, current = current,
            relation = relation, gap_m = gap_m,
            depth_m = current$depth_m,
            source_depth = current$source_depth,
            direction = direction
          )
          break
        }
        previous_delta <- previous$temperature - ref$temperature
        current_delta <- current$temperature - ref$temperature
        monotonic_departure <- abs(previous_delta) <= tolerance ||
          sign(previous_delta) == sign(current_delta)
        denominator <- current$departure - previous$departure
        if (!monotonic_departure || !is.finite(denominator) ||
            denominator <= tolerance) {
          crossing <- list(
            status = "MLD_UNRESOLVED_NONMONOTONIC_BRACKET",
            certification = "UNRESOLVED_NONMONOTONIC_DEPARTURE",
            path = "NONMONOTONIC_THRESHOLD_BRACKET",
            previous = previous, current = current,
            relation = relation, gap_m = gap_m
          )
          break
        }
        fraction <- (threshold - previous$departure) / denominator
        if (!is.finite(fraction) || fraction < -tolerance ||
            fraction > 1 + tolerance) {
          crossing <- list(
            status = "MLD_UNRESOLVED_INVALID_INTERPOLATION",
            certification = "UNRESOLVED_NUMERICAL_BRACKET",
            path = "INVALID_THRESHOLD_INTERPOLATION",
            previous = previous, current = current,
            relation = relation, gap_m = gap_m
          )
          break
        }
        fraction <- min(1, max(0, fraction))
        depth_m <- previous$depth_m +
          fraction * (current$depth_m - previous$depth_m)
        crossing <- list(
          status = "MLD_INTERPOLATED_THRESHOLD_CROSSING",
          certification = "CERTIFIED_INTERPOLATED_LOCAL_MLD",
          path = "COMPLETE_THROUGH_MLD",
          previous = previous, current = current,
          relation = relation, gap_m = gap_m,
          depth_m = depth_m,
          source_depth = .mld_source_depth(prepared, depth_m),
          direction = direction, fraction = fraction
        )
        break
      }
      previous <- current
    }
  }

  if (is.null(crossing)) {
    if (!is.null(first_gap)) {
      crossing <- c(first_gap, list(
        status = "MLD_UNRESOLVED_BEFORE_SUPPORT_GAP",
        certification = "UNRESOLVED_GAPPED_PATH",
        path = "GAPPED_PATH_WITHOUT_CERTIFIED_CROSSING"
      ))
    } else {
      crossing <- list(
        status = "MLD_OPEN_AT_PROFILE_BOTTOM",
        certification = "CERTIFIED_OPEN_BOTTOM",
        path = "COMPLETE_TO_PROFILE_BOTTOM",
        previous = previous, current = NULL,
        relation = NA_character_, gap_m = NA_real_
      )
    }
  }
  crossing$n_path_points <- n_path_points
  crossing$n_path_finite <- n_path_finite
  row <- .mld_row(
    prepared, profile, reference_depth_m, threshold,
    reference = ref,
    crossing = crossing,
    status = crossing$status,
    certification_status = crossing$certification
  )
  list(
    row = row,
    reference_evidence = ref$evidence,
    crossing_evidence = .mld_crossing_evidence(crossing)
  )
}

.mld_reference <- function(
    z, source_z, temperature, relation, gap_m, reference_depth_m, tolerance) {
  outside <- reference_depth_m < min(z) - tolerance ||
    reference_depth_m > max(z) + tolerance
  if (outside) {
    return(.mld_reference_failure(
      "REFERENCE_DEPTH_OUTSIDE_PROFILE",
      list(profile_depth_range_m = range(z))
    ))
  }
  exact <- which(abs(z - reference_depth_m) <= tolerance)
  if (length(exact) == 1L) {
    position <- exact[[1L]]
    if (!is.finite(temperature[[position]])) {
      return(.mld_reference_failure(
        "REFERENCE_TEMPERATURE_UNRESOLVED",
        list(method = "EXACT_POINT", position = position, depth_m = z[[position]])
      ))
    }
    return(list(
      resolved = TRUE, status = "REFERENCE_EXACT_POINT",
      depth_m = z[[position]], source_depth = source_z[[position]],
      temperature = temperature[[position]], exact_position = position,
      anchor_position = position, deep_pair_position = NA_integer_,
      evidence = list(
        method = "EXACT_POINT", depth_m = z[[position]],
        source_depth = source_z[[position]], temperature = temperature[[position]],
        support_relation = "EXACT_POINT", support_gap_m = 0
      )
    ))
  }
  shallow <- max(which(z < reference_depth_m))
  deep <- min(which(z > reference_depth_m))
  pair <- shallow
  evidence <- list(
    method = "LINEAR_INTERPOLATION",
    shallow_depth_m = z[[shallow]], deep_depth_m = z[[deep]],
    shallow_source_depth = source_z[[shallow]],
    deep_source_depth = source_z[[deep]],
    shallow_temperature = temperature[[shallow]],
    deep_temperature = temperature[[deep]],
    support_relation = relation[[pair]], support_gap_m = gap_m[[pair]]
  )
  if (identical(relation[[pair]], "GAPPED_SUPPORT")) {
    return(.mld_reference_failure("REFERENCE_GAPPED_BRACKET", evidence))
  }
  if (!is.finite(temperature[[shallow]]) || !is.finite(temperature[[deep]])) {
    return(.mld_reference_failure(
      "REFERENCE_TEMPERATURE_UNRESOLVED", evidence
    ))
  }
  fraction <- (reference_depth_m - z[[shallow]]) /
    (z[[deep]] - z[[shallow]])
  reference_temperature <- temperature[[shallow]] +
    fraction * (temperature[[deep]] - temperature[[shallow]])
  list(
    resolved = TRUE, status = "REFERENCE_INTERPOLATED_POINT",
    depth_m = reference_depth_m,
    source_depth = source_z[[shallow]] + fraction *
      (source_z[[deep]] - source_z[[shallow]]),
    temperature = reference_temperature,
    exact_position = NA_integer_, anchor_position = NA_integer_,
    deep_pair_position = pair,
    evidence = c(evidence, list(
      fraction = fraction, resolved_temperature = reference_temperature
    ))
  )
}

.mld_reference_failure <- function(status, evidence) {
  list(
    resolved = FALSE, status = status,
    depth_m = NA_real_, source_depth = NA_real_, temperature = NA_real_,
    exact_position = NA_integer_, anchor_position = NA_integer_,
    deep_pair_position = NA_integer_, evidence = evidence
  )
}

.mld_temperature_tolerance <- function(values) {
  finite <- values[is.finite(values)]
  8 * sqrt(.Machine$double.eps) * max(c(1, abs(finite)))
}

.mld_source_depth <- function(prepared, depth_m) {
  vertical <- prepared$metric$vertical
  scale <- vertical$scale_to_m
  if (!is.numeric(scale) || length(scale) != 1L || !is.finite(scale)) {
    scale <- .depth_conversion_factor(vertical$normalized_unit, "m")
  }
  sign <- if (identical(vertical$positive, "down")) 1 else -1
  depth_m / (scale * sign)
}

.mld_row <- function(
    prepared, profile, reference_depth_m, threshold, reference,
    crossing = NULL, status, certification_status) {
  at <- prepared$profile_index[profile, ]
  previous <- crossing$previous %||% NULL
  current <- crossing$current %||% NULL
  resolved_depth_m <- crossing$depth_m %||% NA_real_
  resolved_source_depth <- crossing$source_depth %||% NA_real_
  data.frame(
    longitude = prepared$selected$lon[at[["longitude"]]],
    latitude = prepared$selected$lat[at[["latitude"]]],
    time = prepared$selected$time[at[["time"]]],
    variable = prepared$metadata$current_variable,
    temperature_standard_name = prepared$metadata$standard_name,
    temperature_basis = prepared$temperature_basis,
    temperature_unit = prepared$metadata$unit,
    method = "temperature_threshold",
    reference_depth_requested_m = reference_depth_m,
    reference_depth_resolved_m = reference$depth_m,
    reference_temperature = reference$temperature,
    reference_status = reference$status,
    threshold = threshold,
    threshold_unit = "K_or_degree_Celsius_interval",
    mld_depth = resolved_source_depth,
    depth_unit = prepared$metric$source_unit,
    mld_depth_m = resolved_depth_m,
    crossing_direction = crossing$direction %||% NA_character_,
    bracket_shallow_depth = previous$source_depth %||% NA_real_,
    bracket_deep_depth = current$source_depth %||% NA_real_,
    bracket_shallow_depth_m = previous$depth_m %||% NA_real_,
    bracket_deep_depth_m = current$depth_m %||% NA_real_,
    bracket_shallow_temperature = previous$temperature %||% NA_real_,
    bracket_deep_temperature = current$temperature %||% NA_real_,
    support_relation = crossing$relation %||% NA_character_,
    support_gap_m = crossing$gap_m %||% NA_real_,
    n_path_points = crossing$n_path_points %||% 0L,
    n_path_finite = crossing$n_path_finite %||% 0L,
    path_completeness = crossing$path %||% "REFERENCE_UNRESOLVED",
    status = status,
    certification_status = certification_status,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

.mld_crossing_evidence <- function(crossing) {
  list(
    status = crossing$status,
    shallow_depth_m = crossing$previous$depth_m %||% NA_real_,
    deep_depth_m = crossing$current$depth_m %||% NA_real_,
    shallow_temperature = crossing$previous$temperature %||% NA_real_,
    deep_temperature = crossing$current$temperature %||% NA_real_,
    support_relation = crossing$relation %||% NA_character_,
    support_gap_m = crossing$gap_m %||% NA_real_,
    interpolation_fraction = crossing$fraction %||% NA_real_
  )
}

.mld_private_provenance <- function(provenance) {
  out <- provenance
  locator <- out$source$locator %||% NULL
  if (is.list(locator) && !isTRUE(locator$portable)) {
    base <- locator$basename %||% basename(locator$value)
    out$source$locator$value <- base
    out$source$locator$basename <- base
  }
  out
}

.mld_provenance_evidence <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.numeric(x)) {
    if (any(!is.finite(x))) return(NULL)
    return(x)
  }
  if (!is.list(x) || is.object(x)) return(x)
  out <- lapply(x, .mld_provenance_evidence)
  out[!vapply(out, is.null, logical(1L))]
}
