#' Resolve an explicit oxygen-threshold zone and its observed boundaries
#'
#' @param x A direct source-profile `<ocean_cube>` with one eligible preserved
#'   CF oxygen concentration variable.
#' @param threshold Required finite non-negative numeric scalar. With
#'   `threshold_unit = NULL` it is interpreted in the exact source unit.
#' @param threshold_unit Optional bounded oxygen unit from the same physical
#'   quantity family as the source variable.
#' @param variable Optional exact eligible current oxygen variable name.
#' @param support Vertical connectivity policy. `"local"` breaks connectivity
#'   at explicit source-support gaps; `"all"` may retain a gapped component but
#'   never fabricates an exact crossing across the gap.
#'
#' @return A base data frame with one row per longitude, latitude, and time.
#'   It describes the threshold-qualified component containing the resolved
#'   global oxygen minimum/core and its upper and lower boundary evidence.
#'
#' @details
#' No oxygen-deficient-zone threshold is built in. Point observations may be
#' linearly localized only inside a valid contiguous threshold bracket. Cell
#' means and gapped brackets return bracket-only evidence. Open profile edges
#' are not extrapolated, and exact thickness is reported only when both point
#' boundaries are exact or locally interpolated.
#'
#' @examples
#' \dontrun{
#' oxygen_boundary(cube, threshold = 20, threshold_unit = "umol kg-1")
#' }
#' @export
oxygen_boundary <- function(
    x,
    threshold,
    threshold_unit = NULL,
    variable = NULL,
    support = c("local", "all")) {
  if (missing(threshold) || !is.numeric(threshold) || length(threshold) != 1L ||
      is.na(threshold) || !is.finite(threshold) || threshold < 0) {
    rlang::abort(
      "`threshold` must be one finite non-negative numeric value.",
      class = "oceancube_oxygen_threshold"
    )
  }
  if (!is.null(threshold_unit) && (
    !is.character(threshold_unit) || length(threshold_unit) != 1L ||
      is.na(threshold_unit) || !nzchar(threshold_unit))) {
    rlang::abort(
      "`threshold_unit` must be NULL or one non-empty oxygen unit.",
      class = "oceancube_oxygen_threshold_unit"
    )
  }
  support <- match.arg(support)
  prepared <- .oxygen_prepare(x, variable)
  requested_unit <- if (is.null(threshold_unit)) {
    prepared$unit
  } else {
    .oxygen_unit_any(threshold_unit)
  }
  if (!identical(requested_unit$family, prepared$unit$family)) {
    rlang::abort(
      paste0(
        "Threshold unit family ", requested_unit$family,
        " cannot be converted to source family ", prepared$unit$family,
        " without forbidden cross-basis assumptions."
      ),
      class = "oceancube_oxygen_unit_cross_basis"
    )
  }
  threshold_canonical <- as.numeric(threshold) * requested_unit$scale_to_canonical
  results <- lapply(seq_len(prepared$profile_count), function(profile) {
    .oxygen_boundary_profile(
      prepared, profile, threshold = as.numeric(threshold),
      threshold_canonical = threshold_canonical,
      threshold_requested_unit = if (is.null(threshold_unit)) {
        prepared$metadata$unit
      } else {
        threshold_unit
      },
      threshold_interpretation = if (is.null(threshold_unit)) {
        "SOURCE_UNIT"
      } else {
        "EXPLICIT_SAME_FAMILY_UNIT"
      },
      support = support
    )
  })
  result <- do.call(rbind, lapply(results, `[[`, "row"))
  rownames(result) <- NULL
  status_counts <- as.list(as.integer(table(result$zone_status)))
  names(status_counts) <- names(table(result$zone_status))
  base_provenance <- .oxygen_private_provenance(prepared$gradient$provenance)
  context <- .provenance_cube_context(
    source = prepared$gradient$source,
    dataset_id = prepared$gradient$dataset_id,
    time = prepared$gradient$time,
    shape = stats::setNames(
      as.integer(.cube_shape(prepared$gradient)), .cube_axis_names()
    ),
    variables = prepared$gradient$vars,
    backend = "memory", provenance = base_provenance
  )
  provenance <- .provenance_append(
    base_provenance,
    operation = "oxygen_boundary",
    parameters = list(
      requested = list(
        threshold = as.numeric(threshold), threshold_unit = threshold_unit,
        variable = variable, support = support
      ),
      resolved = list(
        oxygen_standard_name = prepared$metadata$standard_name,
        oxygen_source_unit = prepared$metadata$unit,
        oxygen_quantity_family = prepared$unit$family,
        oxygen_canonical_unit = prepared$unit$canonical_unit,
        threshold_requested = as.numeric(threshold),
        threshold_requested_unit = if (is.null(threshold_unit)) {
          prepared$metadata$unit
        } else threshold_unit,
        threshold_canonical = threshold_canonical,
        support_policy = support,
        core_types = as.list(table(result$core_type)),
        oxygen_core = lapply(prepared$cores, function(core) list(
          type = core$type,
          member_indices = core$members,
          value_source_unit = core$source_minimum,
          value_canonical = core$canonical_minimum,
          representative_depth_m = core$depth_m,
          shallow_depth_m = core$shallow_depth_m,
          deep_depth_m = core$deep_depth_m
        )),
        component_indices = lapply(results, `[[`, "component"),
        upper_boundary_methods = as.list(table(result$upper_boundary_status)),
        upper_boundary_evidence = lapply(seq_len(nrow(result)), function(i) list(
          status = result$upper_boundary_status[[i]],
          bracket_depths_m = c(
            shallow = result$upper_bracket_shallow_depth_m[[i]],
            deep = result$upper_bracket_deep_depth_m[[i]]
          ),
          support_relation = result$upper_support_relation[[i]],
          support_gap_m = result$upper_support_gap_m[[i]]
        )),
        lower_boundary_methods = as.list(table(result$lower_boundary_status)),
        lower_boundary_evidence = lapply(seq_len(nrow(result)), function(i) list(
          status = result$lower_boundary_status[[i]],
          bracket_depths_m = c(
            shallow = result$lower_bracket_shallow_depth_m[[i]],
            deep = result$lower_bracket_deep_depth_m[[i]]
          ),
          support_relation = result$lower_support_relation[[i]],
          support_gap_m = result$lower_support_gap_m[[i]]
        )),
        interpolation_profiles = which(
          result$upper_boundary_status == "INTERPOLATED_THRESHOLD_CROSSING" |
            result$lower_boundary_status == "INTERPOLATED_THRESHOLD_CROSSING"
        ),
        gapped_profiles = which(grepl(
          "GAPPED", paste(result$upper_boundary_status,
                           result$lower_boundary_status)
        )),
        exact_thickness_profiles = which(is.finite(result$zone_observed_thickness_m)),
        zone_status_counts = status_counts,
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
    scientific_method = .provenance_method("oxygen_boundary", list()),
    context = context
  )
  exact_status <- c("EXACT_THRESHOLD_POINT", "INTERPOLATED_THRESHOLD_CROSSING")
  qa <- list(
    profiles_total = prepared$profile_count,
    profiles_complete = as.integer(sum(result$oxygen_profile_completeness == 1)),
    profiles_incomplete = as.integer(sum(result$oxygen_profile_completeness < 1)),
    unique_minima = as.integer(sum(result$core_type == "UNIQUE_MINIMUM")),
    plateau_cores = as.integer(sum(
      result$core_type == "CONTIGUOUS_MINIMUM_PLATEAU"
    )),
    ambiguous_disjoint_minima = as.integer(sum(
      result$core_type == "AMBIGUOUS_DISJOINT_MINIMA"
    )),
    flat_profiles = as.integer(sum(result$core_type == "FLAT_OXYGEN_PROFILE")),
    threshold_zones_present = as.integer(sum(result$zone_status %in% c(
      "THRESHOLD_ZONE_PRESENT", "THRESHOLD_ZONE_SPANS_PROFILE"
    ))),
    threshold_zones_absent = as.integer(sum(
      result$zone_status == "THRESHOLD_ZONE_ABSENT"
    )),
    exact_point_boundaries = as.integer(sum(
      result$upper_boundary_status == "EXACT_THRESHOLD_POINT" |
        result$lower_boundary_status == "EXACT_THRESHOLD_POINT"
    )),
    interpolated_point_boundaries = as.integer(sum(
      result$upper_boundary_status == "INTERPOLATED_THRESHOLD_CROSSING" |
        result$lower_boundary_status == "INTERPOLATED_THRESHOLD_CROSSING"
    )),
    cell_mean_brackets = as.integer(sum(grepl(
      "CELL_MEAN_THRESHOLD_BRACKET_ONLY",
      paste(result$upper_boundary_status, result$lower_boundary_status)
    ))),
    gapped_brackets = as.integer(sum(grepl(
      "GAPPED", paste(result$upper_boundary_status, result$lower_boundary_status)
    ))),
    open_profile_edges = as.integer(sum(grepl(
      "OPEN_AT_PROFILE_EDGE",
      paste(result$upper_boundary_status, result$lower_boundary_status)
    ))),
    exact_thickness_profiles = as.integer(sum(
      result$upper_boundary_status %in% exact_status &
        result$lower_boundary_status %in% exact_status
    )),
    netcdf_scientific_payload_reads = prepared$netcdf_reads,
    variables_read = prepared$variables_read,
    levels_read = prepared$levels_read,
    zone_status_counts = status_counts
  )
  attr(result, "oceancube_provenance") <- provenance
  attr(result, "oceancube_qa") <- list(oxygen_boundary = qa)
  result
}

.oxygen_boundary_profile <- function(
    prepared, profile, threshold, threshold_canonical,
    threshold_requested_unit, threshold_interpretation, support) {
  core <- prepared$cores[[profile]]
  at <- prepared$profile_index[profile, ]
  base <- list(
    longitude = prepared$selected$lon[at[["longitude"]]],
    latitude = prepared$selected$lat[at[["latitude"]]],
    time = prepared$selected$time[at[["time"]]],
    variable = prepared$metadata$current_variable,
    oxygen_standard_name = prepared$metadata$standard_name,
    oxygen_quantity_family = prepared$unit$family,
    oxygen_value_semantics = if (identical(
      prepared$descriptor_item$input_value_semantics, "VERTICAL_POINT"
    )) "VERTICAL_POINT" else "VERTICAL_CELL_MEAN",
    core_value_semantics = if (identical(
      prepared$descriptor_item$input_value_semantics, "VERTICAL_POINT"
    )) "POINT_VALUE_MINIMUM" else "CELL_MEAN_MINIMUM",
    source_unit = prepared$metadata$unit,
    canonical_unit = prepared$unit$canonical_unit,
    threshold_requested = threshold,
    threshold_requested_unit = threshold_requested_unit,
    threshold_unit_interpretation = threshold_interpretation,
    threshold_canonical = threshold_canonical,
    threshold_canonical_unit = prepared$unit$canonical_unit,
    support_policy = support,
    oxygen_profile_completeness = core$completeness,
    core_type = core$type,
    core_value = core$source_minimum,
    core_value_canonical = core$canonical_minimum,
    core_depth_m = core$depth_m,
    core_shallow_depth_m = core$shallow_depth_m,
    core_deep_depth_m = core$deep_depth_m
  )
  empty_boundary <- function(status = "NO_BOUNDARY") list(
    depth = NA_real_, depth_m = NA_real_, status = status,
    shallow_m = NA_real_, deep_m = NA_real_, relation = NA_character_,
    gap_m = NA_real_, shallow_bounds = c(NA_real_, NA_real_),
    deep_bounds = c(NA_real_, NA_real_)
  )
  invalid <- c(
    INCOMPLETE_OXYGEN_PROFILE = "INCOMPLETE_OXYGEN_PROFILE",
    AMBIGUOUS_DISJOINT_MINIMA = "AMBIGUOUS_DISJOINT_MINIMA",
    FLAT_OXYGEN_PROFILE = "FLAT_OXYGEN_PROFILE"
  )
  if (core$type %in% names(invalid)) {
    upper <- lower <- empty_boundary(invalid[[core$type]])
    return(.oxygen_boundary_row(
      base, invalid[[core$type]], upper, lower, NA_real_, NA_real_,
      NA_real_, "NO_CERTIFIED_THRESHOLD_ZONE", integer()
    ))
  }
  values <- prepared$canonical_matrix[, profile]
  tolerance <- .oxygen_tolerance(c(values, threshold_canonical))
  if (threshold_canonical < core$canonical_minimum - tolerance) {
    upper <- lower <- empty_boundary("THRESHOLD_ZONE_ABSENT")
    return(.oxygen_boundary_row(
      base, "THRESHOLD_ZONE_ABSENT", upper, lower, NA_real_, NA_real_,
      NA_real_, "CERTIFIED_THRESHOLD_ZONE_ABSENT", integer()
    ))
  }
  low <- values <= threshold_canonical + tolerance
  physical <- order(prepared$depth_m)
  core_positions <- sort(match(core$members, physical))
  edge_allowed <- function(left_position) {
    pair <- sort(physical[c(left_position, left_position + 1L)])
    pair_index <- min(pair)
    relation <- prepared$descriptor$support_relation[[pair_index]]
    identical(support, "all") || !identical(relation, "GAPPED_SUPPORT")
  }
  if (length(core_positions) > 1L && any(!vapply(
    core_positions[-length(core_positions)], edge_allowed, logical(1L)
  ))) {
    upper <- lower <- empty_boundary("CORE_SPLIT_BY_SUPPORT_GAP")
    return(.oxygen_boundary_row(
      base, "CORE_SPLIT_BY_SUPPORT_GAP", upper, lower, NA_real_, NA_real_,
      NA_real_, "NO_CERTIFIED_THRESHOLD_ZONE", integer()
    ))
  }
  start <- min(core_positions)
  end <- max(core_positions)
  while (start > 1L && low[physical[start - 1L]] && edge_allowed(start - 1L)) {
    start <- start - 1L
  }
  while (end < length(physical) && low[physical[end + 1L]] && edge_allowed(end)) {
    end <- end + 1L
  }
  component <- as.integer(physical[seq.int(start, end)])
  upper <- .oxygen_threshold_boundary(
    prepared, profile, component_position = start, side = "UPPER",
    threshold_canonical = threshold_canonical, tolerance = tolerance,
    support = support, physical = physical, low = low
  )
  lower <- .oxygen_threshold_boundary(
    prepared, profile, component_position = end, side = "LOWER",
    threshold_canonical = threshold_canonical, tolerance = tolerance,
    support = support, physical = physical, low = low
  )
  zone_status <- if (start == 1L && end == length(physical)) {
    "THRESHOLD_ZONE_SPANS_PROFILE"
  } else {
    "THRESHOLD_ZONE_PRESENT"
  }
  exact <- c("EXACT_THRESHOLD_POINT", "INTERPOLATED_THRESHOLD_CROSSING")
  thickness <- if (upper$status %in% exact && lower$status %in% exact) {
    lower$depth_m - upper$depth_m
  } else {
    NA_real_
  }
  certification <- if (is.finite(thickness)) {
    "CERTIFIED_EXACT_POINT_THRESHOLD_ZONE"
  } else if (identical(zone_status, "THRESHOLD_ZONE_SPANS_PROFILE")) {
    "OBSERVED_THRESHOLD_ZONE_OPEN_EDGES"
  } else {
    "OBSERVED_THRESHOLD_ZONE_BOUNDARIES_INEXACT"
  }
  .oxygen_boundary_row(
    base, zone_status, upper, lower,
    min(prepared$depth_m[component]), max(prepared$depth_m[component]),
    thickness, certification, component
  )
}

.oxygen_threshold_boundary <- function(
    prepared, profile, component_position, side, threshold_canonical,
    tolerance, support, physical, low) {
  upper <- identical(side, "UPPER")
  edge <- if (upper) component_position == 1L else
    component_position == length(physical)
  if (edge) {
    return(list(
      depth = NA_real_, depth_m = NA_real_,
      status = paste0(side, "_BOUNDARY_OPEN_AT_PROFILE_EDGE"),
      shallow_m = NA_real_, deep_m = NA_real_, relation = NA_character_,
      gap_m = NA_real_, shallow_bounds = c(NA_real_, NA_real_),
      deep_bounds = c(NA_real_, NA_real_)
    ))
  }
  positions <- if (upper) {
    c(component_position - 1L, component_position)
  } else {
    c(component_position, component_position + 1L)
  }
  indices <- physical[positions]
  shallow_index <- indices[[which.min(prepared$depth_m[indices])]]
  deep_index <- indices[[which.max(prepared$depth_m[indices])]]
  pair_index <- min(indices)
  relation <- prepared$descriptor$support_relation[[pair_index]]
  gap <- prepared$descriptor$support_gap_m[[pair_index]]
  bounds <- prepared$plan$support$bounds_m
  shallow_bounds <- if (length(bounds)) sort(bounds[[shallow_index]]) else
    c(NA_real_, NA_real_)
  deep_bounds <- if (length(bounds)) sort(bounds[[deep_index]]) else
    c(NA_real_, NA_real_)
  result <- list(
    depth = NA_real_, depth_m = NA_real_, status = NA_character_,
    shallow_m = prepared$depth_m[[shallow_index]],
    deep_m = prepared$depth_m[[deep_index]], relation = relation,
    gap_m = gap, shallow_bounds = shallow_bounds, deep_bounds = deep_bounds
  )
  if (identical(relation, "GAPPED_SUPPORT")) {
    result$status <- if (identical(support, "local")) {
      "LOCAL_SUPPORT_GAP_COMPONENT_EDGE"
    } else if (identical(
      prepared$descriptor_item$input_value_semantics, "VERTICAL_CELL_MEAN"
    )) {
      "GAPPED_CELL_MEAN_THRESHOLD_BRACKET_ONLY"
    } else {
      "GAPPED_THRESHOLD_BRACKET_ONLY"
    }
    return(result)
  }
  if (identical(
    prepared$descriptor_item$input_value_semantics, "VERTICAL_CELL_MEAN"
  )) {
    result$status <- "CELL_MEAN_THRESHOLD_BRACKET_ONLY"
    return(result)
  }
  inside_position <- component_position
  inside_index <- physical[[inside_position]]
  inside_value <- prepared$canonical_matrix[[inside_index, profile]]
  if (abs(inside_value - threshold_canonical) <= tolerance) {
    result$depth_m <- prepared$depth_m[[inside_index]]
    result$depth <- .oxygen_depth_from_m(prepared, result$depth_m)
    result$status <- "EXACT_THRESHOLD_POINT"
    return(result)
  }
  first <- indices[[1L]]
  second <- indices[[2L]]
  oxygen_1 <- prepared$canonical_matrix[[first, profile]]
  oxygen_2 <- prepared$canonical_matrix[[second, profile]]
  depth_1 <- prepared$depth_m[[first]]
  depth_2 <- prepared$depth_m[[second]]
  if (!is.finite(oxygen_1) || !is.finite(oxygen_2) ||
      abs(oxygen_2 - oxygen_1) <= tolerance || identical(low[[first]], low[[second]])) {
    result$status <- "NO_LOCAL_THRESHOLD_BRACKET"
    return(result)
  }
  result$depth_m <- depth_1 +
    (threshold_canonical - oxygen_1) / (oxygen_2 - oxygen_1) *
    (depth_2 - depth_1)
  result$depth <- .oxygen_depth_from_m(prepared, result$depth_m)
  result$status <- "INTERPOLATED_THRESHOLD_CROSSING"
  result
}

.oxygen_depth_from_m <- function(prepared, depth_m) {
  vertical <- prepared$selected$metadata$cf$current$vertical
  sign <- if (identical(vertical$positive, "down")) 1 else -1
  depth_m / (vertical$scale_to_m * sign)
}

.oxygen_boundary_row <- function(
    base, zone_status, upper, lower, observed_top, observed_bottom,
    thickness, certification, component) {
  row <- c(base, list(
    zone_status = zone_status,
    upper_boundary_depth = upper$depth,
    upper_boundary_depth_m = upper$depth_m,
    upper_boundary_status = upper$status,
    upper_bracket_shallow_depth_m = upper$shallow_m,
    upper_bracket_deep_depth_m = upper$deep_m,
    upper_support_relation = upper$relation,
    upper_support_gap_m = upper$gap_m,
    upper_shallow_cell_top_m = upper$shallow_bounds[[1L]],
    upper_shallow_cell_bottom_m = upper$shallow_bounds[[2L]],
    upper_deep_cell_top_m = upper$deep_bounds[[1L]],
    upper_deep_cell_bottom_m = upper$deep_bounds[[2L]],
    lower_boundary_depth = lower$depth,
    lower_boundary_depth_m = lower$depth_m,
    lower_boundary_status = lower$status,
    lower_bracket_shallow_depth_m = lower$shallow_m,
    lower_bracket_deep_depth_m = lower$deep_m,
    lower_support_relation = lower$relation,
    lower_support_gap_m = lower$gap_m,
    lower_shallow_cell_top_m = lower$shallow_bounds[[1L]],
    lower_shallow_cell_bottom_m = lower$shallow_bounds[[2L]],
    lower_deep_cell_top_m = lower$deep_bounds[[1L]],
    lower_deep_cell_bottom_m = lower$deep_bounds[[2L]],
    zone_observed_top_m = observed_top,
    zone_observed_bottom_m = observed_bottom,
    zone_observed_thickness_m = thickness,
    certification_status = certification
  ))
  list(row = as.data.frame(row, check.names = FALSE, stringsAsFactors = FALSE),
       component = as.integer(component))
}
