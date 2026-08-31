#' Detect the strongest certified vertical-gradient candidate
#'
#' @param x A memory-backed `<ocean_cube>` produced by [depth_gradient()], or a
#'   supported selection of one, with a current certified C4 vertical-gradient
#'   descriptor.
#' @param polarity Candidate direction. `"absolute"` ranks `abs(gradient)`,
#'   `"positive"` ranks finite gradients above the feature tolerance, and
#'   `"negative"` ranks finite gradients below the negative tolerance.
#' @param support Support policy. `"local"` excludes explicitly gapped
#'   secants; `"all"` also ranks them but labels a winning gapped secant as a
#'   gapped candidate rather than a locally resolved transition.
#'
#' @return A base `data.frame` with one row per longitude, latitude, time, and
#'   variable, in that array-compatible order (longitude fastest). Resolved
#'   candidates retain their signed gradient, magnitude, C4 midpoint, source
#'   pair, spacing, support relation, support gap, and localization half-span.
#'
#' @details
#' This function never calculates a gradient implicitly. It consumes the
#' scientific payload and current descriptor of a certified C4 gradient cube.
#' Tied strongest scores remain ambiguous, flat absolute profiles receive no
#' artificial feature, and incomplete profiles may return only an observed
#' candidate, not a guaranteed global maximum. `localization_half_span_m` is a
#' vertical resolution scale (`spacing_m / 2`), not statistical uncertainty.
#'
#' A candidate is generic: it is not automatically a thermocline, oxycline,
#' halocline, mixed-layer depth, pycnocline, or other physics-aware diagnostic.
#' No smoothing, threshold, prominence, feature width, interpolation, or second
#' derivative is applied. On an ordinary C4 result the operation reads no
#' NetCDF scientific payload.
#'
#' @examples
#' \dontrun{
#' # Offline synthetic examples; `cube` must have certified vertical metadata.
#' gradient <- depth_gradient(cube)
#' depth_feature(gradient, polarity = "absolute")
#' depth_feature(gradient, polarity = "negative")
#'
#' # Explicitly gapped cell secants are excluded by the default local policy.
#' depth_feature(gradient, support = "local")
#' }
#' @export
depth_feature <- function(
    x,
    polarity = c("absolute", "positive", "negative"),
    support = c("local", "all")) {
  polarity <- match.arg(polarity)
  support <- match.arg(support)
  plan <- .vertical_feature_plan(x, polarity, support)
  d <- unname(.cube_shape(x))
  profile_dimensions <- as.integer(d[c(1L, 2L, 4L, 5L)])
  profile_count <- .cube_product_as_double(
    profile_dimensions, "depth_feature output rows"
  )
  if (profile_count > .Machine$integer.max) {
    rlang::abort(
      paste0(
        "depth_feature would produce ",
        base::format(profile_count, scientific = FALSE),
        " rows, exceeding the data-frame row limit. Select fewer profiles."
      ),
      class = "oceancube_vertical_feature_size"
    )
  }
  profile_count <- as.integer(profile_count)

  profile_index <- arrayInd(
    seq_len(profile_count), .dim = profile_dimensions
  )
  colnames(profile_index) <- c("longitude", "latitude", "time", "variable")
  gradient_values <- .cube_read(x)
  gradient_matrix <- matrix(
    aperm(gradient_values, c(3L, 1L, 2L, 4L, 5L)),
    nrow = d[[3L]], ncol = profile_count
  )
  resolved <- .vertical_feature_reduce(gradient_matrix, plan)

  variable_units <- vapply(
    plan$descriptor$variables, `[[`, character(1L), "source_unit"
  )
  gradient_units <- vapply(
    plan$descriptor$variables, `[[`, character(1L), "output_unit"
  )
  result <- data.frame(
    longitude = x$lon[profile_index[, "longitude"]],
    latitude = x$lat[profile_index[, "latitude"]],
    time = x$time[profile_index[, "time"]],
    variable = x$vars[profile_index[, "variable"]],
    variable_unit = variable_units[profile_index[, "variable"]],
    polarity = rep(polarity, profile_count),
    support_policy = rep(support, profile_count),
    feature_depth = resolved$feature_depth,
    depth_unit = rep(plan$depth_unit, profile_count),
    feature_depth_m = resolved$feature_depth_m,
    source_depth_1 = resolved$source_depth_1,
    source_depth_2 = resolved$source_depth_2,
    source_depth_1_m = resolved$source_depth_1_m,
    source_depth_2_m = resolved$source_depth_2_m,
    gradient = resolved$gradient,
    gradient_magnitude = abs(resolved$gradient),
    gradient_unit = gradient_units[profile_index[, "variable"]],
    gradient_index = resolved$gradient_index,
    spacing_m = resolved$spacing_m,
    support_relation = resolved$support_relation,
    support_gap_m = resolved$support_gap_m,
    localization_half_span_m = resolved$localization_half_span_m,
    n_support_eligible = resolved$n_support_eligible,
    n_finite_gradient = resolved$n_finite_gradient,
    gradient_completeness = resolved$gradient_completeness,
    n_matching_candidates = resolved$n_matching_candidates,
    n_tied = resolved$n_tied,
    feature_tolerance = resolved$feature_tolerance,
    status = resolved$status,
    certification_status = resolved$certification_status,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  status_counts <- as.list(as.integer(table(factor(
    result$status,
    levels = .vertical_feature_status_levels()
  ))))
  names(status_counts) <- .vertical_feature_status_levels()
  qa <- list(
    profiles_total = profile_count,
    features_resolved = as.integer(sum(!is.na(result$gradient_index))),
    ambiguous_ties = as.integer(sum(result$status == "AMBIGUOUS_TIE")),
    flat_profiles = as.integer(sum(result$status == "FLAT_PROFILE")),
    no_local_support = as.integer(sum(result$status == "NO_LOCAL_SUPPORT")),
    no_finite_gradient = as.integer(sum(result$status == "NO_FINITE_GRADIENT")),
    incomplete_profiles = as.integer(sum(
      is.finite(result$gradient_completeness) & result$gradient_completeness < 1
    )),
    gapped_candidates = as.integer(sum(
      result$support_relation == "GAPPED_SUPPORT", na.rm = TRUE
    )),
    netcdf_scientific_payload_reads = 0L,
    memory_cube_reads = 1L,
    input_gradient_bytes = as.numeric(utils::object.size(gradient_values)),
    output_table_bytes = as.numeric(utils::object.size(result)),
    large_output = profile_count >= 1e6,
    status_counts = status_counts
  )
  input_shape <- stats::setNames(as.integer(d), .cube_axis_names())
  context <- .provenance_cube_context(
    source = x$source, dataset_id = x$dataset_id, time = x$time,
    shape = input_shape, variables = x$vars, backend = "memory",
    provenance = x$provenance
  )
  table_shape <- c(rows = profile_count, columns = as.integer(ncol(result)))
  provenance <- .provenance_append(
    x$provenance,
    operation = "depth_feature",
    parameters = list(
      requested = list(polarity = polarity, support = support),
      resolved = list(
        input_gradient_schema = plan$descriptor$schema_name,
        input_gradient_schema_version = plan$descriptor$schema_version,
        eligible_support_relations = plan$eligible_relations,
        feature_tolerance_rule = paste0(
          "8*sqrt(.Machine$double.eps)*max(1,abs(finite eligible gradients))"
        ),
        profile_dimensions = stats::setNames(
          profile_dimensions, c("longitude", "latitude", "time", "variable")
        ),
        candidate_ranking_rule = .vertical_feature_ranking_rule(polarity),
        tie_policy = "equal within feature tolerance remains AMBIGUOUS_TIE",
        missing_gradient_policy = paste0(
          "rank finite eligible gradients; incomplete profiles cannot claim ",
          "a guaranteed global maximum"
        ),
        completeness_rule = "n_finite_gradient / n_support_eligible",
        selected_gradient_index = .provenance_compact(result$gradient_index),
        selected_feature_depth = .provenance_compact(result$feature_depth),
        selected_gradient = .provenance_compact(result$gradient),
        selected_support_relation =
          .provenance_compact(result$support_relation),
        selected_spacing_m = .provenance_compact(result$spacing_m),
        selected_support_gap_m = .provenance_compact(result$support_gap_m),
        status_counts = status_counts,
        output_row_count = profile_count,
        netcdf_scientific_payload_reads = 0L,
        memory_cube_reads = 1L
      )
    ),
    output = list(
      backend = "memory",
      shape = stats::setNames(as.integer(table_shape), names(table_shape)),
      variables = x$vars,
      time_kind = context$time_kind
    ),
    scientific_method = .provenance_method(
      "depth_feature", list(polarity = polarity, support = support)
    ),
    context = context
  )
  attr(result, "oceancube_qa") <- list(vertical_feature = qa)
  attr(result, "oceancube_provenance") <- provenance
  result
}

.vertical_feature_plan <- function(x, polarity, support) {
  .check_cube(x)
  if (!identical(.cube_backend(x), "memory")) {
    rlang::abort(
      "depth_feature requires a memory-backed certified C4 gradient cube.",
      class = "oceancube_vertical_feature_input"
    )
  }
  current <- x$metadata$cf$current %||% NULL
  descriptor <- current$vertical_gradient %||% NULL
  vertical <- current$vertical %||% NULL
  if (is.null(descriptor) || is.null(vertical)) {
    rlang::abort(
      paste0(
        "depth_feature requires a current certified depth_gradient output; ",
        "it does not calculate gradients implicitly."
      ),
      class = "oceancube_vertical_feature_input"
    )
  }
  .cf_vertical_gradient_validate(descriptor)
  .cf_vertical_validate(vertical)
  if (!identical(descriptor$schema_name, "oceancube_vertical_gradient") ||
      !identical(descriptor$schema_version, "1.0.0") ||
      !identical(descriptor$certification_status, "CERTIFIED") ||
      !identical(descriptor$output_geometry_status, "GEOMETRY_NO_BOUNDS") ||
      !identical(descriptor$output_bounds_status, "BOUNDS_NOT_APPLICABLE") ||
      !identical(vertical$geometry_status, "GEOMETRY_NO_BOUNDS") ||
      !identical(vertical$bounds_status, "BOUNDS_MISSING")) {
    rlang::abort(
      "The current C4 vertical-gradient descriptor is not certified for C5.",
      class = "oceancube_vertical_feature_input"
    )
  }
  d <- unname(.cube_shape(x))
  variable_names <- vapply(
    descriptor$variables, `[[`, character(1L), "variable"
  )
  pair_valid <- vapply(descriptor$source_pair_indices, function(pair) {
    is.numeric(pair) && length(pair) == 2L && !anyNA(pair) &&
      all(is.finite(pair)) && all(pair == as.integer(pair)) &&
      all(pair >= 1L) && all(pair <= length(descriptor$source_depths))
  }, logical(1L))
  aligned <- length(descriptor$output_depths) == d[[3L]] &&
    identical(as.numeric(descriptor$output_depths), as.numeric(x$depth)) &&
    identical(as.numeric(vertical$source_coordinate), as.numeric(x$depth)) &&
    length(descriptor$source_depths) ==
      length(descriptor$canonical_metric_depths_m) &&
    length(descriptor$source_pair_indices) == d[[3L]] &&
    length(descriptor$spacing_m) == d[[3L]] &&
    length(descriptor$support_relation) == d[[3L]] &&
    length(descriptor$support_gap_m) == d[[3L]] &&
    length(descriptor$variables) == d[[5L]] &&
    identical(variable_names, as.character(x$vars)) &&
    all(pair_valid)
  if (!isTRUE(aligned)) {
    rlang::abort(
      "The current C4 gradient descriptor is stale or misaligned with the cube.",
      class = "oceancube_vertical_feature_descriptor_mismatch"
    )
  }
  if (any(!is.finite(descriptor$spacing_m)) || any(descriptor$spacing_m <= 0)) {
    rlang::abort(
      "The C4 gradient descriptor contains invalid vertical spacing.",
      class = "oceancube_vertical_feature_descriptor_mismatch"
    )
  }
  eligible_relations <- if (identical(support, "local")) {
    c("CONTIGUOUS_SUPPORT", "POINT_SUPPORT_UNBOUNDED")
  } else {
    c("CONTIGUOUS_SUPPORT", "POINT_SUPPORT_UNBOUNDED", "GAPPED_SUPPORT")
  }
  depth_unit <- vertical$units_raw
  if (!is.character(depth_unit) || length(depth_unit) != 1L ||
      is.na(depth_unit) || !nzchar(depth_unit)) {
    rlang::abort(
      "The certified gradient depth coordinate has no usable source unit.",
      class = "oceancube_vertical_feature_descriptor_mismatch"
    )
  }
  list(
    descriptor = descriptor,
    vertical = vertical,
    eligible_relations = eligible_relations,
    eligible = descriptor$support_relation %in% eligible_relations,
    polarity = polarity,
    support = support,
    depth_unit = depth_unit
  )
}

.vertical_feature_reduce <- function(values, plan) {
  descriptor <- plan$descriptor
  n <- ncol(values)
  numeric_na <- rep(NA_real_, n)
  integer_na <- rep(NA_integer_, n)
  character_na <- rep(NA_character_, n)
  out <- list(
    feature_depth = numeric_na, feature_depth_m = numeric_na,
    source_depth_1 = numeric_na, source_depth_2 = numeric_na,
    source_depth_1_m = numeric_na, source_depth_2_m = numeric_na,
    gradient = numeric_na, gradient_index = integer_na,
    spacing_m = numeric_na, support_relation = character_na,
    support_gap_m = numeric_na, localization_half_span_m = numeric_na,
    n_support_eligible = rep(as.integer(sum(plan$eligible)), n),
    n_finite_gradient = integer(n), gradient_completeness = numeric_na,
    n_matching_candidates = integer(n), n_tied = integer(n),
    feature_tolerance = numeric_na,
    status = rep("NO_FINITE_GRADIENT", n),
    certification_status = rep("NO_CANDIDATE", n)
  )
  for (profile in seq_len(n)) {
    if (!any(plan$eligible)) {
      out$status[[profile]] <- if (identical(plan$support, "local")) {
        "NO_LOCAL_SUPPORT"
      } else {
        "NO_ELIGIBLE_SUPPORT"
      }
      next
    }
    gradient <- values[, profile]
    finite <- plan$eligible & is.finite(gradient)
    out$n_finite_gradient[[profile]] <- as.integer(sum(finite))
    out$gradient_completeness[[profile]] <-
      sum(finite) / sum(plan$eligible)
    if (!any(finite)) next
    tolerance <- .vertical_feature_tolerance(gradient[finite])
    out$feature_tolerance[[profile]] <- tolerance
    candidates <- switch(
      plan$polarity,
      absolute = which(finite & abs(gradient) > tolerance),
      positive = which(finite & gradient > tolerance),
      negative = which(finite & gradient < -tolerance)
    )
    out$n_matching_candidates[[profile]] <- as.integer(length(candidates))
    if (!length(candidates)) {
      out$status[[profile]] <- if (identical(plan$polarity, "absolute")) {
        "FLAT_PROFILE"
      } else {
        "NO_MATCHING_POLARITY"
      }
      if (out$gradient_completeness[[profile]] < 1) {
        out$certification_status[[profile]] <-
          "INCOMPLETE_PROFILE_NO_RESOLVED_CANDIDATE"
      }
      next
    }
    scores <- switch(
      plan$polarity,
      absolute = abs(gradient[candidates]),
      positive = gradient[candidates],
      negative = -gradient[candidates]
    )
    tied <- candidates[abs(scores - max(scores)) <= tolerance]
    out$n_tied[[profile]] <- as.integer(length(tied))
    if (length(tied) > 1L) {
      out$status[[profile]] <- "AMBIGUOUS_TIE"
      out$certification_status[[profile]] <-
        if (out$gradient_completeness[[profile]] < 1) {
          "AMBIGUOUS_TIE_INCOMPLETE_PROFILE"
        } else {
          "AMBIGUOUS_TIE"
        }
      next
    }
    selected <- tied[[1L]]
    pair <- as.integer(descriptor$source_pair_indices[[selected]])
    relation <- descriptor$support_relation[[selected]]
    out$feature_depth[[profile]] <- descriptor$output_depths[[selected]]
    out$feature_depth_m[[profile]] <- mean(
      descriptor$canonical_metric_depths_m[pair]
    )
    out$source_depth_1[[profile]] <- descriptor$source_depths[[pair[[1L]]]]
    out$source_depth_2[[profile]] <- descriptor$source_depths[[pair[[2L]]]]
    out$source_depth_1_m[[profile]] <-
      descriptor$canonical_metric_depths_m[[pair[[1L]]]]
    out$source_depth_2_m[[profile]] <-
      descriptor$canonical_metric_depths_m[[pair[[2L]]]]
    out$gradient[[profile]] <- gradient[[selected]]
    out$gradient_index[[profile]] <- as.integer(selected)
    out$spacing_m[[profile]] <- descriptor$spacing_m[[selected]]
    out$support_relation[[profile]] <- relation
    out$support_gap_m[[profile]] <- descriptor$support_gap_m[[selected]]
    out$localization_half_span_m[[profile]] <-
      descriptor$spacing_m[[selected]] / 2
    out$status[[profile]] <- switch(
      relation,
      CONTIGUOUS_SUPPORT = "LOCAL_CONTIGUOUS_CANDIDATE",
      POINT_SUPPORT_UNBOUNDED = "LOCAL_POINT_BRACKET_CANDIDATE",
      GAPPED_SUPPORT = "GAPPED_SECANT_CANDIDATE"
    )
    out$certification_status[[profile]] <-
      if (out$gradient_completeness[[profile]] < 1) {
        "OBSERVED_CANDIDATE_INCOMPLETE_PROFILE"
      } else {
        switch(
          relation,
          CONTIGUOUS_SUPPORT = "CERTIFIED_LOCAL_CONTIGUOUS_CANDIDATE",
          POINT_SUPPORT_UNBOUNDED = "CERTIFIED_LOCAL_POINT_BRACKET_CANDIDATE",
          GAPPED_SUPPORT = "CERTIFIED_GAPPED_SECANT_CANDIDATE"
        )
      }
  }
  out
}

.vertical_feature_tolerance <- function(x) {
  8 * sqrt(.Machine$double.eps) * max(1, abs(x))
}

.vertical_feature_ranking_rule <- function(polarity) {
  switch(
    polarity,
    absolute = "argmax(abs(gradient)) above effective-zero tolerance",
    positive = "argmax(gradient) for gradient greater than tolerance",
    negative = "argmin(gradient) for gradient less than negative tolerance"
  )
}

.vertical_feature_status_levels <- function() {
  c(
    "LOCAL_CONTIGUOUS_CANDIDATE", "LOCAL_POINT_BRACKET_CANDIDATE",
    "GAPPED_SECANT_CANDIDATE", "AMBIGUOUS_TIE", "FLAT_PROFILE",
    "NO_MATCHING_POLARITY", "NO_FINITE_GRADIENT", "NO_LOCAL_SUPPORT",
    "NO_ELIGIBLE_SUPPORT"
  )
}
