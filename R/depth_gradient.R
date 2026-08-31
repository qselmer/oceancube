#' Compute certified vertical secant gradients
#'
#' @param x An `<ocean_cube>` with a certified multi-level metric-depth axis
#'   and supported current vertical value semantics.
#' @param method Gradient method. `"auto"` resolves each variable independently
#'   from current semantics. `"point"` requires point-valued variables and
#'   `"cell"` requires certified vertical cell means.
#'
#' @return A memory-backed `<ocean_cube>` with one fewer depth level. Values are
#'   signed adjacent-level secant gradients with respect to physical ocean depth
#'   in metres, positive downward. Output depths are source-unit midpoints and
#'   carry no physical layer bounds.
#'
#' @details
#' For adjacent levels, the gradient is `(X[i + 1] - X[i]) /
#' (D[i + 1] - D[i])`, where `D` is canonical positive-down depth in metres.
#' The same first-order secant is used for point values and adjacent cell means,
#' but their semantics remain distinct. Irregular spacing and increasing or
#' decreasing storage order are supported. Explicit support gaps are not filled:
#' a secant may span a gap, and the gap relation and size are recorded.
#'
#' Missing or non-finite values make only the adjacent pairs that use them
#' missing. No smoothing, higher-order stencil, endpoint derivative,
#' interpolation, missing-level bridge, or extrapolation is performed. Derived
#' units are represented symbolically per metre. Gradient midpoint coordinates
#' are point locations without certified bounds, so downstream thickness,
#' volume, and vertical integration remain unavailable.
#'
#' @examples
#' \dontrun{
#' # Offline point profile: 0, 10, 20 m and values 1, 21, 41.
#' point <- read_nc("synthetic-point-profile.nc", vars = "temperature")
#' depth_gradient(point, method = "point")
#'
#' # Offline cell means over explicit metric bounds.
#' cells <- read_nc("synthetic-cell-mean-profile.nc", vars = "temperature")
#' depth_gradient(cells, method = "cell")
#' }
#' @export
depth_gradient <- function(x, method = c("auto", "point", "cell")) {
  method <- match.arg(method)
  plan <- .vertical_gradient_plan(x, method)
  d <- unname(.cube_shape(x))
  values <- .cube_read(x, index = list(depth = seq_len(d[[3L]])))
  lower <- values[, , seq_len(d[[3L]] - 1L), , , drop = FALSE]
  upper <- values[, , seq.int(2L, d[[3L]]), , , drop = FALSE]
  valid <- is.finite(lower) & is.finite(upper)
  out <- sweep(upper - lower, 3L, plan$metric$delta_m, "/")
  out[!valid | !is.finite(out)] <- NA_real_

  output_shape <- stats::setNames(
    as.integer(c(d[[1L]], d[[2L]], d[[3L]] - 1L, d[[4L]], d[[5L]])),
    .cube_axis_names()
  )
  provenance_context <- .provenance_cube_context(
    source = x$source, dataset_id = x$dataset_id, time = x$time,
    shape = output_shape, variables = x$vars, backend = "memory",
    provenance = x$provenance
  )
  provenance <- .provenance_append(
    x$provenance,
    operation = "depth_gradient",
    parameters = list(
      requested = list(method = method),
      resolved = list(
        variables = unname(plan$variables),
        source_depths = plan$metric$source_depths,
        source_depth_unit = plan$metric$source_unit,
        canonical_metric_depths_m = plan$metric$canonical_m,
        derivative_coordinate = "canonical physical ocean depth",
        derivative_coordinate_unit = "m",
        derivative_positive_direction = "down",
        source_order = plan$metric$source_order,
        source_pair_indices = plan$metric$source_pair_indices,
        output_midpoint_depths = plan$metric$midpoints_source,
        spacing_m = plan$metric$spacing_m,
        support_relation = plan$support$relation,
        support_gap_m = plan$support$gap_m,
        gradient_equation = "(X[i+1]-X[i])/(D[i+1]-D[i])",
        missing_value_policy = plan$descriptor$missing_value_policy,
        source_units = plan$units$source,
        derived_units = plan$units$derived,
        unit_status = plan$units$status,
        one_indexed_read = TRUE,
        output_bounds_status = "BOUNDS_NOT_APPLICABLE",
        output_shape = output_shape
      )
    ),
    output = .provenance_summary(provenance_context),
    scientific_method = .provenance_method(
      "depth_gradient", list(resolved_method = plan$resolved_method)
    ),
    context = provenance_context
  )
  result <- ocean_cube(
    lon = x$lon, lat = x$lat, depth = plan$metric$midpoints_source,
    time = x$time, vars = x$vars, data = out, units = plan$units$derived,
    source = x$source, dataset_id = x$dataset_id,
    spatial_extent = x$spatial_extent, temporal_extent = x$temporal_extent,
    depth_extent = range(plan$metric$midpoints_source), mask = x$mask,
    dc = x$dc, provenance = provenance
  )
  attr(result$depth, "units") <- plan$metric$source_unit
  attr(result$depth, "positive") <- plan$metric$positive
  .attach_cube_metadata(
    result,
    .cf_metadata_for_vertical_gradient(
      x$metadata %||% NULL,
      midpoints = plan$metric$midpoints_source,
      descriptor = plan$descriptor
    )
  )
}

.vertical_metric_depth <- function(x) {
  .check_cube(x)
  if (length(x$depth) < 2L) {
    rlang::abort(
      "Certified vertical gradients require at least two depth levels.",
      class = "oceancube_vertical_gradient_unsupported"
    )
  }
  vertical <- x$metadata$cf$current$vertical %||% NULL
  if (is.null(vertical)) {
    rlang::abort(
      "Certified vertical gradients require preserved current CF vertical semantics.",
      class = "oceancube_vertical_gradient_unsupported"
    )
  }
  .cf_vertical_validate(vertical)
  if (!identical(vertical$kind, "DEPTH_LENGTH") ||
      !identical(vertical$runtime_status, "VERTICAL_RUNTIME_SUPPORTED") ||
      !vertical$normalized_unit %in% c("m", "km") ||
      !vertical$positive %in% c("down", "up")) {
    rlang::abort(
      paste0(
        "Certified vertical gradients require multi-level metric DEPTH_LENGTH; current kind/status is `",
        vertical$kind, "`/`", vertical$runtime_status, "`."
      ),
      class = "oceancube_vertical_gradient_unsupported"
    )
  }
  source <- as.numeric(x$depth)
  if (any(!is.finite(source)) ||
      !.cf_vertical_source_order(source) %in% c("INCREASING", "DECREASING")) {
    rlang::abort(
      "Gradient source depths must be finite, unique, and strictly monotonic.",
      class = "oceancube_vertical_gradient_unsupported"
    )
  }
  scale <- vertical$scale_to_m
  if (!is.numeric(scale) || length(scale) != 1L || !is.finite(scale)) {
    scale <- .depth_conversion_factor(vertical$normalized_unit, "m")
  }
  sign <- if (identical(vertical$positive, "down")) 1 else -1
  canonical <- source * scale * sign
  delta <- diff(canonical)
  if (any(!is.finite(delta)) || any(delta == 0) ||
      !.cf_vertical_source_order(canonical) %in% c("INCREASING", "DECREASING")) {
    rlang::abort(
      "Canonical positive-down metric depths must be strictly monotonic.",
      class = "oceancube_vertical_gradient_unsupported"
    )
  }
  list(
    source_depths = source,
    source_unit = vertical$units_raw,
    normalized_unit = vertical$normalized_unit,
    positive = vertical$positive,
    source_order = .cf_vertical_source_order(source),
    canonical_order = .cf_vertical_source_order(canonical),
    canonical_m = canonical,
    delta_m = delta,
    spacing_m = abs(delta),
    midpoints_source = (source[-length(source)] + source[-1L]) / 2,
    source_pair_indices = lapply(
      seq_len(length(source) - 1L), function(i) as.integer(c(i, i + 1L))
    ),
    vertical = vertical
  )
}

.vertical_gradient_current_item <- function(items, variable) {
  if (!is.list(items) || !length(items)) return(NULL)
  matched <- Filter(function(item) {
    is.list(item) && length(item$variable) == 1L &&
      (identical(item$variable, variable) ||
        identical(.cf_basename(item$variable), .cf_basename(variable)))
  }, items)
  if (length(matched) == 1L) matched[[1L]] else NULL
}

.vertical_gradient_semantics <- function(x) {
  current <- x$metadata$cf$current
  gradient <- current$vertical_gradient %||% NULL
  sampling <- current$vertical_sampling %||% NULL
  reduction <- current$vertical_reduction %||% NULL
  fallback <- .vertical_value_semantics(x)
  out <- lapply(x$vars, function(variable) {
    if (!is.null(gradient)) {
      return(list(
        variable = variable, status = "VERTICAL_GRADIENT_OUTPUT_UNSUPPORTED",
        resolved_method = NA_character_, source = "current vertical_gradient",
        input_value_semantics = "VERTICAL_SECANT_GRADIENT",
        output_value_semantics = NA_character_, certification_status = "UNSUPPORTED"
      ))
    }
    sampled <- if (is.null(sampling)) NULL else {
      .vertical_gradient_current_item(sampling$variables, variable)
    }
    if (!is.null(sampled)) {
      return(switch(
        sampled$output_value_semantics,
        INTERPOLATED_POINT_VALUE = list(
          variable = variable, status = "DERIVED_VERTICAL_POINT",
          resolved_method = "point", source = "current vertical_sampling",
          input_value_semantics = "INTERPOLATED_POINT_VALUE",
          output_value_semantics = "DERIVED_POINT_SECANT_GRADIENT",
          certification_status = "CERTIFIED"
        ),
        SAMPLED_CELL_MEAN_RECONSTRUCTION = list(
          variable = variable,
          status = "C3_CELL_RECONSTRUCTION_UNSUPPORTED",
          resolved_method = NA_character_, source = "current vertical_sampling",
          input_value_semantics = "SAMPLED_CELL_MEAN_RECONSTRUCTION",
          output_value_semantics = NA_character_,
          certification_status = "UNSUPPORTED"
        ),
        list(
          variable = variable, status = "CURRENT_SAMPLING_UNSUPPORTED",
          resolved_method = NA_character_, source = "current vertical_sampling",
          input_value_semantics = sampled$output_value_semantics,
          output_value_semantics = NA_character_,
          certification_status = "UNSUPPORTED"
        )
      ))
    }
    if (!is.null(reduction)) {
      if (identical(reduction$method, "overlap_weighted_mean") &&
          identical(reduction$geometric_certification, "CERTIFIED") &&
          identical(reduction$value_semantic_certification, "CERTIFIED") &&
          identical(reduction$certification_status, "CERTIFIED")) {
        return(list(
          variable = variable, status = "DERIVED_VERTICAL_CELL_MEAN",
          resolved_method = "cell", source = "current vertical_reduction",
          input_value_semantics = "CERTIFIED_OVERLAP_WEIGHTED_LAYER_MEAN",
          output_value_semantics = "DERIVED_CELL_MEAN_SECANT_GRADIENT",
          certification_status = "CERTIFIED"
        ))
      }
      return(list(
        variable = variable,
        status = if (identical(reduction$method, "metric_cell_mean_integral")) {
          "VERTICAL_INTEGRAL_UNSUPPORTED"
        } else {
          "CURRENT_REDUCTION_UNCERTIFIED"
        },
        resolved_method = NA_character_, source = "current vertical_reduction",
        input_value_semantics = reduction$method,
        output_value_semantics = NA_character_, certification_status = "UNSUPPORTED"
      ))
    }
    original <- fallback[[variable]]
    if (identical(original$status, "VERTICAL_POINT")) {
      list(
        variable = variable, status = "VERTICAL_POINT",
        resolved_method = "point", source = "current/source CF classifier",
        input_value_semantics = "VERTICAL_POINT",
        output_value_semantics = "POINT_SECANT_GRADIENT",
        certification_status = "CERTIFIED"
      )
    } else if (identical(original$status, "VERTICAL_CELL_MEAN")) {
      list(
        variable = variable, status = "VERTICAL_CELL_MEAN",
        resolved_method = "cell", source = "current/source CF classifier",
        input_value_semantics = "VERTICAL_CELL_MEAN",
        output_value_semantics = "CELL_MEAN_SECANT_GRADIENT",
        certification_status = "CERTIFIED"
      )
    } else {
      list(
        variable = variable, status = original$status,
        resolved_method = NA_character_, source = "current/source CF classifier",
        input_value_semantics = original$status,
        output_value_semantics = NA_character_, certification_status = "UNSUPPORTED"
      )
    }
  })
  names(out) <- x$vars
  out
}

.vertical_gradient_methods <- function(semantics, requested) {
  supported <- vapply(
    semantics,
    function(item) identical(item$certification_status, "CERTIFIED"),
    logical(1L)
  )
  compatible <- supported & vapply(semantics, function(item) {
    identical(requested, "auto") || identical(item$resolved_method, requested)
  }, logical(1L))
  if (any(!compatible)) {
    details <- vapply(semantics[!compatible], function(item) {
      paste0(item$variable, "=", item$status)
    }, character(1L))
    rlang::abort(
      paste0(
        "Certified vertical gradient method `", requested,
        "` is incompatible with: ", paste(details, collapse = ", "), "."
      ),
      class = "oceancube_vertical_gradient_semantics_unsupported"
    )
  }
  stats::setNames(
    vapply(semantics, `[[`, character(1L), "resolved_method"),
    names(semantics)
  )
}

.vertical_gradient_support <- function(metric, require_cell = FALSE) {
  vertical <- metric$vertical
  status <- vertical$bounds_status
  if (!status %in% c("BOUNDS_VALID", "BOUNDS_MISSING")) {
    rlang::abort(
      paste0("Vertical gradient cannot use invalid explicit bounds (`", status, "`)."),
      class = "oceancube_vertical_gradient_geometry_unsupported"
    )
  }
  if (!identical(status, "BOUNDS_VALID")) {
    if (isTRUE(require_cell)) {
      rlang::abort(
        "Certified cell-mean gradients require valid explicit vertical bounds.",
        class = "oceancube_vertical_gradient_geometry_unsupported"
      )
    }
    return(list(
      relation = rep("POINT_SUPPORT_UNBOUNDED", length(metric$spacing_m)),
      gap_m = rep(NA_real_, length(metric$spacing_m)),
      bounds_m = list(), tolerance_m = NA_real_
    ))
  }
  if (length(vertical$bounds) != length(metric$source_depths)) {
    rlang::abort(
      "Vertical bounds do not match the source depth axis.",
      class = "oceancube_vertical_gradient_geometry_unsupported"
    )
  }
  paired <- do.call(rbind, vertical$bounds)
  bounds_unit <- vertical$bounds_unit %||% vertical$normalized_unit
  scale <- .depth_conversion_factor(bounds_unit, "m")
  sign <- if (identical(vertical$positive, "down")) 1 else -1
  paired_m <- paired * scale * sign
  lower <- pmin(paired_m[, 1L], paired_m[, 2L])
  upper <- pmax(paired_m[, 1L], paired_m[, 2L])
  tolerance <- .vertical_geometry_tolerance(metric$canonical_m, paired_m)
  if (any(!is.finite(paired_m)) || any(upper <= lower) ||
      any(metric$canonical_m < lower - tolerance |
          metric$canonical_m > upper + tolerance)) {
    rlang::abort(
      "Vertical bounds must be finite positive-width intervals containing their representative levels.",
      class = "oceancube_vertical_gradient_geometry_unsupported"
    )
  }
  ordered <- order(lower, upper)
  if (length(ordered) > 1L && any(
    lower[ordered][-1L] < upper[ordered][-length(ordered)] - tolerance
  )) {
    rlang::abort(
      "Overlapping vertical source-cell bounds cannot define certified gradients.",
      class = "oceancube_vertical_gradient_geometry_unsupported"
    )
  }
  gap <- relation <- vector("list", length(metric$spacing_m))
  for (i in seq_along(metric$spacing_m)) {
    pair <- c(i, i + 1L)
    shallow <- pair[[which.min(metric$canonical_m[pair])]]
    deep <- pair[[which.max(metric$canonical_m[pair])]]
    separation <- lower[[deep]] - upper[[shallow]]
    gap[[i]] <- max(0, separation)
    relation[[i]] <- if (separation > tolerance) {
      "GAPPED_SUPPORT"
    } else {
      "CONTIGUOUS_SUPPORT"
    }
  }
  list(
    relation = unlist(relation, use.names = FALSE),
    gap_m = as.numeric(unlist(gap, use.names = FALSE)),
    bounds_m = lapply(seq_len(nrow(paired_m)), function(i) as.numeric(paired_m[i, ])),
    tolerance_m = tolerance
  )
}

.vertical_gradient_units <- function(x) {
  source <- .vertical_units_values(x)
  if (is.null(source) || anyNA(source)) {
    failed <- if (is.null(source)) x$vars else names(source)[is.na(source)]
    rlang::abort(
      paste0(
        "Certified vertical gradients require a declared unit for: ",
        paste(failed, collapse = ", "), "."
      ),
      class = "oceancube_vertical_gradient_unit_unsupported"
    )
  }
  derived <- ifelse(source == "1", "m-1", paste(source, "m-1"))
  status <- ifelse(
    source == "1", "SYMBOLIC_DIMENSIONLESS_PER_METRE", "SYMBOLIC_UNNORMALIZED"
  )
  list(
    source = stats::setNames(as.list(unname(source)), names(source)),
    derived = stats::setNames(as.list(unname(derived)), names(derived)),
    status = stats::setNames(as.list(unname(status)), names(status))
  )
}

.vertical_gradient_descriptor <- function(
    method, semantics, methods, metric, support, units, resolved_method) {
  variables <- lapply(seq_along(semantics), function(i) {
    item <- semantics[[i]]
    variable <- item$variable
    list(
      variable = variable,
      input_value_semantics = item$input_value_semantics,
      semantic_source = item$source,
      resolved_method = methods[[variable]],
      output_value_semantics = item$output_value_semantics,
      source_unit = units$source[[variable]],
      output_unit = units$derived[[variable]],
      unit_status = units$status[[variable]],
      scientific_method_id = if (identical(methods[[variable]], "point")) {
        "oceancube:adjacent_point_secant_gradient"
      } else {
        "oceancube:adjacent_cell_mean_secant_gradient"
      },
      certification_status = "CERTIFIED"
    )
  })
  list(
    schema_name = "oceancube_vertical_gradient",
    schema_version = "1.0.0",
    derivative_coordinate = "canonical physical ocean depth",
    derivative_coordinate_unit = "m",
    derivative_positive_direction = "down",
    method_requested = method,
    resolved_method = resolved_method,
    variables = variables,
    source_depths = metric$source_depths,
    canonical_metric_depths_m = metric$canonical_m,
    output_depths = metric$midpoints_source,
    source_pair_indices = metric$source_pair_indices,
    spacing_m = metric$spacing_m,
    support_relation = support$relation,
    support_gap_m = support$gap_m,
    gradient_equation = "(X[i+1]-X[i])/(D[i+1]-D[i])",
    missing_value_policy = paste0(
      "a non-finite value makes only adjacent pairs using it NA; ",
      "missing levels are never bridged"
    ),
    output_bounds_status = "BOUNDS_NOT_APPLICABLE",
    output_geometry_status = "GEOMETRY_NO_BOUNDS",
    standard_name_status = "DERIVATION_PENDING",
    cell_methods_status = "DERIVATION_PENDING_NO_SYNTHETIC_CLAIM",
    certification_status = "CERTIFIED"
  )
}

.vertical_gradient_plan <- function(x, method) {
  metric <- .vertical_metric_depth(x)
  semantics <- .vertical_gradient_semantics(x)
  methods <- .vertical_gradient_methods(semantics, method)
  support <- .vertical_gradient_support(metric, any(methods == "cell"))
  units <- .vertical_gradient_units(x)
  resolved_method <- if (length(unique(methods)) == 1L) {
    unname(methods[[1L]])
  } else {
    "mixed"
  }
  descriptor <- .vertical_gradient_descriptor(
    method, semantics, methods, metric, support, units, resolved_method
  )
  list(
    metric = metric, semantics = semantics, methods = methods,
    variables = stats::setNames(descriptor$variables, x$vars),
    support = support, units = units, resolved_method = resolved_method,
    descriptor = descriptor
  )
}
