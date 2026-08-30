#' Sample a cube at physical metric depths
#'
#' @param x An `<ocean_cube>` with a certified CF metric-depth coordinate.
#' @param depth Non-empty, finite, strictly increasing numeric target depths,
#'   interpreted in the source depth-coordinate units.
#' @param method Sampling method. `"auto"` uses source CF value semantics:
#'   vertical cell means use cell containment and vertical point values use
#'   local linear interpolation. `"cell"` and `"linear"` require every
#'   selected variable to satisfy the corresponding contract.
#'
#' @return A memory-backed `<ocean_cube>` whose depth coordinates are exactly
#'   `depth`. Scientific variable units are unchanged. The output deliberately
#'   has no physical depth-cell bounds.
#'
#' @details
#' This function reconstructs values at requested physical depths; it is not a
#' discrete selector. [cube_slice()] selects stored coordinates (exactly or by
#' nearest neighbour), while [cube_extract()] returns existing stored cells as
#' a table. `depth_sample()` never adds nearest-neighbour or extrapolation
#' behavior.
#'
#' For CF vertical cell means, `method = "cell"` requires valid explicit metric
#' bounds and returns the containing cell mean under a
#' piecewise-constant-cell-mean reconstruction. Shared interior cell boundaries
#' and explicit gaps are errors. For vertical point values, `method = "linear"`
#' returns an exact stored value when matched within the common vertical
#' tolerance, otherwise it performs transparent two-point interpolation.
#' Explicit physical gaps are never bridged. A missing or non-finite required
#' source value produces `NA`, without imputation.
#'
#' Only certified dimensional ocean depth in metres or kilometres is supported.
#' Pressure, height, dimensionless, parametric and surface-only coordinates are
#' rejected. Sampled outputs have point coordinates but no certified layer
#' support, so they do not automatically qualify for thickness, volume or
#' vertical integration.
#'
#' @examples
#' \dontrun{
#' # Offline synthetic CF point profile: depths 0, 10, 20 m; values 0, 20, 40.
#' point <- read_nc("synthetic-point-profile.nc", vars = "temperature")
#' depth_sample(point, c(5, 12.5), method = "linear")
#'
#' # Offline synthetic CF cell means with explicit 0-10, 10-20, 20-30 m bounds.
#' cells <- read_nc("synthetic-cell-mean-profile.nc", vars = "temperature")
#' depth_sample(cells, c(2, 12, 28), method = "cell")
#' }
#' @export
depth_sample <- function(x, depth, method = c("auto", "cell", "linear")) {
  method <- match.arg(method)
  plan <- .vertical_sampling_plan(x, depth, method)
  d <- unname(.cube_shape(x))
  out <- array(
    NA_real_,
    dim = c(d[[1L]], d[[2L]], length(depth), d[[4L]], d[[5L]])
  )

  values <- .cube_read(x, index = list(depth = plan$required_indices))
  local_index <- stats::setNames(
    seq_along(plan$required_indices),
    as.character(plan$required_indices)
  )
  for (k in seq_along(x$vars)) {
    variable_plan <- plan$variables[[k]]
    for (j in seq_along(variable_plan$targets)) {
      target <- variable_plan$targets[[j]]
      if (target$status %in% c("CELL_CONTAINED", "EXACT_POINT")) {
        at <- unname(local_index[[as.character(target$source_index)]])
        sampled <- array(
          values[, , at, , k, drop = FALSE],
          dim = c(d[[1L]], d[[2L]], d[[4L]])
        )
      } else {
        lower_at <- unname(local_index[[as.character(target$lower_index)]])
        upper_at <- unname(local_index[[as.character(target$upper_index)]])
        lower <- array(
          values[, , lower_at, , k, drop = FALSE],
          dim = c(d[[1L]], d[[2L]], d[[4L]])
        )
        upper <- array(
          values[, , upper_at, , k, drop = FALSE],
          dim = c(d[[1L]], d[[2L]], d[[4L]])
        )
        sampled <- lower + target$upper_weight * (upper - lower)
        sampled[!is.finite(lower) | !is.finite(upper)] <- NA_real_
      }
      sampled[!is.finite(sampled)] <- NA_real_
      out[, , j, , k] <- sampled
    }
  }

  output_shape <- stats::setNames(
    as.integer(c(d[[1L]], d[[2L]], length(depth), d[[4L]], d[[5L]])),
    .cube_axis_names()
  )
  provenance_context <- .provenance_cube_context(
    source = x$source, dataset_id = x$dataset_id, time = x$time,
    shape = output_shape, variables = x$vars, backend = "memory",
    provenance = x$provenance
  )
  provenance <- .provenance_append(
    x$provenance,
    operation = "depth_sample",
    parameters = list(
      requested = list(depth = as.numeric(depth), method = method),
      resolved = list(
        requested_unit = plan$requested_unit,
        source_depths = as.numeric(x$depth),
        source_order = plan$source_order,
        variables = unname(plan$variables),
        required_source_depth_indices = plan$required_indices,
        one_indexed_read = TRUE,
        no_extrapolation = TRUE,
        gap_policy = plan$gap_policy,
        boundary_policy = plan$boundary_policy,
        missing_value_policy = plan$missing_value_policy,
        output_depth = as.numeric(depth),
        output_bounds_status = "BOUNDS_NOT_APPLICABLE",
        output_shape = output_shape
      )
    ),
    output = .provenance_summary(provenance_context),
    scientific_method = .provenance_method(
      "depth_sample", list(resolved_method = plan$resolved_method)
    ),
    context = provenance_context
  )
  result <- ocean_cube(
    lon = x$lon, lat = x$lat, depth = as.numeric(depth), time = x$time,
    vars = x$vars, data = out, units = x$units, source = x$source,
    dataset_id = x$dataset_id, spatial_extent = x$spatial_extent,
    temporal_extent = x$temporal_extent, depth_extent = range(depth),
    mask = x$mask, dc = x$dc, provenance = provenance
  )
  attr(result$depth, "units") <- plan$requested_unit
  attr(result$depth, "positive") <- plan$positive
  .attach_cube_metadata(
    result,
    .cf_metadata_for_vertical_sampling(
      x$metadata %||% NULL,
      targets = as.numeric(depth),
      descriptor = plan$descriptor
    )
  )
}

.vertical_sampling_validate_request <- function(x, depth) {
  .check_cube(x)
  .check_numeric_vector(depth, "depth")
  if (!is.null(dim(depth)) || !length(depth)) {
    .abort_badarg("depth", "must be a non-empty numeric vector.")
  }
  if (any(!is.finite(depth))) {
    .abort_badarg("depth", "must contain only finite values.")
  }
  if (length(depth) > 1L && any(diff(depth) <= 0)) {
    .abort_badarg(
      "depth",
      "must be strictly increasing without duplicates; requests are not sorted silently."
    )
  }
  vertical <- x$metadata$cf$current$vertical %||% NULL
  if (is.null(vertical)) {
    rlang::abort(
      "Certified depth sampling requires preserved CF vertical semantics.",
      class = "oceancube_vertical_sampling_unsupported"
    )
  }
  .cf_vertical_validate(vertical)
  if (length(x$depth) < 2L ||
      !identical(vertical$kind, "DEPTH_LENGTH") ||
      !identical(vertical$runtime_status, "VERTICAL_RUNTIME_SUPPORTED") ||
      !vertical$normalized_unit %in% c("m", "km")) {
    rlang::abort(
      paste0(
        "Certified depth sampling requires a multi-level metric DEPTH_LENGTH axis; current kind/status is `",
        vertical$kind, "`/`", vertical$runtime_status, "`."
      ),
      class = "oceancube_vertical_sampling_unsupported"
    )
  }
  if (!vertical$source_order %in% c("INCREASING", "DECREASING")) {
    rlang::abort(
      "Source depths must be finite and strictly monotonic without duplicates.",
      class = "oceancube_vertical_sampling_unsupported"
    )
  }
  vertical
}

.vertical_sampling_methods <- function(semantics, requested) {
  eligible <- vapply(semantics, function(item) {
    if (identical(requested, "auto")) {
      item$status %in% c("VERTICAL_CELL_MEAN", "VERTICAL_POINT")
    } else if (identical(requested, "cell")) {
      identical(item$status, "VERTICAL_CELL_MEAN")
    } else {
      identical(item$status, "VERTICAL_POINT")
    }
  }, logical(1L))
  if (any(!eligible)) {
    failed <- vapply(semantics[!eligible], function(item) {
      paste0(item$variable, "=", item$status)
    }, character(1L))
    rlang::abort(
      paste0(
        "Vertical sampling method `", requested,
        "` is incompatible with: ", paste(failed, collapse = ", "), "."
      ),
      class = "oceancube_vertical_sampling_semantics_unsupported"
    )
  }
  resolved <- vapply(semantics, function(item) {
    if (!identical(requested, "auto")) return(requested)
    if (identical(item$status, "VERTICAL_CELL_MEAN")) "cell" else "linear"
  }, character(1L))
  stats::setNames(resolved, names(semantics))
}

.vertical_sampling_cell_targets <- function(x, targets) {
  support <- .vertical_support_engine(x, mode = "integral")
  factor <- .depth_conversion_factor(support$depth_unit, support$unit)
  converted <- as.numeric(targets) * factor
  tolerance <- .vertical_geometry_tolerance(
    support$lower, support$upper, converted
  )
  minimum <- min(support$lower)
  maximum <- max(support$upper)
  plans <- lapply(seq_along(converted), function(i) {
    target <- converted[[i]]
    if (target < minimum - tolerance || target > maximum + tolerance) {
      rlang::abort(
        paste0("Requested depth `", targets[[i]], "` is outside physical support."),
        class = "oceancube_vertical_sampling_outside"
      )
    }
    contained <- which(
      target >= support$lower - tolerance &
        target <= support$upper + tolerance
    )
    if (!length(contained)) {
      rlang::abort(
        paste0("Requested depth `", targets[[i]], "` lies in an explicit vertical gap."),
        class = "oceancube_vertical_sampling_gap"
      )
    }
    if (length(contained) != 1L) {
      rlang::abort(
        paste0(
          "Requested depth `", targets[[i]],
          "` lies on a shared interior cell boundary."
        ),
        class = "oceancube_vertical_boundary_ambiguous"
      )
    }
    at <- contained[[1L]]
    list(
      target_depth = as.numeric(targets[[i]]),
      target_depth_in_bounds_unit = target,
      status = "CELL_CONTAINED",
      source_index = as.integer(at),
      source_depth = as.numeric(x$depth[[at]]),
      source_lower_bound = as.numeric(support$lower[[at]]),
      source_upper_bound = as.numeric(support$upper[[at]]),
      source_bounds_unit = support$unit,
      tolerance = tolerance
    )
  })
  list(targets = plans, tolerance = tolerance, support = support)
}

.vertical_sampling_linear_targets <- function(x, targets, vertical) {
  source <- as.numeric(x$depth)
  ordered <- order(source)
  sorted <- source[ordered]
  tolerance <- .vertical_geometry_tolerance(source, targets)
  has_bounds <- identical(vertical$bounds_status, "BOUNDS_VALID") &&
    length(vertical$bounds) == length(source)
  if (!has_bounds && !identical(vertical$bounds_status, "BOUNDS_MISSING")) {
    rlang::abort(
      paste0(
        "Point interpolation cannot ignore invalid explicit vertical bounds (`",
        vertical$bounds_status, "`)."
      ),
      class = "oceancube_vertical_sampling_unsupported"
    )
  }
  paired <- NULL
  if (has_bounds) {
    paired <- do.call(rbind, vertical$bounds)
    bounds_unit <- vertical$bounds_unit %||% vertical$normalized_unit
    paired <- paired * .depth_conversion_factor(
      bounds_unit, vertical$normalized_unit
    )
  }
  plans <- lapply(seq_along(targets), function(i) {
    target <- as.numeric(targets[[i]])
    exact <- which(abs(source - target) <= tolerance)
    if (length(exact) == 1L) {
      return(list(
        target_depth = target,
        status = "EXACT_POINT",
        source_index = as.integer(exact),
        source_depth = as.numeric(source[[exact]]),
        lower_weight = 1,
        upper_weight = 0,
        tolerance = tolerance
      ))
    }
    if (target < sorted[[1L]] - tolerance ||
        target > sorted[[length(sorted)]] + tolerance) {
      rlang::abort(
        paste0("Requested depth `", target, "` is outside the source point domain."),
        class = "oceancube_vertical_sampling_outside"
      )
    }
    upper_position <- which(sorted > target + tolerance)[[1L]]
    lower_position <- upper_position - 1L
    lower_index <- ordered[[lower_position]]
    upper_index <- ordered[[upper_position]]
    if (has_bounds) {
      lower_upper <- max(paired[lower_index, ])
      upper_lower <- min(paired[upper_index, ])
      if (upper_lower > lower_upper + tolerance) {
        rlang::abort(
          paste0(
            "Requested depth `", target,
            "` would interpolate across an explicit vertical gap."
          ),
          class = "oceancube_vertical_sampling_gap"
        )
      }
    }
    lower_depth <- source[[lower_index]]
    upper_depth <- source[[upper_index]]
    upper_weight <- (target - lower_depth) / (upper_depth - lower_depth)
    list(
      target_depth = target,
      status = "LINEAR_INTERPOLATED",
      lower_index = as.integer(lower_index),
      upper_index = as.integer(upper_index),
      lower_depth = as.numeric(lower_depth),
      upper_depth = as.numeric(upper_depth),
      lower_weight = as.numeric(1 - upper_weight),
      upper_weight = as.numeric(upper_weight),
      explicit_bounds_checked = has_bounds,
      tolerance = tolerance
    )
  })
  list(targets = plans, tolerance = tolerance, explicit_bounds_checked = has_bounds)
}

.vertical_sampling_descriptor <- function(
    depth, method, vertical, variables, resolved_method) {
  list(
    schema_name = "oceancube_vertical_sampling",
    schema_version = "1.0.0",
    requested_depths = as.numeric(depth),
    requested_unit = vertical$units_raw,
    method_requested = method,
    variables = lapply(variables, function(item) {
      list(
        variable = item$variable,
        input_value_semantics = item$input_value_semantics,
        resolved_method = item$resolved_method,
        output_value_semantics = item$output_value_semantics,
        scientific_method_id = item$scientific_method_id,
        certification_status = "CERTIFIED"
      )
    }),
    input_source_order = vertical$source_order,
    no_extrapolation = TRUE,
    gap_policy = "explicit physical gaps are never bridged",
    boundary_policy = paste0(
      "shared interior cell boundaries are ambiguous; outer boundaries ",
      "belonging to one cell are accepted"
    ),
    missing_value_policy = paste0(
      "non-finite containing-cell or required point values return NA; ",
      "no imputation or alternate-depth search"
    ),
    output_support = "REQUESTED_POINT_COORDINATES",
    output_bounds_status = "BOUNDS_NOT_APPLICABLE",
    current_standard_name_status = "DERIVATION_PENDING",
    current_cell_methods_status = "DERIVATION_PENDING_NO_SYNTHETIC_CLAIM",
    certification_status = "CERTIFIED",
    resolved_method = resolved_method
  )
}

.vertical_sampling_plan <- function(x, depth, method) {
  vertical <- .vertical_sampling_validate_request(x, depth)
  semantics <- .vertical_value_semantics(x)
  methods <- .vertical_sampling_methods(semantics, method)
  cell <- if (any(methods == "cell")) {
    .vertical_sampling_cell_targets(x, depth)
  } else {
    NULL
  }
  linear <- if (any(methods == "linear")) {
    .vertical_sampling_linear_targets(x, depth, vertical)
  } else {
    NULL
  }
  variables <- lapply(seq_along(x$vars), function(i) {
    variable <- x$vars[[i]]
    resolved <- methods[[variable]]
    target_plan <- if (identical(resolved, "cell")) cell$targets else linear$targets
    list(
      variable = variable,
      input_value_semantics = semantics[[variable]]$status,
      resolved_method = resolved,
      output_value_semantics = if (identical(resolved, "cell")) {
        "SAMPLED_CELL_MEAN_RECONSTRUCTION"
      } else {
        "INTERPOLATED_POINT_VALUE"
      },
      scientific_method_id = if (identical(resolved, "cell")) {
        "oceancube:vertical_cell_mean_sampling"
      } else {
        "oceancube:vertical_linear_point_interpolation"
      },
      targets = target_plan,
      status = "CERTIFIED"
    )
  })
  names(variables) <- x$vars
  required <- sort(unique(unlist(lapply(variables, function(variable) {
    unlist(lapply(variable$targets, function(target) {
      if (identical(target$status, "LINEAR_INTERPOLATED")) {
        c(target$lower_index, target$upper_index)
      } else {
        target$source_index
      }
    }), use.names = FALSE)
  }), use.names = FALSE)))
  resolved_method <- if (length(unique(methods)) == 1L) {
    unname(methods[[1L]])
  } else {
    "mixed"
  }
  descriptor <- .vertical_sampling_descriptor(
    depth, method, vertical, variables, resolved_method
  )
  list(
    requested_depths = as.numeric(depth),
    requested_unit = vertical$units_raw,
    positive = vertical$positive,
    source_order = vertical$source_order,
    variables = variables,
    required_indices = as.integer(required),
    resolved_method = resolved_method,
    gap_policy = descriptor$gap_policy,
    boundary_policy = descriptor$boundary_policy,
    missing_value_policy = descriptor$missing_value_policy,
    descriptor = descriptor
  )
}
