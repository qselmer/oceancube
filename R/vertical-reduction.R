.vertical_cell_method_pairs <- function(value) {
  if (is.null(value)) return(data.frame(axis = character(), method = character()))
  text <- as.character(value)
  if (length(text) != 1L || is.na(text) || !nzchar(trimws(text))) return(NULL)
  pattern <- paste0(
    "[A-Za-z_][A-Za-z0-9_./-]*[[:space:]]*:[[:space:]]*",
    "[A-Za-z_][A-Za-z0-9_]*"
  )
  hits <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1L]]
  if (!length(hits) || identical(hits, "")) {
    return(data.frame(axis = character(), method = character()))
  }
  data.frame(
    axis = sub("[[:space:]]*:.*$", "", hits),
    method = tolower(sub("^.*:[[:space:]]*", "", hits)),
    stringsAsFactors = FALSE
  )
}

.vertical_current_source_variable <- function(x, variable) {
  metadata <- x$metadata %||% NULL
  if (is.null(metadata)) return(NULL)
  current <- metadata$cf$current$variables
  candidates <- current[
    current == variable |
      vapply(current, .cf_basename, character(1L)) == variable
  ]
  if (length(candidates) != 1L) return(NULL)
  metadata$cf$source$variables$map[[candidates[[1L]]]]
}

.vertical_value_semantics <- function(x) {
  vertical <- x$metadata$cf$current$vertical %||% NULL
  axis_id <- if (is.null(vertical)) NA_character_ else vertical$source_axis_id
  axis_names <- if (length(axis_id) == 1L && !is.na(axis_id)) {
    unique(c(axis_id, .cf_basename(axis_id)))
  } else {
    character()
  }
  out <- lapply(x$vars, function(variable) {
    record <- .vertical_current_source_variable(x, variable)
    raw <- if (is.null(record)) NULL else {
      .cf_attribute_value(record$attributes, "cell_methods")
    }
    pairs <- .vertical_cell_method_pairs(raw)
    status <- "VERTICAL_SEMANTICS_UNRESOLVED"
    recognized <- NA_character_
    reason <- "current CF vertical axis or source variable metadata is unresolved"
    if (length(axis_names) && !is.null(record)) {
      if (is.null(pairs)) {
        status <- "VERTICAL_CELL_METHOD_AMBIGUOUS"
        reason <- "cell_methods is not a single non-missing character value"
      } else {
        selected <- pairs[pairs$axis %in% axis_names, , drop = FALSE]
        methods <- unique(selected$method)
        if (!nrow(selected)) {
          status <- "VERTICAL_POINT"
          reason <- "no cell method is declared for the exact vertical axis"
        } else if (length(methods) != 1L) {
          status <- "VERTICAL_CELL_METHOD_AMBIGUOUS"
          reason <- "multiple vertical cell methods are declared"
        } else {
          recognized <- methods[[1L]]
          status <- switch(
            recognized,
            mean = "VERTICAL_CELL_MEAN",
            sum = "VERTICAL_CELL_SUM",
            point = "VERTICAL_POINT",
            "VERTICAL_CELL_OTHER"
          )
          reason <- paste0("exact vertical cell method is `", recognized, "`")
        }
      }
    }
    list(
      variable = variable,
      vertical_axis_id = axis_id,
      cell_methods_raw = if (is.null(raw)) NA_character_ else as.character(raw),
      recognized_method = recognized,
      status = status,
      eligible_for_metric_integration = identical(status, "VERTICAL_CELL_MEAN"),
      reason = reason
    )
  })
  names(out) <- x$vars
  out
}

.vertical_units_values <- function(x) {
  if (is.null(x$units) || length(x$units) != length(x$vars)) return(NULL)
  raw <- if (is.null(names(x$units))) x$units else x$units[x$vars]
  values <- vapply(raw, function(unit) {
    if (length(unit) != 1L || is.na(unit) || !nzchar(trimws(as.character(unit)))) {
      NA_character_
    } else {
      trimws(as.character(unit))
    }
  }, character(1L))
  stats::setNames(values, x$vars)
}

.vertical_integral_units <- function(x) {
  source <- .vertical_units_values(x)
  if (is.null(source) || anyNA(source)) {
    failed <- if (is.null(source)) x$vars else names(source)[is.na(source)]
    rlang::abort(
      paste0(
        "Certified vertical integration requires a declared source unit for: ",
        paste(failed, collapse = ", "), "."
      ),
      class = "oceancube_vertical_unit_semantics_unsupported"
    )
  }
  derived <- ifelse(source == "1", "m", paste(source, "m"))
  stats::setNames(as.list(unname(derived)), names(derived))
}

.vertical_integral_value <- function(values, weights_m) {
  positive <- weights_m > 0
  if (!any(positive)) return(NA_real_)
  if (any(!is.finite(values[positive]))) return(NA_real_)
  sum(values[positive] * weights_m[positive])
}

.vertical_reduction_descriptor <- function(
    method, semantics, support, units = NULL) {
  value_certified <- length(semantics) > 0L && all(vapply(
    semantics,
    function(item) identical(item$status, "VERTICAL_CELL_MEAN"),
    logical(1L)
  ))
  geometry_certified <- identical(support$certification_status, "CERTIFIED")
  list(
    schema_name = "oceancube_vertical_reduction",
    schema_version = "1.0.0",
    method = if (identical(method, "mean")) "overlap_weighted_mean" else {
      "metric_cell_mean_integral"
    },
    input_value_semantics = unname(semantics),
    geometric_support = support$support_basis,
    integration_measure = if (identical(method, "integral")) "vertical_length" else {
      "not_applicable"
    },
    integration_unit = if (identical(method, "integral")) "m" else NA_character_,
    subcell_assumption = if (identical(support$mode, "explicit_bounds")) {
      "piecewise_constant_cell_mean"
    } else {
      "legacy_centre_derived_support"
    },
    coverage_policy = if (identical(support$mode, "explicit_bounds")) {
      "full geometric coverage required; zero coverage returns NA; gaps are not filled"
    } else {
      "legacy coverage not certified"
    },
    missing_value_policy = if (identical(method, "integral")) {
      "strict: any non-finite positive-overlap contributor returns NA"
    } else {
      "finite contributors are renormalized over positive overlap"
    },
    derived_units = if (identical(method, "integral")) units %||% list() else list(),
    unit_status = if (identical(method, "integral")) {
      "SYMBOLIC_UNNORMALIZED_UNVALIDATED"
    } else {
      "UNCHANGED"
    },
    current_standard_name_status = "DERIVATION_PENDING",
    cf_cell_method_status = "DERIVATION_PENDING_NO_SYNTHETIC_VERTICAL_SUM",
    geometric_certification = if (geometry_certified) "CERTIFIED" else "UNCERTIFIED",
    value_semantic_certification = if (value_certified) "CERTIFIED" else "UNCERTIFIED",
    certification_status = if (geometry_certified && value_certified) {
      "CERTIFIED"
    } else {
      "UNCERTIFIED"
    }
  )
}

.vertical_validate_reduction_request <- function(x, depth) {
  .check_cube(x)
  .check_numeric_vector(depth, "depth")
  if (length(x$depth) < 2L || all(is.na(x$depth))) {
    rlang::abort("`x` must contain at least two valid depth levels.")
  }
  if (length(depth) < 2L) {
    .abort_badarg("depth", "must contain at least two values.")
  }
  if (any(!is.finite(depth)) || any(diff(depth) <= 0)) {
    .abort_badarg(
      "depth",
      "must be sorted increasingly and contain only finite, distinct values."
    )
  }
  lapply(seq_len(length(depth) - 1L), function(i) depth[c(i, i + 1L)])
}

.vertical_reduce <- function(x, depth, method = c("mean", "integral")) {
  method <- match.arg(method)
  bins <- .vertical_validate_reduction_request(x, depth)
  layer_depth <- vapply(bins, mean, numeric(1L))
  support <- .vertical_support_engine(
    x,
    mode = if (identical(method, "mean")) "layer_mean" else "integral"
  )
  resolved <- .vertical_resolve_bins(support, bins)
  semantics <- .vertical_value_semantics(x)
  units <- x$units
  if (identical(method, "integral")) {
    failed <- names(semantics)[!vapply(
      semantics,
      function(item) isTRUE(item$eligible_for_metric_integration),
      logical(1L)
    )]
    if (length(failed)) {
      details <- vapply(semantics[failed], function(item) {
        paste0(item$variable, "=", item$status)
      }, character(1L))
      rlang::abort(
        paste0(
          "Certified vertical integration requires CF vertical cell means; incompatible variables: ",
          paste(details, collapse = ", "), "."
        ),
        class = "oceancube_vertical_value_semantics_unsupported"
      )
    }
    units <- .vertical_integral_units(x)
  }
  descriptor <- .vertical_reduction_descriptor(method, semantics, support, units)

  d <- unname(.cube_shape(x))
  out <- array(NA_real_, dim = c(d[1L], d[2L], length(bins), d[4L], d[5L]))
  contributing <- lapply(resolved$weights, function(weight) which(weight > 0))
  read_index <- sort(unique(unlist(contributing, use.names = FALSE)))
  if (length(read_index)) {
    layer_values <- .cube_read(x, index = list(depth = read_index))
    for (b in seq_along(bins)) {
      idx <- contributing[[b]]
      if (!length(idx)) next
      local_idx <- match(idx, read_index)
      weights_native <- resolved$weights[[b]][idx]
      weights <- if (identical(method, "integral")) {
        weights_native * support$scale_to_m
      } else {
        weights_native
      }
      reducer <- if (identical(method, "integral")) {
        function(values) .vertical_integral_value(values, weights)
      } else {
        function(values) .weighted_mean(values, weights)
      }
      for (k in seq_len(d[5L])) {
        values <- layer_values[, , local_idx, , k, drop = FALSE]
        values <- array(values, dim = dim(values)[1:4])
        out[, , b, , k] <- apply(values, c(1, 2, 4), reducer)
      }
    }
  }

  operation <- if (identical(method, "mean")) "layer_mean" else "layer_integral"
  output_shape <- stats::setNames(
    as.integer(c(d[1L], d[2L], length(bins), d[4L], d[5L])),
    .cube_axis_names()
  )
  provenance_context <- .provenance_cube_context(
    source = x$source, dataset_id = x$dataset_id, time = x$time,
    shape = output_shape, variables = x$vars, backend = "memory",
    provenance = x$provenance
  )
  source_bounds <- if (identical(support$mode, "explicit_bounds")) {
    lapply(seq_len(nrow(support$bounds)), function(i) as.numeric(support$bounds[i, ]))
  } else {
    list()
  }
  metre_weights <- if (identical(support$mode, "explicit_bounds")) {
    lapply(resolved$weights, function(weight) as.numeric(weight * support$scale_to_m))
  } else {
    list()
  }
  provenance <- .provenance_append(
    x$provenance,
    operation = operation,
    parameters = list(
      requested = list(depth = depth),
      resolved = list(
        layer_ranges = bins,
        n_layers = as.integer(length(bins)),
        layer_centers = layer_depth,
        depth_representation = "arithmetic midpoint of requested layer bounds",
        support_basis = support$support_basis,
        weight_basis = if (identical(support$mode, "explicit_bounds")) {
          "exact interval overlap against explicit cell bounds"
        } else {
          "legacy centre-derived edges via .depth_edges/.depth_weights"
        },
        source_bounds = source_bounds,
        bounds_source = support$bounds_source,
        overlap_weights_source_unit = lapply(resolved$weights, as.numeric),
        overlap_weights = lapply(resolved$weights, as.numeric),
        overlap_weights_m = metre_weights,
        coverage_fraction = resolved$coverage_fraction,
        coverage_status = as.character(resolved$coverage_status),
        coverage_tolerance = resolved$tolerance,
        coverage_policy = descriptor$coverage_policy,
        requested_depth_unit = support$depth_unit %||% support$unit %||% NA_character_,
        source_bounds_unit = support$unit %||% NA_character_,
        vertical_unit = support$depth_unit %||% support$unit %||% NA_character_,
        integration_unit = descriptor$integration_unit,
        contributing_depth_cells = lapply(contributing, as.integer),
        source_depth_centers = as.numeric(x$depth),
        input_value_semantics = unname(semantics),
        subcell_assumption = descriptor$subcell_assumption,
        missing_value_policy = descriptor$missing_value_policy,
        derived_units = descriptor$derived_units,
        unit_status = descriptor$unit_status,
        geometric_certification = descriptor$geometric_certification,
        value_semantic_certification = descriptor$value_semantic_certification,
        vertical_certification = descriptor$certification_status,
        output_shape = output_shape
      )
    ),
    output = .provenance_summary(provenance_context),
    scientific_method = .provenance_method(operation, list()),
    context = provenance_context
  )
  result <- ocean_cube(
    lon = x$lon, lat = x$lat, depth = layer_depth, time = x$time,
    vars = x$vars, data = out, units = units, source = x$source,
    dataset_id = x[["dataset_id"]], spatial_extent = x$spatial_extent,
    temporal_extent = x$temporal_extent, depth_extent = range(depth),
    mask = x$mask, dc = x$dc, provenance = provenance
  )
  if (identical(support$mode, "explicit_bounds")) {
    output_bounds <- do.call(rbind, bins)
    attr(output_bounds, "units") <- support$depth_unit %||% support$unit
    attr(output_bounds, "positive") <- support$positive
    attr(result$depth, "bounds") <- output_bounds
    attr(result$depth, "units") <- support$depth_unit %||% support$unit
    attr(result$depth, "positive") <- support$positive
  }
  .attach_cube_metadata(
    result,
    .cf_metadata_for_vertical_reduction(
      x$metadata %||% NULL,
      operation = operation,
      bins = bins,
      centers = layer_depth,
      certified_geometry = identical(support$certification_status, "CERTIFIED"),
      descriptor = descriptor
    )
  )
}
