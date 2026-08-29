#' Select discrete positions or coordinate values from an ocean cube
#'
#' `cube_slice()` selects discrete cells from an `<ocean_cube>` either by
#' one-based array position or by stored coordinate value. It materializes only
#' the requested selection as an independent memory-backed cube.
#'
#' @param x A valid `<ocean_cube>` using the memory or NetCDF backend.
#' @param longitude,latitude,depth,time,variable Optional selectors. `NULL`
#'   keeps the complete axis. Calendar-aware time accepts compatible
#'   `oceancube_cf_time` values or calendar-valid character dates.
#' @param by Whether selectors are coordinate `"value"`s or one-based
#'   `"index"` positions.
#' @param match Coordinate matching method. `"exact"` requires a stored value;
#'   `"nearest"` selects the closest stored coordinate within the cube domain.
#'   Variable names are always matched exactly.
#' @param tolerance Optional fully named list of maximum nearest-neighbour
#'   distances. Numeric scalar tolerances are used for `longitude`, `latitude`,
#'   and `depth`; `time` requires a scalar `difftime`. Equality with the
#'   tolerance is accepted.
#'
#' @return An `<ocean_cube>` using the memory backend and retaining all five
#'   dimensions.
#'
#' @details
#' `by = "index"` removes the ambiguity between a coordinate such as longitude
#' 2 and the second longitude position. In this mode `match` and `tolerance`
#' must not be supplied.
#'
#' Exact numeric matching uses the values stored in the coordinate vector,
#' without an implicit floating-point tolerance. Nearest-neighbour matching is
#' independent on each axis, chooses the earlier instant on temporal ties, and rejects
#' requests outside the stored axis range. It is not interpolation and does not
#' alter scientific values.
#' Calendar-aware exact and nearest matching uses the same-calendar ordinal and
#' sub-day metric; cross-calendar comparisons are rejected.
#'
#' Requested order and repeated spatial or depth coordinates are preserved.
#' Resolved time coordinates must remain unique and strictly increasing because
#' the result is itself a canonical cube. Variable names remain unique, so
#' repeated variable selections are rejected.
#'
#' The result is always materialized in memory, but only after all selectors
#' have been resolved and only through one indexed `.cube_read()` call.
#' Consequently memory use is proportional to the requested selection, not
#' necessarily to the complete source cube. `cube_slice()` selects discrete
#' points; it is not a range-based crop or an event extraction operation.
#'
#' @seealso [cube_collect()]
#' @export
#'
#' @examples
#' values <- array(seq_len(3 * 1 * 1 * 2 * 1), dim = c(3, 1, 1, 2, 1))
#' cube <- ocean_cube(
#'   lon = c(-80, -79, -78),
#'   lat = -12,
#'   depth = 0,
#'   time = as.Date(c("2020-01-01", "2020-02-01")),
#'   vars = "temperature",
#'   data = values
#' )
#' cube_slice(cube, longitude = c(-78, -80), by = "value")
#' cube_slice(cube, longitude = c(3L, 1L), by = "index")
#' cube_slice(
#'   cube,
#'   longitude = -79.4,
#'   by = "value",
#'   match = "nearest",
#'   tolerance = list(longitude = 0.5)
#' )
cube_slice <- function(x, longitude = NULL, latitude = NULL, depth = NULL,
                       time = NULL, variable = NULL,
                       by = c("value", "index"),
                       match = c("exact", "nearest"),
                       tolerance = NULL) {
  match_was_missing <- missing(match)
  .check_cube(x)
  backend_from <- .cube_backend(x)
  if (!backend_from %in% c("memory", "netcdf")) {
    rlang::abort(
      paste0("Unsupported ocean_cube backend: '", backend_from, "'."),
      class = "oceancube_unsupported_backend"
    )
  }

  by <- base::match.arg(by)
  method <- base::match.arg(match)
  if (identical(by, "index")) {
    if (!isTRUE(match_was_missing) || !is.null(tolerance)) {
      .abort_badarg(
        "by",
        paste(
          "`by = \"index\"` accepts positional selectors only;",
          "do not supply `match` or `tolerance`."
        )
      )
    }
    method <- "index"
  } else if (identical(method, "exact") && !is.null(tolerance)) {
    .abort_badarg(
      "tolerance",
      "is only available with `by = \"value\", match = \"nearest\"`."
    )
  }

  selectors <- list(
    longitude = longitude,
    latitude = latitude,
    depth = depth,
    time = time,
    variable = variable
  )
  resolved <- .resolve_cube_slice(
    x,
    selectors = selectors,
    by = by,
    method = method,
    tolerance = tolerance
  )
  .validate_time_axis(
    x$time[resolved$index$time],
    arg = "resolved time selection"
  )

  read_plan <- if (identical(backend_from, "netcdf")) {
    .plan_cube_index_read(x, resolved$index)
  } else {
    NULL
  }
  values <- .cube_read(x, index = resolved$index)
  auxiliary <- .selection_auxiliary_components(x, resolved$index)
  selected_units <- .selection_units(
    x$units,
    x$vars,
    resolved$index$variable
  )

  read_diagnostics <- if (is.null(read_plan)) {
    NULL
  } else {
    list(
      source_file = x$storage$file$normalized_path,
      physical_start = read_plan$physical_start,
      physical_count = read_plan$physical_count,
      variables = x$vars[read_plan$variable_index],
      values_requested = read_plan$values_requested,
      values_in_envelope = read_plan$values_in_envelope,
      amplification = read_plan$amplification
    )
  }
  if (is.null(auxiliary$qa)) auxiliary$qa <- list()
  if (!is.list(auxiliary$qa)) auxiliary$qa <- list(previous = auxiliary$qa)
  auxiliary$qa$selection <- list(
    resolved_indices = resolved$index,
    matched_coordinates = resolved$matched,
    distances = resolved$distance,
    netcdf_read = read_diagnostics,
    discarded_components = auxiliary$discarded
  )
  output_shape <- stats::setNames(as.integer(dim(values)), .cube_axis_names())
  selected_time <- x$time[resolved$index$time]
  provenance_context <- .provenance_cube_context(
    source = x$source,
    dataset_id = x$dataset_id,
    time = selected_time,
    shape = output_shape,
    variables = x$vars[resolved$index$variable],
    backend = "memory",
    provenance = x$provenance
  )
  provenance <- .provenance_append(
    x$provenance,
    operation = "cube_slice",
    parameters = list(
      requested = list(
        selectors = .provenance_compact(resolved$requested),
        by = by,
        match = method,
        tolerance = .provenance_compact(resolved$tolerance)
      ),
      resolved = list(
        selected_counts = vapply(resolved$index, length, integer(1)),
        selected_variables = x$vars[resolved$index$variable],
        output_shape = output_shape,
        discarded_components = auxiliary$discarded
      )
    ),
    output = .provenance_summary(provenance_context),
    scientific_method = .provenance_method("cube_slice", list()),
    context = provenance_context
  )

  .new_selected_memory_cube(
    x = x,
    data = values,
    index = resolved$index,
    units = selected_units,
    auxiliary = auxiliary,
    provenance = provenance
  )
}

.resolve_cube_slice <- function(x, selectors, by, method, tolerance,
                                allow_variable_duplicates = FALSE) {
  axes <- .cube_axis_names()
  coordinates <- list(
    longitude = x$lon,
    latitude = x$lat,
    depth = x$depth,
    time = x$time,
    variable = x$vars
  )

  if (identical(by, "index")) {
    supplied <- selectors[!vapply(selectors, is.null, logical(1))]
    index <- .validate_cube_index(supplied, unname(.cube_shape(x)))
    if (!isTRUE(allow_variable_duplicates) && anyDuplicated(index$variable)) {
      .abort_badarg(
        "variable",
        paste(
          "repeated variable positions would violate the unique-variable",
          "<ocean_cube> contract; select each variable once."
        )
      )
    }
    return(list(
      index = index,
      requested = selectors,
      matched = Map(`[`, coordinates, index),
      distance = stats::setNames(vector("list", length(axes)), axes),
      tolerance = NULL
    ))
  }

  validated_tolerance <- .validate_slice_tolerance(tolerance, selectors)
  index <- stats::setNames(vector("list", length(axes)), axes)
  matched <- stats::setNames(vector("list", length(axes)), axes)
  distance <- stats::setNames(vector("list", length(axes)), axes)

  for (axis in axes) {
    requested <- selectors[[axis]]
    if (is.null(requested)) {
      index[[axis]] <- seq_along(coordinates[[axis]])
      matched[[axis]] <- NULL
      distance[[axis]] <- NULL
      next
    }

    result <- if (identical(axis, "variable")) {
      .resolve_selected_variables(
        coordinates[[axis]],
        requested,
        allow_duplicates = allow_variable_duplicates
      )
    } else if (identical(axis, "time")) {
      .resolve_time_values(
        coordinates[[axis]],
        requested,
        method,
        validated_tolerance[[axis]]
      )
    } else {
      .resolve_axis_values(
        coordinates[[axis]],
        requested,
        axis,
        method,
        validated_tolerance[[axis]]
      )
    }
    index[[axis]] <- result$index
    matched[[axis]] <- result$matched
    distance[[axis]] <- result$distance
  }

  list(
    index = .validate_cube_index(index, unname(.cube_shape(x))),
    requested = selectors,
    matched = matched,
    distance = distance,
    tolerance = validated_tolerance
  )
}

.validate_slice_selector_vector <- function(x, axis, type) {
  if (!is.null(dim(x))) {
    .abort_badarg(axis, paste0("must be a ", type, " vector, not an array or matrix."))
  }
  if (length(x) == 0L) {
    .abort_badarg(axis, "must not be empty.")
  }
  invisible(TRUE)
}

.slice_format_values <- function(x) {
  formatted <- if (!is.na(.time_class(x))) {
    .time_format(x)
  } else {
    format(x, trim = TRUE, usetz = TRUE)
  }
  paste(formatted, collapse = ", ")
}

.slice_outside_domain <- function(axis, value, domain) {
  rlang::abort(
    paste0(
      "Requested ", axis, " ", .slice_format_values(value),
      " is outside the cube domain [",
      .slice_format_values(domain[[1L]]), ", ",
      .slice_format_values(domain[[2L]]),
      "]. Use a value inside the domain or `by = \"index\"`."
    ),
    class = "oceancube_slice_outside_domain"
  )
}

.slice_not_found <- function(axis, requested) {
  rlang::abort(
    paste0(
      "Exact ", axis, " value(s) not found: ",
      .slice_format_values(requested),
      ". Use a stored coordinate or `match = \"nearest\"`."
    ),
    class = "oceancube_slice_not_found"
  )
}

.slice_beyond_tolerance <- function(axis, requested, distance, tolerance) {
  rlang::abort(
    paste0(
      "Nearest ", axis, " for ", .slice_format_values(requested),
      " is at distance ", .slice_format_values(distance),
      ", exceeding tolerance ", .slice_format_values(tolerance), "."
    ),
    class = "oceancube_slice_tolerance"
  )
}

.resolve_axis_values <- function(axis_values, requested, axis, method,
                                 tolerance = NULL) {
  .validate_slice_selector_vector(requested, axis, "numeric")
  if (!is.numeric(requested)) {
    .abort_badarg(axis, "must be numeric when `by = \"value\"`.")
  }

  is_surface <- identical(axis, "depth") &&
    length(axis_values) == 1L &&
    is.na(axis_values[[1L]]) &&
    !is.nan(axis_values[[1L]])
  if (is_surface) {
    if (identical(method, "exact") &&
        all(is.na(requested) & !is.nan(requested))) {
      return(list(
        index = rep.int(1L, length(requested)),
        matched = rep(NA_real_, length(requested)),
        distance = rep(0, length(requested))
      ))
    }
    .abort_badarg(
      "depth",
      paste(
        "this is a surface cube with `depth = NA_real_`;",
        "use `depth = NA_real_`, `match = \"exact\"`, or `by = \"index\"`."
      )
    )
  }
  if (anyNA(requested) || any(!is.finite(requested))) {
    .abort_badarg(axis, "must contain finite, non-missing coordinate values.")
  }

  if (identical(method, "exact")) {
    index <- base::match(requested, axis_values, nomatch = 0L)
    missing <- requested[index == 0L]
    if (length(missing) > 0L) .slice_not_found(axis, missing)
    return(list(
      index = as.integer(index),
      matched = axis_values[index],
      distance = rep(0, length(index))
    ))
  }

  domain <- range(axis_values)
  index <- integer(length(requested))
  distances <- numeric(length(requested))
  for (i in seq_along(requested)) {
    if (requested[[i]] < domain[[1L]] || requested[[i]] > domain[[2L]]) {
      .slice_outside_domain(axis, requested[[i]], domain)
    }
    delta <- abs(axis_values - requested[[i]])
    index[[i]] <- which.min(delta)
    distances[[i]] <- delta[[index[[i]]]]
    if (!is.null(tolerance) && distances[[i]] > tolerance) {
      .slice_beyond_tolerance(axis, requested[[i]], distances[[i]], tolerance)
    }
  }
  list(
    index = as.integer(index),
    matched = axis_values[index],
    distance = distances
  )
}

.slice_time_numeric <- function(x) {
  if (!inherits(x, c("Date", "POSIXct", "oceancube_cf_time"))) {
    rlang::abort(
      paste(
        "The cube time coordinate cannot be matched safely as Date, POSIXct,",
        "or oceancube_cf_time; use `by = \"index\"`."
      ),
      class = "oceancube_slice_time"
    )
  }
  .time_key(x)
}

.resolve_time_values <- function(axis_values, requested, method,
                                 tolerance = NULL) {
  .validate_slice_selector_vector(requested, "time", "temporal")
  requested <- .time_parse_selector(requested, axis_values, arg = "time")
  expected_class <- .time_class(axis_values)
  if (!inherits(requested, expected_class) ||
      !.time_compatible(requested, axis_values)) {
    .abort_badarg(
      "time",
      paste0(
        "must inherit from ", expected_class,
        " to match this cube without an implicit temporal conversion;",
        " use the same class or `by = \"index\"`."
      )
    )
  }
  if (anyNA(requested)) {
    .abort_badarg("time", "must not contain missing values.")
  }

  axis_numeric <- .slice_time_numeric(axis_values)
  requested_numeric <- .slice_time_numeric(requested)
  if (any(!is.finite(requested_numeric))) {
    .abort_badarg("time", "must contain finite temporal values.")
  }

  if (identical(method, "exact")) {
    index <- base::match(requested_numeric, axis_numeric, nomatch = 0L)
    missing <- requested[index == 0L]
    if (length(missing) > 0L) .slice_not_found("time", missing)
    return(list(
      index = as.integer(index),
      matched = axis_values[index],
      distance = as.difftime(rep(0, length(index)), units = "secs")
    ))
  }

  domain_numeric <- range(axis_numeric)
  index <- integer(length(requested))
  distance_seconds <- numeric(length(requested))
  tolerance_seconds <- if (is.null(tolerance)) {
    NULL
  } else {
    as.numeric(tolerance, units = "secs")
  }
  for (i in seq_along(requested_numeric)) {
    if (requested_numeric[[i]] < domain_numeric[[1L]] ||
        requested_numeric[[i]] > domain_numeric[[2L]]) {
      .slice_outside_domain("time", requested[[i]], range(axis_values))
    }
    delta <- abs(axis_numeric - requested_numeric[[i]])
    minimum <- min(delta)
    tied <- which(delta == minimum)
    index[[i]] <- tied[[which.min(axis_numeric[tied])]]
    distance_seconds[[i]] <- delta[[index[[i]]]]
    if (!is.null(tolerance_seconds) &&
        distance_seconds[[i]] > tolerance_seconds) {
      .slice_beyond_tolerance(
        "time",
        requested[[i]],
        as.difftime(distance_seconds[[i]], units = "secs"),
        tolerance
      )
    }
  }
  list(
    index = as.integer(index),
    matched = axis_values[index],
    distance = as.difftime(distance_seconds, units = "secs")
  )
}

.resolve_selected_variables <- function(axis_values, requested,
                                         allow_duplicates = FALSE) {
  .validate_slice_selector_vector(requested, "variable", "character")
  if (!is.character(requested)) {
    .abort_badarg("variable", "must be a character vector when `by = \"value\"`.")
  }
  if (anyNA(requested) || any(!nzchar(requested))) {
    .abort_badarg("variable", "must contain non-empty, non-missing names.")
  }
  if (!isTRUE(allow_duplicates) && anyDuplicated(requested)) {
    .abort_badarg(
      "variable",
      paste(
        "must not contain duplicates because variable names are unique in",
        "an <ocean_cube>."
      )
    )
  }
  index <- base::match(requested, axis_values, nomatch = 0L)
  missing <- requested[index == 0L]
  if (length(missing) > 0L) {
    rlang::abort(
      paste0(
        "Unknown variable name(s): ",
        paste0("`", missing, "`", collapse = ", "),
        ". Available variables: ",
        paste0("`", axis_values, "`", collapse = ", "),
        "."
      ),
      class = "oceancube_slice_variable"
    )
  }
  list(
    index = as.integer(index),
    matched = axis_values[index],
    distance = rep(0, length(index))
  )
}

.validate_slice_tolerance <- function(tolerance, selectors) {
  if (is.null(tolerance)) return(NULL)
  if (!is.list(tolerance)) {
    .abort_badarg(
      "tolerance",
      "must be NULL or a fully named list for nearest matching."
    )
  }
  tolerance_names <- names(tolerance)
  if (length(tolerance) == 0L ||
      is.null(tolerance_names) ||
      anyNA(tolerance_names) ||
      any(!nzchar(tolerance_names))) {
    .abort_badarg("tolerance", "must be a non-empty, fully named list.")
  }
  if (anyDuplicated(tolerance_names)) {
    .abort_badarg("tolerance", "must not contain duplicate axis names.")
  }
  allowed <- c("longitude", "latitude", "depth", "time")
  unknown <- setdiff(tolerance_names, allowed)
  if (length(unknown) > 0L) {
    .abort_badarg(
      "tolerance",
      paste0(
        "unknown or unsupported axis name(s): ",
        paste(unknown, collapse = ", "),
        "; `variable` never uses nearest matching."
      )
    )
  }
  selected <- names(selectors)[!vapply(selectors, is.null, logical(1))]
  unused <- setdiff(tolerance_names, selected)
  if (length(unused) > 0L) {
    .abort_badarg(
      "tolerance",
      paste0(
        "was supplied for unselected axis/axes: ",
        paste(unused, collapse = ", "),
        ". Supply a selector for each tolerated axis."
      )
    )
  }

  for (axis in tolerance_names) {
    value <- tolerance[[axis]]
    if (identical(axis, "time")) {
      if (!inherits(value, "difftime") ||
          length(value) != 1L ||
          is.na(value) ||
          !is.finite(as.numeric(value, units = "secs")) ||
          as.numeric(value, units = "secs") < 0) {
        .abort_badarg(
          "tolerance$time",
          "must be one finite, non-negative `difftime` value."
        )
      }
    } else if (!is.numeric(value) ||
               length(value) != 1L ||
               is.na(value) ||
               !is.finite(value) ||
               value < 0) {
      .abort_badarg(
        paste0("tolerance$", axis),
        "must be one finite, non-negative numeric value."
      )
    }
  }
  tolerance
}

.selection_units <- function(units, variables, variable_index) {
  if (is.null(units)) return(NULL)
  if (is.null(names(units))) {
    return(units[variable_index])
  }
  units[variables[variable_index]]
}

.selection_contains_dimensional_data <- function(x) {
  if (!is.null(dim(x))) return(TRUE)
  if (!is.list(x)) return(FALSE)
  any(vapply(x, .selection_contains_dimensional_data, logical(1)))
}

.selection_auxiliary_components <- function(x, index) {
  discarded <- character()

  dc <- NULL
  if (!is.null(x$dc)) {
    expected <- c(length(x$lon), length(x$lat))
    if (is.matrix(x$dc) && identical(unname(dim(x$dc)), as.integer(expected))) {
      dc <- x$dc[index$longitude, index$latitude, drop = FALSE]
    } else {
      discarded <- c(discarded, "dc")
    }
  }

  mask <- NULL
  if (!is.null(x$mask)) {
    expected <- c(length(x$lon), length(x$lat), length(x$depth))
    if (inherits(x$mask, "ocean_mask") &&
        is.array(x$mask$mask) &&
        identical(unname(dim(x$mask$mask)), as.integer(expected))) {
      mask <- x$mask
      mask$mask <- x$mask$mask[
        index$longitude,
        index$latitude,
        index$depth,
        drop = FALSE
      ]
      mask$lon <- x$lon[index$longitude]
      mask$lat <- x$lat[index$latitude]
      mask$depth <- x$depth[index$depth]
    } else {
      discarded <- c(discarded, "mask")
    }
  }

  if (!is.null(x$climatology)) discarded <- c(discarded, "climatology")
  if (!is.null(x$anomaly)) discarded <- c(discarded, "anomaly")

  qa <- x$qa
  if (!is.null(qa) && .selection_contains_dimensional_data(qa)) {
    qa <- NULL
    discarded <- c(discarded, "qa")
  }

  list(
    dc = dc,
    mask = mask,
    climatology = NULL,
    anomaly = NULL,
    qa = qa,
    discarded = unique(discarded)
  )
}
