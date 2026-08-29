#' Crop an ocean cube with closed coordinate ranges
#'
#' `cube_crop()` retains every stored cell centre inside closed coordinate
#' intervals. It materializes only the resulting selection as an independent
#' memory-backed `<ocean_cube>`.
#'
#' @param x A valid `<ocean_cube>` using the memory or NetCDF backend.
#' @param longitude,latitude,depth Optional numeric ranges `c(lower, upper)`.
#'   `NULL` keeps the complete axis.
#' @param time Optional two-value temporal range compatible with `x$time`.
#'   Calendar-aware axes accept compatible `oceancube_cf_time` values or
#'   calendar-valid character dates. POSIXct limits match elapsed instants and
#'   may use a display timezone other than the cube's canonical UTC.
#' @param variable Optional exact variable names. `NULL` keeps all variables.
#' @param bbox Optional numeric vector named exactly `xmin`, `ymin`, `xmax`,
#'   and `ymax`. It is an alternative to `longitude` and `latitude`.
#' @param outside How ranges extending beyond the cube domain are handled.
#'   `"error"` rejects them; `"clip"` intersects them with the domain.
#'
#' @return An `<ocean_cube>` using the memory backend and retaining all five
#'   dimensions.
#'
#' @details
#' Ranges are closed: cell centres exactly equal to either limit are included.
#' Limits must be supplied as `c(lower, upper)`. The public API retains this
#' two-limit contract; the shared internal time resolver also supports one open
#' bound for bounded downstream workflows. A range that
#' intersects the numeric domain but contains no stored centre is an error.
#' No nearest-neighbour adjustment, interpolation, cell-bound calculation, or
#' coordinate generation is performed.
#'
#' `bbox` is interpreted in the units and longitude convention already used by
#' `x$lon` and `x$lat`. General `sf` geometries, `sf::st_bbox()` objects,
#' reprojection, longitude conversion, polygons, and boxes crossing the
#' antimeridian are outside this initial contract.
#'
#' The result is always materialized in memory through one indexed
#' `.cube_read()` call. For NetCDF inputs, the backend reads only the resolved
#' block and requested variables, so memory use is proportional to the crop
#' rather than necessarily to the complete source cube.
#'
#' `cube_crop()` selects intervals of stored centres. Use [cube_slice()] for
#' explicit coordinate values or positional indices.
#'
#' @seealso [cube_slice()], [cube_collect()]
#' @export
#'
#' @examples
#' values <- array(seq_len(3 * 2 * 1 * 2 * 1), dim = c(3, 2, 1, 2, 1))
#' cube <- ocean_cube(
#'   lon = c(-80, -79, -78),
#'   lat = c(-12, -11),
#'   depth = 0,
#'   time = as.Date(c("2020-01-01", "2020-02-01")),
#'   vars = "temperature",
#'   data = values
#' )
#' cube_crop(cube, longitude = c(-79.8, -78.8))
#' cube_crop(
#'   cube,
#'   bbox = c(xmin = -80, ymin = -12, xmax = -79, ymax = -11)
#' )
cube_crop <- function(x, longitude = NULL, latitude = NULL, depth = NULL,
                      time = NULL, variable = NULL, bbox = NULL,
                      outside = c("error", "clip")) {
  .check_cube(x)
  backend_from <- .cube_backend(x)
  if (!backend_from %in% c("memory", "netcdf")) {
    rlang::abort(
      paste0("Unsupported ocean_cube backend: '", backend_from, "'."),
      class = "oceancube_unsupported_backend"
    )
  }
  outside <- base::match.arg(outside)

  spatial <- .resolve_crop_bbox(
    bbox = bbox,
    longitude = longitude,
    latitude = latitude
  )
  ranges <- list(
    longitude = spatial$longitude,
    latitude = spatial$latitude,
    depth = depth,
    time = time
  )
  resolved <- .resolve_cube_crop(
    x,
    ranges = ranges,
    variable = variable,
    outside = outside
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
  auxiliary$qa$crop <- list(
    resolved_indices = resolved$index,
    selected_coordinates = resolved$selected,
    domains = resolved$domain,
    netcdf_read = read_diagnostics,
    discarded_components = auxiliary$discarded
  )
  selected_time <- x$time[resolved$index$time]
  provenance_context <- .provenance_cube_context(
    source = x$source,
    dataset_id = x$dataset_id,
    time = selected_time,
    shape = resolved$output_shape,
    variables = x$vars[resolved$index$variable],
    backend = "memory",
    provenance = x$provenance
  )
  provenance <- .provenance_append(
    x$provenance,
    operation = "cube_crop",
    parameters = list(
      requested = list(
        bbox = bbox,
        ranges = .provenance_compact(resolved$requested),
        outside = outside
      ),
      resolved = list(
        ranges_applied = .provenance_compact(resolved$applied),
        clipped = resolved$clipped,
        selected_counts = vapply(resolved$index, length, integer(1)),
        selected_variables = x$vars[resolved$index$variable],
        output_shape = resolved$output_shape,
        discarded_components = auxiliary$discarded
      )
    ),
    output = .provenance_summary(provenance_context),
    scientific_method = .provenance_method("cube_crop", list()),
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

.resolve_crop_bbox <- function(bbox, longitude, latitude) {
  if (is.null(bbox)) {
    return(list(longitude = longitude, latitude = latitude))
  }
  if (!is.null(longitude) || !is.null(latitude)) {
    .abort_badarg(
      "bbox",
      paste(
        "cannot be combined with `longitude` or `latitude`; use either",
        "`bbox`, or the two explicit spatial ranges."
      )
    )
  }
  if (inherits(bbox, "bbox")) {
    .abort_badarg(
      "bbox",
      paste(
        "`sf::st_bbox()` objects are not supported because the cube CRS is",
        "not formalized; supply a named numeric vector in the units and",
        "longitude convention of the cube."
      )
    )
  }
  if (!is.numeric(bbox) || !is.null(dim(bbox))) {
    .abort_badarg(
      "bbox",
      "must be a numeric vector named xmin, ymin, xmax, and ymax."
    )
  }
  if (length(bbox) != 4L) {
    .abort_badarg("bbox", "must contain exactly four values.")
  }
  bbox_names <- names(bbox)
  expected <- c("xmin", "ymin", "xmax", "ymax")
  if (is.null(bbox_names) ||
      anyNA(bbox_names) ||
      any(!nzchar(bbox_names)) ||
      anyDuplicated(bbox_names) ||
      !setequal(bbox_names, expected)) {
    .abort_badarg(
      "bbox",
      "names must be exactly xmin, ymin, xmax, and ymax, in any order."
    )
  }
  if (anyNA(bbox) || any(!is.finite(bbox))) {
    .abort_badarg("bbox", "must contain finite, non-missing limits.")
  }

  list(
    longitude = unname(bbox[c("xmin", "xmax")]),
    latitude = unname(bbox[c("ymin", "ymax")])
  )
}

.resolve_cube_crop <- function(x, ranges, variable, outside) {
  coordinates <- list(
    longitude = x$lon,
    latitude = x$lat,
    depth = x$depth,
    time = x$time
  )
  index <- stats::setNames(vector("list", 5L), .cube_axis_names())
  requested <- applied <- domain <- selected <-
    stats::setNames(vector("list", 4L), names(coordinates))
  clipped <- stats::setNames(rep(FALSE, 4L), names(coordinates))

  for (axis in names(coordinates)) {
    result <- if (identical(axis, "time")) {
      .resolve_time_crop_range(
        coordinates[[axis]],
        ranges[[axis]],
        outside
      )
    } else {
      .resolve_numeric_crop_range(
        coordinates[[axis]],
        ranges[[axis]],
        axis,
        outside
      )
    }
    index[[axis]] <- result$index
    requested[axis] <- list(result$requested)
    applied[axis] <- list(result$applied)
    domain[axis] <- list(result$domain)
    selected[axis] <- list(coordinates[[axis]][result$index])
    clipped[[axis]] <- result$clipped
  }

  if (is.null(variable)) {
    index$variable <- seq_along(x$vars)
  } else {
    index$variable <- .resolve_selected_variables(
      x$vars,
      variable
    )$index
  }
  index <- .validate_cube_index(index, unname(.cube_shape(x)))
  selected$variable <- x$vars[index$variable]

  list(
    index = index,
    requested = requested,
    applied = applied,
    domain = domain,
    clipped = clipped,
    selected = selected,
    output_shape = stats::setNames(
      as.integer(lengths(index)),
      .cube_axis_names()
    )
  )
}

.validate_numeric_crop_range <- function(requested, axis) {
  if (!is.numeric(requested) || !is.null(dim(requested))) {
    .abort_badarg(
      axis,
      "must be a numeric range `c(lower, upper)`."
    )
  }
  if (length(requested) != 2L) {
    .abort_badarg(axis, "must contain exactly two limits.")
  }
  if (anyNA(requested) || any(!is.finite(requested))) {
    .abort_badarg(axis, "must contain finite, non-missing limits.")
  }
  if (requested[[1L]] > requested[[2L]]) {
    .abort_badarg(
      axis,
      paste(
        "limits must be ordered as `c(lower, upper)`;",
        "reversed ranges and antimeridian crossings are not supported."
      )
    )
  }
  requested
}

.crop_range_outside <- function(axis, requested, domain) {
  rlang::abort(
    paste0(
      "Requested ", axis, " range [",
      .slice_format_values(requested[[1L]]), ", ",
      .slice_format_values(requested[[2L]]),
      "] exceeds the cube domain [",
      .slice_format_values(domain[[1L]]), ", ",
      .slice_format_values(domain[[2L]]),
      "]. Use `outside = \"clip\"` to use the overlapping interval."
    ),
    class = "oceancube_crop_outside_domain"
  )
}

.crop_range_no_overlap <- function(axis, requested, domain) {
  rlang::abort(
    paste0(
      "Requested ", axis, " range [",
      .slice_format_values(requested[[1L]]), ", ",
      .slice_format_values(requested[[2L]]),
      "] does not intersect the cube domain [",
      .slice_format_values(domain[[1L]]), ", ",
      .slice_format_values(domain[[2L]]),
      "]; choose an overlapping range."
    ),
    class = "oceancube_crop_no_overlap"
  )
}

.crop_range_no_coordinates <- function(axis, requested) {
  rlang::abort(
    paste0(
      "The requested ", axis,
      " range [", .slice_format_values(requested[[1L]]), ", ",
      .slice_format_values(requested[[2L]]),
      "] intersects the domain but contains no cube coordinates. ",
      "Choose limits containing at least one stored cell centre."
    ),
    class = "oceancube_crop_no_coordinates"
  )
}

.resolve_numeric_crop_range <- function(axis_values, requested, axis,
                                        outside) {
  is_surface <- identical(axis, "depth") &&
    length(axis_values) == 1L &&
    is.na(axis_values[[1L]]) &&
    !is.nan(axis_values[[1L]])
  if (is_surface) {
    if (!is.null(requested)) {
      .abort_badarg(
        "depth",
        paste(
          "this is a surface cube with no numeric depth coordinate;",
          "leave `depth = NULL` to retain the surface layer."
        )
      )
    }
    return(list(
      index = 1L,
      requested = NULL,
      applied = NULL,
      domain = c(NA_real_, NA_real_),
      clipped = FALSE
    ))
  }

  domain <- range(axis_values)
  if (is.null(requested)) {
    return(list(
      index = seq_along(axis_values),
      requested = NULL,
      applied = NULL,
      domain = domain,
      clipped = FALSE
    ))
  }
  requested <- .validate_numeric_crop_range(requested, axis)
  exceeds <- requested[[1L]] < domain[[1L]] ||
    requested[[2L]] > domain[[2L]]
  if (identical(outside, "error") && exceeds) {
    .crop_range_outside(axis, requested, domain)
  }

  applied <- requested
  if (identical(outside, "clip")) {
    applied[[1L]] <- max(requested[[1L]], domain[[1L]])
    applied[[2L]] <- min(requested[[2L]], domain[[2L]])
    if (applied[[1L]] > applied[[2L]]) {
      .crop_range_no_overlap(axis, requested, domain)
    }
  }
  index <- which(
    !is.na(axis_values) &
      axis_values >= applied[[1L]] &
      axis_values <= applied[[2L]]
  )
  if (length(index) == 0L) {
    .crop_range_no_coordinates(axis, applied)
  }

  list(
    index = as.integer(index),
    requested = requested,
    applied = applied,
    domain = domain,
    clipped = isTRUE(any(applied != requested))
  )
}

.crop_time_class <- function(x) {
  resolved <- .time_class(x)
  if (!is.na(resolved)) return(resolved)
  rlang::abort(
    paste(
      "The cube time coordinate is not safely decoded to a supported temporal representation;",
      "use `cube_slice(..., by = \"index\")`."
    ),
    class = "oceancube_crop_time"
  )
}

.crop_posix_timezone <- function(x) {
  timezone <- attr(x, "tzone")
  if (is.null(timezone) || length(timezone) == 0L || is.na(timezone[[1L]])) {
    return("")
  }
  timezone[[1L]]
}

.resolve_time_crop_range <- function(axis_values, requested, outside) {
  expected_class <- .crop_time_class(axis_values)
  domain <- range(axis_values)
  if (is.null(requested)) {
    return(list(
      index = seq_along(axis_values),
      requested = NULL,
      applied = NULL,
      domain = domain,
      clipped = FALSE
    ))
  }
  open_bounds <- is.list(requested) &&
    !inherits(requested, c("Date", "POSIXct", "oceancube_cf_time"))
  if (open_bounds) {
    if (length(requested) != 2L || all(vapply(requested, is.null, logical(1)))) {
      .abort_badarg(
        "time",
        "internal open ranges require two bounds with at least one supplied value."
      )
    }
    supplied <- lapply(requested, function(bound) {
      if (is.null(bound)) return(NULL)
      .time_parse_selector(bound, axis_values, arg = "time")
    })
  } else {
    requested <- .time_parse_selector(requested, axis_values, arg = "time")
    if (length(requested) != 2L ||
        !inherits(requested, expected_class) ||
        !.time_compatible(requested, axis_values) || !is.null(dim(requested))) {
      .abort_badarg(
        "time",
        paste0(
          "must be a two-value ", expected_class,
          " range compatible with the cube time axis."
        )
      )
    }
    supplied <- list(requested[[1L]], requested[[2L]])
  }
  for (bound in supplied) {
    if (is.null(bound)) next
    if (!inherits(bound, expected_class) ||
        !.time_compatible(bound, axis_values) || length(bound) != 1L ||
        is.na(bound) || !is.finite(.time_key(bound))) {
      .abort_badarg(
        "time",
        paste0("bounds must be finite, non-missing ", expected_class, " values.")
      )
    }
  }
  lower <- supplied[[1L]] %||% domain[[1L]]
  upper <- supplied[[2L]] %||% domain[[2L]]
  if (identical(expected_class, "POSIXct")) {
    lower <- .as_utc_posixct(lower)
    upper <- .as_utc_posixct(upper)
  }
  bounds <- c(lower, upper)
  if (identical(expected_class, "POSIXct")) bounds <- .as_utc_posixct(bounds)
  if (.time_key(bounds[1L]) > .time_key(bounds[2L])) {
    .abort_badarg(
      "time",
      "limits must be ordered from the earlier to the later instant."
    )
  }

  exceeds <- .time_key(bounds[1L]) < .time_key(domain[1L]) ||
    .time_key(bounds[2L]) > .time_key(domain[2L])
  if (identical(outside, "error") && exceeds) {
    .crop_range_outside("time", bounds, domain)
  }
  applied <- bounds
  if (identical(outside, "clip")) {
    if (.time_key(bounds[1L]) < .time_key(domain[1L])) applied[1L] <- domain[1L]
    if (.time_key(bounds[2L]) > .time_key(domain[2L])) applied[2L] <- domain[2L]
    if (.time_key(applied[1L]) > .time_key(applied[2L])) {
      .crop_range_no_overlap("time", bounds, domain)
    }
  }
  axis_key <- .time_key(axis_values)
  applied_key <- .time_key(applied)
  index <- which(axis_key >= applied_key[[1L]] & axis_key <= applied_key[[2L]])
  if (length(index) == 0L) {
    .crop_range_no_coordinates("time", applied)
  }

  list(
    index = as.integer(index),
    requested = requested,
    applied = applied,
    domain = domain,
    clipped = isTRUE(any(.time_key(applied) != .time_key(bounds)))
  )
}
