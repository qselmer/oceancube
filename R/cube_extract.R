#' Extract ocean-cube cells as a data frame
#'
#' `cube_extract()` resolves the same discrete selectors as [cube_slice()] but
#' returns their Cartesian product as a base `data.frame` instead of creating
#' another cube.
#'
#' @param x A valid `<ocean_cube>` using the memory or NetCDF backend.
#' @param longitude,latitude,depth,time,variable Optional selectors. `NULL`
#'   retains the complete axis.
#' @param by Whether selectors contain stored coordinate `"value"`s or
#'   one-based `"index"` positions.
#' @param match Coordinate matching method. `"exact"` requires stored values;
#'   `"nearest"` selects the nearest stored coordinate inside the domain.
#' @param tolerance Optional fully named list of maximum distances for nearest
#'   matching, using numeric values for spatial/depth axes and `difftime` for
#'   time.
#' @param mode Extraction intent: discrete `"point"`s, a vertical `"profile"`,
#'   a temporal `"series"`, or a general Cartesian `"table"`.
#' @param format `"long"` returns one row per selected array value; `"wide"`
#'   returns one row per longitude-latitude-depth-time key and one column per
#'   variable.
#' @param keep_index Add the selected one-based indices in the original cube.
#' @param keep_distance Add requested values and nearest-match distances.
#'
#' @return A base `data.frame` with lightweight selection and provenance
#'   attributes. Long output contains `longitude`, `latitude`, `depth`, `time`,
#'   `variable`, `unit`, and `value`.
#'
#' @details
#' The selectors form a Cartesian product. In long output, rows follow R array
#' order: longitude changes fastest, followed by latitude, depth, time, and
#' variable. Requested order and repeated coordinate positions are preserved.
#' Wide output rejects repeated variables or coordinate keys because silently
#' aggregating them would be ambiguous.
#'
#' Nearest matching chooses a stored cell and never interpolates. With
#' `keep_distance = TRUE`, diagnostics distinguish requested values from the
#' selected coordinates. This option is available only with
#' `by = "value", match = "nearest"`.
#' Temporal matching keeps `Date` civil-date semantics separate from POSIXct
#' instant semantics. POSIXct results remain UTC with sub-day precision.
#' Equidistant temporal nearest matches choose the earlier instant, and time
#' tolerances must be finite, non-negative scalar `difftime` values.
#'
#' A profile requires exactly one longitude, latitude, and time. A series
#' requires exactly one longitude, latitude, and depth. Point and table modes do
#' not change the Cartesian-product semantics.
#'
#' NetCDF inputs are resolved without opening the file and then read once
#' through the backend. Only the selected envelope and variables are read.
#' Extracting without selectors requests the complete cube and can create a
#' large table; the expected rows and approximate array bytes are calculated
#' before reading.
#' Source temporal provenance is attached to the returned table without copying
#' the backend's raw numeric time vector.
#'
#' Unlike `link_events()`, which enriches independent event rows,
#' `cube_extract()` generates the Cartesian grid of its axis selectors.
#' [cube_crop()] selects continuous ranges and returns a cube; [cube_slice()]
#' selects discrete positions and returns a cube.
#'
#' | Function | Input | Semantics | Output |
#' | --- | --- | --- | --- |
#' | `cube_slice()` | selectors | Cartesian product | cube |
#' | `cube_crop()` | ranges | rectangular subdomain | cube |
#' | `cube_extract()` | selectors | Cartesian product | table |
#' | `link_events()` | event rows | row by row | enriched table |
#'
#' @seealso [cube_slice()], [cube_crop()], [link_events()]
#' @export
#'
#' @examples
#' values <- array(seq_len(2 * 1 * 2 * 2 * 1), dim = c(2, 1, 2, 2, 1))
#' cube <- ocean_cube(
#'   lon = c(-80, -79),
#'   lat = -11,
#'   depth = c(0, 50),
#'   time = as.Date(c("2020-01-01", "2020-02-01")),
#'   vars = "temperature",
#'   units = c(temperature = "degC"),
#'   data = values
#' )
#' cube_extract(
#'   cube,
#'   longitude = -79,
#'   latitude = -11,
#'   time = as.Date("2020-02-01"),
#'   mode = "profile"
#' )
#' cube_extract(cube, depth = 0, mode = "table", format = "wide")
cube_extract <- function(x, longitude = NULL, latitude = NULL, depth = NULL,
                         time = NULL, variable = NULL,
                         by = c("value", "index"),
                         match = c("exact", "nearest"),
                         tolerance = NULL,
                         mode = c("point", "profile", "series", "table"),
                         format = c("long", "wide"),
                         keep_index = FALSE,
                         keep_distance = FALSE) {
  match_was_missing <- missing(match)
  .check_cube(x)
  backend <- .cube_backend(x)
  if (!backend %in% c("memory", "netcdf")) {
    rlang::abort(
      paste0("Unsupported ocean_cube backend: '", backend, "'."),
      class = "oceancube_unsupported_backend"
    )
  }

  by <- base::match.arg(by)
  method <- base::match.arg(match)
  mode <- base::match.arg(mode)
  format <- base::match.arg(format)
  .validate_extract_flag(keep_index, "keep_index")
  .validate_extract_flag(keep_distance, "keep_distance")

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
  if (isTRUE(keep_distance) &&
      (!identical(by, "value") || !identical(method, "nearest"))) {
    .abort_badarg(
      "keep_distance",
      paste(
        "is available only with `by = \"value\"` and",
        "`match = \"nearest\"`; disable it or use nearest matching."
      )
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
    tolerance = tolerance,
    allow_variable_duplicates = TRUE
  )
  .validate_extract_mode(x, resolved$index, mode)
  estimate <- .estimate_cube_extract(
    resolved$index,
    format = format,
    keep_index = keep_index,
    keep_distance = keep_distance,
    selectors = selectors
  )
  if (identical(format, "wide")) {
    .validate_extract_wide(x, resolved$index, keep_index)
  }

  read_plan <- if (identical(backend, "netcdf")) {
    .plan_cube_index_read(x, resolved$index)
  } else {
    NULL
  }
  values <- .cube_read(x, index = resolved$index)
  expected_shape <- as.integer(lengths(resolved$index))
  if (!is.array(values) ||
      !identical(unname(dim(values)), expected_shape)) {
    rlang::abort(
      "The cube backend did not return the resolved five-dimensional selection.",
      class = "oceancube_extract_backend"
    )
  }

  selected_units <- .extract_table_units(x, resolved$index$variable)
  result <- if (identical(format, "long")) {
    .cube_extract_long(
      x,
      values,
      resolved,
      selected_units,
      selectors,
      keep_index,
      keep_distance
    )
  } else {
    .cube_extract_wide(
      x,
      values,
      resolved,
      selected_units,
      selectors,
      keep_index,
      keep_distance
    )
  }

  selected <- .extract_selected_coordinates(x, resolved$index)
  selection <- list(
    requested = selectors,
    selected = selected,
    indices = resolved$index,
    distances = resolved$distance,
    tolerance = resolved$tolerance
  )
  provenance <- list(
    operation = "cube_extract",
    backend = backend,
    mode = mode,
    format = format,
    by = by,
    match = method,
    selectors_requested = selectors,
    indices_resolved = resolved$index,
    shape_selected = stats::setNames(expected_shape, .cube_axis_names()),
    rows_expected_long = estimate$rows_long,
    rows_expected_wide = estimate$rows_wide,
    columns_expected = estimate$columns,
    approximate_array_bytes = estimate$array_bytes,
    large_output = estimate$large_output,
    rows_returned = nrow(result),
    variables = x$vars[resolved$index$variable],
    keep_index = keep_index,
    keep_distance = keep_distance,
    source = x$source,
    dataset_id = x$dataset_id,
    time = .find_time_provenance(x$provenance),
    source_provenance = x$provenance,
    netcdf_read = if (is.null(read_plan)) {
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
    },
    extracted_utc = .netcdf_as_utc(Sys.time())
  )

  attr(result, "oceancube_backend") <- backend
  attr(result, "oceancube_shape") <-
    stats::setNames(expected_shape, .cube_axis_names())
  attr(result, "oceancube_selection") <- selection
  attr(result, "units") <-
    stats::setNames(selected_units, x$vars[resolved$index$variable])
  attr(result, "oceancube_provenance") <- provenance
  result
}

.validate_extract_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .abort_badarg(arg, "must be one non-missing logical value.")
  }
  invisible(TRUE)
}

.validate_extract_mode <- function(x, index, mode) {
  fixed <- switch(
    mode,
    profile = c("longitude", "latitude", "time"),
    series = c("longitude", "latitude", "depth"),
    character()
  )
  for (axis in fixed) {
    count <- length(index[[axis]])
    if (count != 1L) {
      rlang::abort(
        paste0(
          "Mode `", mode, "` requires exactly one ", axis,
          " position; the selector resolved ", count,
          ". Select one position on this axis or use `mode = \"table\"`."
        ),
        class = "oceancube_extract_mode"
      )
    }
  }
  if (identical(mode, "profile")) {
    is_surface <- length(x$depth) == 1L &&
      is.na(x$depth[[1L]]) &&
      !is.nan(x$depth[[1L]])
    if (!is_surface && anyNA(x$depth[index$depth])) {
      rlang::abort(
        paste(
          "Mode `profile` requires a usable numeric depth axis or an explicit",
          "surface cube with `depth = NA_real_`."
        ),
        class = "oceancube_extract_mode"
      )
    }
  }
  invisible(TRUE)
}

.estimate_cube_extract <- function(index, format, keep_index, keep_distance,
                                   selectors) {
  shape <- as.double(lengths(index))
  rows_long <- .cube_product_as_double(shape, "cube_extract long rows")
  rows_wide <- .cube_product_as_double(
    shape[seq_len(4L)],
    "cube_extract wide rows"
  )
  rows_materialized <- if (identical(format, "long")) rows_long else rows_wide
  if (rows_materialized > .Machine$integer.max) {
    rlang::abort(
      paste0(
        "The requested extraction would produce ",
        base::format(rows_materialized, scientific = FALSE),
        " rows, exceeding the data-frame row limit. Select fewer positions."
      ),
      class = "oceancube_extract_size"
    )
  }
  if (rows_long > .Machine$double.xmax / 8) {
    rlang::abort(
      "Cannot estimate cube_extract array bytes: numeric overflow.",
      class = "oceancube_size_overflow"
    )
  }
  diagnostic_axes <- sum(!vapply(
    selectors[seq_len(4L)],
    is.null,
    logical(1)
  ))
  columns <- if (identical(format, "long")) {
    7L + if (isTRUE(keep_index)) 5L else 0L +
      if (isTRUE(keep_distance)) 2L * diagnostic_axes else 0L
  } else {
    4L + length(index$variable) +
      if (isTRUE(keep_index)) 5L else 0L +
      if (isTRUE(keep_distance)) 2L * diagnostic_axes else 0L
  }
  list(
    shape = shape,
    rows_long = rows_long,
    rows_wide = rows_wide,
    columns = as.integer(columns),
    array_bytes = rows_long * 8,
    large_output = rows_materialized >= 1e6
  )
}

.extract_cube_coordinates <- function(x) {
  list(
    longitude = x$lon,
    latitude = x$lat,
    depth = x$depth,
    time = x$time,
    variable = x$vars
  )
}

.extract_selected_coordinates <- function(x, index) {
  Map(`[`, .extract_cube_coordinates(x), index)
}

.extract_table_units <- function(x, variable_index) {
  selected <- .selection_units(x$units, x$vars, variable_index)
  if (is.null(selected)) {
    return(rep(NA_character_, length(variable_index)))
  }
  vapply(
    selected,
    function(unit) {
      if (length(unit) != 1L || is.na(unit)) {
        NA_character_
      } else {
        as.character(unit)
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
}

.extract_local_indices <- function(dimensions) {
  dimensions <- as.integer(dimensions)
  positions <- seq_len(prod(dimensions))
  local <- arrayInd(positions, .dim = dimensions)
  colnames(local) <- .cube_axis_names()[seq_len(length(dimensions))]
  local
}

.append_extract_indices <- function(out, local, resolved, axes) {
  for (axis in axes) {
    out[[paste0(axis, "_index")]] <-
      resolved$index[[axis]][local[, axis]]
  }
  out
}

.append_extract_distances <- function(out, local, resolved, selectors, axes) {
  for (axis in axes) {
    if (is.null(selectors[[axis]])) next
    local_position <- local[, axis]
    out[[paste0(axis, "_requested")]] <-
      selectors[[axis]][local_position]
    out[[paste0(axis, "_distance")]] <-
      resolved$distance[[axis]][local_position]
  }
  out
}

.cube_extract_long <- function(x, values, resolved, units, selectors,
                               keep_index, keep_distance) {
  local <- .extract_local_indices(dim(values))
  selected <- .extract_selected_coordinates(x, resolved$index)
  out <- data.frame(
    longitude = selected$longitude[local[, "longitude"]],
    latitude = selected$latitude[local[, "latitude"]],
    depth = selected$depth[local[, "depth"]],
    time = selected$time[local[, "time"]],
    variable = selected$variable[local[, "variable"]],
    unit = units[local[, "variable"]],
    value = as.vector(values),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (isTRUE(keep_index)) {
    out <- .append_extract_indices(
      out,
      local,
      resolved,
      .cube_axis_names()
    )
  }
  if (isTRUE(keep_distance)) {
    out <- .append_extract_distances(
      out,
      local,
      resolved,
      selectors,
      .cube_axis_names()[seq_len(4L)]
    )
  }
  out
}

.validate_extract_wide <- function(x, index, keep_index) {
  variables <- x$vars[index$variable]
  if (anyDuplicated(variables)) {
    .abort_badarg(
      "format",
      paste(
        "`format = \"wide\"` cannot represent duplicate variables;",
        "remove duplicates or use `format = \"long\"`."
      )
    )
  }
  selected <- .extract_selected_coordinates(x, index)
  duplicate_axes <- .cube_axis_names()[seq_len(4L)][vapply(
    selected[seq_len(4L)],
    anyDuplicated,
    integer(1)
  ) > 0L]
  if (length(duplicate_axes) > 0L) {
    .abort_badarg(
      "format",
      paste0(
        "`format = \"wide\"` has duplicate coordinate keys on: ",
        paste(duplicate_axes, collapse = ", "),
        ". Remove duplicates or use `format = \"long\"`."
      )
    )
  }
  if (isTRUE(keep_index) && length(index$variable) > 1L) {
    .abort_badarg(
      "keep_index",
      paste(
        "cannot provide one `variable_index` per wide row when several",
        "variables are selected; use `format = \"long\"` or one variable."
      )
    )
  }
  invisible(TRUE)
}

.cube_extract_wide <- function(x, values, resolved, units, selectors,
                               keep_index, keep_distance) {
  spatial_dimensions <- dim(values)[seq_len(4L)]
  local <- .extract_local_indices(spatial_dimensions)
  selected <- .extract_selected_coordinates(x, resolved$index)
  out <- data.frame(
    longitude = selected$longitude[local[, "longitude"]],
    latitude = selected$latitude[local[, "latitude"]],
    depth = selected$depth[local[, "depth"]],
    time = selected$time[local[, "time"]],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  value_matrix <- matrix(
    as.vector(values),
    nrow = nrow(local),
    ncol = length(resolved$index$variable)
  )
  variables <- selected$variable
  for (i in seq_along(variables)) {
    out[[variables[[i]]]] <- value_matrix[, i]
  }
  if (isTRUE(keep_index)) {
    out <- .append_extract_indices(
      out,
      local,
      resolved,
      .cube_axis_names()[seq_len(4L)]
    )
    out$variable_index <- resolved$index$variable[[1L]]
  }
  if (isTRUE(keep_distance)) {
    out <- .append_extract_distances(
      out,
      local,
      resolved,
      selectors,
      .cube_axis_names()[seq_len(4L)]
    )
  }
  out
}
