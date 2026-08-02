# Internal memory-backend operations ---------------------------------------

.cube_axis_names <- function() {
  c("longitude", "latitude", "depth", "time", "variable")
}

.cube_backend <- function(x) {
  if (!inherits(x, "ocean_cube")) {
    rlang::abort(
      paste(
        "`x` must inherit from <ocean_cube>;",
        "it must be an <ocean_cube> object before its backend can be identified."
      ),
      class = "oceancube_bad_cube"
    )
  }
  if (!is.list(x)) {
    rlang::abort(
      "Cannot determine the <ocean_cube> backend: the object must be a list.",
      class = "oceancube_bad_cube"
    )
  }
  if ("data" %in% names(x) && is.array(x$data)) {
    # Legacy and current objects store their values directly in a 5D array.
    return("memory")
  }
  if ("data" %in% names(x)) {
    rlang::abort(
      "Cannot determine the <ocean_cube> backend from `x$data`; the memory backend requires an array.",
      class = "oceancube_bad_cube"
    )
  }
  if ("storage" %in% names(x)) {
    .validate_netcdf_storage(x$storage, check_file = FALSE)
    return("netcdf")
  }

  rlang::abort(
    "Cannot determine the <ocean_cube> backend: neither memory data nor a storage descriptor is present.",
    class = "oceancube_bad_cube"
  )
}

.validate_memory_backend <- function(x) {
  if (!is.list(x) || !"data" %in% names(x)) {
    rlang::abort(
      "Invalid memory backend: required storage component `data` is missing.",
      class = "oceancube_bad_cube"
    )
  }

  storage <- x$data
  if (!is.array(storage) || !is.numeric(storage)) {
    .abort_badarg("data", "must be a numeric array for the memory backend.")
  }

  actual <- dim(storage)
  expected <- as.integer(c(
    length(x$lon),
    length(x$lat),
    length(x$depth),
    length(x$time),
    length(x$vars)
  ))
  axes <- .cube_axis_names()

  if (length(actual) != 5L || !identical(unname(actual), expected)) {
    affected <- if (length(actual) == 5L) {
      axes[actual != expected]
    } else {
      "dimensionality"
    }
    rlang::abort(
      paste0(
        "Invalid memory backend shape on axis ",
        paste0("`", affected, "`", collapse = ", "),
        ": expected [",
        paste(expected, collapse = " x "),
        "] from the logical cube, obtained [",
        paste(actual, collapse = " x "),
        "] from physical storage."
      ),
      class = "oceancube_bad_cube"
    )
  }

  invisible(stats::setNames(as.integer(actual), axes))
}

.cube_storage_shape <- function(x) {
  backend <- .cube_backend(x)
  if (identical(backend, "memory")) {
    return(.validate_memory_backend(x))
  }
  if (identical(backend, "netcdf")) {
    .validate_netcdf_storage(x$storage, check_file = FALSE)
    return(x$storage$dimensions$shape)
  }

  rlang::abort(
    paste0("Unsupported ocean_cube backend: '", backend, "'."),
    class = "oceancube_unsupported_backend"
  )
}

.require_memory_backend <- function(x) {
  .check_cube(x)
  backend <- .cube_backend(x)

  if (!identical(backend, "memory")) {
    rlang::abort(
      paste0("Unsupported ocean_cube backend: '", backend, "'."),
      class = "oceancube_unsupported_backend"
    )
  }

  invisible(backend)
}

.validate_cube_position <- function(position, axis, size) {
  if (!is.numeric(position)) {
    .abort_badarg(
      paste0("index$", axis),
      "must be numeric positions."
    )
  }
  if (!is.null(dim(position))) {
    .abort_badarg(
      paste0("index$", axis),
      "must be a vector."
    )
  }
  if (length(position) == 0L) {
    .abort_badarg(
      paste0("index$", axis),
      "must not be empty."
    )
  }
  if (anyNA(position) || any(!is.finite(position))) {
    .abort_badarg(
      paste0("index$", axis),
      "must contain finite, non-missing positions."
    )
  }
  if (any(position != floor(position))) {
    .abort_badarg(
      paste0("index$", axis),
      "must contain whole-number positions."
    )
  }
  if (any(position < 1 | position > size)) {
    .abort_badarg(
      paste0("index$", axis),
      paste0(
        "positions must be between 1 and ",
        size,
        "; received: ",
        paste(position, collapse = ", "),
        "."
      )
    )
  }

  as.integer(position)
}

.validate_cube_index <- function(index, shape) {
  axes <- .cube_axis_names()

  if (!is.list(index)) {
    .abort_badarg(
      "index",
      "must be NULL or a named list of positional axis indices."
    )
  }
  if (length(index) > 0L) {
    index_names <- names(index)
    if (is.null(index_names) ||
        anyNA(index_names) ||
        any(!nzchar(index_names))) {
      .abort_badarg(
        "index",
        paste0(
          "must be fully named with axes from: ",
          paste(axes, collapse = ", "),
          "."
        )
      )
    }
    if (anyDuplicated(index_names)) {
      .abort_badarg("index", "must not contain duplicate axis names.")
    }

    unknown <- setdiff(index_names, axes)
    if (length(unknown) > 0L) {
      .abort_badarg(
        "index",
        paste0("Unknown axis name(s): ", paste(unknown, collapse = ", "), ".")
      )
    }
  }

  normalized <- stats::setNames(
    lapply(shape, seq_len),
    axes
  )
  for (axis in names(index)) {
    axis_number <- match(axis, axes)
    normalized[[axis_number]] <- .validate_cube_position(
      index[[axis]],
      axis,
      shape[[axis_number]]
    )
  }

  normalized
}

.validate_cube_block_vector <- function(x, arg) {
  axes <- .cube_axis_names()

  if (!is.numeric(x)) {
    .abort_badarg(arg, "must be numeric.")
  }
  if (!is.null(dim(x))) {
    .abort_badarg(arg, "must be a vector.")
  }
  if (length(x) != 5L) {
    .abort_badarg(arg, "must have length 5.")
  }
  if (anyNA(x) || any(!is.finite(x))) {
    .abort_badarg(arg, "must contain finite, non-missing values.")
  }
  if (any(x != floor(x))) {
    .abort_badarg(arg, "must contain whole-number values.")
  }
  if (any(x < 1)) {
    .abort_badarg(arg, "values must be at least 1.")
  }

  x_names <- names(x)
  if (!is.null(x_names) && !identical(x_names, axes)) {
    .abort_badarg(
      arg,
      paste0(
        "names must be exactly: ",
        paste(axes, collapse = ", "),
        ", in that order."
      )
    )
  }

  as.integer(x)
}

.validate_cube_block <- function(start, count, shape) {
  axes <- .cube_axis_names()
  start <- .validate_cube_block_vector(start, "start")
  count <- .validate_cube_block_vector(count, "count")
  block_end <- as.double(start) + as.double(count) - 1

  outside <- which(block_end > shape)
  if (length(outside) > 0L) {
    axis_number <- outside[[1L]]
    rlang::abort(
      paste0(
        "Invalid block on axis `",
        axes[[axis_number]],
        "`: end position ",
        format(block_end[[axis_number]], scientific = FALSE),
        " exceeds axis size ",
        shape[[axis_number]],
        "."
      ),
      class = "oceancube_bad_block"
    )
  }

  list(
    start = start,
    count = count,
    index = stats::setNames(
      Map(
        function(first, length_out) {
          seq.int(from = first, length.out = length_out)
        },
        start,
        count
      ),
      axes
    )
  )
}

.cube_read <- function(x, index = NULL, drop = FALSE) {
  backend <- .cube_backend(x)
  if (identical(backend, "netcdf")) {
    return(.cube_read_netcdf(x, index = index, drop = drop))
  }
  .require_memory_backend(x)
  storage <- x$data

  if (!is.logical(drop) || length(drop) != 1L || is.na(drop)) {
    .abort_badarg("drop", "must be a single non-missing logical value.")
  }
  if (isTRUE(drop)) {
    .abort_badarg(
      "drop",
      "`drop = TRUE` is not supported; backend reads always preserve all 5 dimensions."
    )
  }

  if (is.null(index)) {
    return(storage)
  }

  index <- .validate_cube_index(index, unname(dim(storage)))
  do.call(
    `[`,
    c(list(storage), unname(index), list(drop = FALSE))
  )
}

.cube_read_block <- function(x, start, count) {
  backend <- .cube_backend(x)
  if (identical(backend, "netcdf")) {
    return(.cube_read_block_netcdf(x, start = start, count = count))
  }
  .require_memory_backend(x)
  block <- .validate_cube_block(
    start = start,
    count = count,
    shape = unname(.cube_storage_shape(x))
  )

  .cube_read(x, index = block$index, drop = FALSE)
}

.cube_read_spatial_pairs_memory <- function(x, longitude_index,
                                            latitude_index, depth_index,
                                            time_index, variable_index) {
  .require_memory_backend(x)
  index <- .validate_spatial_pair_read(
    x, longitude_index, latitude_index, depth_index, time_index,
    variable_index
  )
  pairs <- .spatial_pair_map(index$longitude, index$latitude)
  storage <- x$data
  output <- array(
    NA_real_,
    dim = c(
      point = length(index$longitude),
      depth = length(index$depth),
      time = 1L,
      variable = length(index$variable)
    )
  )

  for (point in seq_along(index$longitude)) {
    output[point, , 1L, ] <- storage[
      index$longitude[[point]],
      index$latitude[[point]],
      index$depth,
      index$time,
      index$variable,
      drop = FALSE
    ]
  }

  n_unique_variables <- length(unique(index$variable))
  n_paired <- nrow(pairs$unique_pairs) * length(index$depth) *
    n_unique_variables
  rectangle_cells <-
    (diff(range(pairs$unique_pairs$longitude_index)) + 1) *
    (diff(range(pairs$unique_pairs$latitude_index)) + 1)
  attr(output, "oceancube_read_metrics") <- list(
    n_points = length(index$longitude),
    n_unique_pairs = nrow(pairs$unique_pairs),
    n_variables = length(index$variable),
    n_depth = length(index$depth),
    n_open = 0L,
    n_ncvar_get = 0L,
    n_values_requested = length(output),
    n_values_read = n_paired,
    n_values_paired = n_paired,
    n_values_bounding_rectangle =
      rectangle_cells * length(index$depth) * n_unique_variables,
    read_amplification =
      rectangle_cells / nrow(pairs$unique_pairs)
  )
  output
}

.cube_write_block <- function(x, values, start, count = dim(values)) {
  backend <- .cube_backend(x)
  if (identical(backend, "netcdf")) {
    .check_cube(x)
    rlang::abort(
      paste(
        "NetCDF backend is read-only.",
        "Collect the cube into memory before modifying values."
      ),
      class = "oceancube_netcdf_read_only"
    )
  }
  .require_memory_backend(x)
  storage <- x$data

  if (!is.array(values) ||
      !typeof(values) %in% c("integer", "double")) {
    .abort_badarg("values", "must be a numeric array.")
  }
  if (length(dim(values)) != 5L) {
    .abort_badarg("values", "must have exactly 5 dimensions.")
  }
  if (missing(count)) {
    # Dimension labels are descriptive; an omitted count depends only on shape.
    count <- unname(dim(values))
  }

  block <- .validate_cube_block(
    start = start,
    count = count,
    shape = unname(dim(storage))
  )
  if (!identical(unname(dim(values)), block$count)) {
    rlang::abort(
      paste0(
        "Invalid `values` dimensions: [",
        paste(dim(values), collapse = " x "),
        "] must equal `count` [",
        paste(block$count, collapse = " x "),
        "]."
      ),
      class = "oceancube_bad_block"
    )
  }

  target_type <- typeof(storage)
  values_type <- typeof(values)
  if (identical(target_type, "integer") &&
      !identical(values_type, "integer")) {
    .abort_badarg(
      "values",
      "integer backend data requires integer `values` to avoid silent type promotion."
    )
  }
  if (!target_type %in% c("integer", "double")) {
    rlang::abort(
      paste0(
        "Unsupported memory backend storage type: '",
        target_type,
        "'."
      ),
      class = "oceancube_unsupported_backend"
    )
  }

  out <- x
  out_data <- storage
  out_data <- do.call(
    `[<-`,
    c(
      list(out_data),
      unname(block$index),
      list(value = values)
    )
  )

  if (!identical(typeof(out_data), target_type)) {
    rlang::abort(
      "A memory backend write must preserve the storage type of `x$data`.",
      class = "oceancube_bad_block"
    )
  }

  out$data <- out_data
  .check_cube(out)
  out
}

.new_collected_memory_cube <- function(x, data, provenance) {
  out <- ocean_cube(
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    time = x$time,
    vars = x$vars,
    data = data,
    units = x$units,
    source = x$source,
    dataset_id = x$dataset_id,
    spatial_extent = x$spatial_extent,
    temporal_extent = x$temporal_extent,
    depth_extent = x$depth_extent,
    mask = x$mask,
    dc = x$dc,
    climatology = x$climatology,
    anomaly = x$anomaly,
    provenance = provenance,
    qa = x$qa
  )
  out$time <- x$time
  dimnames(out$data) <- dimnames(data)
  .check_cube(out)
  out
}

.new_selected_memory_cube <- function(x, data, index, units, auxiliary,
                                      provenance) {
  selected_time <- x$time[index$time]
  out <- ocean_cube(
    lon = x$lon[index$longitude],
    lat = x$lat[index$latitude],
    depth = x$depth[index$depth],
    time = selected_time,
    vars = x$vars[index$variable],
    data = data,
    units = units,
    source = x$source,
    dataset_id = x$dataset_id,
    mask = auxiliary$mask,
    dc = auxiliary$dc,
    climatology = auxiliary$climatology,
    anomaly = auxiliary$anomaly,
    provenance = provenance,
    qa = auxiliary$qa
  )
  # ocean_cube() currently normalizes POSIXct to Date. A selection retains the
  # decoded class already validated on the source header.
  out$time <- selected_time
  out$temporal_extent <- range(selected_time)
  dimnames(out$data) <- dimnames(data)
  .check_cube(out)
  out
}
