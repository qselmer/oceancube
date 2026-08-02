# Internal backend-independent index planning ------------------------------

.cube_product_as_double <- function(x, label) {
  x <- as.double(x)
  if (length(x) == 0L || anyNA(x) || any(!is.finite(x)) || any(x < 0)) {
    rlang::abort(
      paste0("Cannot calculate ", label, " from invalid dimensions."),
      class = "oceancube_size_overflow"
    )
  }
  if (any(x == 0)) return(0)

  log_product <- sum(log(x))
  if (!is.finite(log_product) ||
      log_product > log(.Machine$double.xmax)) {
    rlang::abort(
      paste0("Cannot calculate ", label, ": numeric overflow."),
      class = "oceancube_size_overflow"
    )
  }
  result <- prod(x)
  if (!is.finite(result)) {
    rlang::abort(
      paste0("Cannot calculate ", label, ": numeric overflow."),
      class = "oceancube_size_overflow"
    )
  }
  result
}

.plan_cube_index_read <- function(x, index = NULL) {
  shape <- .cube_shape(x)
  axes <- .cube_axis_names()
  if (is.null(index)) index <- list()
  requested <- .validate_cube_index(index, unname(shape))
  physical_axes <- axes[seq_len(4L)]
  physical_requested <- requested[physical_axes]

  physical_start <- stats::setNames(
    as.integer(vapply(
      physical_requested,
      min,
      numeric(1)
    )),
    physical_axes
  )
  physical_end <- stats::setNames(
    as.integer(vapply(
      physical_requested,
      max,
      numeric(1)
    )),
    physical_axes
  )
  physical_count <- stats::setNames(
    physical_end - physical_start + 1L,
    physical_axes
  )
  physical_index <- stats::setNames(
    Map(
      seq.int,
      from = physical_start,
      to = physical_end
    ),
    physical_axes
  )
  local_index <- stats::setNames(
    Map(
      function(global, envelope) {
        as.integer(match(global, envelope))
      },
      physical_requested,
      physical_index
    ),
    physical_axes
  )

  variable_index <- requested$variable
  output_shape <- stats::setNames(
    as.integer(lengths(requested)),
    axes
  )
  values_requested <- .cube_product_as_double(
    output_shape,
    "requested cube values"
  )
  values_in_envelope <- .cube_product_as_double(
    c(physical_count, length(variable_index)),
    "NetCDF envelope values"
  )

  list(
    requested = requested,
    physical_start = physical_start,
    physical_count = physical_count,
    physical_index = physical_index,
    local_index = local_index,
    variable_index = variable_index,
    output_shape = output_shape,
    values_requested = values_requested,
    values_in_envelope = values_in_envelope,
    amplification = values_in_envelope / values_requested
  )
}

.cube_read_spatial_pairs <- function(x, longitude_index, latitude_index,
                                     depth_index, time_index,
                                     variable_index) {
  backend <- .cube_backend(x)
  if (identical(backend, "memory")) {
    return(.cube_read_spatial_pairs_memory(
      x, longitude_index, latitude_index, depth_index, time_index,
      variable_index
    ))
  }
  if (identical(backend, "netcdf")) {
    return(.cube_read_spatial_pairs_netcdf(
      x, longitude_index, latitude_index, depth_index, time_index,
      variable_index
    ))
  }
  rlang::abort(
    paste0("Unsupported ocean_cube backend: '", backend, "'."),
    class = "oceancube_unsupported_backend"
  )
}

.validate_spatial_pair_read <- function(x, longitude_index, latitude_index,
                                        depth_index, time_index,
                                        variable_index) {
  shape <- unname(.cube_shape(x))
  index <- .validate_cube_index(
    list(
      longitude = longitude_index,
      latitude = latitude_index,
      depth = depth_index,
      time = time_index,
      variable = variable_index
    ),
    shape
  )
  if (length(index$longitude) != length(index$latitude)) {
    .abort_badarg(
      "path",
      "longitude and latitude indices must have equal lengths and form row-aligned pairs."
    )
  }
  if (length(index$time) != 1L) {
    .abort_badarg(
      "time",
      "a transect read requires exactly one resolved time position."
    )
  }
  index
}

.spatial_pair_map <- function(longitude_index, latitude_index) {
  keys <- paste(longitude_index, latitude_index, sep = "\r")
  unique_position <- !duplicated(keys)
  list(
    unique_pairs = data.frame(
      longitude_index = longitude_index[unique_position],
      latitude_index = latitude_index[unique_position],
      stringsAsFactors = FALSE
    ),
    point_to_pair = as.integer(match(keys, keys[unique_position]))
  )
}
