A2_GENERATOR_SEED <- 303002L

A2_TOLERANCE <- list(
  exact = 0,
  transect_absolute = 1e-12,
  temporal_absolute = 1e-12,
  trend_absolute = 1e-12,
  visualization_absolute = 1e-12
)

a2_make_cube <- function(
    longitude_n = 3L,
    latitude_n = 2L,
    depth_n = 2L,
    time_n = 6L,
    variable_n = 2L,
    time_class = c("Date", "POSIXct"),
    irregular_time = FALSE,
    time = NULL,
    field = c("index", "constant", "linear"),
    intercept = 5,
    slope = 2,
    missingness = c(
      "none", "single", "profile", "time_point", "cell_time",
      "alternating", "boundary"
    ),
    surface = FALSE) {
  size_inputs <- list(longitude_n, latitude_n, depth_n, time_n, variable_n)
  if (any(lengths(size_inputs) != 1L)) {
    stop("Generator dimensions must be positive integer-like scalars.")
  }
  sizes <- unlist(size_inputs, use.names = FALSE)
  if (any(!is.finite(sizes)) || any(sizes < 1) || any(sizes != floor(sizes))) {
    stop("Generator dimensions must be positive integer-like scalars.")
  }
  sizes <- as.integer(sizes)
  names(sizes) <- c("longitude", "latitude", "depth", "time", "variable")
  time_class <- match.arg(time_class)
  field <- match.arg(field)
  missingness <- match.arg(missingness)

  longitude <- if (sizes[["longitude"]] == 1L) {
    -80
  } else {
    seq(-80, -78, length.out = sizes[["longitude"]])
  }
  latitude <- if (sizes[["latitude"]] == 1L) {
    -12
  } else {
    seq(-12, -10, length.out = sizes[["latitude"]])
  }
  depth <- if (isTRUE(surface)) {
    if (sizes[["depth"]] != 1L) stop("Surface generators require depth_n = 1.")
    NA_real_
  } else if (sizes[["depth"]] == 1L) {
    0
  } else {
    seq(0, 50, length.out = sizes[["depth"]])
  }

  if (is.null(time)) {
    offsets <- if (isTRUE(irregular_time)) {
      cumsum(c(0, rep(c(1, 2, 4), length.out = sizes[["time"]] - 1L)))
    } else {
      seq_len(sizes[["time"]]) - 1L
    }
    time <- if (identical(time_class, "Date")) {
      as.Date("2020-01-01") + offsets
    } else {
      as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + offsets * 86400
    }
  } else {
    if (length(time) != sizes[["time"]]) {
      stop("Supplied time must have exactly time_n elements.")
    }
    time_class <- if (inherits(time, "Date")) "Date" else "POSIXct"
  }

  variables <- paste0("variable_", seq_len(sizes[["variable"]]))
  units <- stats::setNames(
    rep(c("degC", "mmol m-3"), length.out = sizes[["variable"]]),
    variables
  )
  shape <- unname(sizes)
  values <- array(NA_real_, dim = shape)
  spatial_n <- prod(shape[1:3])
  elapsed_days <- if (inherits(time, "Date")) {
    as.numeric(time - time[[1L]])
  } else {
    as.numeric(difftime(time, time[[1L]], units = "days"))
  }

  for (variable_index in seq_len(shape[[5L]])) {
    for (time_index in seq_len(shape[[4L]])) {
      values[, , , time_index, variable_index] <- switch(
        field,
        index = variable_index * 10000 + time_index * 1000 +
          array(seq_len(spatial_n), dim = shape[1:3]),
        constant = intercept + variable_index - 1,
        linear = intercept + variable_index - 1 + slope * elapsed_days[[time_index]]
      )
    }
  }

  if (identical(missingness, "single")) {
    values[[1L]] <- NA_real_
  } else if (identical(missingness, "profile")) {
    values[1L, 1L, , 1L, 1L] <- NA_real_
  } else if (identical(missingness, "time_point")) {
    values[, , , 1L, ] <- NA_real_
  } else if (identical(missingness, "cell_time")) {
    values[1L, 1L, 1L, , 1L] <- NA_real_
  } else if (identical(missingness, "alternating")) {
    values[seq.int(1L, length(values), by = 2L)] <- NA_real_
  } else if (identical(missingness, "boundary")) {
    values[c(1L, length(values))] <- NA_real_
  }

  ocean_cube(
    lon = longitude,
    lat = latitude,
    depth = depth,
    time = time,
    data = values,
    vars = variables,
    units = units,
    source = "deterministic A2 generator",
    dataset_id = paste0("a2-", tolower(time_class)),
    provenance = list(source_identity = "a2-deterministic-generator")
  )
}

a2_make_netcdf_pair <- function(variables = c("temperature", "oxygen"), ...) {
  file <- make_netcdf_backend_fixture(...)
  storage <- .new_netcdf_storage(
    file,
    variables,
    source = "deterministic A2 NetCDF fixture",
    dataset_id = "a2-netcdf"
  )
  netcdf <- .new_netcdf_cube(storage)
  list(file = file, netcdf = netcdf, memory = cube_collect(netcdf))
}

a2_scientific_snapshot <- function(x) {
  list(
    shape = .cube_shape(x),
    longitude = x$lon,
    latitude = x$lat,
    depth = x$depth,
    time_class = class(x$time),
    time_timezone = attr(x$time, "tzone", exact = TRUE),
    time_numeric = as.numeric(x$time),
    variable = x$vars,
    units = x$units,
    values = .cube_read(x),
    source = x$source,
    dataset_id = x$dataset_id,
    provenance = x$provenance
  )
}

a2_expect_science_equal <- function(actual, expected, tolerance = A2_TOLERANCE$exact) {
  expect_identical(actual$lon, expected$lon)
  expect_identical(actual$lat, expected$lat)
  expect_identical(actual$depth, expected$depth)
  expect_identical(class(actual$time), class(expected$time))
  expect_identical(
    attr(actual$time, "tzone", exact = TRUE),
    attr(expected$time, "tzone", exact = TRUE)
  )
  expect_equal(as.numeric(actual$time), as.numeric(expected$time), tolerance = 0)
  expect_identical(actual$vars, expected$vars)
  expect_identical(actual$units, expected$units)
  expect_identical(.cube_shape(actual), .cube_shape(expected))
  if (identical(tolerance, 0)) {
    expect_identical(.cube_read(actual), .cube_read(expected))
  } else {
    expect_equal(.cube_read(actual), .cube_read(expected), tolerance = tolerance)
  }
  invisible(actual)
}

a2_expect_source_unchanged <- function(x, before) {
  expect_identical(a2_scientific_snapshot(x), before)
  invisible(x)
}

a2_provenance_contains <- function(x, value) {
  fields <- unlist(x, recursive = TRUE, use.names = FALSE)
  any(as.character(fields) == value)
}

a2_full_crop <- function(x) {
  cube_crop(
    x,
    longitude = range(x$lon),
    latitude = range(x$lat),
    depth = if (all(is.na(x$depth))) NULL else range(x$depth),
    time = range(x$time),
    variable = x$vars
  )
}

a2_full_slice <- function(x) {
  cube_slice(
    x,
    longitude = x$lon,
    latitude = x$lat,
    depth = x$depth,
    time = x$time,
    variable = x$vars
  )
}

a2_plot_payload <- function(plot) {
  data <- plot$data
  rownames(data) <- NULL
  data
}

a2_plot_layer_xy <- function(plot) {
  layer <- ggplot2::ggplot_build(plot)$data[[1L]]
  keep <- intersect(c("x", "y"), names(layer))
  out <- layer[keep]
  rownames(out) <- NULL
  out
}
