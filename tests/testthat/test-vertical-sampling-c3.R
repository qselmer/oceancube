c3_point_path <- function(
    values = c(0, 20, 40), depths = c(0, 10, 20), unit = "m",
    descending = FALSE, bounds = NULL, cell_methods = "z: point") {
  if (identical(unit, "km")) {
    depths <- depths / 1000
    if (!is.null(bounds)) bounds <- bounds / 1000
  }
  if (isTRUE(descending)) {
    depths <- rev(depths)
    values <- rev(values)
    if (!is.null(bounds)) bounds <- bounds[nrow(bounds):1L, , drop = FALSE]
  }
  make_cf_vertical_fixture(
    values = depths, units = unit, bounds = bounds, bounds_units = unit,
    cell_methods = cell_methods, data_values = rep(values, each = 4L),
    variable_units = "K"
  )
}

c3_cell_path <- function(
    values = c(2, 4, 8), centres = c(5, 15, 25),
    bounds = rbind(c(0, 10), c(10, 20), c(20, 30)), unit = "m",
    descending = FALSE) {
  if (identical(unit, "km")) {
    centres <- centres / 1000
    bounds <- bounds / 1000
  }
  if (isTRUE(descending)) {
    centres <- rev(centres)
    bounds <- bounds[nrow(bounds):1L, , drop = FALSE]
    values <- rev(values)
  }
  make_cf_vertical_fixture(
    values = centres, units = unit, bounds = bounds, bounds_units = unit,
    cell_methods = "z: mean", data_values = rep(values, each = 4L),
    variable_units = "K"
  )
}

c3_mixed_path <- function() {
  file <- tempfile("oceancube-c3-mixed-", fileext = ".nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", -80)
  lat <- ncdf4::ncdim_def("lat", "degrees_north", -12)
  z <- ncdf4::ncdim_def("z", "m", c(5, 15, 25))
  time <- ncdf4::ncdim_def("time", "days since 2000-01-01", 0)
  nv <- ncdf4::ncdim_def("nv", "", 1:2, create_dimvar = FALSE)
  nc <- ncdf4::nc_create(file, list(
    ncdf4::ncvar_def("cell_mean", "K", list(lon, lat, z, time)),
    ncdf4::ncvar_def("point_value", "K", list(lon, lat, z, time)),
    ncdf4::ncvar_def("z_bnds", "m", list(nv, z))
  ))
  ncdf4::ncvar_put(nc, "cell_mean", array(c(2, 4, 8), c(1, 1, 3, 1)))
  ncdf4::ncvar_put(nc, "point_value", array(c(0, 20, 40), c(1, 1, 3, 1)))
  ncdf4::ncvar_put(nc, "z_bnds", t(rbind(c(0, 10), c(10, 20), c(20, 30))))
  ncdf4::ncatt_put(nc, "z", "standard_name", "depth")
  ncdf4::ncatt_put(nc, "z", "positive", "down")
  ncdf4::ncatt_put(nc, "z", "axis", "Z")
  ncdf4::ncatt_put(nc, "z", "bounds", "z_bnds")
  ncdf4::ncatt_put(nc, "cell_mean", "cell_methods", "z: mean")
  ncdf4::ncatt_put(nc, "point_value", "cell_methods", "z: point")
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.13")
  ncdf4::nc_close(nc)
  file
}

test_that("point sampling is exact or locally linear with analytic values", {
  x <- read_nc(c3_point_path(), vars = "temperature")
  sampled <- depth_sample(x, c(5, 10, 12.5, 15), method = "auto")
  descriptor <- sampled$metadata$cf$current$vertical_sampling
  step <- tail(sampled$provenance$history, 1L)[[1L]]

  expect_equal(as.numeric(sampled$data), rep(c(10, 20, 25, 30), each = 4L))
  expect_identical(sampled$depth, structure(
    c(5, 10, 12.5, 15), units = "m", positive = "down"
  ))
  expect_identical(sampled$units, x$units)
  expect_identical(descriptor$resolved_method, "linear")
  expect_identical(
    descriptor$variables$temperature$output_value_semantics,
    "INTERPOLATED_POINT_VALUE"
  )
  expect_identical(
    vapply(
      step$parameters$resolved$variables[[1L]]$targets,
      `[[`, character(1L), "status"
    ),
    c("LINEAR_INTERPOLATED", "EXACT_POINT", "LINEAR_INTERPOLATED",
      "LINEAR_INTERPOLATED")
  )
  target <- step$parameters$resolved$variables[[1L]]$targets[[3L]]
  expect_equal(target$lower_weight + target$upper_weight, 1)
  expect_equal(target$upper_weight, 0.25)
})

test_that("point sampling supports descending axes and metre-kilometre parity", {
  increasing <- read_nc(c3_point_path(), vars = "temperature")
  descending <- read_nc(c3_point_path(descending = TRUE), vars = "temperature")
  kilometres <- read_nc(c3_point_path(unit = "km"), vars = "temperature")

  expect_equal(
    as.numeric(depth_sample(descending, c(5, 12.5))$data),
    as.numeric(depth_sample(increasing, c(5, 12.5))$data)
  )
  expect_equal(
    as.numeric(depth_sample(kilometres, c(0.005, 0.0125))$data),
    as.numeric(depth_sample(increasing, c(5, 12.5))$data)
  )
  descending_result <- depth_sample(descending, c(5, 12.5))
  expect_identical(
    descending_result$metadata$cf$current$vertical_sampling$input_source_order,
    "DECREASING"
  )
  expect_identical(
    descending_result$metadata$cf$current$vertical$source_order,
    "INCREASING"
  )
})

test_that("point sampling uses one tolerance for Float32-like exactness", {
  path <- c3_point_path(
    values = c(0, 20, 40), depths = c(0, 0.1, 0.2)
  )
  x <- read_nc(path, vars = "temperature")
  result <- depth_sample(x, 0.1 + 1e-9)
  target <- tail(result$provenance$history, 1L)[[1L]]$parameters$
    resolved$variables[[1L]]$targets[[1L]]

  expect_equal(as.numeric(result$data), rep(20, 4L))
  expect_identical(target$status, "EXACT_POINT")
})

test_that("point missingness never imputes or searches other depths", {
  x <- read_nc(c3_point_path(values = c(0, NA, 40)), vars = "temperature")

  expect_true(all(is.na(depth_sample(x, 5)$data)))
  expect_true(all(is.na(depth_sample(x, 10)$data)))
  expect_true(all(is.na(depth_sample(x, 15)$data)))
})

test_that("point sampling rejects extrapolation and explicit physical gaps", {
  plain <- read_nc(c3_point_path(), vars = "temperature")
  gapped <- read_nc(c3_point_path(
    bounds = rbind(c(-2.5, 2.5), c(7.5, 12.5), c(17.5, 22.5))
  ), vars = "temperature")

  expect_error(
    depth_sample(plain, -1), class = "oceancube_vertical_sampling_outside"
  )
  expect_error(
    depth_sample(plain, 21), class = "oceancube_vertical_sampling_outside"
  )
  expect_error(
    depth_sample(gapped, 5), class = "oceancube_vertical_sampling_gap"
  )
})

test_that("cell means use explicit piecewise-constant containment", {
  x <- read_nc(c3_cell_path(), vars = "temperature")
  sampled <- depth_sample(x, c(2, 7, 12, 28), method = "auto")
  descriptor <- sampled$metadata$cf$current$vertical_sampling

  expect_equal(as.numeric(sampled$data), rep(c(2, 2, 4, 8), each = 4L))
  expect_identical(descriptor$resolved_method, "cell")
  expect_identical(
    descriptor$variables$temperature$output_value_semantics,
    "SAMPLED_CELL_MEAN_RECONSTRUCTION"
  )
  expect_identical(
    descriptor$variables$temperature$input_value_semantics,
    "VERTICAL_CELL_MEAN"
  )
})

test_that("cell boundary gap outside and missing policies are deterministic", {
  cells <- read_nc(c3_cell_path(), vars = "temperature")
  gapped <- read_nc(c3_cell_path(
    values = c(2, 8), centres = c(2.5, 12.5),
    bounds = rbind(c(0, 5), c(10, 15))
  ), vars = "temperature")
  missing <- read_nc(c3_cell_path(values = c(2, NA, 8)), vars = "temperature")

  expect_error(
    depth_sample(cells, 10), class = "oceancube_vertical_boundary_ambiguous"
  )
  expect_equal(as.numeric(depth_sample(cells, c(0, 30))$data),
               rep(c(2, 8), each = 4L))
  expect_error(
    depth_sample(gapped, 7.5), class = "oceancube_vertical_sampling_gap"
  )
  expect_error(
    depth_sample(cells, 31), class = "oceancube_vertical_sampling_outside"
  )
  expect_true(all(is.na(depth_sample(missing, 12)$data)))
})

test_that("cell sampling requires bounds and method semantics are strict", {
  cells <- read_nc(c3_cell_path(), vars = "temperature")
  cell_without_bounds <- read_nc(c3_point_path(
    values = c(2, 4, 8), depths = c(5, 15, 25),
    cell_methods = "z: mean"
  ), vars = "temperature")
  points <- read_nc(c3_point_path(), vars = "temperature")

  expect_error(
    depth_sample(cell_without_bounds, 12),
    class = "oceancube_vertical_geometry_unsupported"
  )
  expect_error(
    depth_sample(cells, 12, method = "linear"),
    class = "oceancube_vertical_sampling_semantics_unsupported"
  )
  expect_error(
    depth_sample(points, 12, method = "cell"),
    class = "oceancube_vertical_sampling_semantics_unsupported"
  )
})

test_that("auto supports mixed variables and explicit methods fail atomically", {
  x <- read_nc(
    c3_mixed_path(), vars = c("cell_mean", "point_value")
  )
  result <- depth_sample(x, 12, method = "auto")
  descriptor <- result$metadata$cf$current$vertical_sampling
  step <- tail(result$provenance$history, 1L)[[1L]]

  expect_equal(as.numeric(result$data[, , , , 1L]), 4)
  expect_equal(as.numeric(result$data[, , , , 2L]), 14)
  expect_identical(descriptor$resolved_method, "mixed")
  expect_identical(
    vapply(descriptor$variables, `[[`, character(1L), "resolved_method"),
    c(cell_mean = "cell", point_value = "linear")
  )
  expect_identical(
    step$parameters$resolved$required_source_depth_indices,
    c(1L, 2L)
  )
  expect_error(
    depth_sample(x, 12, method = "cell"),
    regexp = "point_value=VERTICAL_POINT",
    class = "oceancube_vertical_sampling_semantics_unsupported"
  )
  expect_error(
    depth_sample(x, 12, method = "linear"),
    regexp = "cell_mean=VERTICAL_CELL_MEAN",
    class = "oceancube_vertical_sampling_semantics_unsupported"
  )
})

test_that("unsupported value semantic classes fail before payload reads", {
  statuses <- c(
    "z: sum" = "VERTICAL_CELL_SUM",
    "z: maximum" = "VERTICAL_CELL_OTHER",
    "z: mean z: sum" = "VERTICAL_CELL_METHOD_AMBIGUOUS"
  )
  for (method in names(statuses)) {
    x <- cube_open(c3_point_path(cell_methods = method), vars = "temperature")
    observed <- 0L
    testthat::local_mocked_bindings(
      .cube_read_netcdf = function(...) {
        observed <<- observed + 1L
        stop("payload read must not occur")
      },
      .package = "oceancube"
    )
    expect_error(
      depth_sample(x, 5), regexp = statuses[[method]],
      class = "oceancube_vertical_sampling_semantics_unsupported"
    )
    expect_identical(observed, 0L)
  }
})

test_that("non-depth and surface coordinates remain outside C3", {
  configurations <- list(
    list(units = "dbar", standard_name = "sea_water_pressure", positive = NULL),
    list(units = "m", standard_name = "height", positive = "up"),
    list(units = "1", standard_name = NULL, positive = "down"),
    list(
      values = c(0.1, 0.5, 0.9), units = "1",
      standard_name = "ocean_sigma_coordinate", positive = "down",
      formula_terms = "sigma: z eta: eta depth: bathymetry"
    )
  )
  for (configuration in configurations) {
    path <- do.call(make_cf_vertical_fixture, c(configuration, list(
      cell_methods = "z: point"
    )))
    x <- read_nc(path, vars = "temperature")
    expect_error(
      depth_sample(x, 0.2), class = "oceancube_vertical_sampling_unsupported"
    )
  }
  oisst <- read_nc(
    test_path("fixtures", "real-data", "noaa-oisst21-surface-time-fv1.nc"),
    vars = "sst"
  )
  expect_error(
    depth_sample(oisst, 0), class = "oceancube_vertical_sampling_unsupported"
  )
})

test_that("target validation preserves a canonical requested depth axis", {
  x <- read_nc(c3_point_path(), vars = "temperature")
  expect_error(depth_sample(x, numeric()), class = "oceancube_bad_argument")
  expect_error(depth_sample(x, c(10, 5)), class = "oceancube_bad_argument")
  expect_error(depth_sample(x, c(5, 5)), class = "oceancube_bad_argument")
  expect_error(depth_sample(x, NA_real_), class = "oceancube_bad_argument")
  expect_error(depth_sample(x, Inf), class = "oceancube_bad_argument")
  expect_error(depth_sample(x, 5, method = "nearest"), class = "simpleError")
})

test_that("deferred sampling performs one bounded indexed depth read", {
  original <- oceancube:::.cube_read_netcdf
  point <- cube_open(c3_point_path(), vars = "temperature")
  cell <- cube_open(c3_cell_path(), vars = "temperature")
  mixed <- cube_open(
    c3_mixed_path(), vars = c("cell_mean", "point_value")
  )
  observed <- list()
  testthat::local_mocked_bindings(
    .cube_read_netcdf = function(x, index = NULL, drop = FALSE) {
      observed[[length(observed) + 1L]] <<- index$depth
      original(x, index = index, drop = drop)
    },
    .package = "oceancube"
  )

  expect_equal(as.numeric(depth_sample(point, 5)$data), rep(10, 4L))
  expect_identical(observed, list(c(1L, 2L)))
  observed <- list()
  expect_equal(as.numeric(depth_sample(cell, 12)$data), rep(4, 4L))
  expect_identical(observed, list(2L))
  observed <- list()
  mixed_result <- depth_sample(mixed, 12)
  expect_equal(as.numeric(mixed_result$data), c(4, 14))
  expect_identical(observed, list(c(1L, 2L)))
  observed <- list()
  expect_error(
    depth_sample(cell, 10), class = "oceancube_vertical_boundary_ambiguous"
  )
  expect_identical(observed, list())
})

test_that("sampled outputs never fabricate physical layer support", {
  x <- read_nc(c3_point_path(), vars = "temperature")
  sampled <- depth_sample(x, c(5, 12.5))
  vertical <- sampled$metadata$cf$current$vertical

  expect_identical(vertical$geometry_status, "GEOMETRY_NO_BOUNDS")
  expect_identical(vertical$bounds_status, "BOUNDS_MISSING")
  expect_identical(vertical$bounds, list())
  expect_null(attr(sampled$depth, "bounds", exact = TRUE))
  expect_error(
    cube_layer_thickness(sampled),
    class = "oceancube_vertical_geometry_unsupported"
  )
  expect_error(
    cube_cell_volume(sampled),
    class = "oceancube_vertical_geometry_unsupported"
  )
  expect_error(
    layer_integral(sampled, c(5, 12.5)),
    class = "oceancube_vertical_geometry_unsupported"
  )
  expect_error(
    layer_mean(sampled, c(5, 12.5)),
    class = "oceancube_vertical_geometry_unsupported"
  )
})

test_that("source CF is immutable and stale reduction descriptors are cleared", {
  source <- read_nc(c3_cell_path(), vars = "temperature")
  reduced <- layer_mean(source, c(0, 10, 20, 30))
  source_before <- serialize(reduced$metadata$cf$source, NULL)
  sampled <- depth_sample(reduced, c(5, 15, 25), method = "cell")

  expect_identical(serialize(sampled$metadata$cf$source, NULL), source_before)
  expect_null(sampled$metadata$cf$current$vertical_reduction)
  expect_identical(
    sampled$metadata$cf$current$vertical_sampling$schema_name,
    "oceancube_vertical_sampling"
  )
  expect_identical(
    sampled$metadata$cf$current$vertical_sampling$current_standard_name_status,
    "DERIVATION_PENDING"
  )
  expect_identical(
    sampled$metadata$cf$current$vertical_sampling$current_cell_methods_status,
    "DERIVATION_PENDING_NO_SYNTHETIC_CLAIM"
  )
  current_text <- paste(
    capture.output(str(sampled$metadata$cf$current)), collapse = " "
  )
  expect_false(grepl("depth: point", current_text, fixed = TRUE))
})

test_that("sampling metadata and provenance round-trip deterministically", {
  x <- read_nc(c3_point_path(), vars = "temperature")
  first <- depth_sample(x, c(5, 12.5))
  second <- depth_sample(x, c(5, 12.5))
  restored <- unserialize(serialize(first, NULL))
  file <- tempfile("oceancube-c3-roundtrip-", fileext = ".rds")
  saveRDS(first, file)
  from_rds <- readRDS(file)
  step <- tail(first$provenance$history, 1L)[[1L]]

  expect_identical(first, second)
  expect_identical(restored, first)
  expect_identical(from_rds, first)
  expect_true(oceancube:::.provenance_validate(first$provenance, strict = TRUE)$valid)
  expect_identical(step$operation, "depth_sample")
  expect_identical(
    step$scientific_method$id,
    "oceancube:vertical_linear_point_interpolation"
  )
  canonical <- paste(capture.output(str(list(
    first$metadata$cf$current$vertical_sampling,
    step$parameters
  ))), collapse = " ")
  expect_false(grepl("[A-Za-z]:[/\\\\]", canonical))
  expect_false(grepl("oquispe|hostname|tempfile", canonical, ignore.case = TRUE))
})

test_that("WOA monthly cell sampling is bounded and gap-safe", {
  path <- test_path("fixtures", "real-data", "noaa-woa23-monthly-vertical-fv1.nc")
  eager <- read_nc(path, vars = c("t_an", "s_an"))
  source <- eager$data[, , 2L, , , drop = FALSE]
  sampled <- depth_sample(eager, c(8, 10, 12), method = "auto")

  expect_identical(
    vapply(
      sampled$metadata$cf$current$vertical_sampling$variables,
      `[[`, character(1L), "input_value_semantics"
    ),
    c(t_an = "VERTICAL_CELL_MEAN", s_an = "VERTICAL_CELL_MEAN")
  )
  for (j in seq_len(3L)) {
    expect_equal(
      as.numeric(sampled$data[, , j, , , drop = FALSE]),
      as.numeric(source)
    )
  }
  boundary <- depth_sample(eager, c(7.5, 12.5))
  expect_equal(
    as.numeric(boundary$data[, , 1L, , , drop = FALSE]),
    as.numeric(source)
  )
  expect_equal(
    as.numeric(boundary$data[, , 2L, , , drop = FALSE]),
    as.numeric(source)
  )
  expect_error(
    depth_sample(eager, 5), class = "oceancube_vertical_sampling_gap"
  )
})

test_that("WOA deferred sampling reads only the contained source cell", {
  path <- test_path("fixtures", "real-data", "noaa-woa23-monthly-vertical-fv1.nc")
  x <- cube_open(path, vars = c("t_an", "s_an"))
  original <- oceancube:::.cube_read_netcdf
  observed <- list()
  testthat::local_mocked_bindings(
    .cube_read_netcdf = function(x, index = NULL, drop = FALSE) {
      observed[[length(observed) + 1L]] <<- index$depth
      original(x, index = index, drop = drop)
    },
    .package = "oceancube"
  )

  sampled <- depth_sample(x, 10)
  expect_equal(dim(sampled$data)[3L], 1L)
  expect_identical(observed, list(2L))
})

test_that("C3 adds exactly one stable public signature", {
  expect_identical(names(formals(depth_sample)), c("x", "depth", "method"))
  expect_identical(length(getNamespaceExports("oceancube")), 43L)
})
