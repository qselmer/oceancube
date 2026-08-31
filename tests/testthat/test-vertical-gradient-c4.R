c4_point_path <- function(
    values = c(1, 21, 41), depths = c(0, 10, 20), unit = "m",
    descending = FALSE, bounds = NULL, variable_unit = "K") {
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
    cell_methods = "z: point", data_values = rep(values, each = 4L),
    variable_units = variable_unit
  )
}

c4_cell_path <- function(
    values = c(11, 31, 51), centres = c(5, 15, 25),
    bounds = rbind(c(0, 10), c(10, 20), c(20, 30)), unit = "m",
    descending = FALSE, variable_unit = "K") {
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
    variable_units = variable_unit
  )
}

c4_mixed_path <- function() {
  path <- tempfile("oceancube-c4-mixed-", fileext = ".nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", -80)
  lat <- ncdf4::ncdim_def("lat", "degrees_north", -12)
  z <- ncdf4::ncdim_def("z", "m", c(5, 15, 25))
  time <- ncdf4::ncdim_def("time", "days since 2000-01-01", 0)
  nv <- ncdf4::ncdim_def("nv", "", 1:2, create_dimvar = FALSE)
  nc <- ncdf4::nc_create(path, list(
    ncdf4::ncvar_def("point_value", "K", list(lon, lat, z, time)),
    ncdf4::ncvar_def("cell_mean", "1", list(lon, lat, z, time)),
    ncdf4::ncvar_def("z_bnds", "m", list(nv, z))
  ))
  ncdf4::ncvar_put(nc, "point_value", array(c(1, 21, 41), c(1, 1, 3, 1)))
  ncdf4::ncvar_put(nc, "cell_mean", array(c(10, 20, 30), c(1, 1, 3, 1)))
  ncdf4::ncvar_put(nc, "z_bnds", t(rbind(c(0, 10), c(10, 20), c(20, 30))))
  ncdf4::ncatt_put(nc, "z", "standard_name", "depth")
  ncdf4::ncatt_put(nc, "z", "positive", "down")
  ncdf4::ncatt_put(nc, "z", "axis", "Z")
  ncdf4::ncatt_put(nc, "z", "bounds", "z_bnds")
  ncdf4::ncatt_put(nc, "point_value", "cell_methods", "z: point")
  ncdf4::ncatt_put(nc, "cell_mean", "cell_methods", "z: mean")
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.13")
  ncdf4::nc_close(nc)
  path
}

test_that("analytic point secants use positive-down metres and midpoint locations", {
  x <- read_nc(c4_point_path(), vars = "temperature")
  result <- depth_gradient(x)
  descriptor <- result$metadata$cf$current$vertical_gradient

  expect_equal(as.numeric(result$data), rep(c(2, 2), each = 4L))
  expect_identical(result$depth, structure(c(5, 15), units = "m", positive = "down"))
  expect_identical(result$units, list(temperature = "K m-1"))
  expect_identical(descriptor$derivative_coordinate_unit, "m")
  expect_identical(descriptor$derivative_positive_direction, "down")
  expect_identical(descriptor$spacing_m, c(10, 10))
  expect_identical(descriptor$support_relation, rep("POINT_SUPPORT_UNBOUNDED", 2))
  expect_identical(descriptor$variables[[1L]]$output_value_semantics,
                   "POINT_SECANT_GRADIENT")
})

test_that("irregular and nonlinear point profiles use adjacent first-order secants", {
  irregular <- read_nc(c4_point_path(
    values = c(7, 22, 67, 217), depths = c(0, 5, 20, 70)
  ), vars = "temperature")
  nonlinear <- read_nc(c4_point_path(
    values = c(0, 100, 400), depths = c(0, 10, 20)
  ), vars = "temperature")

  expect_equal(as.numeric(depth_gradient(irregular)$data), rep(3, 12L))
  expect_equal(as.numeric(depth_gradient(nonlinear)$data), rep(c(10, 30), each = 4L))
  expect_identical(depth_gradient(nonlinear)$depth,
                   structure(c(5, 15), units = "m", positive = "down"))
})

test_that("cell means use representative-level secants for uniform and nonuniform cells", {
  uniform <- read_nc(c4_cell_path(), vars = "temperature")
  nonuniform <- read_nc(c4_cell_path(
    values = c(6, 26, 71), centres = c(2.5, 12.5, 35),
    bounds = rbind(c(0, 5), c(5, 20), c(20, 50))
  ), vars = "temperature")

  uniform_result <- depth_gradient(uniform)
  nonuniform_result <- depth_gradient(nonuniform)
  expect_equal(as.numeric(uniform_result$data), rep(2, 8L))
  expect_equal(as.numeric(nonuniform_result$data), rep(2, 8L))
  expect_identical(
    uniform_result$metadata$cf$current$vertical_gradient$support_relation,
    rep("CONTIGUOUS_SUPPORT", 2L)
  )
  expect_identical(
    uniform_result$metadata$cf$current$vertical_gradient$variables[[1L]]$
      output_value_semantics,
    "CELL_MEAN_SECANT_GRADIENT"
  )
})

test_that("gapped cell secants are computed but remain explicitly gapped", {
  x <- read_nc(c4_cell_path(
    values = c(5, 25, 70), centres = c(2.5, 12.5, 35),
    bounds = rbind(c(0, 5), c(10, 15), c(30, 40))
  ), vars = "temperature")
  result <- depth_gradient(x)
  descriptor <- result$metadata$cf$current$vertical_gradient

  expect_equal(as.numeric(result$data), rep(2, 8L))
  expect_identical(descriptor$support_relation, rep("GAPPED_SUPPORT", 2L))
  expect_equal(descriptor$support_gap_m, c(5, 15))
  expect_equal(descriptor$spacing_m, c(10, 22.5))
  expect_match(descriptor$gradient_equation, "X\\[i\\+1\\]")

  points <- read_nc(c4_point_path(
    values = c(5, 25, 70), depths = c(2.5, 12.5, 35),
    bounds = rbind(c(0, 5), c(10, 15), c(30, 40))
  ), vars = "temperature")
  point_descriptor <- depth_gradient(points)$metadata$cf$current$vertical_gradient
  expect_identical(point_descriptor$support_relation,
                   rep("GAPPED_SUPPORT", 2L))
  expect_equal(point_descriptor$support_gap_m, c(5, 15))
})

test_that("overlapping or absent cell support is not certified", {
  overlap <- read_nc(c4_cell_path(
    bounds = rbind(c(0, 12), c(10, 22), c(22, 30))
  ), vars = "temperature")
  no_bounds <- read_nc(make_cf_vertical_fixture(
    values = c(5, 15, 25), units = "m", cell_methods = "z: mean",
    data_values = rep(c(11, 31, 51), each = 4L), variable_units = "K"
  ), vars = "temperature")

  expect_error(depth_gradient(overlap),
               class = "oceancube_vertical_gradient_geometry_unsupported")
  expect_error(depth_gradient(no_bounds),
               class = "oceancube_vertical_gradient_geometry_unsupported")
})

test_that("descending storage and metre-kilometre coordinates are invariant", {
  metres <- read_nc(c4_point_path(), vars = "temperature")
  kilometres <- read_nc(c4_point_path(unit = "km"), vars = "temperature")
  descending <- read_nc(c4_point_path(descending = TRUE), vars = "temperature")

  expect_equal(as.numeric(depth_gradient(kilometres)$data),
               as.numeric(depth_gradient(metres)$data))
  expect_equal(as.numeric(depth_gradient(descending)$data), rep(2, 8L))
  expect_identical(depth_gradient(descending)$depth,
                   structure(c(15, 5), units = "m", positive = "down"))
  expect_identical(depth_gradient(kilometres)$units,
                   list(temperature = "K m-1"))
})

test_that("missingness is local and missing levels are never bridged", {
  x <- read_nc(c4_point_path(
    values = c(1, NA, 41, 61), depths = c(0, 10, 20, 30)
  ), vars = "temperature")
  values <- as.numeric(depth_gradient(x)$data)

  expect_true(all(is.na(values[1:8])))
  expect_equal(values[9:12], rep(2, 4L))
})

test_that("mixed auto resolves per variable and explicit methods fail atomically", {
  x <- read_nc(c4_mixed_path(), vars = c("point_value", "cell_mean"))
  result <- depth_gradient(x)
  descriptor <- result$metadata$cf$current$vertical_gradient

  expect_equal(as.numeric(result$data[, , , , 1L]), c(2, 2))
  expect_equal(as.numeric(result$data[, , , , 2L]), c(1, 1))
  expect_identical(descriptor$resolved_method, "mixed")
  expect_identical(
    vapply(descriptor$variables, `[[`, character(1L), "resolved_method"),
    c("point", "cell")
  )
  expect_identical(result$units,
                   list(point_value = "K m-1", cell_mean = "m-1"))
  expect_error(depth_gradient(x, method = "point"),
               class = "oceancube_vertical_gradient_semantics_unsupported")
  expect_error(depth_gradient(x, method = "cell"),
               class = "oceancube_vertical_gradient_semantics_unsupported")
})

test_that("C3 current semantics permit derived points and reject cell reconstructions", {
  point <- read_nc(c4_point_path(), vars = "temperature")
  derived_point <- depth_sample(point, c(2, 8, 12, 18), method = "linear")
  point_gradient <- depth_gradient(derived_point)
  point_descriptor <- point_gradient$metadata$cf$current$vertical_gradient

  expect_equal(as.numeric(point_gradient$data), rep(2, 12L))
  expect_identical(
    point_descriptor$variables[[1L]]$input_value_semantics,
    "INTERPOLATED_POINT_VALUE"
  )
  expect_identical(
    point_descriptor$variables[[1L]]$output_value_semantics,
    "DERIVED_POINT_SECANT_GRADIENT"
  )
  expect_null(point_gradient$metadata$cf$current$vertical_sampling)

  cells <- read_nc(c4_cell_path(), vars = "temperature")
  reconstructed <- depth_sample(cells, c(2, 7, 12, 28), method = "cell")
  expect_error(
    depth_gradient(reconstructed), regexp = "C3_CELL_RECONSTRUCTION_UNSUPPORTED",
    class = "oceancube_vertical_gradient_semantics_unsupported"
  )

  mixed <- read_nc(c4_mixed_path(), vars = c("point_value", "cell_mean"))
  mixed_sampled <- depth_sample(mixed, c(7, 12, 18, 23), method = "auto")
  expect_error(
    depth_gradient(mixed_sampled), regexp = "cell_mean=C3_CELL_RECONSTRUCTION_UNSUPPORTED",
    class = "oceancube_vertical_gradient_semantics_unsupported"
  )
})

test_that("certified layer means are supported and metric integrals are rejected", {
  x <- read_nc(c4_cell_path(), vars = "temperature")
  means <- layer_mean(x, c(0, 10, 20, 30))
  integrals <- layer_integral(x, c(0, 10, 20, 30))
  result <- depth_gradient(means)

  expect_equal(as.numeric(result$data), rep(2, 8L))
  expect_identical(
    result$metadata$cf$current$vertical_gradient$variables[[1L]]$
      input_value_semantics,
    "CERTIFIED_OVERLAP_WEIGHTED_LAYER_MEAN"
  )
  expect_null(result$metadata$cf$current$vertical_reduction)
  expect_error(
    depth_gradient(integrals), regexp = "VERTICAL_INTEGRAL_UNSUPPORTED",
    class = "oceancube_vertical_gradient_semantics_unsupported"
  )
})

test_that("gradient outputs have no cell support and cannot be differentiated again", {
  x <- read_nc(c4_point_path(), vars = "temperature")
  result <- depth_gradient(x)
  vertical <- result$metadata$cf$current$vertical

  expect_identical(vertical$geometry_status, "GEOMETRY_NO_BOUNDS")
  expect_identical(vertical$bounds_status, "BOUNDS_MISSING")
  expect_identical(vertical$bounds, list())
  expect_null(attr(result$depth, "bounds", exact = TRUE))
  expect_error(cube_layer_thickness(result),
               class = "oceancube_vertical_geometry_unsupported")
  expect_error(cube_cell_volume(result),
               class = "oceancube_vertical_geometry_unsupported")
  expect_error(layer_integral(result, c(5, 15)),
               class = "oceancube_vertical_geometry_unsupported")
  expect_error(depth_gradient(result), regexp = "VERTICAL_GRADIENT_OUTPUT_UNSUPPORTED",
               class = "oceancube_vertical_gradient_semantics_unsupported")
})

test_that("units are required and symbolic derivation is bounded", {
  dimensionless <- read_nc(c4_point_path(variable_unit = "1"), vars = "temperature")
  unknown <- read_nc(c4_point_path(variable_unit = "furlong"), vars = "temperature")
  missing <- read_nc(c4_point_path(variable_unit = ""), vars = "temperature")

  expect_identical(depth_gradient(dimensionless)$units,
                   list(temperature = "m-1"))
  expect_identical(depth_gradient(unknown)$units,
                   list(temperature = "furlong m-1"))
  expect_identical(
    depth_gradient(unknown)$metadata$cf$current$vertical_gradient$
      variables[[1L]]$unit_status,
    "SYMBOLIC_UNNORMALIZED"
  )
  expect_error(depth_gradient(missing),
               class = "oceancube_vertical_gradient_unit_unsupported")
})

test_that("unsupported vertical kinds and singleton surfaces remain rejected", {
  pressure <- read_nc(make_cf_vertical_fixture(
    units = "dbar", standard_name = "sea_water_pressure", positive = NULL,
    cell_methods = "z: point", variable_units = "K"
  ), vars = "temperature")
  height <- read_nc(make_cf_vertical_fixture(
    units = "m", standard_name = "height", positive = "up",
    cell_methods = "z: point", variable_units = "K"
  ), vars = "temperature")
  oisst <- read_nc(
    test_path("fixtures", "real-data", "noaa-oisst21-surface-time-fv1.nc"),
    vars = "sst"
  )
  positive_up <- read_nc(make_cf_vertical_fixture(
    values = c(0, -10, -20), units = "m", standard_name = "depth",
    positive = "up", cell_methods = "z: point",
    data_values = rep(c(1, 21, 41), each = 4L), variable_units = "K"
  ), vars = "temperature")
  manual <- ocean_cube(
    lon = -80, lat = -12, depth = c(0, 10),
    time = as.Date("2000-01-01"), vars = "temperature",
    data = array(c(1, 21), c(1, 1, 2, 1, 1)), units = "K"
  )

  expect_error(depth_gradient(pressure),
               class = "oceancube_vertical_gradient_unsupported")
  expect_error(depth_gradient(height),
               class = "oceancube_vertical_gradient_unsupported")
  expect_error(depth_gradient(oisst),
               class = "oceancube_vertical_gradient_unsupported")
  expect_identical(
    positive_up$metadata$cf$current$vertical$runtime_status,
    "VERTICAL_CONFLICT"
  )
  expect_error(depth_gradient(positive_up),
               class = "oceancube_vertical_gradient_unsupported")
  expect_error(depth_gradient(manual),
               class = "oceancube_vertical_gradient_unsupported")
})

test_that("deferred gradients perform one complete-profile indexed read", {
  x <- cube_open(c4_point_path(), vars = "temperature")
  original <- oceancube:::.cube_read_netcdf
  observed <- list()
  testthat::local_mocked_bindings(
    .cube_read_netcdf = function(x, index = NULL, drop = FALSE) {
      observed[[length(observed) + 1L]] <<- index$depth
      original(x, index = index, drop = drop)
    },
    .package = "oceancube"
  )

  result <- depth_gradient(x)
  expect_equal(as.numeric(result$data), rep(2, 8L))
  expect_identical(observed, list(1:3))
})

test_that("WOA monthly produces cell-mean secants with explicit gap evidence", {
  path <- test_path("fixtures", "real-data", "noaa-woa23-monthly-vertical-fv1.nc")
  x <- read_nc(path, vars = c("t_an", "s_an"))
  result <- depth_gradient(x)
  descriptor <- result$metadata$cf$current$vertical_gradient

  expected <- (x$data[, , 2L, , 1L] - x$data[, , 1L, , 1L]) / 10
  expect_equal(as.numeric(result$data[, , 1L, , 1L]), as.numeric(expected))
  expect_identical(result$depth[[1L]], 5)
  expect_identical(descriptor$spacing_m[[1L]], 10)
  expect_true(any(descriptor$support_relation == "GAPPED_SUPPORT"))
  expect_true(all(descriptor$support_gap_m >= 0))
  expect_true(all(vapply(
    descriptor$variables,
    function(item) identical(item$output_value_semantics,
                              "CELL_MEAN_SECANT_GRADIENT"),
    logical(1L)
  )))
})

test_that("WOA deferred gradient reads all scientifically required levels once", {
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

  result <- depth_gradient(x)
  expect_identical(dim(result$data)[[3L]], length(x$depth) - 1L)
  expect_identical(observed, list(seq_along(x$depth)))
})

test_that("metadata provenance serialization and privacy are deterministic", {
  x <- read_nc(c4_point_path(), vars = "temperature")
  source_before <- serialize(x$metadata$cf$source, NULL)
  first <- depth_gradient(x)
  second <- depth_gradient(x)
  restored <- unserialize(serialize(first, NULL))
  path <- tempfile("oceancube-c4-roundtrip-", fileext = ".rds")
  saveRDS(first, path)
  from_rds <- readRDS(path)
  step <- tail(first$provenance$history, 1L)[[1L]]

  expect_identical(first, second)
  expect_identical(restored, first)
  expect_identical(from_rds, first)
  expect_identical(serialize(first$metadata$cf$source, NULL), source_before)
  expect_identical(
    first$metadata$cf$current$vertical_gradient$standard_name_status,
    "DERIVATION_PENDING"
  )
  expect_identical(
    first$metadata$cf$current$vertical_gradient$cell_methods_status,
    "DERIVATION_PENDING_NO_SYNTHETIC_CLAIM"
  )
  expect_identical(step$operation, "depth_gradient")
  expect_identical(step$scientific_method$id,
                   "oceancube:adjacent_point_secant_gradient")
  canonical <- paste(capture.output(str(list(
    first$metadata$cf$current$vertical_gradient,
    step$parameters
  ))), collapse = " ")
  expect_false(grepl("[A-Za-z]:[/\\\\]", canonical))
  expect_false(grepl("oquispe|hostname|tempfile", canonical, ignore.case = TRUE))
})

test_that("C4 adds exactly one public signature", {
  expect_identical(names(formals(depth_gradient)), c("x", "method"))
  expect_identical(length(getNamespaceExports("oceancube")), 43L)
})
