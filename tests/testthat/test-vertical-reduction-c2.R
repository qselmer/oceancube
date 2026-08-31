c2_vertical_path <- function(
    values = c(1, 3, 5), unit = "K", depth_unit = "m",
    cell_methods = "area: mean z: mean time: point",
    descending = FALSE) {
  centres <- c(5, 15, 25)
  bounds <- rbind(c(0, 10), c(10, 20), c(20, 30))
  if (identical(depth_unit, "km")) {
    centres <- centres / 1000
    bounds <- bounds / 1000
  }
  if (isTRUE(descending)) {
    centres <- rev(centres)
    bounds <- bounds[3:1, , drop = FALSE]
    values <- rev(values)
  }
  make_cf_vertical_fixture(
    values = centres,
    units = depth_unit,
    bounds = bounds,
    bounds_units = depth_unit,
    cell_methods = cell_methods,
    data_values = rep(values, each = 4L),
    variable_units = unit
  )
}

c2_mixed_path <- function(salinity_method = "z: point") {
  file <- tempfile("oceancube-c2-mixed-", fileext = ".nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", -80)
  lat <- ncdf4::ncdim_def("lat", "degrees_north", -12)
  z <- ncdf4::ncdim_def("z", "m", c(5, 15, 25))
  time <- ncdf4::ncdim_def("time", "days since 2000-01-01", 0)
  nv <- ncdf4::ncdim_def("nv", "", 1:2, create_dimvar = FALSE)
  nc <- ncdf4::nc_create(file, list(
    ncdf4::ncvar_def("temperature", "K", list(lon, lat, z, time)),
    ncdf4::ncvar_def("salinity", "1", list(lon, lat, z, time)),
    ncdf4::ncvar_def("z_bnds", "m", list(nv, z))
  ))
  ncdf4::ncvar_put(nc, "temperature", array(c(1, 3, 5), c(1, 1, 3, 1)))
  ncdf4::ncvar_put(nc, "salinity", array(c(10, 20, 30), c(1, 1, 3, 1)))
  ncdf4::ncvar_put(nc, "z_bnds", t(rbind(c(0, 10), c(10, 20), c(20, 30))))
  ncdf4::ncatt_put(nc, "z", "standard_name", "depth")
  ncdf4::ncatt_put(nc, "z", "positive", "down")
  ncdf4::ncatt_put(nc, "z", "axis", "Z")
  ncdf4::ncatt_put(nc, "z", "bounds", "z_bnds")
  ncdf4::ncatt_put(nc, "temperature", "cell_methods", "z: mean")
  ncdf4::ncatt_put(nc, "salinity", "cell_methods", salinity_method)
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.13")
  ncdf4::nc_close(nc)
  file
}

test_that("certified reductions reproduce analytic full and clipped layers", {
  x <- read_nc(c2_vertical_path(), vars = "temperature")

  full_mean <- layer_mean(x, c(0, 30))
  full_integral <- layer_integral(x, c(0, 30))
  clipped_mean <- layer_mean(x, c(5, 25))
  clipped_integral <- layer_integral(x, c(5, 25))
  multiple <- layer_integral(x, c(0, 10, 30))

  expect_equal(as.numeric(full_mean$data), rep(3, 4))
  expect_equal(as.numeric(full_integral$data), rep(90, 4))
  expect_equal(as.numeric(clipped_mean$data), rep(3, 4))
  expect_equal(as.numeric(clipped_integral$data), rep(60, 4))
  expect_equal(as.numeric(multiple$data), rep(c(10, 80), each = 4))
  expect_identical(full_integral$units, list(temperature = "K m"))
  expect_identical(
    full_integral$metadata$cf$current$vertical_reduction$certification_status,
    "CERTIFIED"
  )
})

test_that("subcell integration is piecewise-constant and units convert to metres", {
  metres <- read_nc(c2_vertical_path(values = c(4, 3, 5)), vars = "temperature")
  kilometres <- read_nc(c2_vertical_path(depth_unit = "km"), vars = "temperature")
  descending <- read_nc(c2_vertical_path(descending = TRUE), vars = "temperature")

  expect_equal(as.numeric(layer_integral(metres, c(2, 8))$data), rep(24, 4))
  expect_equal(
    as.numeric(layer_integral(kilometres, c(0, 0.03))$data),
    rep(90, 4),
    tolerance = 1e-5
  )
  expect_equal(as.numeric(layer_integral(descending, c(0, 30))$data), rep(90, 4))
})

test_that("nonuniform explicit cells use exact overlap length", {
  path <- make_cf_vertical_fixture(
    values = c(1, 6, 20), units = "m",
    bounds = rbind(c(0, 2), c(2, 10), c(10, 30)), bounds_units = "m",
    cell_methods = "z: mean", data_values = rep(c(2, 4, 6), each = 4),
    variable_units = "K"
  )
  x <- read_nc(path, vars = "temperature")

  expect_equal(as.numeric(layer_mean(x, c(0, 30))$data), rep(5.2, 4))
  expect_equal(as.numeric(layer_integral(x, c(0, 30))$data), rep(156, 4))
})

test_that("integral missingness is strict while means retain renormalization", {
  x <- read_nc(c2_vertical_path(values = c(1, NA, 5)), vars = "temperature")

  expect_true(all(is.na(layer_integral(x, c(0, 30))$data)))
  expect_equal(as.numeric(layer_mean(x, c(0, 30))$data), rep(3, 4))
})

test_that("coverage and deferred reads retain the C1 safety contract", {
  path <- c2_vertical_path()
  deferred <- cube_open(path, vars = "temperature")
  original <- oceancube:::.cube_read_netcdf
  observed <- list()
  testthat::local_mocked_bindings(
    .cube_read_netcdf = function(x, index = NULL, drop = FALSE) {
      observed[[length(observed) + 1L]] <<- index$depth
      original(x, index = index, drop = drop)
    },
    .package = "oceancube"
  )

  result <- layer_integral(deferred, c(0, 10))
  expect_equal(as.numeric(result$data), rep(10, 4))
  expect_identical(observed, list(1L))
  expect_error(
    layer_integral(deferred, c(0, 35)),
    class = "oceancube_vertical_partial_coverage"
  )
  expect_identical(observed, list(1L))
  outside <- layer_integral(deferred, c(40, 50))
  expect_true(all(is.na(outside$data)))
  expect_identical(observed, list(1L))
})

test_that("only exact CF vertical cell means are eligible", {
  statuses <- c(
    "z: point" = "VERTICAL_POINT",
    "z: sum" = "VERTICAL_CELL_SUM",
    "z: maximum" = "VERTICAL_CELL_OTHER",
    "z: mean z: sum" = "VERTICAL_CELL_METHOD_AMBIGUOUS",
    "depth_aux: mean" = "VERTICAL_POINT"
  )
  for (method in names(statuses)) {
    x <- read_nc(c2_vertical_path(cell_methods = method), vars = "temperature")
    expect_error(
      layer_integral(x, c(0, 30)),
      regexp = statuses[[method]],
      class = "oceancube_vertical_value_semantics_unsupported"
    )
  }
  missing <- read_nc(c2_vertical_path(cell_methods = NULL), vars = "temperature")
  expect_error(
    layer_integral(missing, c(0, 30)),
    regexp = "VERTICAL_POINT",
    class = "oceancube_vertical_value_semantics_unsupported"
  )
})

test_that("mixed-variable operations fail atomically and name incompatibilities", {
  x <- read_nc(c2_mixed_path(), vars = c("temperature", "salinity"))
  expect_error(
    layer_integral(x, c(0, 30)),
    regexp = "salinity=VERTICAL_POINT",
    class = "oceancube_vertical_value_semantics_unsupported"
  )
})

test_that("multiple variables retain aligned deterministic derived units", {
  x <- read_nc(
    c2_mixed_path(salinity_method = "z: mean"),
    vars = c("temperature", "salinity")
  )
  result <- layer_integral(x, c(0, 30))

  expect_identical(result$units, list(temperature = "K m", salinity = "m"))
  expect_equal(as.numeric(result$data[, , , , 1]), 90)
  expect_equal(as.numeric(result$data[, , , , 2]), 600)

  selected <- cube_slice(result, variable = "temperature", by = "value")
  descriptor <- selected$metadata$cf$current$vertical_reduction
  expect_identical(
    vapply(descriptor$input_value_semantics, `[[`, character(1L), "variable"),
    "temperature"
  )
  expect_identical(descriptor$derived_units, list(temperature = "K m"))
})

test_that("symbolic derived units are bounded and missing units fail", {
  dimensionless <- read_nc(c2_vertical_path(unit = "1"), vars = "temperature")
  missing <- read_nc(c2_vertical_path(unit = ""), vars = "temperature")

  expect_identical(layer_integral(dimensionless, c(0, 30))$units,
                   list(temperature = "m"))
  expect_error(
    layer_integral(missing, c(0, 30)),
    class = "oceancube_vertical_unit_semantics_unsupported"
  )
})

test_that("non-depth vertical kinds fail before value integration", {
  cases <- list(
    pressure = list(
      units = "dbar", standard_name = "sea_water_pressure", positive = NULL,
      bounds = rbind(c(0, 10), c(10, 20), c(20, 30))
    ),
    height = list(
      units = "m", standard_name = "height", positive = "up",
      bounds = rbind(c(0, 10), c(10, 20), c(20, 30))
    ),
    dimensionless = list(
      units = "1", standard_name = NULL, positive = "down",
      bounds = rbind(c(0, 10), c(10, 20), c(20, 30))
    )
  )
  for (case in cases) {
    path <- make_cf_vertical_fixture(
      values = c(5, 15, 25), units = case$units,
      standard_name = case$standard_name, positive = case$positive,
      bounds = case$bounds, bounds_units = case$units,
      cell_methods = "z: mean", data_values = rep(c(1, 3, 5), each = 4)
    )
    x <- read_nc(path, vars = "temperature")
    expect_error(
      layer_integral(x, c(0, 30)),
      class = "oceancube_vertical_geometry_unsupported"
    )
  }

  parametric <- read_nc(make_cf_vertical_fixture(
    values = c(0.1, 0.5, 0.9), units = "1",
    standard_name = "ocean_sigma_coordinate", positive = "down",
    formula_terms = "sigma: z eta: eta depth: bathymetry",
    cell_methods = "z: mean"
  ), vars = "temperature")
  expect_error(
    layer_integral(parametric, c(0, 1)),
    class = "oceancube_vertical_geometry_unsupported"
  )
})

test_that("source CF metadata is immutable and no false depth sum is asserted", {
  x <- read_nc(c2_vertical_path(), vars = "temperature")
  source_before <- serialize(x$metadata$cf$source, NULL)
  result <- layer_integral(x, c(0, 30))
  descriptor <- result$metadata$cf$current$vertical_reduction

  expect_identical(serialize(result$metadata$cf$source, NULL), source_before)
  expect_identical(descriptor$current_standard_name_status, "DERIVATION_PENDING")
  expect_identical(
    descriptor$cf_cell_method_status,
    "DERIVATION_PENDING_NO_SYNTHETIC_VERTICAL_SUM"
  )
  expect_false(grepl(
    "depth: sum",
    paste(capture.output(str(result$metadata$cf$current)), collapse = " "),
    fixed = TRUE
  ))
})

test_that("integral provenance is deterministic and schema-valid", {
  x <- read_nc(c2_vertical_path(), vars = "temperature")
  first <- layer_integral(x, c(5, 25))
  second <- layer_integral(x, c(5, 25))
  step <- tail(first$provenance$history, 1L)[[1L]]
  resolved <- step$parameters$resolved

  expect_true(.provenance_validate(first$provenance, strict = TRUE)$valid)
  expect_identical(step$operation, "layer_integral")
  expect_identical(step$scientific_method$id,
                   "oceancube:vertical_metric_integral")
  expect_identical(resolved$overlap_weights_source_unit, list(c(5, 10, 5)))
  expect_identical(resolved$overlap_weights_m, list(c(5, 10, 5)))
  expect_identical(resolved$subcell_assumption,
                   "piecewise_constant_cell_mean")
  expect_identical(first$provenance, second$provenance)
})

test_that("WOA23 monthly cell means support exact and subcell integration", {
  path <- test_path("fixtures", "real-data", "noaa-woa23-monthly-vertical-fv1.nc")
  eager <- read_nc(path, vars = "t_an")
  source_value <- as.numeric(eager$data[, , 2, , , drop = FALSE])

  exact <- layer_integral(eager, c(7.5, 12.5))
  subcell <- layer_integral(cube_open(path, vars = "t_an"), c(8, 12))

  expect_equal(as.numeric(exact$data), source_value * 5)
  expect_equal(as.numeric(subcell$data), source_value * 4)
  expect_error(
    layer_integral(eager, c(0, 50)),
    class = "oceancube_vertical_partial_coverage"
  )
})

test_that("C2 adds one stable public signature", {
  expect_identical(names(formals(layer_integral)), c("x", "depth"))
  expect_identical(length(getNamespaceExports("oceancube")), 43L)
})
