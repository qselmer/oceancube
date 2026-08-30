monthly_woa_fixture <- function() {
  test_path("fixtures", "real-data", "noaa-woa23-monthly-vertical-fv1.nc")
}

test_that("WOA23 January carries the certified current vertical descriptor", {
  file <- monthly_woa_fixture()
  eager <- read_nc(file, vars = c("t_an", "s_an"))
  deferred <- cube_open(file, vars = c("t_an", "s_an"))
  vertical <- eager$metadata$cf$current$vertical

  expect_identical(vertical, deferred$metadata$cf$current$vertical)
  expect_identical(vertical$schema_name, "oceancube_cf_vertical")
  expect_identical(vertical$schema_version, "1.0.0")
  expect_identical(vertical$kind, "DEPTH_LENGTH")
  expect_identical(vertical$runtime_status, "VERTICAL_RUNTIME_SUPPORTED")
  expect_identical(vertical$geometry_status, "GEOMETRY_METRIC_BOUNDS_SUPPORTED")
  expect_identical(vertical$normalized_unit, "m")
  expect_identical(vertical$positive, "down")
  expect_identical(vertical$source_order, "INCREASING")
  expect_identical(vertical$coverage_contiguous, FALSE)
  expect_identical(vertical$source_coordinate, c(0, 10, 20, 50, 100, 200))
  expect_identical(vertical$bounds_shape, c(6L, 2L))
  expect_identical(
    do.call(rbind, vertical$bounds),
    rbind(
      c(0, 2.5), c(7.5, 12.5), c(17.5, 22.5),
      c(47.5, 52.5), c(97.5, 112.5), c(187.5, 212.5)
    )
  )
  expect_identical(eager$metadata$cf$source, deferred$metadata$cf$source)
})

test_that("WOA23 January geometry uses CF bounds without payload reads", {
  skip_if_not_installed("sf")
  x <- cube_open(monthly_woa_fixture(), vars = c("t_an", "s_an"))
  testthat::local_mocked_bindings(
    .cube_read_netcdf = function(...) stop("scientific payload read"),
    .package = "oceancube"
  )

  thickness <- cube_layer_thickness(x, unit = "m")
  volume <- cube_cell_volume(x, unit = "m3")
  expect_identical(as.numeric(thickness), c(2.5, 5, 5, 5, 15, 25))
  expect_identical(attr(thickness, "bounds_source"), "CF depth_bnds")
  expect_identical(dim(volume), c(9L, 12L, 6L))
  expect_true(all(is.finite(volume)))
  expect_true(all(volume > 0))
})

test_that("collect and depth selections preserve current vertical truth", {
  source <- cube_open(monthly_woa_fixture(), vars = c("t_an", "s_an"))
  collected <- cube_collect(source)
  expect_identical(
    collected$metadata$cf$current$vertical,
    source$metadata$cf$current$vertical
  )

  selected <- cube_slice(collected, depth = c(20, 100), by = "value")
  expect_identical(
    selected$metadata$cf$current$vertical$source_coordinate,
    c(20, 100)
  )
  expect_identical(
    do.call(rbind, selected$metadata$cf$current$vertical$bounds),
    rbind(c(17.5, 22.5), c(97.5, 112.5))
  )
  expect_identical(selected$metadata$cf$current$vertical$coverage_contiguous, FALSE)
  expect_identical(
    selected$metadata$cf$source,
    collected$metadata$cf$source
  )

  cropped <- cube_crop(collected, depth = c(10, 50))
  expect_identical(
    cropped$metadata$cf$current$vertical$source_coordinate,
    c(10, 20, 50)
  )
  expect_length(cropped$metadata$cf$current$vertical$bounds, 3L)
})

test_that("vertical metadata is deterministic serializable and path-private", {
  first <- read_nc(monthly_woa_fixture(), vars = "t_an")$metadata$cf$current$vertical
  second <- read_nc(monthly_woa_fixture(), vars = "t_an")$metadata$cf$current$vertical
  expect_identical(first, second)
  expect_identical(unserialize(serialize(first, NULL)), first)
  path <- tempfile(fileext = ".rds")
  saveRDS(first, path)
  expect_identical(readRDS(path), first)
  text <- paste(capture.output(str(first)), collapse = " ")
  expect_false(grepl("oquispe|\\.packages|Temp|hostname", text, ignore.case = TRUE))
})

test_that("missing and malformed depth semantics block metric geometry", {
  valid_bounds <- rbind(c(-1, 4), c(4, 14), c(14, 24))
  cases <- list(
    missing_units = list(units = NULL, positive = "down", status = "VERTICAL_UNIT_UNSUPPORTED"),
    missing_positive = list(units = "m", positive = NULL, status = "VERTICAL_CONFLICT"),
    invalid_positive = list(units = "m", positive = "sideways", status = "VERTICAL_CONFLICT"),
    semantic_conflict = list(units = "m", positive = "up", status = "VERTICAL_CONFLICT")
  )
  for (case in cases) {
    file <- do.call(make_cf_vertical_fixture, c(case[c("units", "positive")], list(
      bounds = valid_bounds
    )))
    x <- read_nc(file, vars = "temperature")
    expect_identical(x$metadata$cf$current$vertical$runtime_status, case$status)
    expect_error(cube_layer_thickness(x), class = "oceancube_vertical_geometry_unsupported")
  }

  duplicate <- make_cf_vertical_fixture(values = c(0, 10, 10))
  nonmonotonic <- make_cf_vertical_fixture(values = c(0, 20, 10))
  for (file in c(duplicate, nonmonotonic)) {
    malformed <- read_nc(file, vars = "temperature")
    expect_identical(
      malformed$metadata$cf$current$vertical$runtime_status,
      "VERTICAL_CONFLICT"
    )
    expect_error(
      cube_layer_thickness(malformed, rbind(c(-1, 4), c(4, 14), c(14, 24))),
      class = "oceancube_vertical_geometry_unsupported"
    )
  }
})

test_that("vertical bounds statuses are deterministic and safe", {
  configurations <- list(
    missing = list(bounds = NULL, expected = "BOUNDS_MISSING"),
    wrong_shape = list(
      bounds = rbind(c(-1, 4, 5), c(4, 14, 15), c(14, 24, 25)),
      bounds_shape = "wrong", expected = "BOUNDS_INVALID_SHAPE"
    ),
    overlap = list(bounds = rbind(c(-1, 8), c(7, 14), c(14, 24)), expected = "BOUNDS_OVERLAP"),
    outside = list(bounds = rbind(c(1, 4), c(4, 14), c(14, 24)), expected = "BOUNDS_CENTRE_OUTSIDE"),
    unit_mismatch = list(
      bounds = rbind(c(-1, 4), c(4, 14), c(14, 24)),
      bounds_units = "Pa", expected = "BOUNDS_UNIT_INCOMPATIBLE"
    )
  )
  for (configuration in configurations) {
    expected <- configuration$expected
    configuration$expected <- NULL
    file <- do.call(make_cf_vertical_fixture, configuration)
    x <- read_nc(file, vars = "temperature")
    expect_identical(x$metadata$cf$current$vertical$bounds_status, expected)
    expect_error(cube_layer_thickness(x), class = "oceancube_vertical_geometry_unsupported")
  }
})

test_that("source order is independent of positive and m/km bounds convert explicitly", {
  descending_file <- make_cf_vertical_fixture(
    values = c(20, 10, 0),
    bounds = rbind(c(14, 24), c(4, 14), c(-1, 4))
  )
  descending <- read_nc(descending_file, vars = "temperature")
  expect_identical(
    descending$metadata$cf$current$vertical$source_order,
    "DECREASING"
  )
  expect_identical(descending$metadata$cf$current$vertical$positive, "down")
  expect_identical(as.numeric(cube_layer_thickness(descending)), c(10, 10, 5))

  kilometre_bounds <- make_cf_vertical_fixture(
    bounds = rbind(c(-0.001, 0.004), c(0.004, 0.014), c(0.014, 0.024)),
    bounds_units = "km"
  )
  converted <- read_nc(kilometre_bounds, vars = "temperature")
  expect_identical(
    converted$metadata$cf$current$vertical$geometry_status,
    "GEOMETRY_METRIC_BOUNDS_SUPPORTED"
  )
  expect_equal(
    as.numeric(cube_layer_thickness(converted, unit = "m")),
    c(5, 10, 10),
    tolerance = 1e-7
  )
})

test_that("height pressure dimensionless and parametric axes remain non-geometric", {
  bounds <- rbind(c(0, 5), c(5, 15), c(15, 25))
  height <- read_nc(make_cf_vertical_fixture(
    standard_name = "height", positive = "up", bounds = bounds
  ), vars = "temperature")
  pressure <- read_nc(make_cf_vertical_fixture(
    units = "dbar", standard_name = NULL, positive = NULL, bounds = bounds,
    bounds_units = "dbar", axis = NULL, vertical_name = "pressure_coordinate"
  ), vars = "temperature")
  dimensionless <- read_nc(make_cf_vertical_fixture(
    units = "1", standard_name = NULL, positive = "down"
  ), vars = "temperature")
  parametric <- read_nc(make_cf_vertical_fixture(
    values = c(0.1, 0.5, 0.9), units = "1",
    standard_name = "ocean_sigma_coordinate", positive = "down",
    formula_terms = "sigma: z eta: eta depth: bathymetry"
  ), vars = "temperature")

  expect_identical(height$metadata$cf$current$vertical$kind, "HEIGHT_LENGTH")
  expect_identical(pressure$metadata$cf$current$vertical$kind, "PRESSURE")
  expect_identical(pressure$metadata$cf$current$vertical$positive, "down")
  expect_identical(dimensionless$metadata$cf$current$vertical$kind, "DIMENSIONLESS_GENERIC")
  expect_identical(parametric$metadata$cf$current$vertical$kind, "PARAMETRIC")
  expect_identical(
    parametric$metadata$cf$current$vertical$formula_terms_status,
    "PARAMETRIC_STRUCTURALLY_RECOGNIZED"
  )
  expect_identical(
    parametric$metadata$cf$current$vertical$runtime_status,
    "VERTICAL_PARAMETRIC_DEFERRED"
  )
  for (x in list(height, pressure, dimensionless, parametric)) {
    expect_error(cube_cell_volume(x), class = "oceancube_vertical_geometry_unsupported")
  }
})

test_that("OISST explicit singleton and no-depth surface remain distinct", {
  oisst <- read_nc(
    test_path("fixtures", "real-data", "noaa-oisst21-surface-time-fv1.nc"),
    vars = "sst", depth_name = "zlev"
  )
  explicit <- oisst$metadata$cf$current$vertical
  expect_identical(explicit$kind, "DEPTH_LENGTH")
  expect_identical(explicit$surface_status, "EXPLICIT_SINGLETON")
  expect_identical(explicit$geometry_status, "GEOMETRY_NO_BOUNDS")
  expect_error(cube_layer_thickness(oisst), class = "oceancube_vertical_geometry_unsupported")

  file <- make_netcdf_backend_fixture()
  surface <- cube_open(file, vars = "sst")
  expect_identical(surface$metadata$cf$current$vertical$kind, "SURFACE_SINGLETON")
})

test_that("meaning-changing transforms make vertical semantics pending", {
  x <- read_nc(monthly_woa_fixture(), vars = "t_an")
  transformed <- layer_mean(x, c(0, 50, 200))
  vertical <- transformed$metadata$cf$current$vertical
  expect_identical(vertical$runtime_status, "VERTICAL_DERIVATION_PENDING")
  expect_identical(vertical$geometry_status, "GEOMETRY_DERIVATION_PENDING")
  expect_false(vertical$canonical_metric)
})
