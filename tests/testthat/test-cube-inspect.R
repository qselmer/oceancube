test_that("cube_inspect describes a valid memory cube", {
  cube <- .make_baseline_fixture()$cube
  inspection <- cube_inspect(cube)

  expect_identical(inspection$backend, "memory")
  expect_identical(
    inspection$dimensions,
    c(longitude = 3L, latitude = 2L, depth = 2L, time = 4L, variable = 2L)
  )
  expect_identical(names(inspection$dimensions), .cube_axis_names())
  expect_identical(inspection$storage_dimensions, inspection$dimensions)
  expect_identical(inspection$coordinate_ranges$longitude, c(-80, -78))
  expect_identical(inspection$coordinate_ranges$latitude, c(-12, -11))
  expect_identical(inspection$variables, c("temperature", "oxygen"))
  expect_identical(names(inspection$units), inspection$variables)
})

test_that("coordinate resolution distinguishes regular and irregular axes", {
  regular <- .make_baseline_fixture()$cube
  regular$time <- as.Date("2020-01-01") + 0:3
  regular$temporal_extent <- range(regular$time)
  irregular <- regular
  irregular$lon <- c(-80, -79.5, -78)
  irregular$spatial_extent <- c(
    lon_min = -80, lon_max = -78, lat_min = -12, lat_max = -11
  )

  regular_inspection <- cube_inspect(regular)
  irregular_inspection <- cube_inspect(irregular)

  expect_true(regular_inspection$coordinate_resolution$longitude$regular)
  expect_equal(regular_inspection$coordinate_resolution$longitude$resolution, 1)
  expect_false(irregular_inspection$coordinate_resolution$longitude$regular)
  expect_true(is.na(irregular_inspection$coordinate_resolution$longitude$resolution))
  expect_true(regular_inspection$time_resolution$regular)
})

test_that("estimated memory uses the canonical materialized size", {
  cube <- .make_baseline_fixture()$cube

  inspection <- cube_inspect(cube, missing = "none")

  expect_identical(inspection$estimated_bytes, .cube_estimated_bytes(cube))
  expect_equal(inspection$estimated_bytes, prod(dim(cube$data)) * 8)
})

test_that("memory missing policies compute or defer global and variable metrics", {
  cube <- .make_baseline_fixture()$cube
  cube$data[1, 1, 1, 1, 1] <- NA_real_
  cube$data[2, 1, 1, 1, 2] <- NA_real_
  before <- cube

  automatic <- cube_inspect(cube, missing = "auto")
  none <- cube_inspect(cube, missing = "none")
  full <- cube_inspect(cube, missing = "full")

  expect_true(automatic$missing$computed)
  expect_identical(automatic$missing$missing, 2)
  expect_identical(automatic$missing$by_variable$missing, c(1, 1))
  expect_false(none$missing$computed)
  expect_true(is.na(none$missing$missing))
  expect_identical(none$missing$status, "not requested")
  expect_identical(full$missing$missing, automatic$missing$missing)
  expect_identical(cube, before)
})

test_that("NetCDF auto and none do not materialize data", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  cube <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )
  local_mocked_bindings(
    .cube_read = function(...) stop("unexpected materialization"),
    .package = "oceancube"
  )

  automatic <- cube_inspect(cube, missing = "auto")
  none <- cube_inspect(cube, missing = "none")

  expect_identical(automatic$backend, "netcdf")
  expect_false(automatic$missing$computed)
  expect_identical(automatic$missing$status, "not materialized")
  expect_true(is.na(automatic$missing$missing))
  expect_false(none$missing$computed)
})

test_that("NetCDF full warns before computing missing values", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  cube <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )

  expect_warning(
    inspection <- cube_inspect(cube, missing = "full"),
    "materializes the complete NetCDF cube"
  )
  expect_true(inspection$missing$computed)
  expect_gt(inspection$missing$missing, 0)
})

test_that("inspection classes, components, provenance, and printing are stable", {
  cube <- .make_baseline_fixture()$cube
  cube$source <- "controlled"
  cube$dataset_id <- "fixture-001"
  cube$provenance <- list(provider = "test", operation = "fixture")
  inspection <- cube_inspect(cube)
  required <- c(
    "source", "dataset_id", "backend", "dimensions", "storage_dimensions",
    "coordinate_ranges", "coordinate_resolution", "variables", "units",
    "extents", "time_resolution", "depth_resolution", "estimated_bytes",
    "missing", "validation", "provenance_summary"
  )

  expect_identical(class(inspection), c("ocean_cube_inspection", "list"))
  expect_true(all(required %in% names(inspection)))
  expect_identical(inspection$provenance_summary$fields, c("provider", "operation"))
  expect_false(withVisible(print(inspection))$visible)
  expect_identical(withVisible(print(inspection))$value, inspection)
  expect_match(capture.output(print(inspection))[[1L]], "<ocean_cube_inspection>", fixed = TRUE)
})

test_that("invalid missing policies and invalid cubes fail clearly", {
  cube <- .make_baseline_fixture()$cube
  bad <- cube
  bad$lat[1L] <- -100

  expect_error(cube_inspect(cube, missing = "sample"))
  expect_error(
    cube_inspect(bad),
    class = "oceancube_validation_error"
  )
})
