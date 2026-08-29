real_fixture_path <- function(name) {
  testthat::test_path("fixtures", "real-data", name)
}

test_that("NOAA real-data manifest governs exactly three offline fixtures", {
  manifest <- utils::read.csv(
    real_fixture_path("fixture-manifest.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  files <- c(
    "noaa-oisst21-surface-time-fv1.nc",
    "noaa-etopo2022-bathymetry-fv1.nc",
    "noaa-woa23-vertical-fv1.nc"
  )
  paths <- vapply(files, real_fixture_path, character(1L))

  expect_identical(nrow(manifest), 3L)
  expect_setequal(
    manifest$fixture_id,
    c("FIXTURE-SURFACE-TIME-001", "FIXTURE-BATHYMETRY-001", "FIXTURE-VERTICAL-001")
  )
  expect_true(all(file.exists(paths)))
  expect_equal(as.numeric(file.info(paths)$size), manifest$size_bytes)
  expect_true(all(manifest$size_bytes < 2 * 1024^2))
  expect_lt(sum(manifest$size_bytes), 3 * 1024^2)
  expect_true(all(nchar(manifest$fixture_checksum) == 64L))
  expect_true(all(manifest$CI_eligible))
  expect_false(any(manifest$maintainer_only))
  expect_setequal(
    manifest$current_status,
    c("CURRENT-PASS", "CURRENT-EXPECTED-LIMITATION")
  )
})

test_that("NOAA OISST real-data fixture preserves native packing and coordinates", {
  path <- real_fixture_path("noaa-oisst21-surface-time-fv1.nc")
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  expect_identical(
    vapply(nc$dim, function(x) x$len, integer(1L)),
    c(lon = 36L, lat = 48L, zlev = 1L, time = 4L)
  )
  expect_equal(as.numeric(nc$dim$lon$vals), seq(276.125, 284.875, by = 0.25))
  expect_equal(as.numeric(nc$dim$lat$vals), seq(-17.875, -6.125, by = 0.25))
  expect_identical(as.numeric(nc$dim$zlev$vals), 0)
  expect_equal(as.numeric(nc$dim$time$vals), 15340:15343)
  expect_setequal(names(nc$var), c("sst", "anom", "err", "ice"))
  expect_match(ncdf4::ncatt_get(nc, 0, "title")$value, "Final", fixed = TRUE)
  expect_identical(
    ncdf4::ncatt_get(nc, 0, "oceancube_fixture_source_doi")$value,
    "10.25921/RE9P-PT57"
  )

  for (v in c("sst", "anom", "err", "ice")) {
    attrs <- ncdf4::ncatt_get(nc, v)
    expect_identical(nc$var[[v]]$prec, "short", info = v)
    expect_equal(attrs$`_FillValue`, -999, info = v)
    expect_equal(attrs$scale_factor, 0.01, tolerance = 1e-7, info = v)
    expect_equal(attrs$add_offset, 0, info = v)
    raw <- ncdf4::ncvar_get(nc, v, raw_datavals = TRUE)
    expect_type(raw, "integer")
  }
  decoded <- ncdf4::ncvar_get(nc, "sst")
  expect_true(anyNA(decoded))
  expect_gt(min(decoded, na.rm = TRUE), 10)
  expect_lt(max(decoded, na.rm = TRUE), 35)
})

test_that("NOAA OISST real-data fixture exercises the current public runtime", {
  cube <- read_nc(
    real_fixture_path("noaa-oisst21-surface-time-fv1.nc"),
    vars = c("sst", "anom", "err", "ice"),
    depth_name = "zlev",
    dataset_id = "FIXTURE-SURFACE-TIME-001"
  )

  expect_s3_class(cube, "ocean_cube")
  expect_identical(dim(cube$data), c(36L, 48L, 1L, 4L, 4L))
  expect_true(all(cube$lon >= 0 & cube$lon <= 360))
  expect_identical(
    format(cube$time, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    paste0("2020-01-0", 1:4, " 12:00:00")
  )
  expect_false(any(cube_validate(cube)$status == "FAIL"))
  expect_s3_class(cube_inspect(cube, missing = "none"), "ocean_cube_inspection")

  sliced <- cube_slice(
    cube,
    longitude = 1:2,
    latitude = 1:2,
    time = 1:2,
    variable = 1L,
    by = "index"
  )
  cropped <- cube_crop(
    cube,
    longitude = c(276.125, 277.125),
    latitude = c(-17.875, -16.875),
    time = range(cube$time),
    variable = "sst"
  )
  extracted <- cube_extract(
    cube,
    longitude = cube$lon[[1L]],
    latitude = cube$lat[[1L]],
    depth = cube$depth[[1L]],
    time = cube$time[[1L]],
    variable = "sst"
  )
  aggregated <- suppressWarnings(cube_aggregate_time(cube, by = "month"))

  expect_identical(dim(sliced$data), c(2L, 2L, 1L, 2L, 1L))
  expect_identical(dim(cropped$data), c(5L, 5L, 1L, 4L, 1L))
  expect_s3_class(extracted, "data.frame")
  expect_identical(nrow(extracted), 1L)
  expect_identical(cube_collect(cube), cube)
  expect_identical(dim(aggregated$data), c(36L, 48L, 1L, 1L, 4L))
})

test_that("NOAA ETOPO real-data fixture retains static elevation semantics", {
  path <- real_fixture_path("noaa-etopo2022-bathymetry-fv1.nc")
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  expect_identical(
    vapply(nc$dim, function(x) x$len, integer(1L)),
    c(lon = 540L, lat = 720L)
  )
  expect_identical(names(nc$var), "z")
  expect_false(any(c("time", "depth", "bathymetric_depth") %in% names(nc$dim)))
  expect_equal(range(as.numeric(nc$dim$lon$vals)), c(-83.9916666667, -75.0083333333), tolerance = 1e-9)
  expect_equal(range(as.numeric(nc$dim$lat$vals)), c(-17.9916666667, -6.0083333333), tolerance = 1e-9)
  expect_identical(ncdf4::ncatt_get(nc, "z", "units")$value, "meters")
  expect_match(ncdf4::ncatt_get(nc, "z", "long_name")$value, "Elevation", fixed = TRUE)
  expect_identical(ncdf4::ncatt_get(nc, 0, "oceancube_fixture_license")$value, "CC0-1.0")

  z <- ncdf4::ncvar_get(nc, "z")
  expect_true(any(z < 0))
  expect_true(any(z > 0))
  expect_lt(min(z), -6000)
  expect_gt(max(z), 6000)
})

test_that("NOAA ETOPO static real data records the current reader limitation", {
  expect_error(
    read_nc(real_fixture_path("noaa-etopo2022-bathymetry-fv1.nc")),
    "Could not identify time",
    fixed = TRUE
  )
})

test_that("NOAA WOA23 real-data fixture preserves vertical and climatology metadata", {
  path <- real_fixture_path("noaa-woa23-vertical-fv1.nc")
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  expect_identical(
    vapply(nc$dim, function(x) x$len, integer(1L)),
    c(nbounds = 2L, lon = 9L, lat = 12L, depth = 6L, time = 1L)
  )
  expect_setequal(
    names(nc$var),
    c("lon_bnds", "lat_bnds", "depth_bnds", "climatology_bounds", "crs", "t_an", "s_an")
  )
  expect_identical(as.numeric(nc$dim$depth$vals), c(0, 10, 20, 50, 100, 200))
  expect_identical(ncdf4::ncatt_get(nc, "depth", "positive")$value, "down")
  expect_identical(ncdf4::ncatt_get(nc, "depth", "bounds")$value, "depth_bnds")
  expect_identical(as.numeric(nc$dim$time$vals), 4614)
  expect_identical(
    ncdf4::ncatt_get(nc, "time", "units")$value,
    "months since 1955-01-01 00:00:00"
  )
  expect_identical(
    ncdf4::ncatt_get(nc, "time", "climatology")$value,
    "climatology_bounds"
  )
  expect_false(ncdf4::ncatt_get(nc, "time", "calendar")$hasatt)
  expect_identical(as.numeric(ncdf4::ncvar_get(nc, "climatology_bounds")), c(4212, 5028))
  expect_identical(dim(ncdf4::ncvar_get(nc, "depth_bnds")), c(2L, 6L))

  expect_identical(ncdf4::ncatt_get(nc, "t_an", "units")$value, "degrees_celsius")
  expect_identical(ncdf4::ncatt_get(nc, "s_an", "units")$value, "1")
  expect_match(ncdf4::ncatt_get(nc, "t_an", "cell_methods")$value, "time: mean over years", fixed = TRUE)
  expect_match(ncdf4::ncatt_get(nc, "s_an", "cell_methods")$value, "time: mean over years", fixed = TRUE)
  expect_equal(ncdf4::ncatt_get(nc, "t_an", "_FillValue")$value, 9.96921e36, tolerance = 1e-6)
  expect_equal(ncdf4::ncatt_get(nc, "s_an", "_FillValue")$value, 9.96921e36, tolerance = 1e-6)
  expect_true(anyNA(ncdf4::ncvar_get(nc, "t_an")))
  expect_true(anyNA(ncdf4::ncvar_get(nc, "s_an")))
})

test_that("NOAA WOA23 climatological real data records the current time limitation", {
  expect_error(
    read_nc(
      real_fixture_path("noaa-woa23-vertical-fv1.nc"),
      vars = c("t_an", "s_an")
    ),
    "Core will not infer a provider-specific offset correction.",
    fixed = TRUE,
    class = "oceancube_netcdf_schema_error"
  )
})
