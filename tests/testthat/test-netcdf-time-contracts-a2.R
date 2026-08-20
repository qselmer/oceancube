test_that("packed NetCDF bounded singletons decode scale offset and missing values", {
  skip_if_not_installed("ncdf4")
  pair <- a2_make_netcdf_pair("temperature")
  withr::local_file(pair$file)
  reads <- list()
  original <- .ncvar_get_block
  local_mocked_bindings(
    .ncvar_get_block = function(nc, variable, start, count) {
      reads[[length(reads) + 1L]] <<- list(
        variable = variable, start = start, count = count
      )
      original(nc, variable, start, count)
    },
    .package = "oceancube"
  )

  decoded <- .cube_read(
    pair$netcdf,
    index = list(
      longitude = 2L, latitude = 1L, depth = 1L,
      time = 1L, variable = 1L
    )
  )
  fill_value <- .cube_read(
    pair$netcdf,
    index = list(
      longitude = 1L, latitude = 2L, depth = 2L,
      time = 2L, variable = 1L
    )
  )
  missing_value <- .cube_read(
    pair$netcdf,
    index = list(
      longitude = 3L, latitude = 1L, depth = 2L,
      time = 3L, variable = 1L
    )
  )

  expect_identical(unname(decoded), array(11112, dim = rep(1L, 5L)))
  expect_true(is.na(fill_value[[1L]]))
  expect_true(is.na(missing_value[[1L]]))
  expect_length(reads, 3L)
  expect_true(all(vapply(reads, function(read) prod(read$count) == 1, logical(1L))))
  expect_true(all(vapply(reads, function(read) identical(read$variable, "temperature"), logical(1L))))
})

test_that("NetCDF first and last singleton blocks retain canonical dimensions", {
  skip_if_not_installed("ncdf4")
  pair <- a2_make_netcdf_pair()
  withr::local_file(pair$file)
  shape <- unname(.cube_shape(pair$netcdf))
  first <- .cube_read_block(pair$netcdf, rep(1L, 5L), rep(1L, 5L))
  last <- .cube_read_block(pair$netcdf, shape, rep(1L, 5L))

  expect_identical(unname(dim(first)), rep(1L, 5L))
  expect_identical(unname(dim(last)), rep(1L, 5L))
  expect_identical(names(dimnames(first)), .cube_axis_names())
  expect_identical(names(dimnames(last)), .cube_axis_names())
  expect_identical(first[[1L]], 11111)
  expect_identical(last[[1L]], 24223)
})

test_that("public NetCDF time decoding preserves fractional negative offset instants", {
  skip_if_not_installed("ncdf4")
  file <- make_netcdf_backend_fixture(
    time_units = "hours since 2000-01-01 06:30:00-05:00",
    time_values = c(-0.5, 0, 1.5, 3)
  )
  withr::local_file(file)
  lazy <- .new_netcdf_cube(.new_netcdf_storage(file, "temperature"))
  eager <- read_nc(file, vars = "temperature")
  expected <- as.POSIXct("2000-01-01 11:00:00", tz = "UTC") + c(0, 1800, 7200, 12600)

  expect_s3_class(lazy$time, "POSIXct")
  expect_s3_class(eager$time, "POSIXct")
  expect_identical(attr(lazy$time, "tzone"), "UTC")
  expect_identical(attr(eager$time, "tzone"), "UTC")
  expect_equal(as.numeric(lazy$time), as.numeric(expected), tolerance = 1e-9)
  expect_equal(as.numeric(eager$time), as.numeric(expected), tolerance = 1e-9)
})

test_that("isolated legacy time wrappers retain canonical supported semantics", {
  raw <- c(-0.5, 0, 0.25)
  decoded <- .read_cf_time(
    raw,
    "hours since 2000-01-01 06:30:00+05:30",
    "gregorian"
  )
  expected <- as.POSIXct("2000-01-01 01:00:00", tz = "UTC") + raw * 3600

  expect_s3_class(decoded, "POSIXct")
  expect_identical(attr(decoded, "tzone"), "UTC")
  expect_equal(as.numeric(decoded), as.numeric(expected), tolerance = 1e-9)

  dates <- as.Date("2020-01-01") + c(0, 2, 5)
  instants <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + c(0, 3600, 9000)
  expect_equal(.slice_time_numeric(dates), as.numeric(dates) * 86400, tolerance = 0)
  expect_equal(.slice_time_numeric(instants), as.numeric(instants), tolerance = 0)
  expect_error(.slice_time_numeric(1:3), class = "oceancube_slice_time")
})
