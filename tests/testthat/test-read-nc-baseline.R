test_that("read_nc returns vector coordinates and preserves data order", {
  fixture <- .make_baseline_fixture()
  nc_path <- tempfile("oceancube-baseline-", fileext = ".nc")
  on.exit(unlink(nc_path), add = TRUE)

  lon_dim <- ncdf4::ncdim_def("lon", "degrees_east", fixture$longitude)
  lat_dim <- ncdf4::ncdim_def("lat", "degrees_north", fixture$latitude)
  depth_dim <- ncdf4::ncdim_def("depth", "m", fixture$depth)
  time_dim <- ncdf4::ncdim_def(
    "time",
    "days since 2020-01-01",
    c(0, 31, 366, 397)
  )
  temp_def <- ncdf4::ncvar_def(
    "temperature",
    "degC",
    list(lon_dim, lat_dim, depth_dim, time_dim),
    missval = -9999,
    prec = "double"
  )
  oxygen_def <- ncdf4::ncvar_def(
    "oxygen",
    "mmol m-3",
    list(lon_dim, lat_dim, depth_dim, time_dim),
    missval = -9999,
    prec = "double"
  )

  nc <- ncdf4::nc_create(nc_path, list(temp_def, oxygen_def))
  on.exit(try(ncdf4::nc_close(nc), silent = TRUE), add = TRUE)
  temperature <- fixture$values[, , , , 1]
  temperature[2, 1, 1, 2] <- NA_real_
  ncdf4::ncvar_put(nc, "temperature", temperature)
  ncdf4::ncvar_put(nc, "oxygen", fixture$values[, , , , 2])
  ncdf4::ncatt_put(nc, "time", "calendar", "gregorian")
  ncdf4::nc_close(nc)
  nc <- NULL

  cube <- read_nc(
    nc_path,
    vars = c("temperature", "oxygen"),
    lon_name = "lon",
    lat_name = "lat",
    depth_name = "depth",
    time_name = "time"
  )

  expect_null(dim(cube$lon))
  expect_null(dim(cube$lat))
  expect_null(dim(cube$depth))
  expect_null(dim(cube$time))
  expect_identical(cube$lon, fixture$longitude)
  expect_identical(cube$lat, fixture$latitude)
  expect_identical(cube$depth, fixture$depth)
  expect_s3_class(cube$time, "POSIXct")
  expect_identical(attr(cube$time, "tzone"), "UTC")
  expect_equal(as.Date(cube$time, tz = "UTC"), fixture$time)
  expect_identical(unname(dim(cube$data)), c(3L, 2L, 2L, 4L, 2L))
  expect_true(is.na(cube$data[2, 1, 1, 2, 1]))
  expect_equal(cube$data[3, 2, 2, 4, 2], 24223)
  expect_identical(unname(unlist(cube$units)), c("degC", "mmol m-3"))

  expect_true(unlink(nc_path) == 0)
})

test_that("read_nc preserves descending and degenerate coordinate values", {
  nc_path <- tempfile("oceancube-degenerate-", fileext = ".nc")
  on.exit(unlink(nc_path), add = TRUE)

  longitude <- c(-78, -79, -80)
  latitude <- -12
  depth <- 50
  time <- as.Date(c("2020-01-01", "2020-01-02"))
  lon_dim <- ncdf4::ncdim_def("lon", "degrees_east", longitude)
  lat_dim <- ncdf4::ncdim_def("lat", "degrees_north", latitude)
  depth_dim <- ncdf4::ncdim_def("depth", "m", depth)
  time_dim <- ncdf4::ncdim_def("time", "days since 2020-01-01", c(0, 1))
  temp_def <- ncdf4::ncvar_def(
    "temperature",
    "degC",
    list(lon_dim, lat_dim, depth_dim, time_dim),
    missval = -9999,
    prec = "double"
  )

  nc <- ncdf4::nc_create(nc_path, temp_def)
  on.exit(try(ncdf4::nc_close(nc), silent = TRUE), add = TRUE)
  ncdf4::ncvar_put(nc, "temperature", array(1:6, dim = c(3, 1, 1, 2)))
  ncdf4::nc_close(nc)
  nc <- NULL

  expect_no_warning(
    cube <- read_nc(
      nc_path,
      vars = "temperature",
      lon_name = "lon",
      lat_name = "lat",
      depth_name = "depth",
      time_name = "time"
    )
  )

  expect_identical(as.numeric(cube$lon), longitude)
  expect_identical(as.numeric(cube$lat), latitude)
  expect_identical(as.numeric(cube$depth), depth)
  expect_s3_class(cube$time, "POSIXct")
  expect_equal(as.Date(cube$time, tz = "UTC"), time)
  expect_null(attr(cube$lon, "dim"))
  expect_null(attr(cube$lat, "dim"))
  expect_null(attr(cube$depth, "dim"))
  expect_identical(unname(dim(cube$data)), c(3L, 1L, 1L, 2L, 1L))
})
