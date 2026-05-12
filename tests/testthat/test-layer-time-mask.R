test_that("layer_mean aggregates depth bins", {
  lon <- 1:2
  lat <- 1:2
  depth <- c(0, 10, 20, 30)
  time <- as.Date("2000-01-01") + 0:2
  data <- array(1, dim = c(2, 2, 4, 3, 1))

  cube <- ocean_cube(lon = lon, lat = lat, depth = depth, time = time, data = data, vars = "thetao")
  layer <- layer_mean(cube, depth = c(0, 15, 30))

  expect_s3_class(layer, "ocean_cube")
  expect_equal(dim(layer$data), c(2, 2, 2, 3, 1))
  expect_true(all(layer$data == 1, na.rm = TRUE))
})

test_that("to_month aggregates daily data", {
  lon <- 1:2
  lat <- 1:2
  depth <- 0
  time <- seq(as.Date("2000-01-01"), as.Date("2000-02-10"), by = "day")
  data <- array(1, dim = c(2, 2, 1, length(time), 1))

  cube <- ocean_cube(lon = lon, lat = lat, depth = depth, time = time, data = data, vars = "thetao")
  mon <- to_month(cube)

  expect_equal(length(mon$time), 2)
  expect_equal(dim(mon$data)[4], 2)
})

test_that("stock_mask and crop_stock mask outside cells", {
  lon <- 1:3
  lat <- c(-12, -11, -10)
  depth <- 0
  time <- as.Date("2000-01-01")
  data <- array(1, dim = c(3, 3, 1, 1, 1))

  cube <- ocean_cube(lon = lon, lat = lat, depth = depth, time = time, data = data, vars = "thetao")
  mask <- stock_mask(cube, stock = "test", lat = c(-11.5, -10.5))
  cropped <- crop_stock(cube, mask)

  expect_s3_class(mask, "ocean_mask")
  expect_s3_class(cropped, "stock_cube")
  expect_true(any(is.na(cropped$data)))
  expect_true(any(is.finite(cropped$data)))
})
