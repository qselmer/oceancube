test_that("ocean_cube constructs a valid 5D cube", {
  lon <- seq(-82, -80, length.out = 3)
  lat <- seq(-12, -10, length.out = 4)
  depth <- c(0, 10)
  time <- as.Date("2000-01-01") + 0:5
  vars <- c("thetao", "so")
  data <- array(rnorm(3 * 4 * 2 * 6 * 2), dim = c(3, 4, 2, 6, 2))

  cube <- ocean_cube(lon = lon, lat = lat, depth = depth, time = time, data = data, vars = vars)

  expect_s3_class(cube, "ocean_cube")
  expect_equal(dim(cube$data), c(3, 4, 2, 6, 2))
  expect_equal(cube$vars, vars)
})

test_that("ocean_cube promotes 4D surface data to 5D", {
  lon <- 1:2
  lat <- 1:3
  time <- as.Date("2000-01-01") + 0:1
  data <- array(1, dim = c(2, 3, 2, 1))

  cube <- ocean_cube(lon = lon, lat = lat, time = time, data = data, vars = "zos")

  expect_equal(dim(cube$data), c(2, 3, 1, 2, 1))
  expect_true(is.na(cube$depth))
})
