test_that("monthly climatology and anomalies are computed", {
  lon <- 1:2
  lat <- 1:2
  depth <- 0
  time <- seq(as.Date("2000-01-01"), as.Date("2001-12-01"), by = "month")
  data <- array(1, dim = c(2, 2, 1, length(time), 1))

  cube <- ocean_cube(lon = lon, lat = lat, depth = depth, time = time, data = data, vars = "thetao")
  clim <- suppressWarnings(clim_month(cube))
  anom <- anom_diff(cube, clim)

  expect_s3_class(clim, "ocean_clim")
  expect_s3_class(anom, "ocean_anom")
  expect_true(all(anom$data == 0, na.rm = TRUE))
})

test_that("annual_index returns one row per year, depth and variable", {
  lon <- 1:2
  lat <- 1:2
  depth <- c(0, 10)
  time <- seq(as.Date("2000-01-01"), as.Date("2001-12-01"), by = "month")
  data <- array(rnorm(2 * 2 * 2 * length(time) * 1), dim = c(2, 2, 2, length(time), 1))

  cube <- ocean_cube(lon = lon, lat = lat, depth = depth, time = time, data = data, vars = "thetao")
  ind <- annual_index(cube)

  expect_s3_class(ind, "ocean_indicators")
  expect_equal(nrow(ind), 2 * 2 * 1)
  expect_true(all(c("year", "var", "depth", "mean_value") %in% names(ind)))
})
