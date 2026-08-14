test_that("clim_day and daily anomalies work", {
  lon <- c(-82, -81)
  lat <- c(-12, -11)
  depth <- 0
  time <- seq(as.Date("2001-01-01"), as.Date("2003-12-31"), by = "day")

  doy_signal <- as.numeric(format(time, "%j")) / 365
  data <- array(NA_real_, dim = c(2, 2, 1, length(time), 1))

  for (i in seq_along(time)) {
    data[, , 1, i, 1] <- 20 + sin(2 * pi * doy_signal[i])
  }

  cube <- ocean_cube(lon = lon, lat = lat, depth = depth, time = time, data = data, vars = "thetao")

  clim <- clim_day(cube, period = c("2001-01-01", "2002-12-31"))
  anom <- suppressWarnings(anom_diff(cube, clim))
  z <- suppressWarnings(anom_z(cube, clim))

  expect_s3_class(clim, "ocean_clim")
  expect_s3_class(anom, "ocean_anom")
  expect_equal(dim(clim$mean)[4], 365)
  expect_true(mean(abs(anom$data), na.rm = TRUE) < 1e-8)
  expect_true(all(is.na(z$data) | is.finite(z$data)))
})

test_that("link_events extracts nearest daily values", {
  lon <- c(-82, -81)
  lat <- c(-12, -11)
  depth <- 0
  time <- as.Date("2001-01-01") + 0:2
  data <- array(seq_len(2 * 2 * 1 * 3 * 1), dim = c(2, 2, 1, 3, 1))

  cube <- ocean_cube(lon = lon, lat = lat, depth = depth, time = time, data = data, vars = "thetao")

  events <- data.frame(
    id = 1:2,
    lon = c(-81.9, -81.1),
    lat = c(-11.9, -11.1),
    date = as.Date(c("2001-01-01", "2001-01-03"))
  )

  out <- link_events(cube, events, vars = "thetao", prefix = "raw")
  expect_true("raw_thetao" %in% names(out))
  expect_equal(nrow(out), 2)
  expect_true(all(is.finite(out$raw_thetao)))
})
