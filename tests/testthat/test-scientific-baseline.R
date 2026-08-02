test_that("monthly climatology and anomaly calculations match hand calculations", {
  cube <- .make_baseline_fixture()$cube
  clim <- clim_month(cube)
  anom <- anom_diff(cube, clim)
  z <- anom_z(cube, clim)
  sn <- signal_noise(cube, clim)

  expect_equal(clim$mean[1, 1, 1, 1, 1], 12111)
  expect_equal(clim$sd[1, 1, 1, 1, 1], sqrt(2000000))
  expect_equal(anom$data[1, 1, 1, 1, 1], -1000)
  expect_equal(z$data[1, 1, 1, 1, 1], -1 / sqrt(2))
  expect_equal(sn$data[1, 1, 1, 1, 1], 1 / sqrt(2))
  expect_equal(as.vector(apply(anom$data, c(1, 2, 3, 5), mean)), rep(0, 24))
  expect_match(capture.output(print(clim))[1], "month climatology", fixed = TRUE)
})

test_that("z scores are missing rather than infinite when climatological sd is zero", {
  fixture <- .make_baseline_fixture()
  constant_data <- array(c(5, 7, 5, 7), dim = c(1, 1, 1, 4, 1))
  constant_cube <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = 0,
    time = fixture$time,
    data = constant_data,
    vars = "temperature"
  )

  z <- anom_z(constant_cube, clim_month(constant_cube))

  expect_true(all(is.na(z$data)))
  expect_false(any(is.infinite(z$data)))
  expect_false(any(is.nan(z$data)))
})

test_that("monthly aggregation handles repeated dates deterministically", {
  repeat_cube <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = 0,
    time = as.Date(c("2020-01-01", "2020-01-15", "2020-02-01")),
    data = array(c(1, 3, 10), dim = c(1, 1, 1, 3, 1)),
    vars = "temperature"
  )

  monthly <- to_month(repeat_cube)

  expect_identical(monthly$time, as.Date(c("2020-01-01", "2020-02-01")))
  expect_equal(as.vector(monthly$data), c(2, 10))
})

test_that("layer and annual calculations match thickness and annual hand calculations", {
  cube <- .make_baseline_fixture()$cube

  layer <- layer_mean(cube, c(0, 50))
  expect_equal(layer$data[1, 1, 1, 1, 1], 11161)
  expect_identical(unname(dim(layer$data)), c(3L, 2L, 1L, 4L, 2L))

  indicators <- annual_index(cube)
  selected <- indicators$var == "temperature" & indicators$depth == 0
  expect_equal(indicators$mean_value[selected], c(11617, 13617))
  expect_equal(indicators$n[selected], c(12L, 12L))
})

test_that("nearest-neighbour event linkage preserves rows and exposes edge matching", {
  cube <- .make_baseline_fixture()$cube
  events <- data.frame(
    event_id = c("inside", "edge", "outside"),
    lon = c(-80, -78, -70),
    lat = c(-12, -11, 0),
    date = as.Date(c("2020-01-01", "2021-02-01", "2020-01-01"))
  )

  linked <- link_events(cube, events, vars = "temperature")

  expect_identical(linked$event_id, events$event_id)
  expect_equal(linked$temperature_value, c(11111, 14123, 11123))
  expect_equal(linked$.oceancube_lon, c(-80, -78, -78))
  expect_equal(linked$.oceancube_lat, c(-12, -11, -11))
})

test_that("stock masks accept named source dimensions with equal sizes", {
  cube <- .make_baseline_fixture()$cube
  mask <- stock_mask(
    cube,
    stock = "south-surface",
    lat = c(-12, -12),
    depth = c(0, 0)
  )

  expect_identical(unname(dim(mask$mask)), unname(dim(cube$data)[1:3]))
  expect_equal(sum(mask$mask), 3)

  cropped <- crop_stock(cube, mask)
  expect_s3_class(cropped, "stock_cube")
  expect_identical(unname(dim(cropped$data)), c(3L, 2L, 2L, 4L, 2L))
  expect_equal(sum(is.finite(cropped$data)), 24)

  for (bad_dims in list(c(2L, 3L, 2L), c(3L, 2L, 1L), c(12L))) {
    bad_mask <- mask
    bad_mask$mask <- array(TRUE, dim = bad_dims)
    expect_error(
      crop_stock(cube, bad_mask),
      "Mask dimensions must match"
    )
  }
})

test_that("daily climatology documents leap handling and prints its real scale", {
  daily_time <- as.Date(c("2019-02-28", "2020-02-28", "2020-02-29", "2021-02-28"))
  daily_cube <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = 0,
    time = daily_time,
    data = array(c(10, 20, 30, 40), dim = c(1, 1, 1, 4, 1)),
    vars = "temperature"
  )

  feb28 <- clim_day(daily_cube, leap = "feb28")
  drop <- clim_day(daily_cube, leap = "drop")
  keep <- clim_day(daily_cube, leap = "keep")

  j_feb28 <- match("02-28", feb28$day)
  j_drop <- match("02-28", drop$day)
  j_keep28 <- match("02-28", keep$day)
  j_keep29 <- match("02-29", keep$day)

  expect_equal(feb28$mean[1, 1, 1, j_feb28, 1], 25)
  expect_equal(feb28$n[1, 1, 1, j_feb28, 1], 4)
  expect_equal(drop$mean[1, 1, 1, j_drop, 1], 70 / 3)
  expect_equal(keep$mean[1, 1, 1, j_keep28, 1], 70 / 3)
  expect_equal(keep$mean[1, 1, 1, j_keep29, 1], 30)
  expect_true(is.na(anom_diff(daily_cube, drop)$data[1, 1, 1, 3, 1]))
  expect_match(capture.output(print(keep))[1], "day climatology", fixed = TRUE)
})
