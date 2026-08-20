a2_periodic_cube <- function(year_effect = 1) {
  time <- seq(as.Date("2020-01-01"), by = "month", length.out = 24L)
  cube <- a2_make_cube(
    longitude_n = 1L, latitude_n = 1L, depth_n = 1L,
    time_n = length(time), variable_n = 1L, time = time
  )
  month_effect <- rep(seq_len(12L), times = 2L)
  replicate_effect <- rep(c(-year_effect, year_effect), each = 12L)
  cube$data[] <- month_effect + replicate_effect
  cube
}

test_that("P7 exact linear trends recover deterministic Date slopes", {
  cases <- list(
    positive = list(slope = 2, irregular = FALSE),
    negative = list(slope = -0.5, irregular = FALSE),
    zero = list(slope = 0, irregular = FALSE),
    irregular = list(slope = 3, irregular = TRUE)
  )

  for (name in names(cases)) {
    case <- cases[[name]]
    cube <- a2_make_cube(
      time_n = 6L, variable_n = 2L, time_class = "Date",
      irregular_time = case$irregular, field = "linear",
      intercept = -7, slope = case$slope
    )
    result <- cube_trend(cube, time_unit = "day")
    expect_equal(
      unname(as.vector(result$data)),
      rep(case$slope, prod(.cube_shape(cube)[c(1L, 2L, 3L, 5L)])),
      tolerance = A2_TOLERANCE$trend_absolute,
      info = name
    )
  }
})

test_that("P7 POSIXct trends use actual irregular UTC elapsed time", {
  cases <- list(
    regular = list(slope = 1.25, irregular = FALSE),
    irregular = list(slope = -2.5, irregular = TRUE)
  )
  for (name in names(cases)) {
    case <- cases[[name]]
    cube <- a2_make_cube(
      time_n = 6L, variable_n = 1L, time_class = "POSIXct",
      irregular_time = case$irregular, field = "linear",
      intercept = 11, slope = case$slope
    )
    result <- cube_trend(cube, time_unit = "day")
    expect_equal(
      unname(as.vector(result$data)),
      rep(case$slope, prod(.cube_shape(cube)[c(1L, 2L, 3L, 5L)])),
      tolerance = A2_TOLERANCE$trend_absolute,
      info = name
    )
    expect_s3_class(result$time, "POSIXct")
    expect_identical(attr(result$time, "tzone"), "UTC")
  }
})

test_that("P8 monthly climatology and difference anomalies center analytically", {
  cube <- a2_periodic_cube(year_effect = 1)
  climatology <- suppressWarnings(cube_climatology(cube, by = "month"))
  difference <- cube_anomaly(cube, climatology, type = "difference")
  standardized <- cube_anomaly(cube, climatology, type = "z")

  expect_equal(
    unname(as.vector(climatology$data)),
    seq_len(12L),
    tolerance = A2_TOLERANCE$temporal_absolute
  )
  expect_equal(
    unname(as.vector(difference$data)),
    rep(c(-1, 1), each = 12L),
    tolerance = A2_TOLERANCE$temporal_absolute
  )
  expect_equal(
    unname(as.vector(standardized$data)),
    rep(c(-1 / sqrt(2), 1 / sqrt(2)), each = 12L),
    tolerance = A2_TOLERANCE$temporal_absolute
  )
  group <- format(cube$time, "%m")
  centered <- tapply(as.vector(difference$data), group, mean)
  expect_equal(as.vector(centered), rep(0, 12L), tolerance = A2_TOLERANCE$temporal_absolute)
})

test_that("P8 zero and near-zero SD follow the current z-anomaly contract", {
  zero_cube <- a2_periodic_cube(year_effect = 0)
  zero_climatology <- suppressWarnings(cube_climatology(zero_cube, by = "month"))
  zero <- cube_anomaly(zero_cube, zero_climatology, type = "z")

  expect_true(all(is.na(zero$data)))
  expect_identical(zero$qa$anomaly$counts$zero_sd, 24)
  expect_identical(zero$qa$anomaly$counts$invalid_sd, 24)

  near_cube <- a2_periodic_cube(year_effect = 1e-12)
  near_climatology <- suppressWarnings(cube_climatology(near_cube, by = "month"))
  near <- cube_anomaly(near_cube, near_climatology, type = "z")

  expect_true(all(is.finite(near$data)))
  expect_equal(
    unname(as.vector(near$data)),
    rep(c(-1 / sqrt(2), 1 / sqrt(2)), each = 12L),
    tolerance = 1e-3
  )
  expect_identical(near$qa$anomaly$counts$zero_sd, 0)
})

test_that("irregular extraction and aggregation preserve source order and instants", {
  cube <- a2_make_cube(time_class = "POSIXct", irregular_time = TRUE)
  extracted <- cube_extract(
    cube,
    longitude = cube$lon[[1L]], latitude = cube$lat[[1L]],
    depth = cube$depth[[1L]], variable = cube$vars[[1L]],
    mode = "series"
  )
  aggregated <- suppressWarnings(cube_aggregate_time(cube, by = "year"))

  expect_equal(as.numeric(extracted$time), as.numeric(cube$time), tolerance = 0)
  expect_true(all(diff(as.numeric(extracted$time)) > 0))
  expect_identical(attr(extracted$time, "tzone"), "UTC")
  expect_s3_class(aggregated$time, "POSIXct")
  expect_identical(attr(aggregated$time, "tzone"), "UTC")
})
