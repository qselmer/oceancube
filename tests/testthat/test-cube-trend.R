if (!exists("make_netcdf_backend_fixture", mode = "function")) {
  source("tests/testthat/helper-netcdf-backend.R", local = TRUE)
}

trend_test_cube <- function(
    time,
    values,
    lon = -80,
    lat = -12,
    depth = 0,
    vars = "temperature",
    units = "degC") {
  shape <- c(length(lon), length(lat), length(depth), length(time), length(vars))
  stopifnot(length(values) == prod(shape))
  ocean_cube(
    lon = lon,
    lat = lat,
    depth = depth,
    time = time,
    data = array(as.numeric(values), dim = shape),
    vars = vars,
    units = units,
    source = "cube_trend unit test",
    dataset_id = "trend-test"
  )
}

trend_scalar <- function(x) unname(as.vector(x$data))

test_that("cube_trend exposes the exact linear-only API", {
  time <- as.Date("2000-01-01") + 0:3
  x <- trend_test_cube(time, 1:4)

  expect_true("cube_trend" %in% getNamespaceExports("oceancube"))
  expect_identical(
    names(formals(cube_trend)),
    c("x", "method", "period", "time_unit", "min_n", "diagnostics")
  )
  expect_identical(formals(cube_trend)$method, "linear")
  expect_null(formals(cube_trend)$period)
  expect_identical(formals(cube_trend)$time_unit, "year")
  expect_identical(formals(cube_trend)$min_n, 3L)
  expect_identical(formals(cube_trend)$diagnostics, FALSE)
  expect_no_error(cube_trend(x, method = "linear", min_n = 2L))
  expect_error(cube_trend(list()), "validation|ocean_cube")

  for (method in list(
      "lin", "ols", "sen", "theil_sen", "LINEAR", c("linear", "sen"), NA_character_)) {
    expect_error(cube_trend(x, method = method), "method")
  }
  for (unit in list("years", "yr", "d", "YEAR", c("year", "day"), NA_character_)) {
    expect_error(cube_trend(x, time_unit = unit), "time_unit")
  }
  for (value in list(1, 0, -1, NA_real_, Inf, 3.5, c(3, 4))) {
    expect_error(cube_trend(x, min_n = value), "min_n")
  }
  for (value in list(NA, 1, 0, "TRUE", c(TRUE, FALSE), logical(), NULL)) {
    expect_error(cube_trend(x, diagnostics = value), "diagnostics")
  }
})

test_that("Date trends use real regular and irregular elapsed time", {
  regular_time <- as.Date(c("2000-01-01", "2001-01-01", "2002-01-01"))
  regular_elapsed <- (as.numeric(regular_time) - as.numeric(regular_time[[1L]])) /
    365.2425
  regular <- trend_test_cube(regular_time, 5 + 2 * regular_elapsed)
  expect_equal(trend_scalar(cube_trend(regular)), 2, tolerance = 1e-12)

  irregular_time <- as.Date(c("2000-01-01", "2001-01-01", "2004-01-01"))
  irregular_elapsed <-
    (as.numeric(irregular_time) - as.numeric(irregular_time[[1L]])) / 365.2425
  irregular <- trend_test_cube(irregular_time, 5 + 2 * irregular_elapsed)
  expect_no_warning(result <- cube_trend(irregular, diagnostics = TRUE))
  expect_equal(trend_scalar(result), 2, tolerance = 1e-12)
  expect_false(result$qa$trend$time_spacing_regular)
  expect_identical(result$time, as.Date("2001-12-31"))
  expect_identical(
    result$qa$trend$time_anchor_semantics,
    "midpoint_of_selected_timestamp_range"
  )

  index_values <- c(5, 7, 9)
  index_result <- cube_trend(trend_test_cube(irregular_time, index_values))
  manual_t <- irregular_elapsed - mean(irregular_elapsed)
  manual_y <- index_values - mean(index_values)
  elapsed_slope <- sum(manual_t * manual_y) / sum(manual_t^2)
  expect_equal(trend_scalar(index_result), elapsed_slope, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(trend_scalar(index_result), 2)))

  tied <- cube_trend(trend_test_cube(as.Date("2000-01-01") + 0:1, c(1, 2)), min_n = 2L)
  expect_identical(tied$time, as.Date("2000-01-01"))
})

test_that("POSIXct preserves UTC subdaily and fractional-second semantics", {
  time <- as.POSIXct(
    c("2026-01-01 00:00:00", "2026-01-01 06:00:00", "2026-01-01 18:00:00"),
    tz = "UTC"
  )
  elapsed_hours <- (as.numeric(time) - as.numeric(time[[1L]])) / 3600
  x <- trend_test_cube(time, 10 + 3 * elapsed_hours)
  result <- cube_trend(x, time_unit = "hour", diagnostics = TRUE)
  expect_equal(trend_scalar(result), 3, tolerance = 1e-12)
  expect_s3_class(result$time, "POSIXct")
  expect_identical(attr(result$time, "tzone"), "UTC")
  expect_equal(as.numeric(result$time), as.numeric(time[[1L]]) + 9 * 3600)

  fractional <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + c(0, 0.5, 1.5)
  fractional_x <- trend_test_cube(fractional, 2 + 4 * c(0, 0.5, 1.5))
  fractional_result <- cube_trend(fractional_x, time_unit = "second")
  expect_equal(trend_scalar(fractional_result), 4, tolerance = 1e-10)
  expect_equal(
    as.numeric(fractional_result$time),
    as.numeric(fractional[[1L]]) + 0.75,
    tolerance = 1e-12
  )
})

test_that("all four time units use the frozen SI conversions", {
  time <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + c(0, 86400, 3 * 86400)
  x <- trend_test_cube(time, c(2, 3, 5), units = "m")
  results <- lapply(c("second", "hour", "day", "year"), function(unit) {
    cube_trend(x, time_unit = unit)
  })
  slopes <- vapply(results, function(value) trend_scalar(value)[[1L]], numeric(1L))
  expect_equal(slopes, c(1 / 86400, 1 / 24, 1, 365.2425), tolerance = 1e-12)
  expect_identical(
    vapply(results, function(value) value$qa$trend$seconds_per_time_unit, numeric(1L)),
    c(1, 3600, 86400, 31556952)
  )
  expect_identical(
    vapply(results, function(value) unname(value$units), character(1L)),
    c("m second-1", "m hour-1", "m day-1", "m year-1")
  )
})

test_that("finite masking, min_n, signs, and OLS diagnostics are exact", {
  time <- as.Date("2000-01-01") + 0:5
  increasing <- trend_test_cube(time, 2 + 3 * 0:5)
  decreasing <- trend_test_cube(time, 8 - 2 * 0:5)
  constant <- trend_test_cube(time, rep(5, 6))
  expect_equal(trend_scalar(cube_trend(increasing, time_unit = "day")), 3)
  expect_equal(trend_scalar(cube_trend(decreasing, time_unit = "day")), -2)
  constant_result <- cube_trend(constant, time_unit = "day", diagnostics = TRUE)
  expect_equal(trend_scalar(constant_result), 0)
  expect_equal(constant_result$trend$diagnostics$intercept[[1L]], 5)
  expect_true(is.na(constant_result$trend$diagnostics$r2[[1L]]))
  expect_equal(constant_result$trend$diagnostics$residual_sd[[1L]], 0)

  masked_values <- c(1, NA, NaN, Inf, -Inf, 6)
  masked <- cube_trend(
    trend_test_cube(time, masked_values),
    time_unit = "day",
    min_n = 2L,
    diagnostics = TRUE
  )
  expect_equal(trend_scalar(masked), 1)
  expect_identical(masked$trend$diagnostics$n_valid[[1L]], 2L)
  expect_equal(masked$trend$diagnostics$time_span[[1L]], 5)
  expect_true(is.na(masked$trend$diagnostics$residual_sd[[1L]]))
  expect_false(any(is.nan(masked$data) | is.infinite(masked$data)))

  insufficient <- cube_trend(
    trend_test_cube(time, c(1, 2, rep(NA, 4))),
    time_unit = "day",
    diagnostics = TRUE
  )
  expect_true(is.na(trend_scalar(insufficient)))
  expect_identical(insufficient$trend$diagnostics$n_valid[[1L]], 2L)
  expect_equal(insufficient$trend$diagnostics$time_span[[1L]], 1)

  all_invalid <- cube_trend(
    trend_test_cube(time, c(NA, NaN, Inf, -Inf, NA, NaN)),
    diagnostics = TRUE
  )
  expect_true(is.na(trend_scalar(all_invalid)))
  expect_identical(all_invalid$trend$diagnostics$n_valid[[1L]], 0L)
  expect_true(is.na(all_invalid$trend$diagnostics$time_span[[1L]]))

  noisy_time <- as.Date("2001-01-01") + 0:3
  noisy_values <- c(1, 2, 2, 4)
  noisy <- cube_trend(
    trend_test_cube(noisy_time, noisy_values),
    time_unit = "day",
    diagnostics = TRUE
  )
  anchor <- as.numeric(noisy$time)
  predictor <- as.numeric(noisy_time) - anchor
  fit <- stats::lm(noisy_values ~ predictor)
  expect_equal(trend_scalar(noisy), unname(stats::coef(fit)[[2L]]), tolerance = 1e-12)
  expect_equal(
    noisy$trend$diagnostics$intercept[[1L]],
    unname(stats::coef(fit)[[1L]]),
    tolerance = 1e-12
  )
  expect_equal(noisy$trend$diagnostics$r2[[1L]], summary(fit)$r.squared, tolerance = 1e-12)
  expect_equal(
    noisy$trend$diagnostics$residual_sd[[1L]],
    summary(fit)$sigma,
    tolerance = 1e-12
  )
})

test_that("periods are inclusive, clipped once, and retained", {
  time <- as.Date("2000-01-01") + 0:4
  x <- trend_test_cube(time, 1:5)
  full <- cube_trend(x, time_unit = "day")
  expect_equal(trend_scalar(full), 1)
  expect_identical(full$qa$trend$period_requested, range(time))

  inside <- cube_trend(x, period = time[c(2, 4)], time_unit = "day")
  expect_equal(trend_scalar(inside), 1)
  expect_identical(inside$qa$trend$period_effective, time[c(2, 4)])
  expect_identical(
    provenance_last_operation(inside)$parameters$resolved$period_effective,
    time[c(2, 4)]
  )

  expect_warning(
    left <- cube_trend(x, period = c(time[[1L]] - 10, time[[3L]]), time_unit = "day"),
    "clipped"
  )
  expect_identical(left$qa$trend$period_effective, time[c(1, 3)])
  expect_warning(
    right <- cube_trend(x, period = c(time[[3L]], time[[5L]] + 10), time_unit = "day"),
    "clipped"
  )
  expect_identical(right$qa$trend$period_effective, time[c(3, 5)])
  expect_error(cube_trend(x, period = c(time[[4L]], time[[2L]])), "ordered")
  expect_error(
    cube_trend(x, period = c(time[[1L]] + 20, time[[5L]] + 20)),
    "does not overlap"
  )
  expect_error(
    cube_trend(x, period = as.POSIXct(time[c(1, 3)], tz = "UTC")),
    "Date semantics"
  )

  singleton <- cube_trend(
    x,
    period = rep(time[[3L]], 2L),
    diagnostics = TRUE
  )
  expect_true(is.na(trend_scalar(singleton)))
  expect_identical(singleton$time, time[[3L]])
  expect_identical(singleton$trend$diagnostics$n_valid[[1L]], 1L)
  expect_equal(singleton$trend$diagnostics$time_span[[1L]], 0)
})

test_that("raw and approved historical derivatives are accepted but pseudo-time is rejected", {
  time <- as.Date(c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01"))
  x <- trend_test_cube(time, c(1, 3, 4, 8))
  aggregate <- suppressWarnings(cube_aggregate_time(x, by = "year", min_n = 1L))
  climatology <- suppressWarnings(cube_climatology(x, by = "month", min_n = 1L))
  difference <- cube_anomaly(x, climatology, type = "difference")
  z <- cube_anomaly(x, climatology, type = "z")
  legacy <- suppressWarnings(clim_month(x))
  noise <- signal_noise(x, legacy)

  for (candidate in list(x, aggregate, difference, z, noise)) {
    expect_s3_class(cube_trend(candidate, min_n = 2L), "ocean_cube")
  }
  expect_error(cube_trend(climatology), "historical time")
  sliced <- cube_slice(climatology, longitude = 1L, by = "index")
  cropped <- cube_crop(climatology, longitude = range(climatology$lon))
  expect_error(cube_trend(sliced), "historical time")
  expect_error(cube_trend(cropped), "historical time")
  expect_error(cube_trend(cube_trend(x)), "historical time")
})

test_that("shape, coordinates, variables, units, and diagnostic allocation follow the cube contract", {
  lon <- c(-81, -80)
  lat <- c(-13, -12)
  depth <- c(0, 50)
  time <- as.Date("2000-01-01") + 0:3
  vars <- c("thetao", "index")
  shape <- c(2, 2, 2, 4, 2)
  values <- array(NA_real_, dim = shape)
  elapsed <- 0:3
  for (variable in 1:2) {
    for (level in 1:2) {
      for (latitude in 1:2) {
        for (longitude in 1:2) {
          slope <- if (variable == 1L) 2 else -1
          values[longitude, latitude, level, , variable] <-
            10 * variable + level + latitude + longitude + slope * elapsed
        }
      }
    }
  }
  x <- trend_test_cube(
    time, values, lon, lat, depth, vars,
    units = c(thetao = "degC", index = "1")
  )
  before <- serialize(x, NULL)
  compact <- cube_trend(x, time_unit = "day")
  detailed <- cube_trend(x, time_unit = "day", diagnostics = TRUE)

  expect_identical(class(detailed), c("ocean_cube", "list"))
  expect_identical(unname(dim(detailed$data)), c(2L, 2L, 2L, 1L, 2L))
  expect_identical(detailed$lon, lon)
  expect_identical(detailed$lat, lat)
  expect_identical(detailed$depth, depth)
  expect_identical(detailed$vars, vars)
  expect_identical(detailed$units, c(thetao = "degC day-1", index = "day-1"))
  expect_true(all(detailed$data[, , , 1, 1] == 2))
  expect_true(all(detailed$data[, , , 1, 2] == -1))
  expect_null(compact$trend$diagnostics)
  expect_setequal(
    names(detailed$trend$diagnostics),
    c("n_valid", "time_span", "intercept", "r2", "residual_sd")
  )
  for (diagnostic in detailed$trend$diagnostics) {
    expect_identical(dim(diagnostic), dim(detailed$data))
  }
  expect_false(any(c(
    "standard_error", "confidence_interval", "p_value", "mann_kendall",
    "kendall_tau", "significance_flag"
  ) %in% names(detailed$trend$diagnostics)))
  expect_identical(serialize(x, NULL), before)

  surface <- trend_test_cube(time, 1:4, depth = NA_real_)
  expect_true(is.na(cube_trend(surface)$depth))
  missing_units <- trend_test_cube(time, 1:4, units = NULL)
  missing_result <- cube_trend(missing_units)
  expect_null(missing_result$units)
  expect_true(missing_result$qa$trend$trend_units_unverified)
})

test_that("trend outputs validate, inspect, serialize, and plot as maps", {
  time <- as.Date("2000-01-01") + 0:3
  x <- trend_test_cube(time, 1:4)
  result <- cube_trend(x, time_unit = "day", diagnostics = TRUE)
  expect_no_error(cube_validate(result, strict = TRUE))
  expect_no_error(cube_inspect(result))
  restored <- unserialize(serialize(result, NULL))
  expect_identical(restored, result)
  expect_identical(.cube_backend(restored), "memory")
  expect_s3_class(viz.map(result, variable = "temperature"), "ggplot")
})

test_that("QA and provenance are compact, explicit, and retain the parent", {
  time <- as.Date(c("2000-01-01", "2000-01-03", "2000-01-08"))
  x <- trend_test_cube(time, c(1, 2, 4))
  x$provenance$extensions$user <- list(
    provider = "offline", request = "trend-test"
  )
  result <- cube_trend(x, time_unit = "day", diagnostics = TRUE)
  qa <- result$qa$trend
  provenance <- provenance_last_operation(result)

  expect_identical(qa$method, "linear")
  expect_identical(qa$time_basis, "elapsed_seconds")
  expect_identical(qa$observation_weighting, "equal_observation")
  expect_false(qa$time_spacing_regular)
  expect_identical(qa$cells_total, 1)
  expect_identical(qa$cells_fitted, 1)
  expect_identical(qa$backend$scientific_passes, 1L)
  expect_true(qa$backend$final_output_materialized)
  expect_identical(provenance$operation, "cube_trend")
  expect_identical(provenance$parameters$resolved$time_basis, "elapsed")
  expect_identical(provenance$parameters$resolved$internal_time_unit, "second")
  expect_identical(provenance$parameters$resolved$finite_value_policy, "is.finite")
  expect_identical(provenance$parameters$resolved$observation_weighting, "equal_observation")
  expect_identical(provenance$parameters$resolved$inference, "none")
  expect_null(result$provenance$parent)
  expect_identical(result$provenance$time$current$kind, "trend_anchor")
  expect_false(any(vapply(qa, is.array, logical(1L))))
  expect_false(any(vapply(provenance, is.array, logical(1L))))
})

test_that("lazy NetCDF trend is bounded, one-pass, and matches memory science", {
  skip_if_not_installed("ncdf4")
  file <- make_netcdf_backend_fixture(
    time_units = "days since 2019-01-01 00:00:00",
    time_values = c(0, 365, 366, 1461)
  )
  withr::local_file(file)
  lazy <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )
  memory <- cube_collect(lazy)
  original_read <- .cube_read
  observed_reads <- 0L
  local_mocked_bindings(
    .cube_read = function(x, index = NULL, drop = FALSE) {
      if (identical(.cube_backend(x), "netcdf")) {
        observed_reads <<- observed_reads + 1L
      }
      original_read(x, index = index, drop = drop)
    },
    .package = "oceancube"
  )

  lazy_result <- cube_trend(lazy, diagnostics = TRUE)
  memory_result <- cube_trend(memory, diagnostics = TRUE)
  expect_equal(lazy_result$data, memory_result$data, tolerance = 1e-12)
  expect_identical(is.na(lazy_result$data), is.na(memory_result$data))
  expect_identical(lazy_result$lon, memory_result$lon)
  expect_identical(lazy_result$lat, memory_result$lat)
  expect_identical(lazy_result$depth, memory_result$depth)
  expect_identical(lazy_result$vars, memory_result$vars)
  expect_identical(lazy_result$time, memory_result$time)
  expect_identical(lazy_result$units, memory_result$units)
  for (name in names(lazy_result$trend$diagnostics)) {
    expect_equal(
      lazy_result$trend$diagnostics[[name]],
      memory_result$trend$diagnostics[[name]],
      tolerance = 1e-12,
      info = name
    )
  }
  lazy_science <- lazy_result$qa$trend
  memory_science <- memory_result$qa$trend
  lazy_science$backend <- NULL
  memory_science$backend <- NULL
  expect_identical(lazy_science, memory_science)
  expect_identical(
    provenance_operation_contract(lazy_result),
    provenance_operation_contract(memory_result)
  )

  metrics <- lazy_result$qa$trend$backend
  source_values <- prod(.cube_shape(lazy))
  expect_identical(metrics$source_backend, "netcdf")
  expect_identical(metrics$input_shape, unname(.cube_shape(lazy)))
  expect_identical(metrics$output_shape, unname(dim(lazy_result$data)))
  expect_identical(metrics$backend_read_count, observed_reads)
  expect_gt(metrics$backend_read_count, 1L)
  expect_equal(metrics$logical_values, source_values)
  expect_gte(metrics$physical_values, metrics$logical_values)
  expect_lt(metrics$maximum_block_values, source_values)
  expect_false(metrics$full_source_materialized)
  expect_true(metrics$final_output_materialized)
  expect_identical(metrics$scientific_passes, 1L)
  expect_identical(.cube_backend(lazy_result), "memory")
})
