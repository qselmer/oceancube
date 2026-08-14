anomaly_test_cube <- function(
    time,
    values,
    lon = -80,
    lat = -12,
    depth = 0,
    vars = "temperature",
    units = "degC") {
  shape <- c(length(lon), length(lat), length(depth), length(time), length(vars))
  ocean_cube(
    lon = lon,
    lat = lat,
    depth = depth,
    time = time,
    data = array(rep(values, length.out = prod(shape)), dim = shape),
    vars = vars,
    units = units,
    source = "anomaly-test"
  )
}

anomaly_manual_climatology <- function(x, by = "month", leap = "feb28",
                                       means = list(), sds = list()) {
  climatology <- if (identical(by, "day")) {
    suppressWarnings(cube_climatology(x, by, leap = leap, diagnostics = TRUE))
  } else {
    suppressWarnings(cube_climatology(x, by, diagnostics = TRUE))
  }
  climatology$data[] <- NA_real_
  climatology$climatology$sd[] <- NA_real_
  for (key in names(means)) {
    index <- match(key, climatology$climatology$group_key)
    climatology$data[, , , index, ] <- means[[key]]
  }
  for (key in names(sds)) {
    index <- match(key, climatology$climatology$group_key)
    climatology$climatology$sd[, , , index, ] <- sds[[key]]
  }
  climatology
}

anomaly_set_calendar <- function(x, calendar) {
  x$provenance$time$calendar <- calendar
  x
}

anomaly_set_climatology_calendar <- function(x, calendar) {
  x$climatology$calendar <- calendar
  x$provenance$time$calendar <- calendar
  x
}

test_that("cube_anomaly exposes the exact API and validates arguments", {
  expect_true(is.function(cube_anomaly))
  expect_identical(
    names(formals(cube_anomaly)),
    c("x", "climatology", "type")
  )
  x <- anomaly_test_cube(as.Date("2020-01-01"), 1)
  climatology <- anomaly_manual_climatology(
    x, means = list("01" = 1), sds = list("01" = 1)
  )
  expect_error(cube_anomaly(list(), climatology), "validation|ocean_cube")
  expect_error(cube_anomaly(x, list()), "canonical ocean_cube")
  expect_error(cube_anomaly(x, climatology, "ratio"), "arg")
  expect_error(cube_anomaly(x, climatology, c("difference", "z")), "type")
})

test_that("canonical climatology scientific structure is enforced", {
  x <- anomaly_test_cube(as.Date(c("2020-01-01", "2021-01-01")), c(1, 3))
  climatology <- anomaly_manual_climatology(
    x, means = list("01" = 2), sds = list("01" = 1)
  )

  missing_sd <- climatology
  missing_sd$climatology$sd <- NULL
  expect_error(cube_anomaly(x, missing_sd), "metadata are incomplete")

  wrong_sd <- climatology
  wrong_sd$climatology$sd <- array(1, c(1, 1, 1, 12))
  expect_error(cube_anomaly(x, wrong_sd), "numeric 5-D")

  missing_key <- climatology
  missing_key$climatology$group_key <- NULL
  expect_error(cube_anomaly(x, missing_key), "metadata are incomplete")

  duplicate_key <- climatology
  duplicate_key$climatology$group_key[[2L]] <-
    duplicate_key$climatology$group_key[[1L]]
  expect_error(cube_anomaly(x, duplicate_key), "unique")

  short_key <- climatology
  short_key$climatology$group_key <-
    short_key$climatology$group_key[-12L]
  expect_error(cube_anomaly(x, short_key), "aligned with its time")

  incomplete_cycle <- climatology
  incomplete_cycle$climatology$group_key[[12L]] <- "13"
  expect_error(cube_anomaly(x, incomplete_cycle), "full recurring cycle")

  negative_sd <- climatology
  negative_sd$climatology$sd[1, 1, 1, 1, 1] <- -1
  expect_error(cube_anomaly(x, negative_sd, "z"), "negative finite")

  stripped <- climatology
  stripped$climatology <- NULL
  expect_error(
    cube_anomaly(x, stripped),
    "Select or crop the source before computing climatology"
  )
})

test_that("coordinates depth variables and order require exact identity", {
  x <- anomaly_test_cube(
    as.Date(c("2020-01-01", "2021-01-01")),
    1:24,
    lon = c(-80, -79, -78),
    lat = c(-10, -9, -8),
    depth = c(0, 10),
    vars = c("thetao", "so"),
    units = c(thetao = "degC", so = "psu")
  )
  climatology <- anomaly_manual_climatology(
    x, means = list("01" = 0), sds = list("01" = 1)
  )
  expect_s3_class(cube_anomaly(x, climatology), "ocean_cube")

  shifted_lon <- climatology
  shifted_lon$lon <- shifted_lon$lon + 1
  shifted_lon$spatial_extent[c("lon_min", "lon_max")] <-
    range(shifted_lon$lon)
  expect_error(cube_anomaly(x, shifted_lon), "lon coordinates")

  reversed_lat <- climatology
  reversed_lat$lat <- rev(reversed_lat$lat)
  expect_error(cube_anomaly(x, reversed_lat), "lat coordinates")

  changed_depth <- climatology
  changed_depth$depth <- c(0, 20)
  changed_depth$depth_extent <- range(changed_depth$depth)
  expect_error(cube_anomaly(x, changed_depth), "depth coordinates")

  reordered_vars <- climatology
  reordered_vars$vars <- rev(reordered_vars$vars)
  expect_error(cube_anomaly(x, reordered_vars), "variables")

  one_variable <- anomaly_test_cube(
    x$time, 1, lon = x$lon, lat = x$lat, depth = x$depth,
    vars = "thetao", units = c(thetao = "degC")
  )
  subset_climatology <- anomaly_manual_climatology(
    one_variable, means = list("01" = 0), sds = list("01" = 1)
  )
  expect_error(cube_anomaly(x, subset_climatology), "variables")
})

test_that("surface NA depth is compatible only with the same sentinel", {
  x <- anomaly_test_cube(
    as.Date(c("2020-01-01", "2021-01-01")), c(1, 3),
    depth = NA_real_
  )
  climatology <- anomaly_manual_climatology(
    x, means = list("01" = 2), sds = list("01" = 1)
  )
  expect_true(all(is.finite(cube_anomaly(x, climatology)$data)))
  changed <- climatology
  changed$depth <- 0
  changed$depth_extent <- c(0, 0)
  expect_error(cube_anomaly(x, changed), "depth coordinates")
})

test_that("unit compatibility is exact and missing units are explicit", {
  x <- anomaly_test_cube(
    as.Date(c("2020-01-01", "2021-01-01")), c(1, 3),
    units = c(temperature = "degC")
  )
  climatology <- anomaly_manual_climatology(
    x, means = list("01" = 2), sds = list("01" = 1)
  )
  difference <- cube_anomaly(x, climatology, "difference")
  z <- cube_anomaly(x, climatology, "z")
  expect_identical(difference$units, x$units)
  expect_identical(z$units, c(temperature = "1"))
  expect_identical(difference$qa$anomaly$alignment$units, "exact")

  missing <- anomaly_test_cube(
    x$time, c(1, 3), units = NULL
  )
  missing_climatology <- anomaly_manual_climatology(
    missing, means = list("01" = 2), sds = list("01" = 1)
  )
  expect_warning(
    missing_result <- cube_anomaly(missing, missing_climatology),
    "compatibility is unverified"
  )
  expect_identical(
    missing_result$qa$anomaly$alignment$units,
    "unverified_missing"
  )

  source_missing <- x
  source_missing$units <- NULL
  expect_error(cube_anomaly(source_missing, climatology), "both be defined")

  climatology_missing <- climatology
  climatology_missing$units <- NULL
  expect_error(cube_anomaly(x, climatology_missing), "both be defined")

  mismatched <- climatology
  mismatched$units <- c(temperature = "K")
  expect_error(cube_anomaly(x, mismatched), "match exactly")
})

test_that("calendar identity and source time class are exact", {
  date_source <- anomaly_test_cube(
    as.Date(c("2020-01-01", "2021-01-01")), c(1, 3)
  )
  date_climatology <- anomaly_manual_climatology(
    date_source, means = list("01" = 2), sds = list("01" = 1)
  )
  for (calendar in c("standard", "gregorian", "proleptic_gregorian")) {
    source_calendar <- anomaly_set_calendar(date_source, calendar)
    climatology_calendar <-
      anomaly_set_climatology_calendar(date_climatology, calendar)
    expect_s3_class(
      cube_anomaly(source_calendar, climatology_calendar),
      "ocean_cube"
    )
  }

  standard_source <- anomaly_set_calendar(date_source, "standard")
  gregorian_climatology <-
    anomaly_set_climatology_calendar(date_climatology, "gregorian")
  expect_error(
    cube_anomaly(standard_source, gregorian_climatology),
    "calendars"
  )

  posix_source <- anomaly_test_cube(
    as.POSIXct(c("2020-01-01 01:00:00", "2021-01-01 12:00:00"), tz = "UTC"),
    c(1, 3)
  )
  posix_climatology <- anomaly_manual_climatology(
    posix_source, means = list("01" = 2), sds = list("01" = 1)
  )
  posix_result <- cube_anomaly(posix_source, posix_climatology)
  expect_s3_class(posix_result$time, "POSIXct")
  expect_identical(posix_result$time, posix_source$time)
  expect_error(
    cube_anomaly(date_source, posix_climatology),
    "time class"
  )
  expect_error(
    cube_anomaly(posix_source, date_climatology),
    "time class"
  )
})

test_that("difference and z science match independent hand calculations", {
  difference_source <- anomaly_test_cube(
    as.Date(c("2022-01-15", "2022-02-15")), c(12, 18)
  )
  difference_climatology <- anomaly_manual_climatology(
    difference_source,
    means = list("01" = 10, "02" = 20),
    sds = list("01" = 2, "02" = 4)
  )
  difference <- cube_anomaly(
    difference_source, difference_climatology, "difference"
  )
  expect_equal(as.vector(difference$data), c(2, -2))

  z_source <- anomaly_test_cube(
    as.Date(c("2022-01-15", "2022-02-15")), c(14, 16)
  )
  z_climatology <- anomaly_manual_climatology(
    z_source,
    means = list("01" = 10, "02" = 20),
    sds = list("01" = 2, "02" = 4)
  )
  z <- cube_anomaly(z_source, z_climatology, "z")
  expect_equal(as.vector(z$data), c(2, -1))
  expect_identical(difference$anomaly$type, "difference")
  expect_identical(z$anomaly$type, "z")
})

test_that("non-finite source and mean values never produce Inf or NaN", {
  x <- anomaly_test_cube(
    as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    c(NA_real_, NaN, Inf, -Inf)
  )
  climatology <- anomaly_manual_climatology(
    x, means = list("01" = 10), sds = list("01" = 2)
  )
  difference <- cube_anomaly(x, climatology, "difference")
  z <- cube_anomaly(x, climatology, "z")
  expect_true(all(is.na(difference$data)))
  expect_true(all(is.na(z$data)))
  expect_false(any(is.infinite(difference$data)))
  expect_false(any(is.nan(difference$data)))
  expect_false(any(is.infinite(z$data)))
  expect_false(any(is.nan(z$data)))

  finite <- anomaly_test_cube(as.Date("2020-01-01"), 12)
  missing_mean <- anomaly_manual_climatology(
    finite, means = list("01" = Inf), sds = list("01" = 2)
  )
  expect_true(all(is.na(cube_anomaly(finite, missing_mean)$data)))
})

test_that("z handles zero non-finite and tiny positive SD exactly", {
  x <- anomaly_test_cube(as.Date("2020-01-01"), 12)
  climatology <- anomaly_manual_climatology(
    x, means = list("01" = 10), sds = list("01" = 0)
  )
  zero <- cube_anomaly(x, climatology, "z")
  expect_true(is.na(zero$data[[1L]]))
  expect_equal(zero$qa$anomaly$counts$zero_sd, 1)
  expect_equal(zero$qa$anomaly$counts$invalid_sd, 1)

  for (bad in list(NA_real_, NaN, Inf)) {
    invalid <- climatology
    invalid$climatology$sd[1, 1, 1, 1, 1] <- bad
    result <- cube_anomaly(x, invalid, "z")
    expect_true(is.na(result$data[[1L]]))
    expect_false(any(is.infinite(result$data)))
    expect_false(any(is.nan(result$data)))
  }

  tiny_source <- anomaly_test_cube(as.Date("2020-01-01"), 10 + 1e-12)
  tiny <- anomaly_manual_climatology(
    tiny_source,
    means = list("01" = 10),
    sds = list("01" = 1e-12)
  )
  expect_equal(
    cube_anomaly(tiny_source, tiny, "z")$data[[1L]],
    1,
    tolerance = 1e-3
  )
})

test_that("daily recurring keys implement keep drop and feb28 exactly", {
  time <- as.Date(c("2020-02-28", "2020-02-29", "2020-03-01"))
  source <- anomaly_test_cube(time, c(12, 22, 32))

  keep <- anomaly_manual_climatology(
    source, by = "day", leap = "keep",
    means = list("02-28" = 10, "02-29" = 20, "03-01" = 30),
    sds = list("02-28" = 2, "02-29" = 2, "03-01" = 2)
  )
  expect_equal(
    as.vector(cube_anomaly(source, keep, "difference")$data),
    c(2, 2, 2)
  )

  drop <- anomaly_manual_climatology(
    source, by = "day", leap = "drop",
    means = list("02-28" = 10, "03-01" = 30),
    sds = list("02-28" = 2, "03-01" = 2)
  )
  dropped <- cube_anomaly(source, drop, "difference")
  expect_equal(as.vector(dropped$data), c(2, NA, 2))
  expect_identical(dropped$time, source$time)
  expect_equal(dropped$qa$anomaly$counts$expected_unmatched, 1)

  feb28 <- anomaly_manual_climatology(
    source, by = "day", leap = "feb28",
    means = list("02-28" = 10, "03-01" = 30),
    sds = list("02-28" = 2, "03-01" = 2)
  )
  expect_equal(
    as.vector(cube_anomaly(source, feb28, "difference")$data),
    c(2, 12, 2)
  )
})

test_that("daily and monthly POSIXct values are matched pointwise", {
  daily_time <- as.POSIXct(c(
    "2026-07-15 01:00:00", "2026-07-15 12:00:00",
    "2026-07-15 23:00:00"
  ), tz = "UTC")
  daily_source <- anomaly_test_cube(daily_time, c(11, 12, 13))
  daily <- anomaly_manual_climatology(
    daily_source, by = "day", leap = "drop",
    means = list("07-15" = 10),
    sds = list("07-15" = 2)
  )
  daily_result <- cube_anomaly(daily_source, daily, "difference")
  expect_equal(as.vector(daily_result$data), c(1, 2, 3))
  expect_identical(daily_result$time, daily_time)

  monthly <- anomaly_manual_climatology(
    daily_source, by = "month",
    means = list("07" = 10),
    sds = list("07" = 2)
  )
  monthly_result <- cube_anomaly(daily_source, monthly, "z")
  expect_equal(as.vector(monthly_result$data), c(0.5, 1, 1.5))
  expect_identical(monthly_result$time, daily_time)
})

test_that("seasonal observations use recurrent meteorological groups", {
  time <- as.Date(c(
    "2025-12-15", "2026-01-15", "2026-02-15",
    "2026-04-15", "2026-07-15", "2026-10-15"
  ))
  source <- anomaly_test_cube(time, c(11, 12, 13, 24, 35, 46))
  climatology <- anomaly_manual_climatology(
    source, by = "season",
    means = list(DJF = 10, MAM = 20, JJA = 30, SON = 40),
    sds = list(DJF = 1, MAM = 2, JJA = 5, SON = 2)
  )
  difference <- cube_anomaly(source, climatology, "difference")
  expect_equal(as.vector(difference$data), c(1, 2, 3, 4, 5, 6))
  expect_identical(difference$time, time)
  expect_identical(difference$anomaly$climatology_by, "season")
})

test_that("baseline can be applied outside or inside its reference years", {
  baseline_source <- anomaly_test_cube(
    as.Date(c("1981-01-01", "2010-01-01")), c(10, 20)
  )
  climatology <- anomaly_manual_climatology(
    baseline_source, means = list("01" = 15), sds = list("01" = sqrt(50))
  )
  outside <- anomaly_test_cube(
    as.Date(c("2011-01-01", "2015-01-01")), c(16, 17)
  )
  inside <- anomaly_test_cube(as.Date("1990-01-01"), 14)
  outside_result <- cube_anomaly(outside, climatology)
  inside_result <- cube_anomaly(inside, climatology)
  expect_equal(as.vector(outside_result$data), c(1, 2))
  expect_equal(inside_result$data[[1L]], -1)
  expect_identical(
    outside_result$anomaly$baseline_requested,
    climatology$climatology$requested_period
  )
  expect_identical(
    outside_result$anomaly$baseline_effective,
    climatology$climatology$effective_period
  )
})

test_that("output preserves source contract QA provenance and serialization", {
  x <- anomaly_test_cube(
    as.Date(c("2020-01-01", "2021-01-01")),
    1:48,
    lon = c(-80, -79),
    lat = c(-12, -11),
    depth = c(0, 20, 40),
    vars = c("thetao", "so"),
    units = c(thetao = "degC", so = "psu")
  )
  climatology <- anomaly_manual_climatology(
    x, means = list("01" = 0), sds = list("01" = 1)
  )
  difference <- cube_anomaly(x, climatology, "difference")
  z <- cube_anomaly(x, climatology, "z")
  expect_identical(class(difference), c("ocean_cube", "list"))
  expect_identical(dim(difference$data), dim(x$data))
  expect_identical(difference$lon, x$lon)
  expect_identical(difference$lat, x$lat)
  expect_identical(difference$depth, x$depth)
  expect_identical(difference$time, x$time)
  expect_identical(difference$vars, x$vars)
  expect_identical(z$units, c(thetao = "1", so = "1"))
  expect_silent(cube_validate(difference, strict = TRUE))
  expect_silent(cube_validate(z, strict = TRUE))
  expect_s3_class(cube_inspect(difference), "ocean_cube_inspection")
  expect_s3_class(cube_inspect(z), "ocean_cube_inspection")
  expect_true(is.list(difference$qa$anomaly$alignment))
  expect_equal(
    difference$qa$anomaly$counts$source_values,
    prod(dim(x$data))
  )
  expect_identical(
    difference$provenance$cube_anomaly$operation,
    "anomaly"
  )
  expect_identical(
    difference$provenance$parent$source,
    x$provenance
  )
  expect_identical(
    difference$provenance$parent$climatology,
    climatology$provenance
  )

  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  saveRDS(difference, file)
  restored <- readRDS(file)
  expect_identical(restored$data, difference$data)
  expect_identical(restored$time, difference$time)
  expect_identical(restored$units, difference$units)
  expect_identical(restored$anomaly, difference$anomaly)
  expect_identical(restored$qa, difference$qa)
  expect_identical(restored$provenance, difference$provenance)
})

test_that("lazy NetCDF source is bounded and matches memory science", {
  skip_if_not_installed("ncdf4")
  file <- make_netcdf_backend_fixture(
    time_units = "days since 2019-01-01 00:00:00",
    time_values = c(0, 365, 366, 731)
  )
  on.exit(unlink(file), add = TRUE)
  lazy <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )
  memory <- cube_collect(lazy)
  climatology <- suppressWarnings(cube_climatology(
    memory, "month", diagnostics = TRUE
  ))

  lazy_difference <- cube_anomaly(lazy, climatology, "difference")
  memory_difference <- cube_anomaly(memory, climatology, "difference")
  lazy_z <- cube_anomaly(lazy, climatology, "z")
  memory_z <- cube_anomaly(memory, climatology, "z")

  expect_equal(lazy_difference$data, memory_difference$data)
  expect_equal(lazy_z$data, memory_z$data)
  expect_identical(is.na(lazy_difference$data), is.na(memory_difference$data))
  expect_identical(is.na(lazy_z$data), is.na(memory_z$data))
  expect_identical(lazy_difference$lon, memory_difference$lon)
  expect_identical(lazy_difference$lat, memory_difference$lat)
  expect_identical(lazy_difference$depth, memory_difference$depth)
  expect_identical(lazy_difference$time, memory_difference$time)
  expect_identical(lazy_difference$vars, memory_difference$vars)
  expect_identical(lazy_difference$units, memory_difference$units)
  expect_identical(lazy_z$units, memory_z$units)
  expect_identical(
    lazy_difference$anomaly,
    memory_difference$anomaly
  )
  metrics <- lazy_z$qa$anomaly$backend
  expect_identical(metrics$source_backend, "netcdf")
  expect_identical(metrics$source_shape, unname(.cube_shape(lazy)))
  expect_identical(metrics$output_shape, unname(dim(lazy_z$data)))
  expect_gt(metrics$read_count, 1L)
  expect_equal(metrics$logical_values, prod(.cube_shape(lazy)))
  expect_gte(metrics$physical_values, metrics$logical_values)
  expect_lt(metrics$max_block, prod(.cube_shape(lazy)))
  expect_false(metrics$full_source_materialized)
  expect_true(metrics$final_output_materialized)
  expect_identical(.cube_backend(lazy_z), "memory")
})

test_that("alignment failure occurs before a lazy source payload read", {
  skip_if_not_installed("ncdf4")
  file <- make_netcdf_backend_fixture(
    time_units = "days since 2019-01-01 00:00:00",
    time_values = c(0, 365, 366, 731)
  )
  on.exit(unlink(file), add = TRUE)
  lazy <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )
  memory <- cube_collect(lazy)
  climatology <- suppressWarnings(cube_climatology(memory, "month"))
  climatology$lon <- climatology$lon + 0.5
  climatology$spatial_extent[c("lon_min", "lon_max")] <-
    range(climatology$lon)
  local_mocked_bindings(
    .cube_read = function(...) stop("source payload was read"),
    .package = "oceancube"
  )
  expect_error(cube_anomaly(lazy, climatology), "lon coordinates")
})

test_that("source and climatology remain immutable on success and error", {
  x <- anomaly_test_cube(
    as.Date(c("2020-01-01", "2021-01-01")), c(1, 3)
  )
  climatology <- anomaly_manual_climatology(
    x, means = list("01" = 2), sds = list("01" = 1)
  )
  before_x <- x
  before_climatology <- climatology
  cube_anomaly(x, climatology, "difference")
  cube_anomaly(x, climatology, "z")
  expect_identical(x, before_x)
  expect_identical(climatology, before_climatology)

  bad <- climatology
  bad$lon <- bad$lon + 1
  bad$spatial_extent[c("lon_min", "lon_max")] <- range(bad$lon)
  expect_error(cube_anomaly(x, bad), "lon coordinates")
  expect_identical(x, before_x)
  expect_identical(climatology, before_climatology)
})

test_that("legacy anomaly wrappers delegate and preserve compatibility class", {
  x <- anomaly_test_cube(
    as.Date(c("2020-01-01", "2021-01-01")), c(1, 3)
  )
  clim <- suppressWarnings(clim_month(x))
  called <- 0L
  original <- cube_anomaly
  local_mocked_bindings(
    cube_anomaly = function(...) {
      called <<- called + 1L
      original(...)
    },
    .package = "oceancube"
  )
  difference <- anom_diff(x, clim)
  z <- anom_z(x, clim)
  expect_identical(called, 2L)
  expect_identical(names(formals(anom_diff)), c("x", "clim"))
  expect_identical(names(formals(anom_z)), c("x", "clim"))
  expect_identical(class(difference), c("ocean_anom", "ocean_cube", "list"))
  expect_identical(class(z), class(difference))
  expect_identical(difference$climatology, clim)
  expect_identical(z$climatology, clim)
  expect_identical(difference$anomaly$method, "difference")
  expect_identical(z$anomaly$method, "z_score")
  expect_identical(z$units, c(temperature = "1"))

  historical <- clim
  historical$provenance$extra$core <- NULL
  expect_error(anom_diff(x, historical), "Recompute with")
  expect_error(anom_z(x, historical), "Recompute with")
})

test_that("signal_noise remains absolute z by default and signed on request", {
  x <- anomaly_test_cube(
    as.Date(c("2020-01-01", "2021-01-01")), c(1, 3)
  )
  clim <- suppressWarnings(clim_month(x))
  z <- anom_z(x, clim)
  absolute <- signal_noise(x, clim)
  signed <- signal_noise(x, clim, signed = TRUE)
  expect_equal(absolute$data, abs(z$data))
  expect_equal(signed$data, z$data)
  expect_identical(absolute$anomaly$method, "signal_to_noise")
  expect_identical(signed$anomaly$method, "signed_signal_to_noise")
  expect_identical(absolute$units, c(temperature = "1"))
  expect_identical(signed$units, c(temperature = "1"))
})
