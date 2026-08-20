signal_noise_test_cube <- function(
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
    source = "signal-noise-test"
  )
}

signal_noise_test_clim <- function(
    x,
    by = c("month", "day"),
    leap = "feb28",
    means = list(),
    sds = list()) {
  by <- match.arg(by)
  climatology <- if (identical(by, "day")) {
    suppressWarnings(clim_day(x, leap = leap))
  } else {
    suppressWarnings(clim_month(x))
  }
  climatology$mean[] <- NA_real_
  climatology$sd[] <- NA_real_
  keys <- dimnames(climatology$mean)[[4L]]
  for (key in names(means)) {
    climatology$mean[, , , match(key, keys), ] <- means[[key]]
  }
  for (key in names(sds)) {
    climatology$sd[, , , match(key, keys), ] <- sds[[key]]
  }
  climatology
}

test_that("signal_noise preserves its API and validates signed strictly", {
  x <- signal_noise_test_cube(
    as.Date(c("2019-01-01", "2020-01-01", "2021-01-01")),
    c(14, 6, 10)
  )
  clim <- signal_noise_test_clim(
    x, means = list("01" = 10), sds = list("01" = 2)
  )

  expect_true("signal_noise" %in% getNamespaceExports("oceancube"))
  expect_identical(names(formals(signal_noise)), c("x", "clim", "signed"))
  expect_identical(formals(signal_noise)$signed, FALSE)
  expect_no_error(signal_noise(x, clim, signed = TRUE))
  expect_no_error(signal_noise(x, clim, signed = FALSE))

  invalid <- list(NA, 1, 0, "TRUE", "FALSE", c(TRUE, FALSE), logical(), NULL)
  for (value in invalid) {
    expect_error(
      signal_noise(x, clim, signed = value),
      "signed.*single non-missing logical",
      info = paste("invalid signed:", paste(value, collapse = ","))
    )
  }
})

test_that("signed and magnitude modes derive from one canonical z result", {
  x <- signal_noise_test_cube(
    as.Date(c("2019-01-01", "2020-01-01", "2021-01-01")),
    c(14, 6, 10)
  )
  clim <- signal_noise_test_clim(
    x, means = list("01" = 10), sds = list("01" = 2)
  )
  z <- anom_z(x, clim)
  magnitude <- signal_noise(x, clim)
  signed <- signal_noise(x, clim, signed = TRUE)

  expect_equal(as.vector(magnitude$data), c(2, 2, 0))
  expect_equal(as.vector(signed$data), c(2, -2, 0))
  expect_identical(signed$data, z$data)
  expect_identical(magnitude$data, abs(z$data))
  expect_identical(
    magnitude$anomaly$method,
    "standardized_anomaly_magnitude"
  )
  expect_identical(signed$anomaly$method, "standardized_anomaly")
  expect_identical(
    magnitude$anomaly$formula,
    "abs((x - climatology) / sd_clim)"
  )
  expect_identical(signed$anomaly$formula, "(x - climatology) / sd_clim")
  expect_identical(magnitude$units, c(temperature = "1"))
  expect_identical(signed$units, c(temperature = "1"))
})

test_that("signal_noise records a compact transformation over canonical QA", {
  x <- signal_noise_test_cube(
    as.Date(c("2019-01-01", "2020-01-01")), c(14, 6)
  )
  clim <- signal_noise_test_clim(
    x, means = list("01" = 10), sds = list("01" = 2)
  )
  z <- anom_z(x, clim)
  magnitude <- signal_noise(x, clim)
  signed <- signal_noise(x, clim, TRUE)

  expect_identical(magnitude$qa$anomaly, z$qa$anomaly)
  expect_identical(signed$qa$anomaly, z$qa$anomaly)
  expect_identical(
    magnitude$qa$signal_noise,
    list(
      base_operation = "standardized_anomaly",
      signed = FALSE,
      transformation = "absolute_value"
    )
  )
  expect_identical(
    signed$qa$signal_noise,
    list(
      base_operation = "standardized_anomaly",
      signed = TRUE,
      transformation = "identity"
    )
  )
  expect_identical(magnitude$provenance$cube_anomaly, z$provenance$cube_anomaly)
  expect_identical(
    magnitude$provenance$signal_noise,
    list(
      operation = "signal_noise",
      base_operation = "standardized_anomaly",
      signed = FALSE,
      transformation = "absolute_value"
    )
  )
  expect_false(any(vapply(magnitude$qa$signal_noise, is.array, logical(1L))))
  expect_false(any(vapply(
    magnitude$provenance$signal_noise, is.array, logical(1L)
  )))
})

test_that("SD and non-finite policies are inherited without new thresholds", {
  time <- as.Date(sprintf("%d-01-01", 2018:2022))
  x <- signal_noise_test_cube(time, c(12, NA, NaN, Inf, -Inf))
  clim <- signal_noise_test_clim(
    x, means = list("01" = 10), sds = list("01" = 2)
  )
  magnitude <- signal_noise(x, clim)
  signed <- signal_noise(x, clim, TRUE)

  expect_equal(as.vector(magnitude$data), c(1, NA, NA, NA, NA))
  expect_equal(as.vector(signed$data), c(1, NA, NA, NA, NA))
  expect_false(any(is.infinite(magnitude$data) | is.nan(magnitude$data)))
  expect_false(any(is.infinite(signed$data) | is.nan(signed$data)))

  for (sd_value in list(0, NA_real_, NaN, Inf)) {
    invalid <- clim
    invalid$sd[1, 1, 1, 1, 1] <- sd_value
    expect_true(is.na(signal_noise(x, invalid, FALSE)$data[[1L]]))
    expect_true(is.na(signal_noise(x, invalid, TRUE)$data[[1L]]))
  }

  negative <- clim
  negative$sd[1, 1, 1, 1, 1] <- -1
  expect_error(signal_noise(x, negative, FALSE), "negative finite")
  expect_error(signal_noise(x, negative, TRUE), "negative finite")

  tiny_x <- signal_noise_test_cube(time[1:2], c(10 + 1e-12, 10))
  tiny <- signal_noise_test_clim(
    tiny_x, means = list("01" = 10), sds = list("01" = 1e-12)
  )
  tiny_signed <- signal_noise(tiny_x, tiny, TRUE)$data[[1L]]
  tiny_magnitude <- signal_noise(tiny_x, tiny, FALSE)$data[[1L]]
  expect_true(is.finite(tiny_signed))
  expect_true(is.finite(tiny_magnitude))
  expect_equal(tiny_signed, 1, tolerance = 1e-3)
  expect_equal(tiny_magnitude, 1, tolerance = 1e-3)
})

test_that("daily leap policies and March 1 are inherited from canonical z", {
  x <- signal_noise_test_cube(
    as.Date(c(
      "2020-02-28", "2020-02-29", "2020-03-01",
      "2021-02-28", "2021-03-01"
    )),
    c(12, 14, 20, 10, 18)
  )

  keep <- signal_noise_test_clim(
    x, "day", "keep",
    means = list("02-28" = 10, "02-29" = 10, "03-01" = 18),
    sds = list("02-28" = 2, "02-29" = 2, "03-01" = 2)
  )
  drop <- signal_noise_test_clim(
    x, "day", "drop",
    means = list("02-28" = 10, "03-01" = 18),
    sds = list("02-28" = 2, "03-01" = 2)
  )
  feb28 <- signal_noise_test_clim(
    x, "day", "feb28",
    means = list("02-28" = 10, "03-01" = 18),
    sds = list("02-28" = 2, "03-01" = 2)
  )

  expect_equal(
    as.vector(signal_noise(x, keep, FALSE)$data),
    c(1, 2, 1, 0, 0)
  )
  dropped <- signal_noise(x, drop, FALSE)
  expect_equal(as.vector(dropped$data), c(1, NA, 1, 0, 0))
  expect_identical(dropped$time, x$time)
  expect_equal(
    as.vector(signal_noise(x, feb28, FALSE)$data),
    c(1, 2, 1, 0, 0)
  )
  expect_identical(
    is.na(signal_noise(x, drop, TRUE)$data),
    is.na(anom_z(x, drop)$data)
  )
  expect_identical(
    signal_noise(x, keep, TRUE)$data,
    anom_z(x, keep)$data
  )
})

test_that("POSIXct instants and the legacy frequency boundary are explicit", {
  time <- as.POSIXct(
    c(
      "2026-07-15 01:00:00", "2026-07-15 12:00:00",
      "2026-07-15 23:00:00"
    ),
    tz = "UTC"
  )
  x <- signal_noise_test_cube(time, c(14, 6, 10))
  monthly <- signal_noise_test_clim(
    x, means = list("07" = 10), sds = list("07" = 2)
  )
  result <- signal_noise(x, monthly)
  expect_identical(result$time, time)
  expect_s3_class(result$time, "POSIXct")
  expect_identical(attr(result$time, "tzone"), "UTC")
  expect_equal(as.vector(result$data), c(2, 2, 0))

  seasonal <- suppressWarnings(cube_climatology(x, "season"))
  expect_error(signal_noise(x, seasonal), "clim must be an ocean_clim")
  expect_s3_class(cube_anomaly(x, seasonal, "z"), "ocean_cube")
})

test_that("canonical alignment errors propagate through signal_noise", {
  time <- as.Date(c("2020-01-01", "2021-01-01"))
  x <- signal_noise_test_cube(
    time, seq_len(32),
    lon = c(-80, -79), lat = c(-12, -11), depth = c(0, 10),
    vars = c("temperature", "oxygen"),
    units = c(temperature = "degC", oxygen = "mmol m-3")
  )
  clim <- signal_noise_test_clim(
    x, means = list("01" = 10), sds = list("01" = 2)
  )

  shifted_lon <- clim
  shifted_lon$lon <- shifted_lon$lon + 1
  expect_error(signal_noise(x, shifted_lon), "lon coordinates")

  reversed_lat <- clim
  reversed_lat$lat <- rev(reversed_lat$lat)
  expect_error(signal_noise(x, reversed_lat), "lat coordinates")

  changed_depth <- clim
  changed_depth$depth <- c(0, 20)
  expect_error(signal_noise(x, changed_depth), "depth coordinates")

  reordered_vars <- clim
  reordered_vars$vars <- rev(reordered_vars$vars)
  reordered_vars$units <- reordered_vars$units[reordered_vars$vars]
  expect_error(signal_noise(x, reordered_vars), "variables")

  changed_units <- clim
  changed_units$units[[1L]] <- "K"
  expect_error(signal_noise(x, changed_units), "match exactly")

  changed_calendar <- clim
  changed_calendar$provenance$extra$core$calendar <- "gregorian"
  expect_error(signal_noise(x, changed_calendar), "calendars")

  posix_x <- signal_noise_test_cube(
    as.POSIXct(time, tz = "UTC"), seq_len(32),
    lon = x$lon, lat = x$lat, depth = x$depth,
    vars = x$vars, units = x$units
  )
  posix_clim <- signal_noise_test_clim(
    posix_x, means = list("01" = 10), sds = list("01" = 2)
  )
  expect_error(signal_noise(x, posix_clim), "time class")
  expect_error(signal_noise(posix_x, clim), "time class")

  incomplete <- clim
  incomplete$provenance$extra$core <- NULL
  expect_error(signal_noise(x, incomplete), "Recompute with")
})

test_that("memory and lazy NetCDF modes are equal with one source computation", {
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
  clim <- suppressWarnings(clim_month(memory))
  original_read <- .cube_read
  source_reads <- 0L
  local_mocked_bindings(
    .cube_read = function(x, ...) {
      if (identical(.cube_backend(x), "netcdf")) {
        source_reads <<- source_reads + 1L
      }
      original_read(x, ...)
    },
    .package = "oceancube"
  )

  lazy_magnitude <- signal_noise(lazy, clim, FALSE)
  magnitude_reads <- source_reads
  source_reads <- 0L
  lazy_signed <- signal_noise(lazy, clim, TRUE)
  signed_reads <- source_reads
  memory_magnitude <- signal_noise(memory, clim, FALSE)
  memory_signed <- signal_noise(memory, clim, TRUE)

  expect_equal(
    lazy_magnitude$data, memory_magnitude$data,
    tolerance = A2_TOLERANCE$temporal_absolute
  )
  expect_equal(
    lazy_signed$data, memory_signed$data,
    tolerance = A2_TOLERANCE$temporal_absolute
  )
  expect_identical(is.na(lazy_magnitude$data), is.na(memory_magnitude$data))
  expect_identical(is.na(lazy_signed$data), is.na(memory_signed$data))
  for (field in c("lon", "lat", "depth", "time", "vars", "units")) {
    expect_identical(lazy_magnitude[[field]], memory_magnitude[[field]])
    expect_identical(lazy_signed[[field]], memory_signed[[field]])
  }
  expect_identical(lazy_magnitude$anomaly, memory_magnitude$anomaly)
  expect_identical(lazy_signed$anomaly, memory_signed$anomaly)
  expect_identical(
    lazy_magnitude$provenance$signal_noise,
    memory_magnitude$provenance$signal_noise
  )
  expect_identical(
    lazy_signed$provenance$signal_noise,
    memory_signed$provenance$signal_noise
  )

  magnitude_metrics <- lazy_magnitude$qa$anomaly$backend
  signed_metrics <- lazy_signed$qa$anomaly$backend
  expect_identical(magnitude_metrics$source_shape, c(3L, 2L, 2L, 4L, 2L))
  expect_identical(magnitude_metrics$output_shape, c(3L, 2L, 2L, 4L, 2L))
  expect_identical(magnitude_reads, magnitude_metrics$read_count)
  expect_identical(signed_reads, signed_metrics$read_count)
  expect_gt(magnitude_metrics$read_count, 1L)
  expect_equal(magnitude_metrics$logical_values, 96)
  expect_equal(magnitude_metrics$physical_values, 96)
  expect_lt(magnitude_metrics$max_block, 96)
  expect_false(magnitude_metrics$full_source_materialized)
  expect_true(magnitude_metrics$final_output_materialized)
  expect_identical(magnitude_metrics, signed_metrics)
  expect_identical(.cube_backend(lazy_magnitude), "memory")
  expect_identical(.cube_backend(lazy_signed), "memory")
})

test_that("outputs are valid serializable and inputs remain immutable", {
  time <- as.POSIXct(
    c("2020-01-01 01:30:00", "2021-01-01 12:45:00"), tz = "UTC"
  )
  x <- signal_noise_test_cube(
    time, seq_len(16), lon = c(-80, -79), depth = c(0, 10),
    vars = c("temperature", "oxygen"),
    units = c(temperature = "degC", oxygen = "mmol m-3")
  )
  clim <- signal_noise_test_clim(
    x, means = list("01" = 5), sds = list("01" = 2)
  )
  x_before <- serialize(x, NULL)
  clim_before <- serialize(clim, NULL)

  magnitude <- signal_noise(x, clim, FALSE)
  signed <- signal_noise(x, clim, TRUE)
  expect_identical(serialize(x, NULL), x_before)
  expect_identical(serialize(clim, NULL), clim_before)
  expect_identical(class(magnitude), c("ocean_anom", "ocean_cube", "list"))
  expect_identical(class(signed), class(magnitude))
  expect_identical(dim(magnitude$data), dim(x$data))
  expect_identical(magnitude$time, x$time)
  expect_identical(magnitude$units, c(temperature = "1", oxygen = "1"))
  expect_false(any(cube_validate(magnitude, strict = TRUE)$status == "FAIL"))
  expect_s3_class(cube_inspect(magnitude), "ocean_cube_inspection")

  file <- tempfile("signal-noise-", fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  saveRDS(magnitude, file)
  restored <- readRDS(file)
  expect_identical(restored, magnitude)

  bad <- clim
  bad$lon <- bad$lon + 1
  bad_before <- serialize(bad, NULL)
  expect_error(signal_noise(x, bad), "lon coordinates")
  expect_identical(serialize(x, NULL), x_before)
  expect_identical(serialize(bad, NULL), bad_before)
})
