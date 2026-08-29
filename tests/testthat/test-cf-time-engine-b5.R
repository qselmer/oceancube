cf_time_test_cube <- function(calendar = "360_day") {
  time <- .decode_cf_time(
    0:3,
    "days since 2001-02-28",
    calendar
  )$decoded_values
  ocean_cube(
    lon = c(-80, -79),
    lat = c(-12, -11),
    depth = c(0, 50),
    time = time,
    vars = "temperature",
    units = c(temperature = "degree_Celsius"),
    data = array(seq_len(2 * 2 * 2 * 4), dim = c(2, 2, 2, 4, 1))
  )
}

test_that("calendar-aware decoding implements the B5 supported subset", {
  cases <- list(
    `360_day` = c("2001-02-28", "2001-02-29", "2001-02-30", "2001-03-01"),
    `365_day` = c("2001-02-28", "2001-03-01", "2001-03-02", "2001-03-03"),
    `366_day` = c("2001-02-28", "2001-02-29", "2001-03-01", "2001-03-02"),
    julian = c("2001-02-28", "2001-03-01", "2001-03-02", "2001-03-03")
  )
  for (calendar in names(cases)) {
    value <- .decode_cf_time(
      0:3,
      "days since 2001-02-28",
      calendar
    )$decoded_values
    expect_s3_class(value, "oceancube_cf_time")
    expect_identical(attr(value, "schema_name", exact = TRUE), "oceancube_cf_time")
    expect_identical(attr(value, "schema_version", exact = TRUE), "1.0.0")
    expect_identical(attr(value, "calendar", exact = TRUE), calendar)
    expect_identical(
      substr(format(value), 1L, 10L),
      unname(cases[[calendar]])
    )
  }

  noleap <- .decode_cf_time(0:1, "days since 2000-02-28", "noleap")
  all_leap <- .decode_cf_time(0:1, "days since 2001-02-28", "all_leap")
  expect_identical(attr(noleap$decoded_values, "calendar"), "365_day")
  expect_identical(attr(all_leap$decoded_values, "calendar"), "366_day")
  expect_identical(substr(format(noleap$decoded_values[[2L]]), 1L, 10L), "2000-03-01")
  expect_identical(substr(format(all_leap$decoded_values[[2L]]), 1L, 10L), "2001-02-29")
})

test_that("standard mixed and Julian leap semantics are truthful", {
  standard <- .decode_cf_time(0:2, "days since 1582-10-04", "standard")$decoded_values
  expect_identical(
    substr(format(standard), 1L, 10L),
    c("1582-10-04", "1582-10-15", "1582-10-16")
  )
  julian <- .decode_cf_time(0:2, "days since 1900-02-28", "julian")$decoded_values
  expect_identical(
    substr(format(julian), 1L, 10L),
    c("1900-02-28", "1900-02-29", "1900-03-01")
  )
  expect_error(
    .decode_cf_time(0, "days since 1582-10-10", "standard"),
    "reform gap"
  )
})

test_that("fixed units, offsets, fractions, and strict axes are enforced", {
  decoded <- .decode_cf_time(
    c(-0.125, 0, 0.25),
    "seconds since 2001-02-30 00:00:00.125+01:30",
    "360_day"
  )
  expect_equal(diff(.time_key(decoded$decoded_values)), c(0.125, 0.25))
  expect_identical(decoded$origin_offset, "+01:30")
  expect_identical(decoded$unit, "seconds")

  expect_error(.decode_cf_time(0:1, "months since 2001-01-01", "360_day"), "units must match")
  expect_error(.decode_cf_time(0:1, "years since 2001-01-01", "360_day"), "units must match")
  expect_error(.decode_cf_time(c(0, Inf), "days since 2001-01-01", "360_day"), "finite numeric")
  expect_error(.decode_cf_time(c(0, 0), "days since 2001-01-01", "360_day"), "unique")
  expect_error(.decode_cf_time(c(1, 0), "days since 2001-01-01", "360_day"), "strictly increasing")
  expect_error(.decode_cf_time(0, "days since 2001-02-29", "365_day"), "invalid")
  expect_error(.decode_cf_time(0, "days since 2001-02-30", "standard"), "invalid")
  expect_error(.decode_cf_time(0, "days since 2001-01-01", "none"), "not supported")
  expect_error(.decode_cf_time(0:1, "days since 9999-12-30", "360_day"), "year envelope")
})

test_that("modern Gregorian behavior remains POSIXct and UTC", {
  for (calendar in c("standard", "gregorian", "proleptic_gregorian")) {
    decoded <- .decode_cf_time(
      c(-0.5, 0, 1.25),
      "days since 2000-01-02 00:00:00-0500",
      calendar
    )$decoded_values
    expect_s3_class(decoded, "POSIXct")
    expect_identical(attr(decoded, "tzone"), "UTC")
  }
  expect_identical(
    .decode_cf_time(0, "days since 2000-01-01", "gregorian")$calendar,
    "gregorian"
  )
})

test_that("eager, deferred, collect, validation, and provenance agree", {
  file <- make_netcdf_backend_fixture(
    calendar = "360_day",
    time_units = "days since 2001-02-28",
    time_values = 0:3
  )
  withr::local_file(file)
  eager <- read_nc(file, vars = "temperature")
  deferred <- cube_open(file, vars = "temperature")
  collected <- cube_collect(deferred)

  expect_s3_class(eager$time, "oceancube_cf_time")
  expect_s3_class(deferred$time, "oceancube_cf_time")
  expect_s3_class(collected$time, "oceancube_cf_time")
  expect_identical(format(eager$time), format(deferred$time))
  expect_identical(format(collected$time), format(deferred$time))
  expect_no_error(cube_validate(eager, strict = TRUE))
  expect_no_error(cube_validate(deferred, strict = TRUE))
  expect_identical(eager$provenance$time$current$class, "oceancube_cf_time")
  expect_identical(eager$provenance$time$current$calendar, "360_day")
  expect_null(eager$provenance$time$current$timezone)
  expect_identical(eager$provenance$time$source$cf_units, "days since 2001-02-28")
})

test_that("slice and crop use calendar-compatible selectors", {
  x <- cf_time_test_cube()
  exact <- cube_slice(x, time = "2001-02-30", match = "exact")
  expect_identical(substr(format(exact$time), 1L, 10L), "2001-02-30")

  nearest <- cube_slice(
    x,
    time = "2001-02-29T12:00:00",
    match = "nearest",
    tolerance = list(time = as.difftime(12, units = "hours"))
  )
  expect_identical(substr(format(nearest$time), 1L, 10L), "2001-02-29")
  expect_equal(as.numeric(nearest$qa$selection$distances$time, units = "secs"), 43200)

  cropped <- cube_crop(x, time = c("2001-02-29", "2001-02-30"))
  expect_identical(
    substr(format(cropped$time), 1L, 10L),
    c("2001-02-29", "2001-02-30")
  )
  expect_error(cube_slice(x, time = as.Date("2001-03-01"), match = "exact"), "compatible")
  expect_error(cube_crop(x, time = c("2001-02-31", "2001-03-01")), "invalid")
})

test_that("table producers and non-temporal carrying preserve CF time", {
  x <- cf_time_test_cube()
  extracted <- cube_extract(
    x,
    longitude = -80,
    latitude = -12,
    depth = 0,
    variable = "temperature",
    match = "exact",
    mode = "series"
  )
  expect_s3_class(extracted$time, "oceancube_cf_time")
  expect_s3_class(extracted[1:2, , drop = FALSE]$time, "oceancube_cf_time")
  rebound <- rbind(
    extracted[1:2, , drop = FALSE],
    extracted[3:4, , drop = FALSE]
  )
  expect_s3_class(rebound$time, "oceancube_cf_time")
  expect_identical(format(rebound$time), format(extracted$time))

  path <- data.frame(
    longitude = c(-80, -79),
    latitude = c(-12, -11)
  )
  transect <- cube_transect(
    x,
    path,
    depth = 0,
    time = "2001-02-30",
    variable = "temperature",
    match = "exact",
    mode = "horizontal"
  )
  expect_s3_class(transect$time, "oceancube_cf_time")
  expect_identical(substr(format(transect$time), 1L, 10L), rep("2001-02-30", 2L))

  carried <- layer_mean(x, depth = c(0, 50))
  expect_s3_class(carried$time, "oceancube_cf_time")
  expect_identical(format(carried$time), format(x$time))
})

test_that("mask, stock, and coast operations carry calendar-aware time", {
  skip_if_not_installed("sf")
  x <- cf_time_test_cube()
  stock <- stock_mask(x, stock = "fixture", lat = c(-12, -11), depth = c(0, 50))
  stock_cube <- crop_stock(x, stock)
  expect_s3_class(stock_cube$time, "oceancube_cf_time")
  expect_identical(format(stock_cube$time), format(x$time))

  ring <- matrix(
    c(-80.5, -12.5, -78.5, -12.5, -78.5, -10.5,
      -80.5, -10.5, -80.5, -12.5),
    ncol = 2,
    byrow = TRUE
  )
  polygon <- sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  masked <- cube_mask(x, polygon)
  coast <- coast_dist(x, polygon)
  expect_s3_class(masked$time, "oceancube_cf_time")
  expect_s3_class(coast$time, "oceancube_cf_time")
  expect_identical(format(masked$time), format(x$time))
  expect_identical(format(coast$time), format(x$time))
})

test_that("unsupported calendar-aware temporal analytics fail coherently", {
  x <- cf_time_test_cube()
  calls <- list(
    cube_aggregate_time = function() cube_aggregate_time(x, by = "month"),
    cube_climatology = function() cube_climatology(x, by = "month"),
    cube_anomaly = function() cube_anomaly(x, NULL),
    cube_trend = function() cube_trend(x),
    to_month = function() to_month(x),
    clim_month = function() clim_month(x),
    clim_day = function() clim_day(x),
    viz_timeseries = function() viz.timeseries(x, variable = "temperature")
  )
  for (name in names(calls)) {
    expect_error(
      calls[[name]](),
      class = "oceancube_cf_time_unsupported_operation",
      info = name
    )
  }
})

test_that("calendar-aware vectors serialize and reject unsafe arithmetic", {
  x <- .decode_cf_time(0:2, "days since 2001-02-28", "360_day")$decoded_values
  file <- tempfile(fileext = ".rds")
  withr::local_file(file)
  saveRDS(x, file)
  restored <- readRDS(file)
  expect_identical(restored, x)
  expect_identical(format(restored), format(x))
  expect_error(x + 1, class = "oceancube_cf_time_operation")

  other <- .decode_cf_time(0:2, "days since 2001-02-28", "365_day")$decoded_values
  expect_error(x == other, class = "oceancube_cf_time_operation")
  expect_error(c(x, other), "different calendar semantics")
})
