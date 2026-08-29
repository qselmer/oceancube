time_foundation_cube <- function(time, values = NULL) {
  if (is.null(values)) values <- seq_along(time)
  ocean_cube(
    lon = -80, lat = -12, depth = 0, time = time, vars = "temperature",
    units = c(temperature = "degC"),
    data = array(as.numeric(values), dim = c(1, 1, 1, length(time), 1))
  )
}

test_that("constructor preserves Date and normalizes POSIXct instants to UTC", {
  dates <- as.Date(c("2026-08-13", "2026-08-14"))
  expect_identical(time_foundation_cube(dates)$time, dates)

  zones <- c("UTC", "America/Lima", "Pacific/Auckland", "Etc/GMT-5")
  for (zone in zones) {
    input <- as.POSIXct(
      c("2026-08-13 00:00:00.125", "2026-08-13 23:59:59.875"),
      tz = zone
    )
    cube <- time_foundation_cube(input)
    expect_s3_class(cube$time, "POSIXct")
    expect_identical(attr(cube$time, "tzone"), "UTC")
    expect_equal(as.numeric(cube$time), as.numeric(input), tolerance = 0)
    expect_equal(diff(as.numeric(cube$time)), diff(as.numeric(input)), tolerance = 0)
  }
})

test_that("character inputs require explicit temporal semantics", {
  date_cube <- time_foundation_cube(c("2026-08-13", "2026-08-14"))
  expect_s3_class(date_cube$time, "Date")

  values <- c(
    "2026-08-13T12:30:00Z",
    "2026-08-13 13:30:00+00:00",
    "2026-08-13T09:30:00-05:00"
  )
  cube <- time_foundation_cube(values)
  expect_s3_class(cube$time, "POSIXct")
  expect_identical(attr(cube$time, "tzone"), "UTC")
  expect_equal(
    as.numeric(cube$time),
    as.numeric(as.POSIXct(c(
      "2026-08-13 12:30:00", "2026-08-13 13:30:00", "2026-08-13 14:30:00"
    ), tz = "UTC")),
    tolerance = 0
  )
  expect_error(
    time_foundation_cube(c("2026-08-13 12:30:00", "2026-08-13 13:30:00")),
    "explicit `Z` or numeric UTC offset"
  )
  expect_error(
    time_foundation_cube(c("2026-08-13", "2026-08-14T00:00:00Z")),
    "must not mix"
  )
})

test_that("canonical constructors reject missing duplicate and unsorted time", {
  expect_error(
    time_foundation_cube(as.Date(c("2020-01-01", NA))),
    "missing"
  )
  expect_error(
    time_foundation_cube(as.Date(c("2020-01-01", "2020-01-01"))),
    "unique"
  )
  expect_error(
    time_foundation_cube(as.Date(c("2020-01-02", "2020-01-01"))),
    "strictly increasing"
  )
})

test_that("CF units retain fractions negative offsets and origin offsets", {
  cases <- list(
    days = list(c(-0.5, 0, 0.25), "days since 2000-01-02"),
    hours = list(c(-1, 0, 1.5), "hours since 2000-01-01 06:30:00"),
    minutes = list(c(-1, 0, 0.5), "minutes since 2000-01-01T00:15:00"),
    seconds = list(c(-0.5, 0, 0.25), "seconds since 2000-01-01 00:00:00Z")
  )
  multipliers <- c(days = 86400, hours = 3600, minutes = 60, seconds = 1)
  for (name in names(cases)) {
    decoded <- .decode_cf_time(cases[[name]][[1L]], cases[[name]][[2L]], "gregorian")
    expect_s3_class(decoded$decoded_values, "POSIXct")
    expect_identical(attr(decoded$decoded_values, "tzone"), "UTC")
    expect_equal(
      diff(as.numeric(decoded$decoded_values)),
      diff(cases[[name]][[1L]]) * multipliers[[name]],
      tolerance = 1e-9
    )
  }

  plus <- .decode_cf_time(0, "hours since 2000-01-01 00:00:00+05:00", "gregorian")
  minus <- .decode_cf_time(0, "hours since 2000-01-01 00:00:00-05:00", "gregorian")
  expect_equal(as.numeric(plus$decoded_values), as.numeric(as.POSIXct("1999-12-31 19:00:00", tz = "UTC")))
  expect_equal(as.numeric(minus$decoded_values), as.numeric(as.POSIXct("2000-01-01 05:00:00", tz = "UTC")))
  expect_identical(plus$origin_offset, "+05:00")
  expect_identical(minus$origin_offset, "-05:00")
  expect_error(.decode_cf_time(0:1, "weeks since 2000-01-01", "gregorian"), "units must match")
})

test_that("CF calendar policy is explicit and calendar-aware", {
  missing <- .decode_cf_time(0:1, "days since 2000-01-01", NA_character_)
  expect_identical(missing$calendar, "standard")
  expect_true(missing$calendar_defaulted)
  for (calendar in c("standard", "gregorian", "proleptic_gregorian")) {
    decoded <- .decode_cf_time(0:1, "days since 2000-01-01", calendar)
    expect_identical(decoded$calendar, calendar)
  }
  for (calendar in c("julian", "365_day", "noleap", "366_day", "all_leap", "360_day")) {
    decoded <- .decode_cf_time(0:1, "days since 2000-01-01", calendar)
    expect_s3_class(decoded$decoded_values, "oceancube_cf_time")
    expect_identical(decoded$calendar, calendar)
  }
  for (calendar in c("none", "custom")) {
    expect_error(
      .decode_cf_time(0:1, "days since 2000-01-01", calendar),
      paste0("Calendar `", calendar, "` is not supported")
    )
  }
  for (calendar in c("standard", "gregorian")) {
    expect_error(
      .decode_cf_time(0:1, "days since 1582-10-14", calendar),
      "reform gap"
    )
  }
  expect_no_error(.decode_cf_time(0:1, "days since 1500-01-01", "proleptic_gregorian"))
})

test_that("Gregorian century leap semantics remain correct", {
  origins <- c("1900-02-28", "2000-02-28", "2004-02-28", "2100-02-28")
  expected <- c("1900-03-01", "2000-02-29", "2004-02-29", "2100-03-01")
  found <- vapply(origins, function(origin) {
    format(.decode_cf_time(1, paste("days since", origin), "proleptic_gregorian")$decoded_values, "%Y-%m-%d")
  }, character(1))
  expect_identical(unname(found), expected)
})

test_that("validation and inspection diagnose legacy temporal axes", {
  cube <- time_foundation_cube(as.Date("2020-01-01") + 0:2)
  duplicate <- cube
  duplicate$time <- as.Date(c("2020-01-01", "2020-01-01", "2020-01-03"))
  duplicate$temporal_extent <- range(duplicate$time)
  unsorted <- cube
  unsorted$time <- as.Date(c("2020-01-03", "2020-01-01", "2020-01-02"))
  unsorted$temporal_extent <- range(unsorted$time)

  duplicate_report <- cube_validate(duplicate)
  unsorted_report <- cube_validate(unsorted)
  expect_identical(duplicate_report$status[duplicate_report$check == "time_unique"], "FAIL")
  expect_identical(unsorted_report$status[unsorted_report$check == "time_strictly_increasing"], "FAIL")
  expect_error(cube_validate(duplicate, strict = TRUE), class = "oceancube_validation_error")
  expect_error(cube_validate(unsorted, strict = TRUE), class = "oceancube_validation_error")

  duplicate_inspect <- cube_inspect(duplicate)
  unsorted_inspect <- cube_inspect(unsorted)
  expect_true(duplicate_inspect$time_summary$duplicates)
  expect_false(duplicate_inspect$time_summary$strictly_increasing)
  expect_false(unsorted_inspect$time_summary$duplicates)
  expect_false(unsorted_inspect$time_summary$strictly_increasing)
  expect_true(is.na(unsorted_inspect$time_resolution$resolution))
})

test_that("inspection reports canonical temporal metadata and intervals", {
  date_cube <- time_foundation_cube(as.Date("2020-01-01") + c(0, 1, 3))
  posix_cube <- time_foundation_cube(as.POSIXct(
    c("2020-01-01 00:00:00", "2020-01-01 06:00:00", "2020-01-01 18:00:00"),
    tz = "UTC"
  ))
  date_info <- cube_inspect(date_cube)$time_summary
  posix_info <- cube_inspect(posix_cube)$time_summary
  expect_identical(date_info$class, "Date")
  expect_identical(date_info$calendar, "proleptic_gregorian")
  expect_false(date_info$regular)
  expect_equal(c(date_info$minimum_positive_interval, date_info$median_positive_interval, date_info$maximum_positive_interval), c(1, 1.5, 2))
  expect_identical(posix_info$class, "POSIXct")
  expect_identical(posix_info$timezone, "UTC")
  expect_equal(c(posix_info$minimum_positive_interval, posix_info$maximum_positive_interval), c(21600, 43200))
})

test_that("exact and nearest matching use canonical instant identity", {
  dates <- time_foundation_cube(as.Date("2020-01-01") + c(0, 2, 4))
  exact <- cube_slice(dates, time = as.Date("2020-01-03"))
  expect_identical(exact$time, as.Date("2020-01-03"))
  expect_error(cube_slice(dates, time = as.Date("2020-01-02")), "not found")

  instants <- time_foundation_cube(as.POSIXct(
    c("2020-01-01 00:00:00", "2020-01-01 02:00:00"), tz = "UTC"
  ))
  nearest <- cube_slice(
    instants,
    time = as.POSIXct("2019-12-31 20:00:00", tz = "America/Lima"),
    match = "nearest"
  )
  expect_identical(nearest$time, instants$time[1L])
  expect_no_error(cube_slice(
    instants,
    time = as.POSIXct("2020-01-01 01:00:00", tz = "UTC"),
    match = "nearest", tolerance = list(time = as.difftime(1, units = "hours"))
  ))
  expect_error(cube_slice(
    instants,
    time = as.POSIXct("2020-01-01 01:00:00", tz = "UTC"),
    match = "nearest", tolerance = list(time = as.difftime(59, units = "mins"))
  ), "exceeding tolerance")
  expect_error(cube_slice(
    instants,
    time = as.POSIXct("2019-12-31 23:00:00", tz = "UTC"), match = "nearest"
  ), "outside")
})

test_that("table extraction preserves requested and repeated selector order", {
  cube <- time_foundation_cube(as.Date("2020-01-01") + 0:2)
  result <- cube_extract(
    cube,
    time = as.Date(c("2020-01-03", "2020-01-01", "2020-01-03")),
    mode = "series"
  )
  expect_identical(result$time, as.Date(c("2020-01-03", "2020-01-01", "2020-01-03")))
  expect_identical(result$value, c(3, 1, 3))
  expect_error(
    cube_slice(cube, time = as.Date(c("2020-01-03", "2020-01-01"))),
    "strictly increasing"
  )
})

test_that("time ranges are closed clipped and internally open", {
  cube <- time_foundation_cube(as.Date("2020-01-01") + 0:3)
  closed <- cube_crop(cube, time = as.Date(c("2020-01-02", "2020-01-03")))
  same <- cube_crop(cube, time = rep(as.Date("2020-01-02"), 2))
  clipped <- cube_crop(cube, time = as.Date(c("2019-01-01", "2020-01-02")), outside = "clip")
  expect_identical(closed$time, as.Date(c("2020-01-02", "2020-01-03")))
  expect_identical(same$time, as.Date("2020-01-02"))
  expect_identical(clipped$time, as.Date(c("2020-01-01", "2020-01-02")))
  expect_error(cube_crop(cube, time = as.Date(c("2020-01-03", "2020-01-02"))), "ordered")
  expect_error(cube_crop(cube, time = as.Date(c("2021-01-01", "2021-01-02"))), "outside")
  lower_open <- .resolve_time_crop_range(cube$time, list(NULL, as.Date("2020-01-02")), "error")
  upper_open <- .resolve_time_crop_range(cube$time, list(as.Date("2020-01-03"), NULL), "error")
  expect_identical(lower_open$index, 1:2)
  expect_identical(upper_open$index, 3:4)
})

test_that("sub-day time survives slice crop extract collect and visualization", {
  times <- as.POSIXct(
    c("2020-01-01 00:00:00.125", "2020-01-01 06:00:00.250", "2020-01-01 12:30:00.500"),
    tz = "UTC"
  )
  cube <- time_foundation_cube(times)
  sliced <- cube_slice(cube, time = times[1:2])
  cropped <- cube_crop(cube, time = times[2:3])
  extracted <- cube_extract(cube, mode = "series")
  collected <- cube_collect(cube)
  plot <- viz.timeseries(cube, "temperature")
  expect_equal(as.numeric(sliced$time), as.numeric(times[1:2]), tolerance = 0)
  expect_equal(as.numeric(cropped$time), as.numeric(times[2:3]), tolerance = 0)
  expect_equal(as.numeric(extracted$time), as.numeric(times), tolerance = 0)
  expect_identical(collected, cube)
  expect_equal(as.numeric(plot$data$time), as.numeric(times), tolerance = 0)
})

test_that("eager lazy and public memory NetCDF paths share time semantics", {
  file <- make_netcdf_backend_fixture(
    calendar = "gregorian",
    time_units = "hours since 2000-01-01 00:00:00+05:00",
    time_values = c(0, 0.5, 6, 24)
  )
  withr::local_file(file)
  eager <- read_nc(file, vars = "temperature")
  lazy <- .new_netcdf_cube(.new_netcdf_storage(file, "temperature"))
  memory <- ocean_cube(
    lon = lazy$lon, lat = lazy$lat, depth = lazy$depth, time = lazy$time,
    vars = lazy$vars, units = lazy$units, data = .cube_read(lazy)
  )
  expect_s3_class(eager$time, "POSIXct")
  expect_s3_class(lazy$time, "POSIXct")
  expect_identical(attr(eager$time, "tzone"), "UTC")
  expect_identical(attr(lazy$time, "tzone"), "UTC")
  expect_equal(as.numeric(eager$time), as.numeric(lazy$time), tolerance = 1e-9)
  expect_equal(as.numeric(memory$time), as.numeric(lazy$time), tolerance = 0)

  selectors <- list(time = lazy$time[2:3], variable = "temperature")
  expect_equal(
    as.numeric(do.call(cube_slice, c(list(x = memory), selectors))$time),
    as.numeric(do.call(cube_slice, c(list(x = lazy), selectors))$time),
    tolerance = 0
  )
  expect_equal(
    as.numeric(cube_crop(memory, time = lazy$time[2:3])$time),
    as.numeric(cube_crop(lazy, time = lazy$time[2:3])$time),
    tolerance = 0
  )
})

test_that("collect preserves compact CF temporal provenance", {
  file <- make_netcdf_backend_fixture(add_calendar = FALSE)
  withr::local_file(file)
  lazy <- .new_netcdf_cube(.new_netcdf_storage(file, "temperature"))
  collected <- cube_collect(lazy)
  metadata <- .find_time_provenance(collected$provenance)
  expect_identical(collected$time, lazy$time)
  expect_identical(metadata$calendar, "standard")
  expect_true(metadata$calendar_defaulted)
  expect_identical(metadata$cf_units, lazy$storage$time$units)
  expect_identical(metadata$cf_origin, lazy$storage$time$origin_text)
  expect_identical(metadata$decoder, "oceancube::.decode_cf_time")
  expect_null(metadata$raw_values)
})

test_that("NetCDF rejects duplicate and unsorted temporal coordinates", {
  duplicate <- make_netcdf_backend_fixture(time_values = c(0, 1, 1, 2))
  unsorted <- make_netcdf_backend_fixture(time_values = c(0, 2, 1, 3))
  withr::local_file(duplicate)
  withr::local_file(unsorted)
  expect_error(.new_netcdf_storage(duplicate, "temperature"), "unique")
  expect_error(.new_netcdf_storage(unsorted, "temperature"), "strictly increasing")
  expect_error(read_nc(duplicate, vars = "temperature"), "unique")
  expect_error(read_nc(unsorted, vars = "temperature"), "strictly increasing")
})
