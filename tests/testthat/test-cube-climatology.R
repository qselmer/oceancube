climatology_cube <- function(time, values, lon = -80, lat = -12, depth = 0,
                             vars = "temperature", units = "degC") {
  ocean_cube(
    lon = lon,
    lat = lat,
    depth = depth,
    time = time,
    data = array(values, dim = c(length(lon), length(lat), length(depth), length(time), length(vars))),
    vars = vars,
    units = units,
    source = "climatology-test"
  )
}

test_that("cube_climatology has the exact public API and validates arguments", {
  expect_true(is.function(cube_climatology))
  expect_identical(
    names(formals(cube_climatology)),
    c("x", "by", "period", "leap", "min_n", "diagnostics")
  )
  x <- climatology_cube(as.Date("2020-01-01"), 1)

  expect_error(cube_climatology(list(), "month"), "validation|ocean_cube")
  expect_error(cube_climatology(x), "by")
  expect_error(cube_climatology(x, "week"), "day.*month.*season")
  expect_error(cube_climatology(x, c("day", "month")), "by")
  expect_error(cube_climatology(x, NA_character_), "by")
  expect_error(cube_climatology(x, "month", period = as.Date("2020-01-01")), "two")
  expect_error(
    cube_climatology(x, "month", period = as.Date(c("2020-02-01", "2020-01-01"))),
    "ordered"
  )
  expect_error(
    cube_climatology(x, "month", period = as.Date(c("2019-01-01", "2019-12-31"))),
    "does not overlap"
  )
  expect_error(cube_climatology(x, "day", leap = "other"), "arg")
  expect_error(cube_climatology(x, "month", leap = "drop"), "only applicable")
  expect_error(cube_climatology(x, "season", leap = "feb28"), "only applicable")
  for (bad in list(0, -1, 1.5, Inf, NA_real_, c(1, 2), "1")) {
    expect_error(cube_climatology(x, "month", min_n = bad), "min_n")
  }
  for (bad in list(NA, 1, c(TRUE, FALSE))) {
    expect_error(cube_climatology(x, "month", diagnostics = bad), "diagnostics")
  }
})

test_that("monthly climatology uses equal month-year rather than raw-density weights", {
  first_year <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + 0:99 * 3600
  time <- c(first_year, as.POSIXct("2021-01-01 00:00:00", tz = "UTC"))
  x <- climatology_cube(time, c(rep(10, 100), 20))

  expect_warning(
    result <- cube_climatology(x, "month", diagnostics = TRUE),
    "irregular or gapped"
  )
  january <- match("01", result$climatology$group_key)
  february <- match("02", result$climatology$group_key)
  expect_equal(result$data[1, 1, 1, january, 1], 15)
  expect_false(isTRUE(all.equal(result$data[1, 1, 1, january, 1], mean(c(rep(10, 100), 20)))))
  expect_equal(result$qa$climatology$n_clim_valid[1, 1, 1, january, 1], 2L)
  expect_equal(result$climatology$n_clim_total[[january]], 2L)
  expect_equal(result$qa$climatology$coverage_fraction[1, 1, 1, january, 1], 1)
  expect_equal(result$climatology$n_clim_total[[february]], 1L)
  expect_equal(result$qa$climatology$n_clim_valid[1, 1, 1, february, 1], 0L)
  expect_equal(result$qa$climatology$coverage_fraction[1, 1, 1, february, 1], 0)
})

test_that("monthly replicate mean and sample SD match hand calculations", {
  x <- climatology_cube(
    as.Date(c("2018-01-01", "2019-01-01", "2020-01-01")),
    c(1, 3, 5)
  )
  result <- suppressWarnings(cube_climatology(x, "month", diagnostics = TRUE))
  january <- match("01", result$climatology$group_key)

  expect_equal(result$data[1, 1, 1, january, 1], 3)
  expect_equal(result$climatology$sd[1, 1, 1, january, 1], 2)
  expect_equal(result$qa$climatology$n_clim_valid[1, 1, 1, january, 1], 3L)
  expect_equal(result$climatology$n_clim_total[[january]], 3L)
  expect_equal(result$qa$climatology$coverage_fraction[1, 1, 1, january, 1], 1)
})

test_that("daily climatology first forms day-year replicates and uses stable keys", {
  time <- as.POSIXct(c(
    "2019-03-01 00:00:00", "2019-03-01 12:00:00",
    "2020-03-01 00:00:00"
  ), tz = "UTC")
  x <- climatology_cube(time, c(2, 4, 9))
  result <- suppressWarnings(cube_climatology(x, "day", leap = "drop", diagnostics = TRUE))
  march <- match("03-01", result$climatology$group_key)

  expect_equal(result$data[1, 1, 1, march, 1], mean(c(mean(c(2, 4)), 9)))
  expect_equal(result$qa$climatology$n_clim_valid[1, 1, 1, march, 1], 2L)
  expect_identical(result$climatology$group_key[[march]], "03-01")
  expect_identical(length(result$climatology$group_key), 365L)
  expect_false("02-29" %in% result$climatology$group_key)
})

test_that("drop keep and feb28 leap contracts are exact", {
  time <- as.Date(c("2019-02-28", "2020-02-28", "2020-02-29", "2021-02-28"))
  x <- climatology_cube(time, c(10, 20, 40, 10))
  drop <- suppressWarnings(cube_climatology(x, "day", leap = "drop", diagnostics = TRUE))
  keep <- suppressWarnings(cube_climatology(x, "day", leap = "keep", diagnostics = TRUE))
  feb28 <- suppressWarnings(cube_climatology(x, "day", leap = "feb28", diagnostics = TRUE))
  drop28 <- match("02-28", drop$climatology$group_key)
  keep28 <- match("02-28", keep$climatology$group_key)
  keep29 <- match("02-29", keep$climatology$group_key)
  merged28 <- match("02-28", feb28$climatology$group_key)

  expect_identical(length(drop$time), 365L)
  expect_identical(length(keep$time), 366L)
  expect_identical(length(feb28$time), 365L)
  expect_equal(drop$data[1, 1, 1, drop28, 1], mean(c(10, 20, 10)))
  expect_equal(keep$data[1, 1, 1, keep28, 1], mean(c(10, 20, 10)))
  expect_equal(keep$data[1, 1, 1, keep29, 1], 40)
  expect_equal(keep$climatology$n_clim_total[[keep29]], 1L)
  expect_equal(keep$qa$climatology$n_clim_valid[1, 1, 1, keep29, 1], 1L)
  expect_true(is.na(keep$climatology$sd[1, 1, 1, keep29, 1]))
  expect_equal(feb28$data[1, 1, 1, merged28, 1], 50 / 3)
  expect_false(isTRUE(all.equal(feb28$data[1, 1, 1, merged28, 1], 20)))
  expect_equal(feb28$qa$climatology$n_clim_valid[1, 1, 1, merged28, 1], 3L)
  expect_equal(feb28$climatology$n_clim_total[[merged28]], 3L)
})

test_that("feb28 merge uses either available leap-day component once", {
  only_feb29 <- climatology_cube(
    as.Date(c("2019-02-28", "2020-02-29", "2021-02-28")),
    c(10, 40, 10)
  )
  result <- suppressWarnings(cube_climatology(
    only_feb29, "day", leap = "feb28", diagnostics = TRUE
  ))
  index <- match("02-28", result$climatology$group_key)

  expect_equal(result$data[1, 1, 1, index, 1], 20)
  expect_equal(result$qa$climatology$n_clim_valid[1, 1, 1, index, 1], 3L)
  expect_equal(result$climatology$n_clim_total[[index]], 3L)
})

test_that("season climatology uses season-year replicates and retains partial DJF", {
  time <- as.Date(c(
    "2020-01-15", "2020-02-15",
    "2020-12-15", "2021-01-15", "2021-02-28"
  ))
  x <- climatology_cube(time, c(10, 10, 20, 20, 20))
  result <- suppressWarnings(cube_climatology(x, "season", diagnostics = TRUE))
  djf <- match("DJF", result$climatology$group_key)
  summary <- result$qa$climatology$period_summary
  djf_rows <- summary$recurring_group == "DJF"

  expect_identical(result$climatology$group_key, c("DJF", "MAM", "JJA", "SON"))
  expect_equal(result$data[1, 1, 1, djf, 1], 15)
  expect_equal(result$qa$climatology$n_clim_valid[1, 1, 1, djf, 1], 2L)
  expect_equal(result$climatology$n_clim_total[[djf]], 2L)
  expect_identical(summary$season_year[djf_rows], c(2020L, 2021L))
  expect_true(summary$partial[which(djf_rows)[[1L]]])
  expect_false(summary$partial[which(djf_rows)[[2L]]])
})

test_that("canonical cycle axes are complete unique increasing and class stable", {
  date_cube <- climatology_cube(as.Date("2020-01-01"), 1)
  posix_cube <- climatology_cube(as.POSIXct("2020-01-01 12:00:00", tz = "UTC"), 1)
  day_drop <- cube_climatology(date_cube, "day", leap = "drop")
  day_feb28 <- cube_climatology(date_cube, "day", leap = "feb28")
  day_keep <- cube_climatology(date_cube, "day", leap = "keep")
  month <- cube_climatology(date_cube, "month")
  season <- cube_climatology(date_cube, "season")
  posix <- cube_climatology(posix_cube, "month")

  expect_equal(range(day_drop$time), as.Date(c("2001-01-01", "2001-12-31")))
  expect_equal(range(day_feb28$time), as.Date(c("2001-01-01", "2001-12-31")))
  expect_equal(range(day_keep$time), as.Date(c("2000-01-01", "2000-12-31")))
  expect_identical(month$time, seq(as.Date("2001-01-01"), as.Date("2001-12-01"), by = "month"))
  expect_identical(
    season$time,
    as.Date(c("2000-12-01", "2001-03-01", "2001-06-01", "2001-09-01"))
  )
  for (result in list(day_drop, day_feb28, day_keep, month, season)) {
    expect_false(anyDuplicated(result$time) > 0L)
    expect_true(all(diff(as.numeric(result$time)) > 0))
    expect_s3_class(result$time, "Date")
  }
  expect_s3_class(posix$time, "POSIXct")
  expect_identical(attr(posix$time, "tzone"), "UTC")
  expect_true(all(format(posix$time, "%H:%M:%S", tz = "UTC") == "00:00:00"))
})

test_that("reference periods preserve Date semantics and clip explicitly", {
  x <- climatology_cube(as.Date("2000-01-01") + 0:2, c(1, 2, 3))
  full <- cube_climatology(x, "day", leap = "drop")
  expect_identical(full$climatology$requested_period, range(x$time))
  expect_identical(full$climatology$effective_period, range(x$time))
  expect_false(full$climatology$period_clipped)

  expect_warning(
    clipped <- cube_climatology(
      x,
      "day",
      period = as.Date(c("1999-01-01", "2000-01-02")),
      leap = "drop"
    ),
    "clipped"
  )
  expect_identical(
    clipped$climatology$requested_period,
    as.Date(c("1999-01-01", "2000-01-02"))
  )
  expect_identical(
    clipped$climatology$effective_period,
    as.Date(c("2000-01-01", "2000-01-02"))
  )
  expect_true(clipped$climatology$period_clipped)
  expect_false(any(clipped$qa$climatology$period_summary$partial))

  single <- cube_climatology(
    x, "day", period = as.Date(c("2000-01-02", "2000-01-02")), leap = "drop"
  )
  expect_identical(single$climatology$effective_period, rep(as.Date("2000-01-02"), 2L))
  expect_equal(single$data[1, 1, 1, match("01-02", single$climatology$group_key), 1], 2)
})

test_that("POSIXct reference periods retain exact UTC instant semantics", {
  time <- as.POSIXct(c(
    "2020-01-01 00:00:00", "2020-01-01 12:00:00", "2020-01-02 00:00:00"
  ), tz = "UTC")
  x <- climatology_cube(time, c(1, 12, 100))
  period <- as.POSIXct(c("2020-01-01 06:00:00", "2020-01-01 18:00:00"), tz = "UTC")
  result <- cube_climatology(x, "day", period = period, leap = "drop")
  january_one <- match("01-01", result$climatology$group_key)

  expect_s3_class(result$climatology$requested_period, "POSIXct")
  expect_identical(result$climatology$requested_period, period)
  expect_identical(result$climatology$effective_period, period)
  expect_equal(result$data[1, 1, 1, january_one, 1], 12)
  expect_identical(attr(result$time, "tzone"), "UTC")
  expect_error(
    cube_climatology(x, "day", period = as.Date(c("2020-01-01", "2020-01-02"))),
    "same POSIXct semantics"
  )
})

test_that("full cycles distinguish no opportunity from invalid expected groups", {
  one_month <- climatology_cube(as.Date("2020-01-01"), 1)
  no_opportunity <- cube_climatology(one_month, "month", diagnostics = TRUE)
  february <- match("02", no_opportunity$climatology$group_key)
  expect_identical(length(no_opportunity$time), 12L)
  expect_equal(no_opportunity$climatology$n_clim_total[[february]], 0L)
  expect_equal(no_opportunity$qa$climatology$n_clim_valid[1, 1, 1, february, 1], 0L)
  expect_true(is.na(no_opportunity$qa$climatology$coverage_fraction[1, 1, 1, february, 1]))
  expect_true(is.na(no_opportunity$data[1, 1, 1, february, 1]))
  expect_true(is.na(no_opportunity$climatology$sd[1, 1, 1, february, 1]))

  gap <- climatology_cube(as.Date(c("2020-01-01", "2020-03-01")), c(1, 3))
  invalid_expected <- suppressWarnings(cube_climatology(gap, "month", diagnostics = TRUE))
  expect_equal(invalid_expected$climatology$n_clim_total[[february]], 1L)
  expect_equal(invalid_expected$qa$climatology$n_clim_valid[1, 1, 1, february, 1], 0L)
  expect_equal(invalid_expected$qa$climatology$coverage_fraction[1, 1, 1, february, 1], 0)
  expect_true(is.na(invalid_expected$data[1, 1, 1, february, 1]))
  expect_true(is.na(invalid_expected$climatology$sd[1, 1, 1, february, 1]))
})

test_that("min_n operates on valid climatological replicates and retains groups", {
  x <- climatology_cube(as.Date(c("2020-01-01", "2021-01-01")), c(1, 3))
  pass <- suppressWarnings(cube_climatology(x, "month", min_n = 2L, diagnostics = TRUE))
  fail <- suppressWarnings(cube_climatology(x, "month", min_n = 3L, diagnostics = TRUE))
  january <- match("01", pass$climatology$group_key)

  expect_equal(pass$data[1, 1, 1, january, 1], 2)
  expect_equal(pass$climatology$sd[1, 1, 1, january, 1], sqrt(2))
  expect_true(is.na(fail$data[1, 1, 1, january, 1]))
  expect_true(is.na(fail$climatology$sd[1, 1, 1, january, 1]))
  expect_equal(fail$qa$climatology$n_clim_valid[1, 1, 1, january, 1], 2L)
  expect_identical(fail$climatology$group_key[[january]], "01")
})

test_that("diagnostics are optional while scientific SD is always available", {
  x <- climatology_cube(as.Date(c("2020-01-01", "2021-01-01")), c(1, 3))
  compact <- suppressWarnings(cube_climatology(x, "month"))
  detailed <- suppressWarnings(cube_climatology(x, "month", diagnostics = TRUE))

  expect_true(is.array(compact$climatology$sd))
  expect_identical(dim(compact$climatology$sd), dim(compact$data))
  expect_null(compact$qa$climatology$n_clim_valid)
  expect_null(compact$qa$climatology$coverage_fraction)
  expect_true(is.data.frame(compact$qa$climatology$period_summary))
  expect_identical(dim(detailed$qa$climatology$n_clim_valid), dim(detailed$data))
  expect_identical(dim(detailed$qa$climatology$coverage_fraction), dim(detailed$data))
})

test_that("all non-time dimensions units and surface depth are preserved", {
  lon <- c(-80, -79)
  lat <- c(-12, -11)
  depth <- c(0, 50)
  vars <- c("temperature", "oxygen", "salinity")
  time <- as.Date(c("2020-01-01", "2021-01-01"))
  values <- seq_len(length(lon) * length(lat) * length(depth) * length(time) * length(vars))
  x <- climatology_cube(
    time, values, lon = lon, lat = lat, depth = depth, vars = vars,
    units = c("degC", "mmol m-3", "1e-3")
  )
  result <- suppressWarnings(cube_climatology(x, "month"))

  expect_identical(result$lon, lon)
  expect_identical(result$lat, lat)
  expect_identical(result$depth, depth)
  expect_identical(result$vars, vars)
  expect_identical(result$units, c("degC", "mmol m-3", "1e-3"))
  expect_identical(unname(dim(result$data)), c(2L, 2L, 2L, 12L, 3L))
  expect_identical(dim(result$climatology$sd), dim(result$data))

  surface <- climatology_cube(time, c(1, 2), depth = NA_real_)
  surface_result <- suppressWarnings(cube_climatology(surface, "month"))
  expect_true(is.na(surface_result$depth))
  expect_identical(dim(surface_result$data)[[3L]], 1L)
})

test_that("climatology cubes validate inspect serialize and remain immutable", {
  x <- climatology_cube(as.Date(c("2020-01-01", "2021-01-01")), c(1, 3))
  before <- serialize(x, NULL)
  result <- suppressWarnings(cube_climatology(x, "month", diagnostics = TRUE))
  validation <- cube_validate(result, strict = TRUE)
  inspection <- cube_inspect(result, missing = "none")

  expect_false(any(validation$status == "FAIL"))
  expect_identical(inspection$time_summary$n, 12L)
  expect_identical(serialize(x, NULL), before)
  expect_identical(cube_collect(result), result)

  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  saveRDS(result, file)
  restored <- readRDS(file)
  expect_identical(restored$data, result$data)
  expect_identical(restored$climatology$sd, result$climatology$sd)
  expect_identical(restored$climatology$group_key, result$climatology$group_key)
  expect_identical(restored$climatology$effective_period, result$climatology$effective_period)
  expect_identical(restored$qa, result$qa)
  expect_identical(restored$provenance, result$provenance)

  sliced <- cube_slice(result, time = result$time[1L], by = "value")
  expect_null(sliced$climatology)
  expect_true("climatology" %in% sliced$provenance$cube_slice$discarded_components)
})

test_that("memory and lazy NetCDF climatologies are scientifically identical and bounded", {
  skip_if_not_installed("ncdf4")
  file <- make_netcdf_backend_fixture(
    time_units = "days since 2019-01-01 00:00:00",
    time_values = c(0, 365, 366, 731)
  )
  on.exit(unlink(file), add = TRUE)
  lazy <- .new_netcdf_cube(.new_netcdf_storage(file, c("temperature", "oxygen")))
  memory <- cube_collect(lazy)

  for (by in c("day", "month", "season")) {
    from_lazy <- suppressWarnings(cube_climatology(lazy, by, diagnostics = TRUE))
    from_memory <- suppressWarnings(cube_climatology(memory, by, diagnostics = TRUE))
    expect_equal(from_lazy$data, from_memory$data, info = by)
    expect_equal(from_lazy$climatology$sd, from_memory$climatology$sd, info = by)
    expect_identical(from_lazy$time, from_memory$time, info = by)
    expect_identical(from_lazy$climatology$group_key, from_memory$climatology$group_key, info = by)
    expect_identical(from_lazy$climatology$n_clim_total, from_memory$climatology$n_clim_total, info = by)
    expect_identical(
      from_lazy$qa$climatology$n_clim_valid,
      from_memory$qa$climatology$n_clim_valid,
      info = by
    )
    expect_equal(
      from_lazy$qa$climatology$coverage_fraction,
      from_memory$qa$climatology$coverage_fraction,
      info = by
    )
    expect_identical(from_lazy$units, from_memory$units, info = by)
    expect_identical(from_lazy$climatology$calendar, from_memory$climatology$calendar, info = by)
    expect_identical(
      from_lazy$climatology$effective_period,
      from_memory$climatology$effective_period,
      info = by
    )
    metrics <- from_lazy$qa$climatology$read_metrics
    expect_gt(metrics$backend_read_count, 0L)
    expect_gt(metrics$logical_source_values, 0)
    expect_gte(metrics$physical_source_values, metrics$logical_source_values)
    expect_lt(metrics$maximum_physical_block_values, prod(metrics$input_shape))
    expect_false(metrics$full_source_cube_materialized)
    expect_false(metrics$full_chronological_intermediate_materialized)
  }
})

test_that("provenance records the complete scientific contract", {
  x <- climatology_cube(as.Date(c("2020-01-01", "2021-01-01")), c(1, 3))
  result <- suppressWarnings(cube_climatology(x, "month", diagnostics = TRUE))
  record <- result$provenance$cube_climatology

  expect_identical(record$requested_period, range(x$time))
  expect_identical(record$effective_period, range(x$time))
  expect_false(record$period_clipped)
  expect_identical(record$by, "month")
  expect_true(is.na(record$leap))
  expect_identical(record$min_n, 1L)
  expect_identical(record$center, "arithmetic_mean")
  expect_identical(record$sd_method, "sample_standard_deviation")
  expect_identical(record$inner_weighting, "equal_observation")
  expect_identical(record$outer_weighting, "equal_period_replicate")
  expect_identical(record$calendar, "proleptic_gregorian")
  expect_identical(record$input_time_class, "Date")
  expect_identical(record$output_time_class, "Date")
  expect_identical(result$provenance$parent, x$provenance)
})

test_that("legacy climatology wrappers delegate while preserving their structure", {
  expect_identical(names(formals(clim_day)), c("x", "period", "leap", "min_n"))
  expect_identical(names(formals(clim_month)), c("x", "period"))
  monthly_input <- climatology_cube(
    c(as.POSIXct("2020-01-01", tz = "UTC") + 0:99 * 3600,
      as.POSIXct("2021-01-01", tz = "UTC")),
    c(rep(10, 100), 20)
  )
  monthly <- suppressWarnings(clim_month(monthly_input))
  monthly_core <- suppressWarnings(cube_climatology(
    monthly_input, "month", diagnostics = TRUE
  ))
  expect_s3_class(monthly, "ocean_clim")
  expect_identical(
    names(monthly),
    c("lon", "lat", "depth", "vars", "period", "mean", "sd", "n",
      "units", "source", "dataset_id", "provenance")
  )
  expect_equal(unname(monthly$mean), unname(monthly_core$data))
  expect_equal(unname(monthly$sd), unname(monthly_core$climatology$sd))
  expect_identical(unname(monthly$n), unname(monthly_core$qa$climatology$n_clim_valid))
  expect_equal(monthly$mean[1, 1, 1, 1, 1], 15)

  daily_input <- climatology_cube(
    as.Date(c("2019-02-28", "2020-02-28", "2020-02-29", "2021-02-28")),
    c(10, 20, 40, 10)
  )
  daily <- suppressWarnings(clim_day(daily_input, leap = "feb28"))
  daily_core <- suppressWarnings(cube_climatology(
    daily_input, "day", leap = "feb28", diagnostics = TRUE
  ))
  index <- match("02-28", daily$day)
  expect_s3_class(daily, "ocean_clim")
  expect_identical(
    names(daily),
    c("scale", "lon", "lat", "depth", "vars", "period", "day", "leap",
      "min_n", "mean", "sd", "n", "units", "source", "dataset_id", "provenance")
  )
  expect_equal(unname(daily$mean), unname(daily_core$data))
  expect_equal(unname(daily$sd), unname(daily_core$climatology$sd))
  expect_identical(unname(daily$n), unname(daily_core$qa$climatology$n_clim_valid))
  expect_equal(daily$mean[1, 1, 1, index, 1], 50 / 3)

  repeated_one_year <- climatology_cube(
    as.POSIXct(c("2020-01-01 00:00:00", "2020-01-01 12:00:00"), tz = "UTC"),
    c(1, 3)
  )
  min_two <- clim_day(repeated_one_year, leap = "drop", min_n = 2L)
  expect_true(is.na(min_two$mean[1, 1, 1, match("01-01", min_two$day), 1]))
})

test_that("legacy anomalies and signal noise remain regression-compatible", {
  x <- climatology_cube(as.Date(c("2020-01-01", "2021-01-01")), c(1, 3))
  clim <- suppressWarnings(clim_month(x))
  difference <- anom_diff(x, clim)
  z <- anom_z(x, clim)
  noise <- signal_noise(x, clim)

  expect_s3_class(difference, "ocean_anom")
  expect_s3_class(z, "ocean_anom")
  expect_s3_class(noise, "ocean_anom")
  expect_equal(as.vector(difference$data), c(-1, 1))
  expect_equal(as.vector(z$data), c(-1 / sqrt(2), 1 / sqrt(2)))
  expect_equal(as.vector(noise$data), rep(1 / sqrt(2), 2))
  expect_identical(difference$climatology, clim)
})
