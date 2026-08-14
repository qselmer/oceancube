if (!exists("ocean_cube", mode = "function")) {
  pkgload::load_all(".", quiet = TRUE)
}

aggregate_test_cube <- function(time, values, lon = -80, lat = -12,
                                depth = 0, vars = "value", units = "unit") {
  shape <- c(length(lon), length(lat), length(depth), length(time), length(vars))
  ocean_cube(
    lon = lon,
    lat = lat,
    depth = depth,
    time = time,
    data = array(values, dim = shape),
    vars = vars,
    units = units,
    provenance = list(provider = "aggregate-test")
  )
}

test_that("cube_aggregate_time exposes the exact controlled API", {
  expect_true("cube_aggregate_time" %in% getNamespaceExports("oceancube"))
  expect_identical(
    names(formals(cube_aggregate_time)),
    c("x", "by", "method", "na.rm", "min_n", "diagnostics")
  )
  x <- aggregate_test_cube(as.Date("2020-01-01"), 1)
  expect_error(cube_aggregate_time(x), "`by`")
  expect_error(cube_aggregate_time(list(), "day"), "validation failed")
  for (bad in list(NULL, NA_character_, "d", "DAY", c("day", "week"), 1)) {
    expect_error(cube_aggregate_time(x, bad), "`by`")
  }
  for (bad in list(NA_character_, "average", c("mean", "sum"), mean)) {
    expect_error(cube_aggregate_time(x, "day", bad), "`method`")
  }
  for (bad in list(NA, 1, c(TRUE, FALSE))) {
    expect_error(cube_aggregate_time(x, "day", na.rm = bad), "`na.rm`")
    expect_error(cube_aggregate_time(x, "day", diagnostics = bad), "`diagnostics`")
  }
  for (bad in list(0, -1, NA_real_, Inf, 1.5, c(1, 2))) {
    expect_error(cube_aggregate_time(x, "day", min_n = bad), "`min_n`")
  }
})

test_that("monthly Date means and regularized gaps match hand calculations", {
  x <- aggregate_test_cube(
    as.Date(c("2020-01-01", "2020-01-15", "2020-02-01")),
    c(1, 3, 10)
  )
  result <- suppressWarnings(cube_aggregate_time(x, "month"))

  expect_s3_class(result, "ocean_cube")
  expect_s3_class(result$time, "Date")
  expect_identical(result$time, as.Date(c("2020-01-01", "2020-02-01")))
  expect_equal(as.vector(result$data), c(2, 10))

  gapped <- aggregate_test_cube(
    as.Date(c("2020-01-20", "2020-01-31", "2020-03-15")),
    c(2, 4, 10)
  )
  regularized <- suppressWarnings(cube_aggregate_time(
    gapped, "month", diagnostics = TRUE
  ))
  expect_identical(
    regularized$time,
    as.Date(c("2020-01-01", "2020-02-01", "2020-03-01"))
  )
  expect_equal(as.vector(regularized$data), c(3, NA, 10))
  expect_identical(
    regularized$qa$temporal_aggregation$periods$n_total,
    c(2L, 0L, 1L)
  )
  expect_identical(
    as.vector(regularized$qa$temporal_aggregation$n_valid),
    c(2L, 0L, 1L)
  )
  expect_equal(
    as.vector(regularized$qa$temporal_aggregation$coverage_fraction),
    c(1, NA, 1)
  )
})

test_that("every frequency regularizes only internal empty periods", {
  cases <- list(
    day = list(
      time = as.Date(c("2020-01-01", "2020-01-03")),
      expected = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
    ),
    week = list(
      time = as.Date(c("2021-01-04", "2021-01-18")),
      expected = as.Date(c("2021-01-04", "2021-01-11", "2021-01-18"))
    ),
    season = list(
      time = as.Date(c("2020-03-15", "2020-09-15")),
      expected = as.Date(c("2020-03-01", "2020-06-01", "2020-09-01"))
    ),
    year = list(
      time = as.Date(c("2020-06-01", "2022-06-01")),
      expected = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01"))
    )
  )
  for (by in names(cases)) {
    x <- aggregate_test_cube(cases[[by]]$time, c(1, 3))
    result <- cube_aggregate_time(x, by, diagnostics = TRUE)
    expect_equal(result$time, cases[[by]]$expected, info = by)
    expect_equal(as.vector(result$data), c(1, NA, 3), info = by)
    expect_identical(
      result$qa$temporal_aggregation$periods$n_total,
      c(1L, 0L, 1L),
      info = by
    )
    expect_identical(
      as.vector(result$qa$temporal_aggregation$n_valid),
      c(1L, 0L, 1L),
      info = by
    )
    expect_equal(
      as.vector(result$qa$temporal_aggregation$coverage_fraction),
      c(1, NA, 1),
      info = by
    )
  }
})

test_that("POSIXct daily aggregation uses UTC midnight and preserves class", {
  x <- aggregate_test_cube(
    as.POSIXct(
      c(
        "2020-01-01 00:00:00", "2020-01-01 12:00:00",
        "2020-01-01 18:00:00"
      ),
      tz = "UTC"
    ),
    c(1, 3, 8)
  )
  daily <- suppressWarnings(cube_aggregate_time(x, "day"))
  expect_s3_class(daily$time, "POSIXct")
  expect_identical(attr(daily$time, "tzone"), "UTC")
  expect_identical(
    daily$time,
    as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  )
  expect_equal(as.vector(daily$data), 4)

  crossing <- aggregate_test_cube(
    as.POSIXct(
      c("2020-01-01 23:30:00", "2020-01-02 00:30:00"),
      tz = "UTC"
    ),
    c(2, 6)
  )
  crossed <- cube_aggregate_time(crossing, "day")
  expect_identical(
    crossed$time,
    as.POSIXct(
      c("2020-01-01 00:00:00", "2020-01-02 00:00:00"),
      tz = "UTC"
    )
  )
  expect_equal(as.vector(crossed$data), c(2, 6))
})

test_that("ISO weeks use portable Monday and Thursday rules across years", {
  x <- aggregate_test_cube(
    as.Date(c("2020-12-28", "2021-01-03", "2021-01-04")),
    c(1, 7, 9)
  )
  result <- suppressWarnings(cube_aggregate_time(x, "week"))
  periods <- result$qa$temporal_aggregation$periods

  expect_identical(result$time, as.Date(c("2020-12-28", "2021-01-04")))
  expect_equal(as.vector(result$data), c(4, 9))
  expect_identical(periods$iso_year, c(2020L, 2021L))
  expect_identical(periods$iso_week, c(53L, 1L))
  expect_true(all(.temporal_monday_offset(result$time) == 0L))
})

test_that("meteorological seasons and DJF year are explicit", {
  djf <- aggregate_test_cube(
    as.Date(c("2025-12-15", "2026-01-15", "2026-02-15")),
    c(3, 6, 9)
  )
  result <- cube_aggregate_time(djf, "season")
  periods <- result$qa$temporal_aggregation$periods
  expect_identical(result$time, as.Date("2025-12-01"))
  expect_equal(as.vector(result$data), 6)
  expect_identical(periods$season, "DJF")
  expect_identical(periods$season_year, 2026L)

  all_seasons <- aggregate_test_cube(
    as.Date(c("2026-03-15", "2026-06-15", "2026-09-15", "2026-12-15")),
    1:4
  )
  seasonal <- suppressWarnings(cube_aggregate_time(all_seasons, "season"))
  expect_identical(
    seasonal$qa$temporal_aggregation$periods$season,
    c("MAM", "JJA", "SON", "DJF")
  )
  expect_identical(
    seasonal$qa$temporal_aggregation$periods$season_year,
    c(2026L, 2026L, 2026L, 2027L)
  )
})

test_that("calendar years use January 1 without annual_index", {
  x <- aggregate_test_cube(
    as.Date(c("2020-12-31", "2021-01-01", "2021-12-31")),
    c(2, 4, 8)
  )
  result <- suppressWarnings(cube_aggregate_time(x, "year"))
  expect_identical(result$time, as.Date(c("2020-01-01", "2021-01-01")))
  expect_equal(as.vector(result$data), c(2, 6))
  expect_identical(
    result$qa$temporal_aggregation$periods$year,
    c(2020L, 2021L)
  )
})

test_that("all controlled reducers match manual values", {
  x <- aggregate_test_cube(as.Date("2020-01-01") + 0:2, c(1, 2, 9))
  expected <- c(mean = 4, sum = 12, min = 1, max = 9, median = 2)
  for (method in names(expected)) {
    if (identical(method, "sum")) {
      expect_warning(
        result <- cube_aggregate_time(x, "month", method = method),
        "sum of sampled finite values"
      )
    } else {
      result <- cube_aggregate_time(x, "month", method = method)
    }
    expect_equal(as.vector(result$data), unname(expected[[method]]), info = method)
    expect_identical(result$units, x$units)
  }
  summed <- suppressWarnings(cube_aggregate_time(x, "month", method = "sum"))
  expect_identical(
    summed$provenance$cube_aggregate_time$unit_semantics,
    "sampled_value_sum"
  )
})

test_that("finite-value, na.rm, and min_n policies never emit non-finite results", {
  time <- as.Date("2020-01-01") + 0:3
  partial <- aggregate_test_cube(time, c(1, NA, 3, NaN))
  removed <- cube_aggregate_time(partial, "month", diagnostics = TRUE)
  retained <- cube_aggregate_time(
    partial, "month", na.rm = FALSE, diagnostics = TRUE
  )
  expect_equal(as.vector(removed$data), 2)
  expect_true(is.na(retained$data[[1L]]))
  expect_identical(
    as.vector(removed$qa$temporal_aggregation$n_valid),
    2L
  )
  expect_equal(
    as.vector(removed$qa$temporal_aggregation$coverage_fraction),
    0.5
  )

  invalid <- aggregate_test_cube(time, c(NA, NaN, Inf, -Inf))
  all_missing <- cube_aggregate_time(invalid, "month", diagnostics = TRUE)
  expect_true(is.na(all_missing$data[[1L]]))
  expect_identical(
    as.vector(all_missing$qa$temporal_aggregation$n_valid),
    0L
  )
  expect_equal(
    as.vector(all_missing$qa$temporal_aggregation$coverage_fraction),
    0
  )
  expect_false(any(is.nan(all_missing$data) | is.infinite(all_missing$data)))

  enough <- cube_aggregate_time(partial, "month", min_n = 2L)
  insufficient <- cube_aggregate_time(partial, "month", min_n = 3L)
  expect_equal(as.vector(enough$data), 2)
  expect_true(is.na(insufficient$data[[1L]]))
  expect_length(insufficient$time, 1L)
})

test_that("diagnostics are lightweight by default and aligned when requested", {
  x <- aggregate_test_cube(as.Date("2020-01-01") + 0:2, c(1, NA, 3))
  light <- cube_aggregate_time(x, "month")
  full <- cube_aggregate_time(x, "month", diagnostics = TRUE)

  expect_true(is.list(light$qa$temporal_aggregation))
  expect_false("n_valid" %in% names(light$qa$temporal_aggregation))
  expect_false("coverage_fraction" %in% names(light$qa$temporal_aggregation))
  expect_identical(
    dim(full$qa$temporal_aggregation$n_valid),
    dim(full$data)
  )
  expect_identical(
    dim(full$qa$temporal_aggregation$coverage_fraction),
    dim(full$data)
  )
  expect_identical(
    full$qa$temporal_aggregation$periods$time,
    full$time
  )
  expect_identical(full$qa$temporal_aggregation$periods$n_total, 3L)
})

test_that("irregular sampling warns once and records equal weighting", {
  x <- aggregate_test_cube(
    as.Date(c("2020-01-01", "2020-01-02", "2020-01-04")),
    c(0, 10, 20)
  )
  messages <- character()
  result <- withCallingHandlers(
    cube_aggregate_time(x, "month"),
    warning = function(condition) {
      messages <<- c(messages, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(messages, 1L)
  expect_match(messages, "equal observation weighting")
  expect_equal(as.vector(result$data), 10)
  expect_true(result$qa$temporal_aggregation$irregular_sampling)
  expect_identical(result$provenance$cube_aggregate_time$weighting, "equal")

  regular <- aggregate_test_cube(as.Date("2020-01-01") + 0:2, 1:3)
  regular_result <- expect_no_warning(cube_aggregate_time(regular, "month"))
  expect_false(regular_result$qa$temporal_aggregation$irregular_sampling)
})

test_that("all non-time dimensions, units, and input remain unchanged", {
  lon <- c(-80, -79.25, -77)
  lat <- c(-12, -10.5)
  depth <- c(0, 25)
  vars <- c("temperature", "oxygen")
  time <- as.Date("2020-01-01") + 0:1
  values <- seq_len(prod(lengths(list(lon, lat, depth, time, vars))))
  x <- aggregate_test_cube(
    time, values, lon, lat, depth, vars,
    units = c(temperature = "degC", oxygen = "mmol m-3")
  )
  before <- x
  result <- cube_aggregate_time(x, "month")

  expect_identical(x, before)
  expect_identical(result$lon, lon)
  expect_identical(result$lat, lat)
  expect_identical(result$depth, depth)
  expect_identical(result$vars, vars)
  expect_identical(result$units, x$units)
  expect_identical(unname(dim(result$data)), c(3L, 2L, 2L, 1L, 2L))

  surface <- aggregate_test_cube(time, 1:2, depth = NA_real_)
  surface_result <- cube_aggregate_time(surface, "month")
  expect_identical(surface_result$depth, NA_real_)
  expect_identical(unname(dim(surface_result$data)), c(1L, 1L, 1L, 1L, 1L))
})

test_that("provenance records the frozen aggregation contract", {
  x <- aggregate_test_cube(as.Date("2020-01-01") + 0:1, c(1, 3))
  result <- cube_aggregate_time(x, "month", min_n = 2L, diagnostics = TRUE)
  record <- result$provenance$cube_aggregate_time

  expect_identical(record$operation, "temporal_aggregation")
  expect_identical(record$by, "month")
  expect_identical(record$method, "mean")
  expect_true(record$na.rm)
  expect_identical(record$min_n, 2L)
  expect_identical(record$weighting, "equal")
  expect_identical(record$calendar, "proleptic_gregorian")
  expect_identical(record$input_time_class, "Date")
  expect_identical(record$output_time_class, "Date")
  expect_match(record$period_definition, "calendar year and month")
  expect_identical(result$provenance$parent, x$provenance)
})

test_that("to_month delegates built-ins and preserves the compatibility surface", {
  x <- aggregate_test_cube(as.Date("2020-01-01") + 0:2, c(1, 2, 9))
  expected <- c(mean = 4, sum = 12, min = 1, max = 9, median = 2)
  functions <- list(mean, sum, min, max, stats::median)
  names(functions) <- names(expected)

  for (method in names(functions)) {
    if (identical(method, "sum")) {
      expect_warning(
        result <- to_month(x, functions[[method]]),
        "sampled finite"
      )
    } else {
      result <- to_month(x, functions[[method]])
    }
    expect_equal(as.vector(result$data), unname(expected[[method]]), info = method)
    expect_true(result$qa$to_month$core_delegated)
    expect_false(result$qa$temporal_aggregation$read_metrics$full_cube_materialized)
    expect_s3_class(result$time, "Date")
  }
  expect_identical(names(formals(to_month)), c("x", "fun"))
})

test_that("to_month keeps the warned POSIXct Date exception", {
  x <- aggregate_test_cube(
    as.POSIXct(
      c("2020-01-01 01:00:00", "2020-01-02 01:00:00"),
      tz = "UTC"
    ),
    c(1, 3)
  )
  expect_warning(
    result <- to_month(x),
    "legacy POSIXct-to-Date"
  )
  expect_s3_class(result$time, "Date")
  expect_equal(as.vector(result$data), 2)
  expect_true(result$qa$to_month$legacy_posixct_date_demotion)
  expect_identical(
    result$provenance$extra$compatibility$legacy_posixct_date_demotion,
    TRUE
  )
  expect_no_error(cube_validate(result, strict = TRUE))
})

test_that("to_month custom functions use the deprecated legacy path", {
  x <- aggregate_test_cube(as.Date("2020-01-01") + 0:2, c(1, 2, 9))
  spread <- function(z, na.rm = FALSE) diff(range(z, na.rm = na.rm))
  expect_warning(
    result <- to_month(x, spread),
    "Arbitrary functions"
  )
  expect_equal(as.vector(result$data), 8)
  expect_identical(result$qa$to_month$path, "legacy_custom")
  expect_true(result$qa$to_month$full_cube_materialized)
  expect_true(result$provenance$extra$compatibility$deprecated)
})

test_that("lazy NetCDF aggregation stays selective and matches memory", {
  skip_if_not_installed("ncdf4")
  file <- make_netcdf_backend_fixture(
    time_units = "days since 2019-12-15 00:00:00",
    time_values = c(0, 31, 122, 397)
  )
  withr::local_file(file)
  storage <- .new_netcdf_storage(
    file,
    variables = c("temperature", "oxygen")
  )
  lazy <- .new_netcdf_cube(storage)
  memory <- cube_collect(lazy)
  original_read <- .cube_read
  netcdf_indices <- list()
  local_mocked_bindings(
    .cube_read = function(x, index = NULL, drop = FALSE) {
      if (identical(.cube_backend(x), "netcdf")) {
        netcdf_indices <<- c(netcdf_indices, list(index))
      }
      original_read(x, index = index, drop = drop)
    },
    .package = "oceancube"
  )

  workflows <- list(
    month_mean = c("month", "mean"),
    season_mean = c("season", "mean"),
    year_mean = c("year", "mean"),
    month_median = c("month", "median")
  )
  for (name in names(workflows)) {
    by <- workflows[[name]][[1L]]
    method <- workflows[[name]][[2L]]
    from_lazy <- suppressWarnings(cube_aggregate_time(
      lazy, by, method = method, diagnostics = TRUE
    ))
    from_memory <- suppressWarnings(cube_aggregate_time(
      memory, by, method = method, diagnostics = TRUE
    ))
    expect_equal(from_lazy$data, from_memory$data, info = name)
    expect_identical(from_lazy$time, from_memory$time, info = name)
    expect_identical(from_lazy$lon, from_memory$lon, info = name)
    expect_identical(from_lazy$lat, from_memory$lat, info = name)
    expect_identical(from_lazy$depth, from_memory$depth, info = name)
    expect_identical(from_lazy$vars, from_memory$vars, info = name)
    expect_identical(from_lazy$units, from_memory$units, info = name)
    expect_identical(
      from_lazy$qa$temporal_aggregation$periods,
      from_memory$qa$temporal_aggregation$periods,
      info = name
    )
    expect_identical(
      from_lazy$qa$temporal_aggregation$n_valid,
      from_memory$qa$temporal_aggregation$n_valid,
      info = name
    )
    expect_equal(
      from_lazy$qa$temporal_aggregation$coverage_fraction,
      from_memory$qa$temporal_aggregation$coverage_fraction,
      info = name
    )
    metrics <- from_lazy$qa$temporal_aggregation$read_metrics
    expect_false(metrics$full_cube_materialized, info = name)
    expect_identical(metrics$logical_values_selected, 96, info = name)
    expect_identical(metrics$physical_values_read, 96, info = name)
    expect_gt(metrics$backend_read_count, 1L)
    expect_identical(
      from_lazy$provenance$cube_aggregate_time[c(
        "operation", "by", "method", "weighting", "calendar",
        "input_time_class", "output_time_class"
      )],
      from_memory$provenance$cube_aggregate_time[c(
        "operation", "by", "method", "weighting", "calendar",
        "input_time_class", "output_time_class"
      )],
      info = name
    )
  }
  expect_true(length(netcdf_indices) > 0L)
  expect_true(all(vapply(netcdf_indices, function(index) !is.null(index), logical(1))))
})
