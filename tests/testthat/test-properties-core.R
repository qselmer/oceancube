test_that("P1 full crop and P2 full slice preserve generated memory cubes", {
  cases <- list(
    date = a2_make_cube(),
    posix_irregular = a2_make_cube(time_class = "POSIXct", irregular_time = TRUE),
    singleton_variable = a2_make_cube(variable_n = 1L),
    structured_na = a2_make_cube(missingness = "boundary")
  )

  for (name in names(cases)) {
    cube <- cases[[name]]
    before <- a2_scientific_snapshot(cube)
    cropped <- a2_full_crop(cube)
    sliced <- a2_full_slice(cube)

    a2_expect_science_equal(cropped, cube)
    a2_expect_science_equal(sliced, cube)
    a2_expect_source_unchanged(cube, before)
    expect_true(
      a2_provenance_contains(cropped$provenance, "a2-deterministic-generator"),
      info = name
    )
    expect_true(
      a2_provenance_contains(sliced$provenance, "a2-deterministic-generator"),
      info = name
    )
  }
})

test_that("P1 and P2 are exact for equivalent NetCDF and memory sources", {
  skip_if_not_installed("ncdf4")
  pair <- a2_make_netcdf_pair()
  withr::local_file(pair$file)
  netcdf_before <- a2_scientific_snapshot(pair$netcdf)

  memory_crop <- a2_full_crop(pair$memory)
  netcdf_crop <- a2_full_crop(pair$netcdf)
  memory_slice <- a2_full_slice(pair$memory)
  netcdf_slice <- a2_full_slice(pair$netcdf)

  a2_expect_science_equal(memory_crop, pair$memory)
  a2_expect_science_equal(netcdf_crop, pair$memory)
  a2_expect_science_equal(memory_slice, pair$memory)
  a2_expect_science_equal(netcdf_slice, pair$memory)
  a2_expect_source_unchanged(pair$netcdf, netcdf_before)
})

test_that("P3 collect preserves canonical axes and backend-equivalent values", {
  memory <- a2_make_cube(time_class = "POSIXct", irregular_time = TRUE)
  expect_identical(cube_collect(memory), memory)
  expect_identical(names(dimnames(memory$data)), c("lon", "lat", "depth", "time", "var"))

  skip_if_not_installed("ncdf4")
  pair <- a2_make_netcdf_pair()
  withr::local_file(pair$file)
  collected <- cube_collect(pair$netcdf)

  a2_expect_science_equal(collected, pair$memory)
  expect_identical(
    names(dimnames(collected$data)),
    c("longitude", "latitude", "depth", "time", "variable")
  )
  expect_identical(.cube_backend(collected), "memory")
})

test_that("P4 representative operations do not mutate their source", {
  monthly_time <- seq(as.Date("2020-01-01"), by = "month", length.out = 24L)
  cube <- a2_make_cube(
    time_n = length(monthly_time), time = monthly_time,
    variable_n = 1L, field = "linear", slope = 0.25
  )
  climatology <- suppressWarnings(cube_climatology(cube, by = "month"))
  path <- data.frame(
    longitude = cube$lon[c(1L, length(cube$lon))],
    latitude = cube$lat[c(1L, length(cube$lat))]
  )
  operations <- list(
    slice = function() cube_slice(cube, time = cube$time[1:6]),
    crop = function() cube_crop(cube, longitude = range(cube$lon[1:2])),
    extract = function() cube_extract(
      cube, longitude = cube$lon[[1L]], latitude = cube$lat[[1L]],
      depth = cube$depth[[1L]], time = cube$time[[1L]],
      variable = cube$vars[[1L]]
    ),
    transect = function() cube_transect(
      cube, path, depth = cube$depth, time = cube$time[[1L]],
      variable = cube$vars[[1L]], match = "exact", mode = "section"
    ),
    aggregate = function() suppressWarnings(cube_aggregate_time(cube, by = "year")),
    climatology = function() suppressWarnings(cube_climatology(cube, by = "month")),
    anomaly = function() cube_anomaly(cube, climatology, type = "difference"),
    trend = function() cube_trend(cube, time_unit = "day")
  )

  for (name in names(operations)) {
    before <- a2_scientific_snapshot(cube)
    expect_no_error(operations[[name]]())
    a2_expect_source_unchanged(cube, before)
  }
})

test_that("P5 untouched dimensions and P6 units remain invariant", {
  cube <- a2_make_cube(time_class = "POSIXct", irregular_time = TRUE)
  spatial <- cube_crop(cube, longitude = range(cube$lon[1:2]))
  temporal <- cube_slice(cube, time = cube$time[2:4])
  variable <- cube_slice(cube, variable = cube$vars[[2L]])
  extracted <- cube_extract(
    cube, longitude = cube$lon[[1L]], latitude = cube$lat[[1L]],
    depth = cube$depth[[1L]], time = cube$time[[1L]],
    variable = cube$vars[[2L]]
  )
  path <- data.frame(
    longitude = cube$lon[1:2], latitude = cube$lat[1:2]
  )
  transect <- cube_transect(
    cube, path, depth = cube$depth, time = cube$time[[1L]],
    variable = cube$vars[[2L]], match = "exact", mode = "section"
  )

  expect_equal(as.numeric(spatial$time), as.numeric(cube$time), tolerance = 0)
  expect_identical(class(spatial$time), class(cube$time))
  expect_identical(spatial$units, cube$units)
  expect_identical(temporal$lon, cube$lon)
  expect_identical(temporal$lat, cube$lat)
  expect_identical(temporal$depth, cube$depth)
  expect_identical(temporal$units, cube$units)
  expect_identical(variable$lon, cube$lon)
  expect_identical(variable$lat, cube$lat)
  expect_identical(variable$depth, cube$depth)
  expect_equal(as.numeric(variable$time), as.numeric(cube$time), tolerance = 0)
  expect_identical(variable$units, cube$units[2L])
  expect_identical(extracted$unit, unname(cube$units[[2L]]))
  expect_identical(attr(transect, "units"), cube$units[2L])
})

test_that("singleton dimensions have explicit outcomes across critical operations", {
  cases <- list(
    longitude = a2_make_cube(longitude_n = 1L),
    latitude = a2_make_cube(latitude_n = 1L),
    depth = a2_make_cube(depth_n = 1L),
    time = a2_make_cube(time_n = 1L),
    variable = a2_make_cube(variable_n = 1L)
  )

  for (name in names(cases)) {
    cube <- cases[[name]]
    expect_false(any(cube_validate(cube)$status == "FAIL"), info = name)
    expect_s3_class(cube_inspect(cube, missing = "none"), "ocean_cube_inspection")
    expect_s3_class(a2_full_slice(cube), "ocean_cube")
    expect_s3_class(a2_full_crop(cube), "ocean_cube")
    expect_s3_class(cube_extract(cube), "data.frame")
    expect_identical(cube_collect(cube), cube, info = name)
    expect_s3_class(
      suppressWarnings(cube_aggregate_time(cube, by = "year")),
      "ocean_cube"
    )
    climatology <- suppressWarnings(cube_climatology(cube, by = "month"))
    expect_s3_class(climatology, "ocean_cube")
    expect_s3_class(cube_anomaly(cube, climatology), "ocean_cube")
    trend <- cube_trend(cube, time_unit = "day")
    expect_s3_class(trend, "ocean_cube")
    if (identical(name, "time")) {
      expect_true(all(is.na(trend$data)), info = "singleton time is insufficient for trend")
    }
  }
})

test_that("structured missingness is deterministic and preserved by pure operations", {
  patterns <- c(
    "none", "single", "profile", "time_point", "cell_time",
    "alternating", "boundary"
  )
  for (pattern in patterns) {
    first <- a2_make_cube(missingness = pattern)
    second <- a2_make_cube(missingness = pattern)
    expect_identical(first$data, second$data, info = pattern)
    expect_false(any(cube_validate(first)$status == "FAIL"), info = pattern)
    expect_identical(cube_collect(first), first, info = pattern)
    a2_expect_science_equal(a2_full_slice(first), first)
    a2_expect_science_equal(a2_full_crop(first), first)
  }
})

test_that("structured NA temporal contracts distinguish na.rm behavior", {
  cube <- a2_make_cube(time_n = 4L, variable_n = 1L, missingness = "single")
  removed <- cube_aggregate_time(cube, by = "year", na.rm = TRUE)
  retained <- cube_aggregate_time(cube, by = "year", na.rm = FALSE)

  expect_true(is.finite(removed$data[1, 1, 1, 1, 1]))
  expect_true(is.na(retained$data[1, 1, 1, 1, 1]))

  cell_missing <- a2_make_cube(
    time_n = 4L, variable_n = 1L, missingness = "cell_time"
  )
  trend <- cube_trend(cell_missing, time_unit = "day")
  expect_true(is.na(trend$data[1, 1, 1, 1, 1]))
  expect_true(is.finite(trend$data[2, 1, 1, 1, 1]))
})
