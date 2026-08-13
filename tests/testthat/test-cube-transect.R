test_that("path pairs, long order, modes, and distances are deterministic", {
  fixture <- .make_baseline_fixture()
  x <- fixture$cube
  path <- data.frame(
    station = c("A", "B", "C"),
    longitude = c(-80, -79, -78),
    latitude = c(-12, -11, -12)
  )
  before <- path
  result <- cube_transect(
    x, path, id_col = "station", depth = c(0, 50),
    time = as.Date("2021-02-01"),
    variable = c("temperature", "oxygen"),
    match = "exact", mode = "section", keep_index = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_false(inherits(result, "ocean_cube"))
  expect_identical(path, before)
  expect_equal(nrow(result), 12L)
  expect_identical(
    result[c("point_order", "depth_index", "variable_index")],
    data.frame(
      point_order = rep(1:3, each = 4),
      depth_index = rep(rep(1:2, each = 2), 3),
      variable_index = rep(1:2, 6)
    )
  )
  expected <- numeric(12)
  cursor <- 1L
  for (point in 1:3) {
    latitude <- c(1L, 2L, 1L)[point]
    for (depth in 1:2) {
      for (variable in 1:2) {
        expected[cursor] <- 10000 * variable + 4000 +
          100 * depth + 10 * latitude + point
        cursor <- cursor + 1L
      }
    }
  }
  expect_equal(result$value, expected)
  expect_equal(result$requested_distance_km[[1L]], 0)
  expect_equal(result$matched_distance_km[[1L]], 0)
  expect_true(all(diff(unique(result$requested_distance_km)) > 0))
  expect_identical(attr(result, "oceancube_mode"), "section")
  expect_identical(attr(result, "units"), c(
    temperature = "degC", oxygen = "mmol m-3"
  ))
})

test_that("spatial selectors remain pairs and repeated cells are preserved", {
  x <- .make_baseline_fixture()$cube
  path <- data.frame(
    id = c("N1", "N2"),
    longitude = c(-79.4, -79.2),
    latitude = c(-11.2, -11.1)
  )
  result <- cube_transect(
    x, path, id_col = "id", depth = 40,
    time = as.Date("2021-01-10"), variable = "temperature",
    match = "nearest",
    tolerance = list(
      longitude = 0.5, latitude = 0.5, depth = 15,
      time = as.difftime(15, units = "days")
    ),
    mode = "horizontal", keep_index = TRUE
  )
  expect_equal(nrow(result), 2L)
  expect_identical(result$point_order, 1:2)
  expect_identical(result$longitude_index, c(2L, 2L))
  expect_identical(result$latitude_index, c(2L, 2L))
  expect_equal(result$value[[1L]], result$value[[2L]])
  expect_gt(result$requested_distance_km[[2L]], 0)
  expect_equal(result$matched_distance_km[[2L]], 0)
  expect_true(all(result$match_distance_km >= 0))
  metrics <- attr(result, "oceancube_provenance")$physical_reads
  expect_equal(metrics$n_points, 2L)
  expect_equal(metrics$n_unique_pairs, 1L)
  expect_equal(metrics$n_values_requested, 2L)
})

test_that("index paths use cube coordinates and preserve exhaustive order", {
  x <- .make_baseline_fixture()$cube
  path <- data.frame(longitude = c(3L, 1L, 2L), latitude = c(2L, 1L, 2L))
  result <- cube_transect(
    x, path, depth = c(2L, 1L), time = 4L,
    variable = c(2L, 1L), by = "index", mode = "section",
    keep_index = TRUE
  )
  expect_identical(
    result$longitude_requested[!duplicated(result$point_order)],
    x$lon[c(3L, 1L, 2L)]
  )
  expect_identical(
    result$latitude_requested[!duplicated(result$point_order)],
    x$lat[c(2L, 1L, 2L)]
  )
  for (row in seq_len(nrow(result))) {
    expect_equal(
      result$value[[row]],
      x$data[
        result$longitude_index[[row]],
        result$latitude_index[[row]],
        result$depth_index[[row]],
        result$time_index[[row]],
        result$variable_index[[row]]
      ]
    )
  }
})

test_that("profile, horizontal, section, and auto validate shape", {
  x <- .make_baseline_fixture()$cube
  one <- data.frame(longitude = -80, latitude = -12)
  two <- data.frame(longitude = c(-80, -79), latitude = c(-12, -11))
  profile <- cube_transect(
    x, one, time = x$time[[1L]], match = "exact", mode = "auto"
  )
  horizontal <- cube_transect(
    x, two, depth = 0, time = x$time[[1L]], match = "exact", mode = "auto"
  )
  section <- cube_transect(
    x, two, time = x$time[[1L]], match = "exact", mode = "auto"
  )
  expect_identical(attr(profile, "oceancube_mode"), "profile")
  expect_identical(attr(horizontal, "oceancube_mode"), "horizontal")
  expect_identical(attr(section, "oceancube_mode"), "section")
  expect_error(
    cube_transect(
      x, two, time = x$time[[1L]], match = "exact", mode = "profile"
    ),
    "incompatible"
  )
  expect_error(cube_transect(x, one, match = "exact"), "exactly one")
})

test_that("wide format preserves scientific names and rejects duplicates", {
  fixture <- .make_baseline_fixture()
  x <- fixture$cube
  x$vars[[1L]] <- "sea temperature"
  names(x$units)[[1L]] <- "sea temperature"
  path <- data.frame(longitude = c(-80, -79), latitude = c(-12, -11))
  wide <- cube_transect(
    x, path, depth = c(0, 50), time = x$time[[1L]],
    variable = c("oxygen", "sea temperature"), match = "exact",
    format = "wide", mode = "section"
  )
  expect_equal(nrow(wide), 4L)
  expect_true(all(c("oxygen", "sea temperature") %in% names(wide)))
  expect_identical(
    names(attr(wide, "units")),
    c("oxygen", "sea temperature")
  )
  expect_error(
    cube_transect(
      x, path, depth = 0, time = x$time[[1L]],
      variable = c("oxygen", "oxygen"), match = "exact",
      format = "wide"
    ),
    "duplicate variables"
  )
})

test_that("invalid paths, geography, selectors, and antimeridian fail early", {
  x <- .make_baseline_fixture()$cube
  expect_error(cube_transect(x, data.frame()), "at least one")
  expect_error(cube_transect(x, list(longitude = -80)), "data frame")
  expect_error(
    cube_transect(x, data.frame(longitude = NA_real_, latitude = -12)),
    "point_order 1"
  )
  expect_error(
    cube_transect(x, data.frame(longitude = -80, latitude = 91)),
    "point_order 1"
  )
  expect_error(
    cube_transect(
      x,
      data.frame(
        longitude = c(170, -170),
        latitude = c(-12, -12)
      )
    ),
    "outside the cube domain|antimeridian"
  )
  expect_error(
    cube_transect(
      x, data.frame(longitude = -80, latitude = -12),
      time = x$time[[1L]], by = "index"
    ),
    "whole-number|positions"
  )
})

test_that("matrix paths and duplicated factor identifiers are safe", {
  x <- .make_baseline_fixture()$cube
  matrix_path <- cbind(longitude = c(-80, -79), latitude = c(-12, -11))
  result <- cube_transect(
    x, matrix_path, depth = 0, time = x$time[[1L]],
    match = "exact"
  )
  expect_equal(nrow(result), 4L)
  factor_path <- data.frame(
    station = factor(c("A", "A")),
    longitude = c(-80, -79),
    latitude = c(-12, -11)
  )
  factor_result <- cube_transect(
    x, factor_path, id_col = "station", depth = 0,
    time = x$time[[1L]], variable = "temperature", match = "exact"
  )
  expect_identical(factor_result$point_id, c("A", "A"))
})

test_that("valid cells containing NA remain represented", {
  fixture <- .make_baseline_fixture()
  fixture$cube$data[1, 1, 1, 1, 1] <- NA_real_
  result <- cube_transect(
    fixture$cube,
    data.frame(longitude = -80, latitude = -12),
    depth = 0, time = fixture$time[[1L]], variable = "temperature",
    match = "exact"
  )
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$value))
})

test_that("NetCDF pair reads use one connection and equal memory", {
  skip_if_not_installed("ncdf4")
  file <- make_netcdf_backend_fixture()
  on.exit(unlink(file), add = TRUE)
  x_netcdf <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )
  x_memory <- cube_collect(x_netcdf)
  path <- data.frame(
    station = c("A", "B", "B2"),
    longitude = c(-80, -79, -79),
    latitude = c(-12, -11, -11)
  )
  args <- list(
    path = path, id_col = "station", depth = c(50, 0),
    time = x_netcdf$time[[4L]], variable = c("oxygen", "temperature"),
    match = "exact", mode = "section", keep_index = TRUE
  )
  memory <- do.call(cube_transect, c(list(x = x_memory), args))
  netcdf <- do.call(cube_transect, c(list(x = x_netcdf), args))
  netcdf_data <- as.data.frame(netcdf)
  memory_data <- as.data.frame(memory)
  attributes(netcdf_data) <- attributes(netcdf_data)[c(
    "names", "row.names", "class"
  )]
  attributes(memory_data) <- attributes(memory_data)[c(
    "names", "row.names", "class"
  )]
  expect_equal(netcdf_data, memory_data)
  metrics <- attr(netcdf, "oceancube_provenance")$physical_reads
  expect_equal(metrics$n_open, 1L)
  expect_equal(metrics$n_unique_pairs, 2L)
  expect_equal(metrics$n_ncvar_get, 4L)
  expect_equal(metrics$n_values_read, 8L)
  expect_true(file.exists(file))

  compare_materialized <- function(call_args) {
    from_memory <- do.call(
      cube_transect, c(list(x = x_memory), call_args)
    )
    from_netcdf <- do.call(
      cube_transect, c(list(x = x_netcdf), call_args)
    )
    attributes(from_memory) <- attributes(from_memory)[c(
      "names", "row.names", "class"
    )]
    attributes(from_netcdf) <- attributes(from_netcdf)[c(
      "names", "row.names", "class"
    )]
    expect_equal(from_netcdf, from_memory)
  }
  compare_materialized(list(
    path = path[1, ], id_col = "station", time = x_netcdf$time[[2L]],
    variable = c("temperature", "oxygen"), match = "exact",
    mode = "profile"
  ))
  compare_materialized(list(
    path = path, id_col = "station", depth = 0,
    time = x_netcdf$time[[2L]], variable = "oxygen", match = "exact",
    mode = "horizontal"
  ))
  compare_materialized(list(
    path = transform(
      path,
      longitude = longitude + c(0.2, -0.2, -0.2),
      latitude = latitude + c(0.2, -0.1, -0.1)
    ),
    id_col = "station", depth = 40, time = x_netcdf$time[[2L]],
    variable = "temperature", match = "nearest",
    tolerance = list(
      longitude = 0.3, latitude = 0.3, depth = 15
    ),
    mode = "horizontal"
  ))
  compare_materialized(list(
    path = path, id_col = "station", depth = c(0, 50),
    time = x_netcdf$time[[2L]],
    variable = c("temperature", "oxygen"), match = "exact",
    mode = "section", format = "wide", keep_index = TRUE
  ))

  materialized <- netcdf
  unlink(file)
  expect_silent(summary(materialized))
  expect_error(do.call(cube_transect, c(list(x = x_netcdf), args)))
})

test_that("the public 0.2 validation gate and signature are preserved", {
  x <- .make_baseline_fixture()$cube
  path <- data.frame(longitude = c(-80, -79), latitude = c(-12, -11))
  calls <- 0L
  original <- cube_validate
  local_mocked_bindings(
    cube_validate = function(x, strict = FALSE) {
      calls <<- calls + 1L
      expect_true(strict)
      original(x, strict = strict)
    },
    .package = "oceancube"
  )
  result <- cube_transect(
    x, path, depth = 0, time = x$time[[1L]], variable = "temperature",
    match = "exact", mode = "horizontal"
  )
  expect_identical(calls, 1L)
  expect_identical(
    names(formals(cube_transect)),
    c(
      "x", "path", "lon_col", "lat_col", "id_col", "depth", "time",
      "variable", "by", "match", "tolerance", "mode", "format",
      "keep_index"
    )
  )
  expect_s3_class(result, "data.frame")
})

test_that("sf and sfc paths are rejected before a CRS can be ignored", {
  skip_if_not_installed("sf")
  x <- .make_baseline_fixture()$cube
  point_sf <- sf::st_as_sf(
    data.frame(longitude = c(-80, -79), latitude = c(-12, -11)),
    coords = c("longitude", "latitude"), crs = 4326
  )
  point_sf$longitude <- c(-80, -79)
  point_sf$latitude <- c(-12, -11)
  candidates <- list(
    point_sf,
    sf::st_set_crs(point_sf, NA),
    sf::st_transform(point_sf, 3857),
    sf::st_sfc(
      sf::st_linestring(matrix(
        c(-80, -12, -79, -11), ncol = 2, byrow = TRUE
      )),
      crs = 4326
    )
  )
  for (candidate in candidates) {
    expect_error(
      cube_transect(
        x, candidate, depth = 0, time = x$time[[1L]],
        variable = "temperature", match = "exact", mode = "horizontal"
      ),
      "does not yet accept sf/sfc paths",
      class = "oceancube_transect_crs_error"
    )
  }
})

test_that("geographic conventions and antimeridian policies are explicit", {
  x <- .make_baseline_fixture()$cube
  expect_error(
    cube_transect(
      x, data.frame(longitude = c(-80, 280), latitude = c(-12, -11)),
      depth = 0, time = x$time[[1L]], variable = "temperature",
      match = "exact", mode = "horizontal"
    ),
    "recognized geographic convention",
    class = "oceancube_transect_crs_error"
  )
  expect_error(
    cube_transect(
      x, data.frame(longitude = c(-80, -79), latitude = c(-12, 91)),
      depth = 0, time = x$time[[1L]], variable = "temperature",
      match = "exact", mode = "horizontal"
    ),
    "outside [-90, 90]",
    fixed = TRUE
  )

  values <- array(seq_len(4), dim = c(2, 2, 1, 1, 1))
  cube_360 <- ocean_cube(
    lon = c(1, 359), lat = c(-1, 1), depth = 0,
    time = as.Date("2021-01-01"), vars = "v", data = values
  )
  expect_error(
    cube_transect(
      cube_360,
      data.frame(longitude = c(359, 1), latitude = c(1, 1)),
      depth = 0, time = cube_360$time, variable = "v",
      match = "exact", mode = "horizontal"
    ),
    "antimeridian",
    class = "oceancube_transect_antimeridian"
  )
  cube_180 <- ocean_cube(
    lon = c(-179, 179), lat = c(-1, 1), depth = 0,
    time = as.Date("2021-01-01"), vars = "v", data = values
  )
  expect_error(
    cube_transect(
      cube_180,
      data.frame(longitude = c(179, -179), latitude = c(1, 1)),
      depth = 0, time = cube_180$time, variable = "v",
      match = "exact", mode = "horizontal"
    ),
    "antimeridian",
    class = "oceancube_transect_antimeridian"
  )
  ordinary_360 <- ocean_cube(
    lon = c(278, 279, 280), lat = c(-12, -11), depth = 0,
    time = as.Date("2021-01-01"), vars = "v",
    data = array(seq_len(6), dim = c(3, 2, 1, 1, 1))
  )
  result <- cube_transect(
    ordinary_360,
    data.frame(longitude = c(278, 280), latitude = c(-12, -11)),
    depth = 0, time = ordinary_360$time, variable = "v",
    match = "exact", mode = "horizontal"
  )
  expect_identical(result$longitude, c(278, 280))
})

test_that("matching warnings make legacy and unrestricted nearest visible", {
  x <- .make_baseline_fixture()$cube
  path <- data.frame(
    longitude = c(-79.8, -79.2), latitude = c(-11.8, -11.2)
  )
  expect_warning(
    legacy <- cube_transect(
      x, path, depth = 0, time = x$time[[1L]],
      variable = "temperature", mode = "horizontal"
    ),
    "legacy implicit nearest",
    class = "oceancube_transect_compat_warning"
  )
  expect_warning(
    unrestricted <- cube_transect(
      x, path, depth = 0, time = x$time[[1L]],
      variable = "temperature", match = "nearest", mode = "horizontal"
    ),
    "no explicit maximum tolerance",
    class = "oceancube_transect_matching_warning"
  )
  expect_no_warning(
    bounded <- cube_transect(
      x, path, depth = 0, time = x$time[[1L]],
      variable = "temperature", match = "nearest",
      tolerance = list(longitude = 0.3, latitude = 0.3),
      mode = "horizontal"
    )
  )
  expect_identical(legacy$value, unrestricted$value)
  expect_identical(unrestricted$value, bounded$value)
  expect_error(
    cube_transect(
      x, path, depth = 0, time = x$time[[1L]],
      variable = "temperature", match = "nearest",
      tolerance = list(longitude = -1), mode = "horizontal"
    ),
    "non-negative"
  )
})

test_that("three Haversine distances have distinct traceable meanings", {
  x <- .make_baseline_fixture()$cube
  exact_path <- data.frame(
    longitude = c(-80, -79, -79), latitude = c(-12, -12, -11)
  )
  exact <- cube_transect(
    x, exact_path, depth = 0, time = x$time[[1L]],
    variable = "temperature", match = "exact", mode = "horizontal"
  )
  expected_east_west <- 108.765141267262
  expected_north_south <- 111.195080233533
  expect_equal(exact$match_distance_km, rep(0, 3), tolerance = 1e-12)
  expect_equal(
    exact$requested_distance_km,
    c(0, expected_east_west, expected_east_west + expected_north_south),
    tolerance = 1e-10
  )
  expect_equal(
    exact$matched_distance_km, exact$requested_distance_km,
    tolerance = 1e-12
  )
  expect_true(all(diff(exact$requested_distance_km) >= 0))

  snapped_path <- data.frame(
    longitude = c(-79.8, -79.2), latitude = c(-12, -11)
  )
  snapped <- cube_transect(
    x, snapped_path, depth = 0, time = x$time[[1L]],
    variable = "temperature", match = "nearest",
    tolerance = list(longitude = 0.3, latitude = 0.1), mode = "horizontal"
  )
  expect_gt(snapped$match_distance_km[[1L]], 0)
  expect_equal(snapped$match_distance_km[[1L]], 21.7530397, tolerance = 1e-7)
  expect_equal(
    attr(snapped, "oceancube_provenance")$maximum_match_distance_km,
    max(snapped$match_distance_km)
  )
})

test_that("irregular and descending axes preserve nearest pair ordering", {
  irregular <- ocean_cube(
    lon = c(-80, -78.2, -74), lat = c(-12.3, -9.7), depth = 0,
    time = as.Date("2021-01-01"), vars = "v",
    data = array(seq_len(6), dim = c(3, 2, 1, 1, 1))
  )
  irregular_result <- cube_transect(
    irregular,
    data.frame(longitude = c(-79, -75), latitude = c(-12, -10)),
    depth = 0, time = irregular$time, variable = "v", match = "nearest",
    tolerance = list(longitude = 1.1, latitude = 0.4),
    mode = "horizontal", keep_index = TRUE
  )
  expect_identical(irregular_result$longitude_index, c(2L, 3L))
  expect_identical(irregular_result$latitude_index, c(1L, 2L))

  descending <- ocean_cube(
    lon = c(-78, -79, -80), lat = c(-11, -12), depth = 0,
    time = as.Date("2021-01-01"), vars = "v",
    data = array(seq_len(6), dim = c(3, 2, 1, 1, 1))
  )
  descending_result <- cube_transect(
    descending,
    data.frame(longitude = c(-79.8, -78.2), latitude = c(-11.8, -11.2)),
    depth = 0, time = descending$time, variable = "v", match = "nearest",
    tolerance = list(longitude = 0.3, latitude = 0.3),
    mode = "horizontal", keep_index = TRUE
  )
  expect_identical(descending_result$longitude_index, c(3L, 1L))
  expect_identical(descending_result$latitude_index, c(2L, 1L))
  expect_identical(descending_result$point_order, 1:2)
})

test_that("ambiguous stored axes and duplicate selectors are rejected", {
  x <- .make_baseline_fixture()$cube
  path <- data.frame(longitude = c(-80, -79), latitude = c(-12, -11))

  duplicate_depth <- x
  duplicate_depth$depth <- c(0, 0)
  expect_error(
    cube_transect(
      duplicate_depth, path, time = x$time[[1L]], variable = "temperature",
      match = "exact", mode = "section"
    ),
    "unique stored depth",
    class = "oceancube_transect_selection_error"
  )
  duplicate_time <- x
  duplicate_time$time[[2L]] <- duplicate_time$time[[1L]]
  expect_error(
    cube_transect(
      duplicate_time, path, depth = 0, time = x$time[[1L]],
      variable = "temperature", match = "exact", mode = "horizontal"
    ),
    "validation failed",
    class = "oceancube_validation_error"
  )
  duplicate_variable <- x
  duplicate_variable$vars <- c("temperature", "temperature")
  expect_error(
    cube_transect(
      duplicate_variable, path, depth = 0, time = x$time[[1L]],
      match = "exact", mode = "horizontal"
    ),
    class = "oceancube_validation_error"
  )

  duplicate_calls <- list(
    list(depth = c(0, 0), time = x$time[[1L]], variable = "temperature"),
    list(depth = 0, time = rep(x$time[[1L]], 2), variable = "temperature"),
    list(
      depth = 0, time = x$time[[1L]],
      variable = c("temperature", "temperature")
    )
  )
  for (selectors in duplicate_calls) {
    expect_error(
      do.call(
        cube_transect,
        c(
          list(x = x, path = path, match = "exact", mode = "horizontal"),
          selectors
        )
      ),
      "Duplicate",
      class = "oceancube_transect_selection_error"
    )
  }
})

test_that("surface, depth, time, and variable selections remain explicit", {
  surface <- ocean_cube(
    lon = c(-80, -79), lat = c(-12, -11), time = as.Date("2021-01-01"),
    data = array(seq_len(4), dim = c(2, 2, 1, 1)), vars = "sst"
  )
  path <- data.frame(longitude = c(-80, -79), latitude = c(-12, -11))
  surface_result <- cube_transect(
    surface, path, time = NULL, variable = "sst", match = "exact",
    mode = "horizontal", keep_index = TRUE
  )
  expect_true(all(is.na(surface_result$depth)))
  expect_identical(surface_result$depth_index, c(1L, 1L))

  x <- .make_baseline_fixture()$cube
  section <- cube_transect(
    x, path, depth = c(50, 0), time = x$time[[3L]],
    variable = c("oxygen", "temperature"), match = "exact",
    mode = "section", keep_index = TRUE
  )
  expect_identical(unique(section$depth), c(50, 0))
  expect_identical(unique(section$depth_index), c(2L, 1L))
  expect_identical(unique(section$time_index), 3L)
  expect_identical(unique(section$variable), c("oxygen", "temperature"))
  expect_identical(unique(section$variable_index), c(2L, 1L))
  expect_error(
    cube_transect(
      x, path, depth = 75, time = x$time[[1L]], variable = "temperature",
      match = "exact", mode = "horizontal"
    ),
    "Exact depth"
  )
  expect_error(
    cube_transect(
      x, path, depth = 0, time = as.Date("2030-01-01"),
      variable = "temperature", match = "exact", mode = "horizontal"
    ),
    "Exact time"
  )
  expect_error(
    cube_transect(
      x, path, depth = 0, time = x$time[[1L]], variable = "salinity",
      match = "exact", mode = "horizontal"
    ),
    "Unknown variable"
  )
})

test_that("path edge cases retain data without clipping or aggregation", {
  x <- .make_baseline_fixture()$cube
  repeated <- data.frame(
    longitude = c(-80, -80, -79), latitude = c(-12, -12, -11)
  )
  repeated_result <- cube_transect(
    x, repeated, depth = 0, time = x$time[[1L]],
    variable = "temperature", match = "exact", mode = "horizontal"
  )
  expect_identical(repeated_result$point_order, 1:3)
  expect_equal(repeated_result$requested_distance_km[1:2], c(0, 0))

  all_zero <- data.frame(
    longitude = c(-80, -80), latitude = c(-12, -12)
  )
  expect_warning(
    zero_result <- cube_transect(
      x, all_zero, depth = 0, time = x$time[[1L]],
      variable = "temperature", match = "exact", mode = "horizontal"
    ),
    "zero total requested distance",
    class = "oceancube_transect_zero_length_warning"
  )
  expect_equal(zero_result$requested_distance_km, c(0, 0))

  profile <- cube_transect(
    x, repeated[1, ], time = x$time[[1L]], variable = "temperature",
    match = "exact", mode = "profile"
  )
  expect_identical(attr(profile, "oceancube_mode"), "profile")
  for (outside in list(
    data.frame(longitude = c(-81, -80), latitude = c(-12, -12)),
    data.frame(longitude = c(-80, -77), latitude = c(-12, -12))
  )) {
    expect_error(
      cube_transect(
        x, outside, depth = 0, time = x$time[[1L]],
        variable = "temperature", match = "nearest",
        tolerance = list(longitude = 2), mode = "horizontal"
      ),
      "outside the cube domain"
    )
  }
  boundary <- cube_transect(
    x, data.frame(longitude = c(-80, -78), latitude = c(-12, -11)),
    depth = 0, time = x$time[[1L]], variable = "temperature",
    match = "exact", mode = "horizontal"
  )
  expect_identical(boundary$longitude, c(-80, -78))

  for (bad in list(NA_real_, Inf)) {
    expect_error(
      cube_transect(
        x, data.frame(longitude = c(bad, -79), latitude = c(-12, -11)),
        depth = 0, time = x$time[[1L]], variable = "temperature",
        match = "exact", mode = "horizontal"
      ),
      "finite and non-missing"
    )
  }
})

test_that("traceability is additive and all inputs remain immutable", {
  x <- .make_baseline_fixture()$cube
  path <- data.frame(
    id = c("A", "B"), longitude = c(-79.8, -79.2),
    latitude = c(-11.8, -11.2)
  )
  depth <- c(50, 0)
  time <- x$time[[2L]]
  variable <- c("oxygen", "temperature")
  tolerance <- list(longitude = 0.3, latitude = 0.3)
  before <- lapply(
    list(x, path, depth, time, variable, tolerance),
    serialize, connection = NULL
  )
  result <- cube_transect(
    x, path, id_col = "id", depth = depth, time = time,
    variable = variable, match = "nearest", tolerance = tolerance,
    mode = "section", keep_index = TRUE
  )
  after <- lapply(
    list(x, path, depth, time, variable, tolerance),
    serialize, connection = NULL
  )
  expect_identical(after, before)
  expect_true(all(c(
    "point_id", "point_order", "longitude_requested", "latitude_requested",
    "longitude", "latitude", "match_distance_km", "requested_distance_km",
    "matched_distance_km", "depth", "time", "variable", "unit", "value",
    "longitude_index", "latitude_index", "depth_index", "time_index",
    "variable_index"
  ) %in% names(result)))

  without_indices <- cube_transect(
    x, path, id_col = "id", depth = 0, time = time,
    variable = "temperature", match = "nearest", tolerance = tolerance,
    mode = "horizontal", keep_index = FALSE
  )
  expect_true("match_distance_km" %in% names(without_indices))
  expect_false(any(grepl("_index$", names(without_indices))))
})

test_that("all-NA cells and repeated matched cells remain separate", {
  x <- .make_baseline_fixture()$cube
  x$data[] <- NA_real_
  path <- data.frame(
    longitude = c(-79.4, -79.2), latitude = c(-11.2, -11.1)
  )
  result <- cube_transect(
    x, path, depth = 0, time = x$time[[1L]], variable = "temperature",
    match = "nearest",
    tolerance = list(longitude = 0.5, latitude = 0.5),
    mode = "horizontal", keep_index = TRUE
  )
  expect_identical(nrow(result), 2L)
  expect_true(all(is.na(result$value)))
  expect_identical(result$point_order, 1:2)
  expect_identical(result$longitude_index, c(2L, 2L))
  expect_identical(result$latitude_index, c(2L, 2L))
  expect_identical(
    attr(result, "oceancube_provenance")$physical_reads$n_unique_pairs,
    1L
  )
})

test_that("non-contiguous NetCDF depths use a selective enclosing block", {
  skip_if_not_installed("ncdf4")
  file <- tempfile("oceancube-transect-depth-", fileext = ".nc")
  withr::local_file(file)
  lon <- c(-80, -79, -78)
  lat <- c(-12, -11)
  depth <- c(0, 25, 50, 100)
  lon_dim <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  lat_dim <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  depth_dim <- ncdf4::ncdim_def("depth", "m", depth)
  time_dim <- ncdf4::ncdim_def(
    "time", "days since 2021-01-01 00:00:00", 0
  )
  definition <- ncdf4::ncvar_def(
    "temperature", "degree_Celsius",
    list(lon_dim, lat_dim, depth_dim, time_dim), missval = -9999
  )
  nc <- ncdf4::nc_create(file, definition)
  values <- array(seq_len(24), dim = c(3, 2, 4, 1))
  ncdf4::ncvar_put(nc, "temperature", values)
  ncdf4::nc_close(nc)

  netcdf <- .new_netcdf_cube(.new_netcdf_storage(file, "temperature"))
  memory <- cube_collect(netcdf)
  path <- data.frame(longitude = c(-80, -78), latitude = c(-12, -11))
  args <- list(
    path = path, depth = c(0, 100), time = netcdf$time,
    variable = "temperature", match = "exact", mode = "section",
    keep_index = TRUE
  )
  from_netcdf <- do.call(cube_transect, c(list(x = netcdf), args))
  from_memory <- do.call(cube_transect, c(list(x = memory), args))
  netcdf_data <- as.data.frame(from_netcdf)
  memory_data <- as.data.frame(from_memory)
  attributes(netcdf_data) <- attributes(netcdf_data)[c(
    "names", "row.names", "class"
  )]
  attributes(memory_data) <- attributes(memory_data)[c(
    "names", "row.names", "class"
  )]
  expect_equal(netcdf_data, memory_data)

  metrics <- attr(from_netcdf, "oceancube_provenance")$physical_reads
  expect_identical(metrics$n_unique_pairs, 2L)
  expect_identical(metrics$n_ncvar_get, 2L)
  expect_identical(metrics$n_values_requested, 4L)
  expect_identical(metrics$n_values_read, 8L)
  expect_lt(metrics$n_values_read, length(values))
  expect_false(isTRUE(metrics$n_values_read == length(values)))
})
