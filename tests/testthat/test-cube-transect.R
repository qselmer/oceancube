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
