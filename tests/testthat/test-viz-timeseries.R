timeseries_test_cube <- function(
    lon = -79, lat = -11, depth = 0,
    time = as.Date("2020-01-01") + 0:3,
    vars = "temperature", units = "degC", data = NULL) {
  if (is.null(data)) {
    data <- array(
      seq_len(length(lon) * length(lat) * length(depth) *
                length(time) * length(vars)),
      dim = c(length(lon), length(lat), length(depth), length(time), length(vars))
    )
  }
  ocean_cube(
    lon = lon, lat = lat, depth = depth, time = time,
    vars = vars, units = units, data = data
  )
}

mock_timeseries_data <- function(
    longitude = -79, latitude = -11, depth = 0,
    time = as.Date("2020-01-01") + 0:2,
    variable = "temperature", unit = "degC", value = seq_along(time)) {
  n <- max(
    length(longitude), length(latitude), length(depth), length(time),
    length(variable), length(unit), length(value)
  )
  out <- data.frame(
    longitude = rep(longitude, length.out = n),
    latitude = rep(latitude, length.out = n),
    depth = rep(depth, length.out = n),
    time = rep(time, length.out = n),
    variable = rep(variable, length.out = n),
    unit = rep(unit, length.out = n),
    value = rep(value, length.out = n),
    stringsAsFactors = FALSE
  )
  attr(out, "oceancube_backend") <- "memory"
  attr(out, "oceancube_selection") <- list(selected = list(time = out$time))
  attr(out, "oceancube_provenance") <- list(
    operation = "cube_extract", mode = "series", rows_returned = nrow(out)
  )
  out
}

with_mock_timeseries <- function(data) {
  local_mocked_bindings(
    cube_extract = function(...) data,
    .package = "oceancube",
    .env = parent.frame()
  )
}

test_that("viz.timeseries is exported with the exact approved signature", {
  expected <- c(
    "x", "variable", "longitude", "latitude", "depth", "time_from",
    "time_to", "match", "tolerance", "limits", "na.rm", "points",
    "title", "subtitle", "caption"
  )

  expect_true("viz.timeseries" %in% getNamespaceExports("oceancube"))
  expect_identical(names(formals(viz.timeseries)), expected)
  expect_identical(formals(viz.timeseries)$match, quote(c("exact", "nearest")))
  expect_identical(formals(viz.timeseries)$na.rm, FALSE)
  expect_identical(formals(viz.timeseries)$points, FALSE)
})

test_that("full memory series is raw, ordered, and drawn as a line", {
  cube <- timeseries_test_cube()
  plot <- viz.timeseries(cube, "temperature")

  expect_s3_class(plot, "ggplot")
  expect_identical(names(plot$data), c("time", "value"))
  expect_identical(plot$data$time, cube$time)
  expect_identical(plot$data$value, 1:4)
  expect_true(inherits(plot$layers[[1L]]$geom, "GeomLine"))
  expect_length(plot$layers, 1L)
  expect_false(any(vapply(plot$layers, function(layer) {
    inherits(layer$geom, c("GeomSmooth", "GeomRibbon"))
  }, logical(1))))
})

test_that("NULL selectors are allowed only for singleton spatial and depth axes", {
  singleton <- viz.timeseries(timeseries_test_cube(), "temperature")
  ambiguous_lon <- timeseries_test_cube(lon = c(-80, -79))
  ambiguous_lat <- timeseries_test_cube(lat = c(-12, -11))
  ambiguous_depth <- timeseries_test_cube(depth = c(0, 50))

  expect_identical(attr(singleton, "oceancube_longitude"), -79)
  expect_identical(attr(singleton, "oceancube_latitude"), -11)
  expect_identical(attr(singleton, "oceancube_depth"), 0)
  expect_error(
    viz.timeseries(ambiguous_lon, "temperature"),
    "longitude.*exactly one", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.timeseries(ambiguous_lat, "temperature"),
    "latitude.*exactly one", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.timeseries(ambiguous_depth, "temperature"),
    "depth.*exactly one", class = "oceancube_viz_selection_error"
  )
})

test_that("exact and nearest selectors are delegated without interpolation", {
  cube <- timeseries_test_cube(
    lon = c(-80, -79), lat = c(-12, -11), depth = c(0, 50)
  )
  exact <- viz.timeseries(
    cube, "temperature", longitude = -79, latitude = -11, depth = 50
  )
  tolerance <- list(longitude = 0.3, latitude = 0.3, depth = 15)
  nearest <- viz.timeseries(
    cube, "temperature", longitude = -79.2, latitude = -11.2, depth = 40,
    match = "nearest", tolerance = tolerance
  )

  expect_identical(attr(exact, "oceancube_match"), "exact")
  expect_identical(attr(exact, "oceancube_longitude"), -79)
  expect_identical(attr(exact, "oceancube_latitude"), -11)
  expect_identical(attr(exact, "oceancube_depth"), 50)
  expect_identical(attr(exact, "oceancube_match_distance_km"), 0)
  expect_identical(attr(nearest, "oceancube_match"), "nearest")
  expect_identical(attr(nearest, "oceancube_tolerance"), tolerance)
  expect_identical(attr(nearest, "oceancube_longitude"), -79)
  expect_identical(attr(nearest, "oceancube_latitude"), -11)
  expect_identical(attr(nearest, "oceancube_depth"), 50)
  expect_identical(attr(nearest, "oceancube_longitude_requested"), -79.2)
  expect_identical(attr(nearest, "oceancube_latitude_requested"), -11.2)
  expect_identical(attr(nearest, "oceancube_depth_requested"), 40)
  expect_gt(attr(nearest, "oceancube_match_distance_km"), 0)
})

test_that("surface and singleton physical depth cubes are supported", {
  surface <- viz.timeseries(
    timeseries_test_cube(depth = NA_real_), "temperature"
  )
  physical <- viz.timeseries(timeseries_test_cube(depth = 25), "temperature")

  expect_true(is.na(attr(surface, "oceancube_depth")))
  expect_identical(attr(physical, "oceancube_depth"), 25)
  expect_s3_class(surface, "ggplot")
  expect_s3_class(physical, "ggplot")
})

test_that("variable is exactly one existing character scalar", {
  cube <- timeseries_test_cube(
    vars = c("temperature", "oxygen"), units = c("degC", "mmol m-3")
  )
  plot <- viz.timeseries(cube, "oxygen")

  expect_identical(attr(plot, "oceancube_variable"), "oxygen")
  expect_identical(plot$labels$y, "oxygen (mmol m-3)")
  expect_error(viz.timeseries(cube, NULL), "variable")
  expect_error(viz.timeseries(cube, character()), "variable")
  expect_error(viz.timeseries(cube, c("temperature", "oxygen")), "variable")
  expect_error(viz.timeseries(cube, "unknown"), "not present")
})

test_that("open and closed time bounds are inclusive", {
  cube <- timeseries_test_cube()
  from <- viz.timeseries(
    cube, "temperature", time_from = as.Date("2020-01-02")
  )
  to <- viz.timeseries(
    cube, "temperature", time_to = as.Date("2020-01-03")
  )
  bounded <- viz.timeseries(
    cube, "temperature", time_from = as.Date("2020-01-02"),
    time_to = as.Date("2020-01-03")
  )

  expect_identical(from$data$time, as.Date("2020-01-01") + 1:3)
  expect_identical(to$data$time, as.Date("2020-01-01") + 0:2)
  expect_identical(bounded$data$time, as.Date("2020-01-01") + 1:2)
  expect_identical(
    attr(bounded, "oceancube_time_range"),
    as.Date(c("2020-01-02", "2020-01-03"))
  )
  expect_identical(attr(bounded, "oceancube_n_time"), 2L)
})

test_that("invalid, reversed, and empty time ranges fail clearly", {
  cube <- timeseries_test_cube(time = as.Date(c("2020-01-01", "2020-01-03")))

  expect_error(
    viz.timeseries(cube, "temperature", time_from = "2020-01-01"),
    "time_from.*Date", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.timeseries(cube, "temperature", time_to = 1),
    "time_to.*Date", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.timeseries(
      cube, "temperature", time_from = as.Date("2020-01-03"),
      time_to = as.Date("2020-01-01")
    ),
    "time_from.*earlier", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.timeseries(
      cube, "temperature", time_from = as.Date("2020-01-02"),
      time_to = as.Date("2020-01-02")
    ),
    "No stored time values", class = "oceancube_viz_selection_error"
  )
})

test_that("POSIXct bounds use instant semantics across display timezones", {
  base <- timeseries_test_cube()
  cube <- ocean_cube(
    lon = base$lon, lat = base$lat, depth = base$depth,
    time = as.POSIXct(base$time, tz = "UTC"), vars = base$vars,
    units = base$units, data = base$data
  )
  from <- cube$time[[2L]]

  plot <- viz.timeseries(cube, "temperature", time_from = from)

  expect_s3_class(plot$data$time, "POSIXct")
  expect_identical(attr(plot$data$time, "tzone"), "UTC")
  equivalent <- viz.timeseries(
    cube, "temperature",
    time_from = as.POSIXct("2020-01-01 19:00:00", tz = "America/Lima")
  )
  expect_identical(equivalent$data$time, plot$data$time)
})

test_that("canonical constructor rejects unsorted and duplicate plotting time", {
  time <- as.Date(c("2020-01-03", "2020-01-01", "2020-01-01", "2020-01-02"))
  expect_error(
    timeseries_test_cube(
      time = time,
      data = array(c(30, 10, 11, 20), dim = c(1, 1, 1, 4, 1))
    ),
    "unique|strictly increasing"
  )
})

test_that("duplicate raw rows are retained without aggregation", {
  data <- mock_timeseries_data(
    time = rep(as.Date("2020-01-01"), 2), value = c(4, 4)
  )
  with_mock_timeseries(data)
  plot <- viz.timeseries(timeseries_test_cube(), "temperature")

  expect_identical(nrow(plot$data), 2L)
  expect_identical(plot$data$value, c(4, 4))
  expect_identical(attr(plot, "oceancube_n_time"), 2L)
})

test_that("NA policy preserves gaps by default and removes only when requested", {
  cube <- timeseries_test_cube(
    time = as.Date("2020-01-01") + 0:2,
    data = array(c(1, NA, 3), dim = c(1, 1, 1, 3, 1))
  )
  raw <- viz.timeseries(cube, "temperature")
  removed <- viz.timeseries(cube, "temperature", na.rm = TRUE)

  expect_identical(nrow(raw$data), 3L)
  expect_true(is.na(raw$data$value[[2L]]))
  expect_identical(removed$data$value, c(1, 3))
  expect_identical(nrow(removed$data), 2L)
  expect_false(raw$layers[[1L]]$geom_params$na.rm)
  expect_true(removed$layers[[1L]]$geom_params$na.rm)
  expect_false(any(vapply(raw$layers, function(layer) {
    inherits(layer$geom, "GeomSmooth")
  }, logical(1))))
})

test_that("all-NA series is plottable unless missing rows are removed", {
  cube <- timeseries_test_cube(
    data = array(rep(NA_real_, 4), dim = c(1, 1, 1, 4, 1))
  )

  expect_s3_class(viz.timeseries(cube, "temperature"), "ggplot")
  expect_error(
    viz.timeseries(cube, "temperature", na.rm = TRUE),
    "empty after removing", class = "oceancube_viz_data_error"
  )
})

test_that("points, labels, titles, and squishing limits follow viz conventions", {
  cube <- timeseries_test_cube()
  plot <- viz.timeseries(
    cube, "temperature", points = TRUE, limits = c(2, 3),
    title = "Raw series", subtitle = "Stored cell", caption = "Offline"
  )

  expect_length(plot$layers, 2L)
  expect_true(inherits(plot$layers[[1L]]$geom, "GeomLine"))
  expect_true(inherits(plot$layers[[2L]]$geom, "GeomPoint"))
  expect_identical(plot$labels$x, "Time")
  expect_identical(plot$labels$y, "temperature (degC)")
  expect_identical(plot$labels$title, "Raw series")
  expect_identical(plot$labels$subtitle, "Stored cell")
  expect_identical(plot$labels$caption, "Offline")
  expect_identical(plot$scales$get_scales("y")$limits, c(2, 3))
  expect_equal(plot$scales$get_scales("y")$oob(c(1, 4), c(2, 3)), c(2, 3))
  expect_identical(nrow(plot$data), 4L)
})

test_that("own arguments and selection failures are explicit", {
  cube <- timeseries_test_cube(
    lon = c(-80, -79), lat = c(-12, -11), depth = c(0, 50)
  )

  expect_error(viz.timeseries(list(), "temperature"),
               class = "oceancube_validation_error")
  expect_error(viz.timeseries(cube, "temperature", longitude = c(-80, -79)),
               "longitude")
  expect_error(viz.timeseries(cube, "temperature", longitude = -79,
                              latitude = c(-12, -11)), "latitude")
  expect_error(viz.timeseries(cube, "temperature", longitude = -79,
                              latitude = -11, depth = c(0, 50)), "depth")
  expect_error(viz.timeseries(cube, "temperature", longitude = -79,
                              latitude = -11, depth = 0, match = "maybe"), "match")
  expect_error(viz.timeseries(cube, "temperature", longitude = -79.5,
                              latitude = -11, depth = 0), "Exact|not found")
  expect_error(viz.timeseries(cube, "temperature", longitude = -79,
                              latitude = -11.5, depth = 0), "Exact|not found")
  expect_error(viz.timeseries(cube, "temperature", longitude = -79,
                              latitude = -11, depth = 25), "Exact|not found")
  expect_error(viz.timeseries(cube, "temperature", longitude = -79.5,
                              latitude = -11, depth = 0, match = "nearest",
                              tolerance = list(longitude = 0.1)), "tolerance|farther")
  expect_error(viz.timeseries(cube, "temperature", longitude = -79,
                              latitude = -11, depth = 0, match = "nearest",
                              tolerance = -1), "tolerance")
})

test_that("limits, flags, and textual labels validate without filtering rows", {
  cube <- timeseries_test_cube()

  expect_error(viz.timeseries(cube, "temperature", limits = 1), "limits")
  expect_error(viz.timeseries(cube, "temperature", limits = c(2, 2)), "limits")
  expect_error(viz.timeseries(cube, "temperature", limits = c(1, Inf)), "limits")
  expect_error(viz.timeseries(cube, "temperature", na.rm = NA), "na.rm")
  expect_error(viz.timeseries(cube, "temperature", points = 1), "points")
  expect_error(viz.timeseries(cube, "temperature", title = NA_character_), "title")
  expect_error(viz.timeseries(cube, "temperature", subtitle = 1), "subtitle")
  expect_error(viz.timeseries(cube, "temperature", caption = c("a", "b")), "caption")
})

test_that("cube_extract series API is the sole data-selection layer", {
  cube <- timeseries_test_cube()
  observed <- NULL
  data <- mock_timeseries_data(
    time = as.Date("2020-01-01") + 1:2, value = c(2, 3)
  )
  local_mocked_bindings(
    cube_extract = function(x, longitude, latitude, depth, time, variable,
                            by, match, tolerance, mode, format, keep_index,
                            keep_distance) {
      observed <<- as.list(environment())
      data
    },
    .package = "oceancube"
  )
  tolerance <- list(longitude = 0.5)

  plot <- viz.timeseries(
    cube, "temperature", time_from = as.Date("2020-01-02"),
    time_to = as.Date("2020-01-03"), match = "nearest",
    tolerance = tolerance
  )

  expect_s3_class(plot, "ggplot")
  expect_identical(observed$variable, "temperature")
  expect_identical(observed$time, as.Date("2020-01-01") + 1:2)
  expect_identical(observed$by, "value")
  expect_identical(observed$match, "nearest")
  expect_identical(observed$tolerance, tolerance)
  expect_identical(observed$mode, "series")
  expect_identical(observed$format, "long")
  expect_false(observed$keep_index)
  expect_true(observed$keep_distance)
  implementation <- paste(deparse(body(viz.timeseries)), collapse = "\n")
  expect_match(implementation, "cube_extract\\(")
  expect_false(grepl("\\.cube_read|ncvar_get|cube_collect|\\$data", implementation))
})

test_that("malformed extraction output is rejected without aggregation", {
  cube <- timeseries_test_cube()

  empty <- mock_timeseries_data()[0, ]
  attr(empty, "oceancube_backend") <- "memory"
  with_mock_timeseries(empty)
  expect_error(viz.timeseries(cube, "temperature"), "empty",
               class = "oceancube_viz_data_error")

  incomplete <- mock_timeseries_data()
  incomplete$unit <- NULL
  with_mock_timeseries(incomplete)
  expect_error(viz.timeseries(cube, "temperature"), "incomplete",
               class = "oceancube_viz_data_error")

  nonnumeric <- mock_timeseries_data()
  nonnumeric$value <- as.character(nonnumeric$value)
  with_mock_timeseries(nonnumeric)
  expect_error(viz.timeseries(cube, "temperature"), "must be numeric",
               class = "oceancube_viz_data_error")

  multiple_location <- mock_timeseries_data(longitude = c(-79, -78))
  with_mock_timeseries(multiple_location)
  expect_error(viz.timeseries(cube, "temperature"), "one finite longitude",
               class = "oceancube_viz_selection_error")

  multiple_depth <- mock_timeseries_data(depth = c(0, 50))
  with_mock_timeseries(multiple_depth)
  expect_error(viz.timeseries(cube, "temperature"), "one finite depth",
               class = "oceancube_viz_selection_error")

  multiple_variable <- mock_timeseries_data(
    variable = c("temperature", "oxygen")
  )
  with_mock_timeseries(multiple_variable)
  expect_error(viz.timeseries(cube, "temperature"), "requested variable",
               class = "oceancube_viz_selection_error")
})

test_that("cube and selectors remain immutable and metadata is additive", {
  cube <- timeseries_test_cube()
  before_cube <- serialize(cube, NULL)
  longitude <- -79
  latitude <- -11
  depth <- 0
  time_from <- as.Date("2020-01-02")
  time_to <- as.Date("2020-01-03")
  tolerance <- NULL
  before_selectors <- serialize(
    list(longitude, latitude, depth, time_from, time_to, tolerance), NULL
  )

  plot <- viz.timeseries(
    cube, "temperature", longitude, latitude, depth,
    time_from, time_to, tolerance = tolerance
  )

  expect_identical(serialize(cube, NULL), before_cube)
  expect_identical(
    serialize(list(longitude, latitude, depth, time_from, time_to, tolerance), NULL),
    before_selectors
  )
  expect_identical(attr(plot, "oceancube_variable"), "temperature")
  expect_identical(attr(plot, "oceancube_longitude"), -79)
  expect_identical(attr(plot, "oceancube_latitude"), -11)
  expect_identical(attr(plot, "oceancube_depth"), 0)
  expect_identical(attr(plot, "oceancube_backend"), "memory")
  expect_identical(attr(plot, "oceancube_match"), "exact")
  expect_identical(attr(plot, "oceancube_tolerance"), tolerance)
  expect_identical(attr(plot, "oceancube_match_distance_km"), 0)
  expect_identical(attr(plot, "oceancube_n_time"), 2L)
  expect_identical(
    attr(plot, "oceancube_time_range"),
    as.Date(c("2020-01-02", "2020-01-03"))
  )
  expect_identical(attr(plot, "oceancube_provenance")$mode, "series")
})

test_that("warnings from cube_extract propagate", {
  cube <- timeseries_test_cube()
  data <- mock_timeseries_data()
  local_mocked_bindings(
    cube_extract = function(...) {
      rlang::warn("scientific selection warning", class = "selection_warning")
      data
    },
    .package = "oceancube"
  )

  expect_warning(
    expect_s3_class(viz.timeseries(cube, "temperature"), "ggplot"),
    "scientific selection warning", class = "selection_warning"
  )
})

test_that("NetCDF full and bounded point series remain selective", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  cube <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )
  full <- viz.timeseries(
    cube, "temperature", longitude = -79, latitude = -11, depth = 0
  )
  bounded <- viz.timeseries(
    cube, "temperature", longitude = -79, latitude = -11, depth = 0,
    time_from = cube$time[[2L]], time_to = cube$time[[3L]]
  )
  full_read <- attr(full, "oceancube_provenance")$netcdf_read
  bounded_read <- attr(bounded, "oceancube_provenance")$netcdf_read
  full_cube_values <- prod(c(3L, 2L, 2L, 4L, 2L))

  expect_s3_class(full, "ggplot")
  expect_s3_class(bounded, "ggplot")
  expect_identical(attr(full, "oceancube_backend"), "netcdf")
  expect_identical(attr(bounded, "oceancube_backend"), "netcdf")
  expect_identical(
    full_read$physical_count,
    c(longitude = 1L, latitude = 1L, depth = 1L, time = 4L)
  )
  expect_identical(full_read$variables, "temperature")
  expect_identical(full_read$values_requested, 4)
  expect_lt(full_read$values_in_envelope, full_cube_values)
  expect_identical(
    bounded_read$physical_count,
    c(longitude = 1L, latitude = 1L, depth = 1L, time = 2L)
  )
  expect_identical(bounded_read$variables, "temperature")
  expect_identical(bounded_read$values_requested, 2)
  expect_identical(bounded_read$values_in_envelope, 2)
  expect_lt(bounded_read$values_in_envelope, full_read$values_in_envelope)
  expect_identical(nrow(full$data), 4L)
  expect_identical(nrow(bounded$data), 2L)
})
