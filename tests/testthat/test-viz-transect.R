transect_test_cube <- function(
    lon = c(-80, -79, -78), lat = -11, depth = c(0, 25, 50),
    time = as.Date("2020-01-01"), vars = "temperature", units = "degC",
    data = NULL) {
  if (is.null(data)) {
    data <- array(
      seq_len(length(lon) * length(lat) * length(depth) *
                length(time) * length(vars)),
      dim = c(length(lon), length(lat), length(depth), length(time), length(vars))
    )
  }
  ocean_cube(
    lon = lon, lat = lat, depth = depth, time = time, vars = vars,
    units = units, data = data
  )
}

transect_test_path <- function(longitude = c(-80, -79, -78),
                               latitude = rep(-11, length(longitude))) {
  data.frame(
    station = paste0("P", seq_along(longitude)),
    longitude = longitude,
    latitude = latitude
  )
}

mock_transect_data <- function(depth = c(0, 50),
                               point_order = 1:3,
                               distances = c(0, 100, 200),
                               values = NULL) {
  n_depth <- length(depth)
  point_position <- rep(seq_along(point_order), each = n_depth)
  depth_position <- rep(seq_along(depth), times = length(point_order))
  if (is.null(values)) values <- seq_along(point_position)
  out <- data.frame(
    point_id = paste0("P", point_order[point_position]),
    point_order = point_order[point_position],
    longitude_requested = -80 + point_position - 1,
    latitude_requested = -11,
    longitude = -80 + point_position - 1,
    latitude = -11,
    match_distance_km = rep(c(0, 0.2, 0.1)[point_position]),
    requested_distance_km = distances[point_position],
    matched_distance_km = distances[point_position] +
      rep(c(0, 1, 2)[point_position]),
    depth = depth[depth_position],
    time = as.Date("2020-01-01"),
    variable = "temperature",
    unit = "degC",
    value = values,
    stringsAsFactors = FALSE
  )
  attr(out, "oceancube_backend") <- "memory"
  attr(out, "oceancube_mode") <- if (n_depth > 1L) "section" else "horizontal"
  attr(out, "oceancube_provenance") <- list(
    physical_reads = list(n_values_read = nrow(out)),
    maximum_match_distance_km = max(out$match_distance_km)
  )
  out
}

with_mock_transect <- function(data) {
  local_mocked_bindings(
    cube_transect = function(...) data,
    .package = "oceancube",
    .env = parent.frame()
  )
}

test_that("viz.transect is exported with the exact 0.2.0 signature", {
  expected <- c(
    "x", "path", "variable", "time", "depth", "lon_col", "lat_col",
    "id_col", "match", "tolerance", "mode", "distance", "limits",
    "na.rm", "reverse_depth", "points", "title", "subtitle", "caption"
  )

  expect_true("viz.transect" %in% getNamespaceExports("oceancube"))
  expect_identical(names(formals(viz.transect)), expected)
  expect_identical(formals(viz.transect)$match, quote(c("exact", "nearest")))
  expect_identical(
    formals(viz.transect)$distance, quote(c("requested", "matched"))
  )
})

test_that("memory sections return canonical raster plots without interpolation", {
  cube <- transect_test_cube()
  path <- transect_test_path()

  plot <- viz.transect(cube, path, "temperature", mode = "section")

  expect_s3_class(plot, "ggplot")
  expect_identical(attr(plot, "oceancube_mode"), "section")
  expect_identical(nrow(plot$data), 9L)
  expect_identical(
    names(plot$data),
    c("point_order", "requested_distance_km", "depth", "value")
  )
  expect_true(inherits(plot$layers[[1L]]$geom, "GeomRaster"))
  expect_false(any(vapply(plot$layers, function(x) {
    inherits(x$geom, c("GeomSmooth", "GeomContour", "GeomDensity2d"))
  }, logical(1))))
  expected <- cube_transect(
    cube, path, depth = NULL, time = NULL, variable = "temperature",
    match = "exact", mode = "section", format = "long"
  )
  expect_identical(plot$data$value, expected$value)
})

test_that("horizontal and surface transects use ordered lines and optional points", {
  path <- transect_test_path()
  cube <- transect_test_cube()
  with_points <- viz.transect(
    cube, path, "temperature", depth = 25, mode = "horizontal"
  )
  line_only <- viz.transect(
    cube, path, "temperature", depth = 25,
    mode = "horizontal", points = FALSE
  )
  surface <- viz.transect(
    transect_test_cube(depth = NA_real_), path, "temperature"
  )

  expect_s3_class(with_points, "ggplot")
  expect_true(inherits(with_points$layers[[1L]]$geom, "GeomLine"))
  expect_true(inherits(with_points$layers[[2L]]$geom, "GeomPoint"))
  expect_length(line_only$layers, 1L)
  expect_true(inherits(line_only$layers[[1L]]$geom, "GeomLine"))
  expect_identical(with_points$data$point_order, 1:3)
  expect_identical(attr(surface, "oceancube_mode"), "horizontal")
  expect_true(all(is.na(attr(surface, "oceancube_depth_range"))))
})

test_that("auto resolves section and horizontal without silently selecting depth", {
  cube <- transect_test_cube()
  path <- transect_test_path()

  section <- viz.transect(cube, path, "temperature")
  horizontal <- viz.transect(cube, path, "temperature", depth = 0)

  expect_identical(attr(section, "oceancube_mode"), "section")
  expect_identical(attr(horizontal, "oceancube_mode"), "horizontal")
  expect_error(
    viz.transect(cube, path, "temperature", depth = 0, mode = "section"),
    "incompatible|at least two", class = "oceancube_transect_mode"
  )
  expect_error(
    viz.transect(cube, path, "temperature", mode = "horizontal"),
    "incompatible|exactly one", class = "oceancube_transect_mode"
  )
  expect_error(
    viz.transect(cube, path[1L, ], "temperature"),
    "at least two ordered path points", class = "oceancube_viz_selection_error"
  )
})

test_that("matching is exact by default and nearest warnings propagate", {
  cube <- transect_test_cube(lat = c(-11, -10))
  exact_path <- transect_test_path()
  nearest_path <- transect_test_path(
    longitude = c(-79.9, -78.9, -78.1),
    latitude = rep(-10.9, 3)
  )

  exact <- expect_silent(
    viz.transect(cube, exact_path, "temperature", depth = 0)
  )
  expect_warning(
    nearest <- viz.transect(
      cube, nearest_path, "temperature", depth = 0, match = "nearest"
    ),
    "no explicit maximum tolerance",
    class = "oceancube_transect_matching_warning"
  )
  bounded <- expect_silent(viz.transect(
    cube, nearest_path, "temperature", depth = 0, match = "nearest",
    tolerance = list(longitude = 0.2, latitude = 0.2)
  ))

  expect_identical(attr(exact, "oceancube_match"), "exact")
  expect_identical(attr(nearest, "oceancube_match"), "nearest")
  expect_identical(
    attr(bounded, "oceancube_tolerance"),
    list(longitude = 0.2, latitude = 0.2)
  )
  expect_gt(attr(nearest, "oceancube_max_match_distance_km"), 0)
})

test_that("requested and matched cumulative distances remain distinct", {
  cube <- transect_test_cube(lat = c(-11, -10))
  path <- transect_test_path(
    longitude = c(-79.9, -79.2, -78.1), latitude = rep(-10.9, 3)
  )
  tolerance <- list(longitude = 0.3, latitude = 0.2)
  requested <- viz.transect(
    cube, path, "temperature", depth = 0, match = "nearest",
    tolerance = tolerance
  )
  matched <- viz.transect(
    cube, path, "temperature", depth = 0, match = "nearest",
    tolerance = tolerance, distance = "matched"
  )

  expect_true("requested_distance_km" %in% names(requested$data))
  expect_true("matched_distance_km" %in% names(matched$data))
  expect_false("match_distance_km" %in% names(requested$data))
  expect_false("match_distance_km" %in% names(matched$data))
  expect_identical(
    requested$labels$x, "Distance along requested transect (km)"
  )
  expect_identical(
    matched$labels$x, "Distance along matched grid path (km)"
  )
  expect_false(isTRUE(all.equal(
    requested$data$requested_distance_km,
    matched$data$matched_distance_km
  )))
})

test_that("regular and irregular section axes select raster or tile", {
  regular <- viz.transect(
    transect_test_cube(depth = c(0, 25, 50)),
    transect_test_path(), "temperature", mode = "section"
  )
  irregular_distance <- viz.transect(
    transect_test_cube(lat = c(-11, -10)),
    transect_test_path(longitude = c(-80, -79, -78),
                       latitude = c(-11, -11, -10.5)),
    "temperature", mode = "section", match = "nearest",
    tolerance = list(longitude = 0, latitude = 0.5), distance = "requested"
  )
  irregular_depth <- viz.transect(
    transect_test_cube(depth = c(0, 20, 55)),
    transect_test_path(), "temperature", mode = "section"
  )

  expect_true(inherits(regular$layers[[1L]]$geom, "GeomRaster"))
  expect_true(inherits(irregular_distance$layers[[1L]]$geom, "GeomTile"))
  expect_true(inherits(irregular_depth$layers[[1L]]$geom, "GeomTile"))
  expect_false(inherits(regular$coordinates, "CoordFixed"))
})

test_that("depth orientation, units, limits, and labels follow viz conventions", {
  cube <- transect_test_cube()
  attr(cube$depth, "units") <- "m"
  reversed <- viz.transect(
    cube, transect_test_path(), "temperature", limits = c(2, 7),
    title = "Transect", subtitle = "Stored cells", caption = "Offline"
  )
  forward <- viz.transect(
    cube, transect_test_path(), "temperature", reverse_depth = FALSE
  )
  horizontal <- viz.transect(
    cube, transect_test_path(), "temperature", depth = 0, limits = c(2, 3)
  )

  expect_identical(reversed$scales$get_scales("y")$trans$name, "reverse")
  expect_null(forward$scales$get_scales("y"))
  expect_identical(reversed$labels$y, "Depth (m)")
  expect_identical(
    reversed$scales$get_scales("fill")$name, "temperature (degC)"
  )
  expect_identical(reversed$scales$get_scales("fill")$limits, c(2, 7))
  expect_identical(horizontal$scales$get_scales("y")$limits, c(2, 3))
  expect_identical(horizontal$labels$y, "temperature (degC)")
  expect_identical(reversed$labels$title, "Transect")
  expect_equal(reversed$scales$get_scales("fill")$oob(c(1, 8), c(2, 7)), c(2, 7))
})

test_that("own arguments are validated before plotting", {
  cube <- transect_test_cube()
  path <- transect_test_path()

  expect_error(viz.transect(cube, path, character()), "variable")
  expect_error(viz.transect(cube, path, c("temperature", "oxygen")), "variable")
  expect_error(viz.transect(cube, path, "temperature", match = "maybe"), "match")
  expect_error(viz.transect(cube, path, "temperature", mode = "profile"), "mode")
  expect_error(viz.transect(cube, path, "temperature", distance = "cell"), "distance")
  expect_error(viz.transect(cube, path, "temperature", limits = c(1, 1)), "limits")
  expect_error(viz.transect(cube, path, "temperature", limits = c(1, Inf)), "limits")
  expect_error(viz.transect(cube, path, "temperature", na.rm = NA), "na.rm")
  expect_error(viz.transect(cube, path, "temperature", reverse_depth = 1), "reverse_depth")
  expect_error(viz.transect(cube, path, "temperature", points = NA), "points")
  expect_error(viz.transect(cube, path, "temperature", title = NA_character_), "title")
  expect_error(viz.transect(cube, path, "temperature", subtitle = 1), "subtitle")
  expect_error(viz.transect(cube, path, "temperature", caption = c("a", "b")), "caption")
})

test_that("cube, path, time, depth, and tolerance selection errors propagate", {
  cube <- transect_test_cube(time = as.Date(c("2020-01-01", "2020-02-01")))
  path <- transect_test_path()
  invalid_cube <- cube
  invalid_cube$lat <- -100
  sf_path <- path
  class(sf_path) <- c("sf", class(sf_path))

  expect_error(viz.transect(invalid_cube, path, "temperature"),
               class = "oceancube_validation_error")
  expect_error(viz.transect(cube, list(longitude = -80), "temperature"),
               "data frame or matrix")
  expect_error(viz.transect(cube, sf_path, "temperature"),
               "does not yet accept sf/sfc", class = "oceancube_transect_crs_error")
  expect_error(viz.transect(cube, path, "unknown"), "[Vv]ariable")
  expect_error(viz.transect(cube, path, "temperature"), "exactly one")
  expect_error(viz.transect(
    cube, path, "temperature", time = as.Date("1999-01-01")
  ), "not found|outside")
  expect_error(viz.transect(
    cube, path, "temperature", time = cube$time[[1L]], depth = 999
  ), "not found|outside")
  expect_error(viz.transect(
    cube, transform(path, longitude = longitude + 0.5), "temperature",
    time = cube$time[[1L]], depth = 0
  ), "not found|[Ee]xact")
  expect_error(viz.transect(
    cube, transform(path, longitude = longitude + 0.4), "temperature",
    time = cube$time[[1L]], depth = 0, match = "nearest",
    tolerance = list(longitude = 0.1)
  ), "tolerance|farther")
  expect_error(viz.transect(
    cube, path, "temperature", time = cube$time[[1L]], depth = 0,
    match = "nearest", tolerance = -1
  ), "tolerance")
})

test_that("delegation uses only the public cube_transect data API", {
  cube <- transect_test_cube()
  path <- transect_test_path()
  observed <- NULL
  data <- mock_transect_data()
  local_mocked_bindings(
    cube_transect = function(x, path, lon_col, lat_col, id_col, depth, time,
                             variable, by, match, tolerance, mode, format,
                             keep_index) {
      observed <<- as.list(environment())
      data
    },
    .package = "oceancube"
  )

  plot <- viz.transect(
    cube, path, "temperature", depth = c(0, 50), lon_col = "longitude",
    lat_col = "latitude", id_col = "station", match = "nearest",
    tolerance = list(longitude = 0.5), mode = "section"
  )

  expect_s3_class(plot, "ggplot")
  expect_identical(observed$path, path)
  expect_identical(observed$lon_col, "longitude")
  expect_identical(observed$lat_col, "latitude")
  expect_identical(observed$id_col, "station")
  expect_identical(observed$variable, "temperature")
  expect_identical(observed$by, "value")
  expect_identical(observed$match, "nearest")
  expect_identical(observed$tolerance, list(longitude = 0.5))
  expect_identical(observed$mode, "section")
  expect_identical(observed$format, "long")
  expect_false(observed$keep_index)
  implementation <- paste(deparse(body(viz.transect)), collapse = "\n")
  expect_match(implementation, "cube_transect\\(", fixed = FALSE)
  expect_false(grepl("\\.cube_read|cube_collect|ncvar_get", implementation))
})

test_that("malformed empty, incomplete, nonnumeric, and all-NA data fail", {
  cube <- transect_test_cube()
  path <- transect_test_path()
  empty <- mock_transect_data()[0, ]
  attr(empty, "oceancube_backend") <- "memory"
  with_mock_transect(empty)
  expect_error(viz.transect(cube, path, "temperature"), "empty",
               class = "oceancube_viz_data_error")

  incomplete <- mock_transect_data()
  incomplete$matched_distance_km <- NULL
  with_mock_transect(incomplete)
  expect_error(viz.transect(cube, path, "temperature"), "incomplete",
               class = "oceancube_viz_data_error")

  nonnumeric <- mock_transect_data()
  nonnumeric$value <- as.character(nonnumeric$value)
  with_mock_transect(nonnumeric)
  expect_error(viz.transect(cube, path, "temperature"), "must be numeric",
               class = "oceancube_viz_data_error")

  all_na <- mock_transect_data(values = rep(NA_real_, 6))
  with_mock_transect(all_na)
  expect_error(viz.transect(cube, path, "temperature"), "empty after removing",
               class = "oceancube_viz_data_error")
  expect_s3_class(
    viz.transect(cube, path, "temperature", na.rm = FALSE), "ggplot"
  )
})

test_that("duplicate logical and plotting cells are rejected without aggregation", {
  cube <- transect_test_cube()
  path <- transect_test_path()
  duplicate_logical <- mock_transect_data()
  duplicate_logical <- rbind(duplicate_logical, duplicate_logical[1L, ])
  attr(duplicate_logical, "oceancube_backend") <- "memory"
  with_mock_transect(duplicate_logical)
  expect_error(
    viz.transect(cube, path, "temperature", mode = "section"),
    "one row per point_order x depth", class = "oceancube_viz_data_error"
  )

  duplicate_distance <- mock_transect_data(distances = c(0, 0, 100))
  with_mock_transect(duplicate_distance)
  expect_error(
    viz.transect(cube, path, "temperature", mode = "section"),
    "non-unique distance positions", class = "oceancube_viz_data_error"
  )

  duplicate_horizontal <- mock_transect_data(depth = 0)
  duplicate_horizontal <- rbind(
    duplicate_horizontal, duplicate_horizontal[1L, , drop = FALSE]
  )
  attr(duplicate_horizontal, "oceancube_backend") <- "memory"
  with_mock_transect(duplicate_horizontal)
  expect_error(
    viz.transect(cube, path, "temperature", mode = "horizontal"),
    "one row per point_order", class = "oceancube_viz_data_error"
  )
})

test_that("distance and order inconsistencies are rejected", {
  cube <- transect_test_cube()
  path <- transect_test_path()
  nonnumeric <- mock_transect_data()
  nonnumeric$requested_distance_km <- as.character(
    nonnumeric$requested_distance_km
  )
  with_mock_transect(nonnumeric)
  expect_error(viz.transect(cube, path, "temperature"), "distance columns",
               class = "oceancube_viz_data_error")

  nonfinite <- mock_transect_data()
  nonfinite$matched_distance_km[[4L]] <- Inf
  with_mock_transect(nonfinite)
  expect_error(viz.transect(
    cube, path, "temperature", distance = "matched"
  ), "distance columns", class = "oceancube_viz_data_error")

  nonmonotonic <- mock_transect_data(distances = c(0, 200, 100))
  with_mock_transect(nonmonotonic)
  expect_error(viz.transect(cube, path, "temperature"), "monotonic",
               class = "oceancube_viz_data_error")
})

test_that("zero-length warnings propagate and section ambiguity is explicit", {
  cube <- transect_test_cube()
  repeated <- transect_test_path(
    longitude = rep(-80, 3), latitude = rep(-11, 3)
  )
  expect_warning(
    horizontal <- viz.transect(cube, repeated, "temperature", depth = 0),
    "zero total requested distance",
    class = "oceancube_transect_zero_length_warning"
  )
  expect_s3_class(horizontal, "ggplot")

  expect_warning(
    expect_error(
      viz.transect(cube, repeated, "temperature", mode = "section"),
      "non-unique distance positions", class = "oceancube_viz_data_error"
    ),
    "zero total requested distance",
    class = "oceancube_transect_zero_length_warning"
  )
})

test_that("plot metadata is additive and inputs remain immutable", {
  cube <- transect_test_cube()
  path <- transect_test_path()
  before_cube <- serialize(cube, NULL)
  before_path <- serialize(path, NULL)
  tolerance <- list(longitude = 0, latitude = 0)

  plot <- viz.transect(
    cube, path, "temperature", depth = 0, match = "nearest",
    tolerance = tolerance, distance = "matched"
  )

  expect_identical(serialize(cube, NULL), before_cube)
  expect_identical(serialize(path, NULL), before_path)
  expect_identical(attr(plot, "oceancube_variable"), "temperature")
  expect_identical(attr(plot, "oceancube_time"), as.Date("2020-01-01"))
  expect_identical(attr(plot, "oceancube_mode"), "horizontal")
  expect_identical(attr(plot, "oceancube_distance"), "matched")
  expect_identical(attr(plot, "oceancube_depth_range"), c(0, 0))
  expect_identical(attr(plot, "oceancube_backend"), "memory")
  expect_identical(attr(plot, "oceancube_match"), "nearest")
  expect_identical(attr(plot, "oceancube_tolerance"), tolerance)
  expect_identical(attr(plot, "oceancube_max_match_distance_km"), 0)
  expect_identical(attr(plot, "oceancube_path_points"), 3L)
})

test_that("NetCDF transects remain selective end to end", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  cube <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )
  path <- transect_test_path()
  observed <- NULL
  original_transect <- cube_transect
  local_mocked_bindings(
    cube_transect = function(...) {
      result <- original_transect(...)
      observed <<- list(
        provenance = attr(result, "oceancube_provenance", exact = TRUE),
        qa = attr(result, "oceancube_qa", exact = TRUE)
      )
      result
    },
    .package = "oceancube"
  )

  plot <- viz.transect(
    cube, path, "temperature",
    time = as.POSIXct("2000-01-02", tz = "UTC"),
    depth = c(0, 50), mode = "section"
  )
  operation <- tail(observed$provenance$history, 1L)[[1L]]
  metrics <- observed$qa$transect$physical_reads
  full_cube_values <- prod(c(3L, 2L, 2L, 4L, 2L))

  expect_s3_class(plot, "ggplot")
  expect_identical(attr(plot, "oceancube_backend"), "netcdf")
  expect_identical(operation$parameters$resolved$n_points, 3L)
  expect_identical(operation$parameters$resolved$selected_depth_count, 2L)
  expect_identical(length(operation$parameters$resolved$selected_variables), 1L)
  expect_identical(metrics$n_open, 1L)
  expect_identical(metrics$n_unique_pairs, 3L)
  expect_identical(metrics$n_ncvar_get, 3L)
  expect_identical(metrics$n_values_read, 6L)
  expect_lt(metrics$n_values_read, full_cube_values)
  expect_false(isTRUE(metrics$n_values_read == full_cube_values))
})
