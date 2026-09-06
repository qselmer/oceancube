hovmoller_test_cube <- function(
    lon = c(-80, -79, -77.5),
    lat = c(-12, -11),
    depth = c(0, 20, 75),
    time = as.Date("2020-01-01") + c(0, 1, 3, 7),
    vars = "temperature",
    units = "degC",
    data = NULL) {
  if (is.null(data)) {
    data <- array(
      seq_len(length(lon) * length(lat) * length(depth) *
                length(time) * length(vars)),
      dim = c(length(lon), length(lat), length(depth), length(time), length(vars))
    )
  }
  ocean_cube(
    lon = lon, lat = lat, depth = depth, time = time,
    vars = vars, units = units, data = data,
    dataset_id = "d2a-hovmoller-test-v1"
  )
}

test_that("viz.hovmoller is the sole D2A export with the frozen signature", {
  expected <- c(
    "x", "variable", "axis", "longitude", "latitude", "depth",
    "time_from", "time_to", "match", "tolerance", "limits", "na.rm",
    "reverse_depth", "title", "subtitle", "caption"
  )

  expect_true("viz.hovmoller" %in% getNamespaceExports("oceancube"))
  expect_identical(names(formals(viz.hovmoller)), expected)
  expect_identical(
    formals(viz.hovmoller)$axis,
    quote(c("longitude", "latitude", "depth"))
  )
  expect_identical(
    formals(viz.hovmoller)$match,
    quote(c("exact", "nearest"))
  )
  expect_false(formals(viz.hovmoller)$na.rm)
  expect_true(formals(viz.hovmoller)$reverse_depth)
  expect_identical(length(getNamespaceExports("oceancube")), 49L)
})

test_that("time by longitude preserves exact selected values and roles", {
  cube <- hovmoller_test_cube()
  prepared <- .viz_prepare_hovmoller(
    cube, "temperature", axis = "longitude", latitude = -11, depth = 20
  )
  extracted <- cube_extract(
    cube, latitude = -11, depth = 20, time = cube$time,
    variable = "temperature", mode = "table", format = "long"
  )
  expected <- extracted[, c("time", "longitude", "value")]
  rownames(expected) <- NULL

  expect_identical(prepared$kind, "HOVMOLLER")
  expect_identical(prepared$schema_version, "1.0.0")
  expect_identical(prepared$data, expected)
  expect_identical(prepared$roles$x, "time")
  expect_identical(prepared$roles$y, "longitude")
  expect_identical(prepared$roles$time, "time")
  expect_identical(prepared$roles$longitude, "longitude")
  expect_null(prepared$roles$latitude)
  expect_true(prepared$support$no_hidden_reduction)
})

test_that("time by latitude and depth retain only exact two-dimensional support", {
  cube <- hovmoller_test_cube()
  latitude <- viz.hovmoller(
    cube, "temperature", axis = "latitude", longitude = -79, depth = 20
  )
  depth <- viz.hovmoller(
    cube, "temperature", axis = "depth", longitude = -79, latitude = -11
  )

  expect_s3_class(latitude, "ggplot")
  expect_s3_class(depth, "ggplot")
  expect_identical(names(latitude$data), c("time", "latitude", "value"))
  expect_identical(names(depth$data), c("time", "depth", "value"))
  expect_identical(nrow(latitude$data), length(cube$lat) * length(cube$time))
  expect_identical(nrow(depth$data), length(cube$depth) * length(cube$time))
  expect_true(inherits(latitude$layers[[1L]]$geom, "GeomTile"))
  expect_true(inherits(depth$layers[[1L]]$geom, "GeomTile"))
})

test_that("omitted non-singleton dimensions fail instead of being averaged", {
  cube <- hovmoller_test_cube()

  expect_error(
    viz.hovmoller(cube, "temperature", axis = "longitude"),
    "latitude.*non-display.*hidden reduction",
    class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.hovmoller(
      cube, "temperature", axis = "longitude", latitude = -11
    ),
    "depth.*non-display.*hidden reduction",
    class = "oceancube_viz_selection_error"
  )
})

test_that("implicit omission is allowed for singleton non-display dimensions", {
  cube <- hovmoller_test_cube(lat = -11, depth = 0)
  plot <- viz.hovmoller(cube, "temperature", axis = "longitude")

  expect_s3_class(plot, "ggplot")
  expect_identical(
    attr(plot, "oceancube_fixed_coordinates"),
    list(latitude = -11, depth = 0)
  )
})

test_that("display-axis selectors are rejected rather than ambiguously collapsed", {
  cube <- hovmoller_test_cube()

  expect_error(
    viz.hovmoller(
      cube, "temperature", axis = "longitude", longitude = -79,
      latitude = -11, depth = 20
    ),
    "longitude.*must be NULL",
    class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.hovmoller(
      cube, "temperature", axis = "depth", longitude = -79,
      latitude = -11, depth = 20
    ),
    "depth.*must be NULL",
    class = "oceancube_viz_selection_error"
  )
})

test_that("nearest fixed-coordinate selection never interpolates", {
  cube <- hovmoller_test_cube()
  tolerance <- list(longitude = 0.3, latitude = 0.3)
  plot <- viz.hovmoller(
    cube, "temperature", axis = "depth",
    longitude = -79.2, latitude = -11.2,
    match = "nearest", tolerance = tolerance
  )
  exact <- viz.hovmoller(
    cube, "temperature", axis = "depth",
    longitude = -79, latitude = -11
  )

  expect_identical(plot$data, exact$data)
  expect_identical(attr(plot, "oceancube_match"), "nearest")
  expect_identical(attr(plot, "oceancube_tolerance"), tolerance)
  expect_identical(
    attr(plot, "oceancube_fixed_coordinates"),
    list(longitude = -79, latitude = -11)
  )
})

test_that("inclusive time bounds preserve stored irregular time", {
  cube <- hovmoller_test_cube(lat = -11, depth = 0)
  plot <- viz.hovmoller(
    cube, "temperature", axis = "longitude",
    time_from = cube$time[[2L]], time_to = cube$time[[3L]]
  )

  expect_identical(unique(plot$data$time), cube$time[2:3])
  expect_identical(
    attr(plot, "oceancube_time_range"),
    range(cube$time[2:3])
  )
  expect_error(
    viz.hovmoller(
      cube, "temperature", axis = "longitude",
      time_from = cube$time[[3L]], time_to = cube$time[[2L]]
    ),
    "earlier"
  )
  expect_error(
    viz.hovmoller(
      cube, "temperature", axis = "longitude",
      time_from = cube$time[[4L]]
    ),
    "at least two stored time"
  )
})

test_that("irregular spacing is disclosed and rendered as stored-centre tiles", {
  cube <- hovmoller_test_cube(lat = -11, depth = c(0, 20, 75))
  prepared <- .viz_prepare_hovmoller(
    cube, "temperature", axis = "depth", longitude = -79
  )
  plot <- .viz_render_ggplot(prepared)

  expect_false(prepared$geometry$regular_time)
  expect_false(prepared$geometry$regular_axis)
  expect_identical(prepared$geometry$support, "STORED_CENTRES")
  expect_identical(
    prepared$geometry$renderer_geometry,
    "TILE_CENTRES_WITH_VISIBLE_GAPS"
  )
  expect_true(inherits(plot$layers[[1L]]$geom, "GeomTile"))
  expect_false(inherits(plot$layers[[1L]]$geom, "GeomRaster"))
})

test_that("missingness survives selection, serialization, and rendering", {
  values <- array(seq_len(3 * 4), dim = c(1, 1, 3, 4, 1))
  values[1, 1, 2, 3, 1] <- NA_real_
  cube <- hovmoller_test_cube(lon = -79, lat = -11, data = values)
  prepared <- .viz_prepare_hovmoller(cube, "temperature", axis = "depth")
  restored <- unserialize(serialize(prepared, NULL))
  plot <- .viz_render_ggplot(restored)

  expect_identical(sum(is.na(prepared$data$value)), 1L)
  expect_identical(restored$data, prepared$data)
  expect_true(is.na(plot$data$value[which(is.na(prepared$data$value))]))
  expect_identical(prepared$support$missing_values, 1L)
  expect_error(
    viz.hovmoller(
      hovmoller_test_cube(
        lon = -79, lat = -11,
        data = array(NA_real_, dim = c(1, 1, 3, 4, 1))
      ),
      "temperature", axis = "depth", na.rm = TRUE
    ),
    "empty after removing",
    class = "oceancube_viz_data_error"
  )
})

test_that("depth stays positive-down while only display orientation reverses", {
  cube <- hovmoller_test_cube(lon = -79, lat = -11)
  reversed <- viz.hovmoller(cube, "temperature", axis = "depth")
  forward <- viz.hovmoller(
    cube, "temperature", axis = "depth", reverse_depth = FALSE
  )

  expect_identical(unique(reversed$data$depth), cube$depth)
  expect_identical(reversed$scales$get_scales("y")$trans$name, "reverse")
  expect_null(forward$scales$get_scales("y"))
  expect_identical(attr(reversed, "oceancube_depth"), cube$depth)
})

test_that("limits and internal scale choices cannot alter science", {
  cube <- hovmoller_test_cube(lon = -79, lat = -11)
  plain <- .viz_prepare_hovmoller(cube, "temperature", axis = "depth")
  limited <- .viz_prepare_hovmoller(
    cube, "temperature", axis = "depth", limits = c(3, 8)
  )
  sequential <- .viz_prepare_hovmoller(
    cube, "temperature", axis = "depth", scale_class = "SEQUENTIAL"
  )

  for (item in list(limited, sequential)) {
    expect_identical(item$data, plain$data)
    expect_identical(item$coordinates, plain$coordinates)
    expect_identical(item$support, plain$support)
    expect_identical(item$provenance, plain$provenance)
    expect_identical(item$qa, plain$qa)
  }
  plot <- .viz_render_ggplot(limited)
  expect_identical(plot$scales$get_scales("fill")$limits, c(3, 8))
  expect_equal(plot$scales$get_scales("fill")$oob(c(1, 10), c(3, 8)), c(3, 8))
})

test_that("scale vocabulary is explicit and divergence never assumes zero", {
  expect_identical(
    .viz_scale_classes,
    c("SEQUENTIAL", "DIVERGING", "CYCLIC", "CATEGORICAL",
      "UNSPECIFIED_CONTINUOUS")
  )
  default <- .viz_scale_spec()
  palette <- .viz_palette_resolve(default)

  expect_identical(default$classification, "UNSPECIFIED_CONTINUOUS")
  expect_null(default$centre)
  expect_identical(palette$engine, "ggplot2::scale_fill_viridis_c")
  expect_identical(palette$family, "viridis")
  expect_error(
    .viz_scale_spec("DIVERGING"),
    "explicit scientifically meaningful",
    class = "oceancube_viz_scale_error"
  )
  expect_identical(.viz_scale_spec("DIVERGING", centre = 2)$centre, 2)
  expect_error(.viz_scale_spec("SEQUENTIAL", centre = 0), "only for a DIVERGING")
  expect_error(.viz_scale_spec(palette = "rainbow"), "only.*viridis")
})

test_that("labels and metadata are authoritative and bounded", {
  cube <- hovmoller_test_cube(lon = -79, lat = -11)
  attr(cube$depth, "units") <- "m"
  plot <- viz.hovmoller(
    cube, "temperature", axis = "depth", limits = c(1, 12),
    title = "Simulated section", subtitle = "Stored support", caption = "No fill"
  )

  expect_identical(plot$labels$x, "Time")
  expect_identical(plot$labels$y, "Depth (m)")
  expect_identical(plot$scales$get_scales("fill")$name, "temperature (degC)")
  expect_identical(plot$labels$title, "Simulated section")
  expect_identical(plot$labels$subtitle, "Stored support")
  expect_identical(plot$labels$caption, "No fill")
  expect_identical(attr(plot, "oceancube_variable"), "temperature")
  expect_identical(attr(plot, "oceancube_axis"), "depth")
  expect_identical(attr(plot, "oceancube_backend"), "memory")
  expect_identical(attr(plot, "oceancube_prepared_kind"), "HOVMOLLER")
  expect_true(is.list(attr(plot, "oceancube_source_semantics")))
})

test_that("invalid calls and malformed extraction output fail deterministically", {
  cube <- hovmoller_test_cube()

  expect_error(viz.hovmoller(list(), "temperature"),
               class = "oceancube_validation_error")
  expect_error(viz.hovmoller(cube, "temperature", axis = "distance"), "axis")
  expect_error(viz.hovmoller(cube, "unknown", axis = "depth",
                             longitude = -79, latitude = -11), "not present")
  expect_error(viz.hovmoller(cube, character(), axis = "depth",
                             longitude = -79, latitude = -11), "variable")
  expect_error(viz.hovmoller(cube, "temperature", axis = "depth",
                             longitude = c(-80, -79), latitude = -11), "longitude")
  expect_error(viz.hovmoller(cube, "temperature", axis = "depth",
                             longitude = -79, latitude = -11, match = "maybe"), "match")
  expect_error(viz.hovmoller(cube, "temperature", axis = "depth",
                             longitude = -79, latitude = -11, limits = c(1, 1)), "limits")
  expect_error(viz.hovmoller(cube, "temperature", axis = "depth",
                             longitude = -79, latitude = -11, na.rm = NA), "na.rm")
  expect_error(viz.hovmoller(cube, "temperature", axis = "depth",
                             longitude = -79, latitude = -11,
                             reverse_depth = 1), "reverse_depth")
  expect_error(viz.hovmoller(cube, "temperature", axis = "depth",
                             longitude = -79, latitude = -11,
                             time_from = "2020-01-01"), "time_from.*Date")
})

test_that("preparation uses cube_extract as its only payload selection layer", {
  cube <- hovmoller_test_cube(lon = -79, lat = -11)
  observed <- NULL
  original <- cube_extract
  local_mocked_bindings(
    cube_extract = function(x, longitude, latitude, depth, time, variable,
                            by, match, tolerance, mode, format, keep_index,
                            keep_distance) {
      observed <<- as.list(environment())
      original(
        x, longitude, latitude, depth, time, variable, by, match,
        tolerance, mode, format, keep_index, keep_distance
      )
    },
    .package = "oceancube"
  )

  plot <- viz.hovmoller(cube, "temperature", axis = "depth")

  expect_s3_class(plot, "ggplot")
  expect_identical(observed$mode, "table")
  expect_identical(observed$format, "long")
  expect_identical(observed$time, cube$time)
  expect_false(observed$keep_index)
  expect_false(observed$keep_distance)
  implementation <- paste(deparse(body(.viz_prepare_hovmoller)), collapse = "\n")
  expect_match(implementation, "cube_extract\\(")
  expect_false(grepl("\\.cube_read|ncvar_get|cube_collect|\\$data", implementation))
})

test_that("NetCDF preparation is one bounded read and rendering is source-free", {
  file <- make_netcdf_backend_fixture()
  cube <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )
  read_count <- 0L
  original_read <- .cube_read
  local_mocked_bindings(
    .cube_read = function(x, index = NULL, drop = FALSE) {
      read_count <<- read_count + 1L
      original_read(x, index = index, drop = drop)
    },
    .package = "oceancube"
  )
  prepared <- .viz_prepare_hovmoller(
    cube, "temperature", axis = "depth",
    longitude = cube$lon[[2L]], latitude = cube$lat[[2L]]
  )
  read_evidence <- prepared$qa$extraction$netcdf_read

  expect_identical(read_count, 1L)
  expect_identical(
    read_evidence$physical_count,
    c(longitude = 1L, latitude = 1L, depth = 2L, time = 4L)
  )
  expect_identical(read_evidence$variables, "temperature")
  expect_lt(read_evidence$values_in_envelope, prod(c(3L, 2L, 2L, 4L, 2L)))
  expect_identical(unlink(file), 0L)
  local_mocked_bindings(
    .cube_read = function(...) stop("renderer attempted a backend read"),
    .package = "oceancube"
  )
  expect_s3_class(.viz_render_ggplot(prepared), "ggplot")
  expect_identical(read_count, 1L)
})

test_that("memory and NetCDF prepared science agree", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  netcdf <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )
  memory <- cube_collect(netcdf)
  netcdf_prepared <- .viz_prepare_hovmoller(
    netcdf, "temperature", axis = "depth",
    longitude = netcdf$lon[[2L]], latitude = netcdf$lat[[2L]]
  )
  memory_prepared <- .viz_prepare_hovmoller(
    memory, "temperature", axis = "depth",
    longitude = memory$lon[[2L]], latitude = memory$lat[[2L]]
  )

  expect_identical(netcdf_prepared$data, memory_prepared$data)
  expect_identical(netcdf_prepared$roles, memory_prepared$roles)
  expect_identical(netcdf_prepared$geometry, memory_prepared$geometry)
  expect_identical(netcdf_prepared$scale, memory_prepared$scale)
  expect_identical(netcdf_prepared$variables, memory_prepared$variables)
})

test_that("prepared HOVMOLLER state is serializable and has no private path", {
  prepared <- .viz_prepare_hovmoller(
    hovmoller_test_cube(lon = -79, lat = -11),
    "temperature", axis = "depth"
  )
  restored <- unserialize(serialize(prepared, NULL))
  rendered <- paste(capture.output(str(prepared)), collapse = "\n")

  expect_identical(restored, prepared)
  expect_true(.validate_oceancube_viz_data(restored))
  expect_false(any(vapply(prepared, is.environment, logical(1))))
  expect_false(grepl("[A-Za-z]:[/\\\\]", rendered))
  expect_s3_class(.viz_render_ggplot(restored), "ggplot")
})

test_that("calendar-aware non-base time is explicitly unsupported", {
  file <- make_netcdf_backend_fixture(calendar = "360_day")
  withr::local_file(file)
  cube <- .new_netcdf_cube(.new_netcdf_storage(file, "temperature"))

  expect_error(
    viz.hovmoller(
      cube, "temperature", axis = "depth",
      longitude = cube$lon[[1L]], latitude = cube$lat[[1L]]
    ),
    class = "oceancube_cf_time_unsupported_operation"
  )
})
