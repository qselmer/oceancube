profile_test_cube <- function(
    lon = c(-80, -79), lat = c(-12, -11),
    depth = structure(c(0, 25, 50, 100), units = "m"),
    time = as.Date("2020-01-01"), vars = "temperature",
    units = "degC", data = NULL) {
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

test_that("viz.profile returns a sorted value-by-depth ggplot", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube(lon = -79, lat = -11)

  plot <- viz.profile(cube, "temperature")

  expect_s3_class(plot, "ggplot")
  expect_identical(names(plot$data), c("depth", "value"))
  expect_identical(plot$data$depth, c(0, 25, 50, 100))
  expect_true(inherits(plot$layers[[1L]]$geom, "GeomLine"))
  expect_identical(plot$layers[[1L]]$geom_params$orientation, "y")
  expect_true(inherits(plot$layers[[2L]]$geom, "GeomPoint"))
})

test_that("points can be disabled without removing the profile line", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube(lon = -79, lat = -11)

  plot <- viz.profile(cube, "temperature", points = FALSE)

  expect_identical(length(plot$layers), 1L)
  expect_true(inherits(plot$layers[[1L]]$geom, "GeomLine"))
})

test_that("single spatial and temporal axes permit NULL selectors", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube(lon = -79, lat = -11)

  plot <- viz.profile(cube, "temperature")

  expect_identical(attr(plot, "oceancube_longitude"), -79)
  expect_identical(attr(plot, "oceancube_latitude"), -11)
  expect_identical(attr(plot, "oceancube_time"), as.Date("2020-01-01"))
})

test_that("explicit longitude, latitude, and time selectors match exactly", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube(
    time = as.Date(c("2020-01-01", "2020-02-01"))
  )

  plot <- viz.profile(
    cube, "temperature", longitude = -79, latitude = -11,
    time = as.Date("2020-02-01")
  )

  expect_identical(attr(plot, "oceancube_longitude"), -79)
  expect_identical(attr(plot, "oceancube_latitude"), -11)
  expect_identical(attr(plot, "oceancube_time"), as.Date("2020-02-01"))
})

test_that("ambiguous NULL spatial and temporal selectors fail", {
  skip_if_not_installed("ggplot2")
  spatial <- profile_test_cube()
  temporal <- profile_test_cube(
    lon = -79, lat = -11,
    time = as.Date(c("2020-01-01", "2020-02-01"))
  )

  expect_error(
    viz.profile(spatial, "temperature", latitude = -11),
    "longitude.*exactly one", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.profile(spatial, "temperature", longitude = -79),
    "latitude.*exactly one", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.profile(temporal, "temperature"),
    "time.*exactly one", class = "oceancube_viz_selection_error"
  )
})

test_that("unknown and inexact selectors fail without nearest matching", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube(
    time = as.Date(c("2020-01-01", "2020-02-01"))
  )

  expect_error(
    viz.profile(cube, "oxygen", longitude = -79, latitude = -11,
                time = as.Date("2020-01-01")),
    "not present", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.profile(cube, "temperature", longitude = -79.5, latitude = -11,
                time = as.Date("2020-01-01")),
    "Could not select", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.profile(cube, "temperature", longitude = -79, latitude = -11.5,
                time = as.Date("2020-01-01")),
    "Could not select", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.profile(cube, "temperature", longitude = -79, latitude = -11,
                time = as.Date("1999-01-01")),
    "Could not select", class = "oceancube_viz_selection_error"
  )
})

test_that("surface and one-depth cubes cannot form profiles", {
  skip_if_not_installed("ggplot2")
  surface <- profile_test_cube(lon = -79, lat = -11, depth = NA_real_)
  one_depth <- profile_test_cube(lon = -79, lat = -11, depth = 10)

  expect_error(
    viz.profile(surface, "temperature"),
    "surface cube", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.profile(one_depth, "temperature"),
    "at least two", class = "oceancube_viz_selection_error"
  )
})

test_that("depth subsets retain at least two exact stored levels", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube(lon = -79, lat = -11)

  plot <- viz.profile(cube, "temperature", depth = c(0, 50))

  expect_identical(plot$data$depth, c(0, 50))
  expect_identical(attr(plot, "oceancube_depth_range"), c(0, 50))
  expect_error(
    viz.profile(cube, "temperature", depth = 0),
    "at least two", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.profile(cube, "temperature", depth = c(0, 999)),
    "Could not select", class = "oceancube_viz_selection_error"
  )
})

test_that("duplicate, missing, and non-finite depth axes are rejected", {
  skip_if_not_installed("ggplot2")
  duplicated <- profile_test_cube(
    lon = -79, lat = -11, depth = c(0, 10, 10, 20)
  )
  missing <- profile_test_cube(lon = -79, lat = -11)
  missing$depth <- c(0, 10, NA_real_, 30)
  infinite <- profile_test_cube(lon = -79, lat = -11)
  infinite$depth <- c(0, 10, Inf, 30)

  expect_error(
    viz.profile(duplicated, "temperature"),
    class = "oceancube_viz_selection_error"
  )
  expect_error(viz.profile(missing, "temperature"))
  expect_error(viz.profile(infinite, "temperature"))
})

test_that("depth orientation and axis labels preserve metadata", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube(lon = -79, lat = -11)
  reversed <- viz.profile(cube, "temperature")
  forward <- viz.profile(cube, "temperature", reverse_depth = FALSE)

  expect_identical(reversed$scales$get_scales("y")$trans$name, "reverse")
  expect_null(forward$scales$get_scales("y"))
  expect_identical(reversed$labels$x, "temperature (degC)")
  expect_identical(reversed$labels$y, "Depth (m)")
})

test_that("profiles without units use plain axis labels", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube(
    lon = -79, lat = -11, depth = c(0, 25, 50, 100), units = NULL
  )

  plot <- viz.profile(cube, "temperature")

  expect_identical(plot$labels$x, "temperature")
  expect_identical(plot$labels$y, "Depth")
})

test_that("limits control the value scale without filtering rows", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube(lon = -79, lat = -11)
  plot <- viz.profile(cube, "temperature", limits = c(2, 3))

  expect_identical(plot$scales$get_scales("x")$limits, c(2, 3))
  expect_identical(nrow(plot$data), 4L)
  expect_true(any(plot$data$value < 2 | plot$data$value > 3))
  expect_error(viz.profile(cube, "temperature", limits = c(1, 1)), "limits")
  expect_error(viz.profile(cube, "temperature", limits = c(1, Inf)), "limits")
})

test_that("logical flags and labels are validated", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube(lon = -79, lat = -11)

  expect_error(viz.profile(cube, "temperature", na.rm = NA), "na.rm")
  expect_error(
    viz.profile(cube, "temperature", reverse_depth = NA), "reverse_depth"
  )
  expect_error(viz.profile(cube, "temperature", points = NA), "points")
  expect_error(viz.profile(cube, "temperature", title = NA_character_), "title")
  expect_error(viz.profile(cube, "temperature", subtitle = character()), "subtitle")
  expect_error(viz.profile(cube, "temperature", caption = 1), "caption")
})

test_that("metadata attributes identify the selected profile", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube(lon = -79, lat = -11)
  plot <- viz.profile(cube, "temperature")

  expect_identical(attr(plot, "oceancube_variable"), "temperature")
  expect_identical(attr(plot, "oceancube_longitude"), -79)
  expect_identical(attr(plot, "oceancube_latitude"), -11)
  expect_identical(attr(plot, "oceancube_time"), as.Date("2020-01-01"))
  expect_identical(attr(plot, "oceancube_depth_range"), c(0, 100))
  expect_identical(attr(plot, "oceancube_backend"), "memory")
})

test_that("viz.profile does not mutate its input cube", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube(lon = -79, lat = -11)
  before <- serialize(cube, NULL)

  viz.profile(cube, "temperature")

  expect_identical(serialize(cube, NULL), before)
})

test_that("invalid cubes fail strict validation", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube(lon = -79, lat = -11)
  cube$lat <- -100

  expect_error(
    viz.profile(cube, "temperature"),
    class = "oceancube_validation_error"
  )
})

test_that("zero-row, duplicate, and nonnumeric extracted profiles fail", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube(lon = -79, lat = -11)
  original_extract <- cube_extract

  local_mocked_bindings(
    cube_extract = function(...) {
      out <- original_extract(...)
      out[FALSE, , drop = FALSE]
    },
    .package = "oceancube"
  )
  expect_error(
    viz.profile(cube, "temperature"),
    "empty", class = "oceancube_viz_data_error"
  )

  local_mocked_bindings(
    cube_extract = function(...) {
      out <- original_extract(...)
      out <- rbind(out, out[1L, , drop = FALSE])
      attr(out, "oceancube_backend") <- "memory"
      out
    },
    .package = "oceancube"
  )
  expect_error(
    viz.profile(cube, "temperature"),
    "duplicate depth", class = "oceancube_viz_data_error"
  )

  local_mocked_bindings(
    cube_extract = function(...) {
      out <- original_extract(...)
      out$value <- as.character(out$value)
      out
    },
    .package = "oceancube"
  )
  expect_error(
    viz.profile(cube, "temperature"),
    "must be numeric", class = "oceancube_viz_data_error"
  )
})

test_that("all-missing profiles respect na.rm", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube(
    lon = -79, lat = -11,
    data = array(NA_real_, dim = c(1, 1, 4, 1, 1))
  )

  expect_error(
    viz.profile(cube, "temperature"),
    "empty after removing missing", class = "oceancube_viz_data_error"
  )
  expect_s3_class(
    viz.profile(cube, "temperature", na.rm = FALSE),
    "ggplot"
  )
})

test_that("viz.profile delegates exact profile selection to cube_extract", {
  skip_if_not_installed("ggplot2")
  cube <- profile_test_cube()
  called <- NULL
  local_mocked_bindings(
    cube_extract = function(x, longitude = NULL, latitude = NULL,
                            depth = NULL, time = NULL, variable = NULL,
                            by = NULL, match = NULL, mode = NULL,
                            format = NULL, ...) {
      called <<- list(
        longitude = longitude, latitude = latitude, depth = depth,
        time = time, variable = variable, by = by, match = match,
        mode = mode, format = format
      )
      out <- data.frame(
        longitude = -79, latitude = -11, depth = c(0, 50),
        time = as.Date("2020-01-01"), variable = "temperature",
        unit = "degC", value = c(18, 12)
      )
      attr(out, "oceancube_backend") <- "memory"
      out
    },
    .package = "oceancube"
  )

  viz.profile(
    cube, "temperature", longitude = -79, latitude = -11,
    depth = c(0, 50)
  )

  expect_identical(called$longitude, -79)
  expect_identical(called$latitude, -11)
  expect_identical(called$depth, c(0, 50))
  expect_identical(called$variable, "temperature")
  expect_identical(called$by, "value")
  expect_identical(called$match, "exact")
  expect_identical(called$mode, "profile")
  expect_identical(called$format, "long")
})

test_that("NetCDF profiles read only one vertical column", {
  skip_if_not_installed("ggplot2")
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  cube <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )
  observed <- NULL
  original_read <- .cube_read
  local_mocked_bindings(
    .cube_read = function(x, index = NULL, drop = FALSE) {
      observed <<- index
      original_read(x, index = index, drop = drop)
    },
    .package = "oceancube"
  )

  plot <- viz.profile(
    cube, "temperature", longitude = -79, latitude = -11,
    time = as.POSIXct("2000-01-01", tz = "UTC")
  )

  expect_s3_class(plot, "ggplot")
  expect_identical(lengths(observed), c(
    longitude = 1L, latitude = 1L, depth = 2L, time = 1L, variable = 1L
  ))
  expect_identical(attr(plot, "oceancube_backend"), "netcdf")
})
