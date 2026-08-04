section_test_cube <- function(
    lon = c(-80, -79, -78), lat = c(-12, -11),
    depth = c(0, 25, 50, 100), time = as.Date("2020-01-01"),
    vars = "temperature", units = "degC", data = NULL) {
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

test_that("viz.section returns a longitude-depth ggplot with canonical data", {
  skip_if_not_installed("ggplot2")
  cube <- section_test_cube(lat = -11)

  plot <- viz.section(cube, "temperature")

  expect_s3_class(plot, "ggplot")
  expect_identical(names(plot$data), c("longitude", "depth", "value"))
  expect_identical(nrow(plot$data), 12L)
  expect_identical(plot$labels$x, "Longitude")
  expect_identical(plot$labels$y, "Depth")
})

test_that("latitude-depth sections fix exactly one longitude", {
  skip_if_not_installed("ggplot2")
  cube <- section_test_cube(lon = -79, lat = c(-12, -11, -10))

  plot <- viz.section(cube, "temperature", section = "latitude-depth")

  expect_identical(names(plot$data), c("latitude", "depth", "value"))
  expect_identical(nrow(plot$data), 12L)
  expect_identical(plot$labels$x, "Latitude")
  expect_identical(attr(plot, "oceancube_fixed_coordinate"),
                   c(longitude = -79))
})

test_that("time and fixed-coordinate selections are exact", {
  skip_if_not_installed("ggplot2")
  cube <- section_test_cube(
    time = as.Date(c("2020-01-01", "2020-02-01"))
  )

  plot <- viz.section(
    cube, "temperature", time = as.Date("2020-02-01"), latitude = -11
  )

  expect_identical(attr(plot, "oceancube_time"), as.Date("2020-02-01"))
  expect_identical(attr(plot, "oceancube_fixed_coordinate"), c(latitude = -11))
  expect_error(
    viz.section(cube, "temperature", time = as.Date("1999-01-01"), latitude = -11),
    "Could not select", class = "oceancube_viz_selection_error"
  )
})

test_that("ambiguous omitted selectors fail clearly", {
  skip_if_not_installed("ggplot2")
  multiple_time <- section_test_cube(
    lat = -11, time = as.Date(c("2020-01-01", "2020-02-01"))
  )
  multiple_lat <- section_test_cube()
  multiple_lon <- section_test_cube(lon = c(-80, -79), lat = c(-12, -11))

  expect_error(
    viz.section(multiple_time, "temperature"),
    "time.*exactly one", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.section(multiple_lat, "temperature"),
    "latitude.*exactly one", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.section(multiple_lon, "temperature", section = "latitude-depth"),
    "longitude.*exactly one", class = "oceancube_viz_selection_error"
  )
})

test_that("surface and one-level cubes cannot form vertical sections", {
  skip_if_not_installed("ggplot2")
  surface <- section_test_cube(lat = -11, depth = NA_real_)
  one_level <- section_test_cube(lat = -11, depth = 10)

  expect_error(
    viz.section(surface, "temperature"),
    "surface cube", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.section(one_level, "temperature"),
    "at least two", class = "oceancube_viz_selection_error"
  )
})

test_that("depth subsets retain exact stored levels", {
  skip_if_not_installed("ggplot2")
  cube <- section_test_cube(lat = -11)

  plot <- viz.section(cube, "temperature", depth = c(0, 50))

  expect_identical(sort(unique(plot$data$depth)), c(0, 50))
  expect_identical(attr(plot, "oceancube_depth_range"), c(0, 50))
  expect_error(
    viz.section(cube, "temperature", depth = 0),
    "at least two", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.section(cube, "temperature", depth = c(0, 999)),
    "Could not select", class = "oceancube_viz_selection_error"
  )
})

test_that("variables, sections, and incompatible selectors are validated", {
  skip_if_not_installed("ggplot2")
  cube <- section_test_cube(lat = -11)

  expect_error(
    viz.section(cube, "oxygen"),
    "not present", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.section(cube, "temperature", section = "diagonal"),
    "section", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.section(cube, "temperature", longitude = -79),
    "longitude.*must be NULL", class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.section(
      section_test_cube(lon = -79), "temperature",
      section = "latitude-depth", latitude = -11
    ),
    "latitude.*must be NULL", class = "oceancube_viz_selection_error"
  )
})

test_that("regular grids use raster and irregular grids use tile", {
  skip_if_not_installed("ggplot2")
  regular <- viz.section(
    section_test_cube(lat = -11, depth = c(0, 25, 50, 75)),
    "temperature"
  )
  irregular <- viz.section(
    section_test_cube(lat = -11, depth = c(0, 20, 55, 100)),
    "temperature"
  )

  expect_true(inherits(regular$layers[[1L]]$geom, "GeomRaster"))
  expect_true(inherits(irregular$layers[[1L]]$geom, "GeomTile"))
  expect_false(inherits(regular$coordinates, "CoordFixed"))
})

test_that("fill labels, limits, and missing-value handling preserve data", {
  skip_if_not_installed("ggplot2")
  cube <- section_test_cube(lat = -11)
  plot <- viz.section(cube, "temperature", limits = c(2, 8))

  expect_identical(plot$scales$get_scales("fill")$name, "temperature (degC)")
  expect_identical(plot$scales$get_scales("fill")$limits, c(2, 8))
  expect_identical(nrow(plot$data), 12L)
  expect_true(any(plot$data$value < 2 | plot$data$value > 8))
  expect_error(viz.section(cube, "temperature", limits = c(1, 1)), "limits")
  expect_error(viz.section(cube, "temperature", limits = c(1, Inf)), "limits")
  expect_error(viz.section(cube, "temperature", na.rm = NA), "na.rm")
})

test_that("depth is reversed by default and can retain its direction", {
  skip_if_not_installed("ggplot2")
  cube <- section_test_cube(lat = -11)
  reversed <- viz.section(cube, "temperature")
  forward <- viz.section(cube, "temperature", reverse_depth = FALSE)

  expect_identical(reversed$scales$get_scales("y")$trans$name, "reverse")
  expect_null(forward$scales$get_scales("y"))
  expect_error(
    viz.section(cube, "temperature", reverse_depth = NA),
    "reverse_depth"
  )
})

test_that("metadata attributes identify the selected section", {
  skip_if_not_installed("ggplot2")
  cube <- section_test_cube(lat = -11)
  plot <- viz.section(cube, "temperature")

  expect_identical(attr(plot, "oceancube_variable"), "temperature")
  expect_identical(attr(plot, "oceancube_time"), as.Date("2020-01-01"))
  expect_identical(attr(plot, "oceancube_section"), "longitude-depth")
  expect_identical(attr(plot, "oceancube_fixed_coordinate"), c(latitude = -11))
  expect_identical(attr(plot, "oceancube_depth_range"), c(0, 100))
  expect_identical(attr(plot, "oceancube_backend"), "memory")
})

test_that("invalid cubes and malformed extracted data fail clearly", {
  skip_if_not_installed("ggplot2")
  cube <- section_test_cube(lat = -11)
  invalid <- cube
  invalid$lat <- -100

  expect_error(
    viz.section(invalid, "temperature"),
    class = "oceancube_validation_error"
  )

  original_extract <- cube_extract
  local_mocked_bindings(
    cube_extract = function(...) {
      out <- original_extract(...)
      out$value <- as.character(out$value)
      out
    },
    .package = "oceancube"
  )
  expect_error(
    viz.section(cube, "temperature"),
    "must be numeric", class = "oceancube_viz_data_error"
  )
})

test_that("duplicate cells are rejected without aggregation", {
  skip_if_not_installed("ggplot2")
  cube <- section_test_cube(lat = -11)
  original_extract <- cube_extract
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
    viz.section(cube, "temperature"),
    "duplicate longitude x depth", class = "oceancube_viz_data_error"
  )
})

test_that("all-missing sections respect na.rm", {
  skip_if_not_installed("ggplot2")
  cube <- section_test_cube(
    lat = -11,
    data = array(NA_real_, dim = c(3, 1, 4, 1, 1))
  )

  expect_error(
    viz.section(cube, "temperature"),
    "empty after removing missing", class = "oceancube_viz_data_error"
  )
  expect_s3_class(
    viz.section(cube, "temperature", na.rm = FALSE),
    "ggplot"
  )
})

test_that("viz.section delegates exact plane selection to cube_extract", {
  skip_if_not_installed("ggplot2")
  cube <- section_test_cube()
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
        longitude = rep(c(-80, -79, -78), 2), latitude = -11,
        depth = rep(c(0, 50), each = 3), time = as.Date("2020-01-01"),
        variable = "temperature", unit = "degC", value = 1:6
      )
      attr(out, "oceancube_backend") <- "memory"
      out
    },
    .package = "oceancube"
  )

  viz.section(cube, "temperature", latitude = -11, depth = c(0, 50))

  expect_identical(called$latitude, -11)
  expect_identical(called$depth, c(0, 50))
  expect_identical(called$variable, "temperature")
  expect_identical(called$by, "value")
  expect_identical(called$match, "exact")
  expect_identical(called$mode, "table")
  expect_identical(called$format, "long")
})

test_that("NetCDF sections read only the selected vertical plane", {
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

  plot <- viz.section(
    cube,
    "temperature",
    time = as.POSIXct("2000-01-01", tz = "UTC"),
    latitude = -11
  )

  expect_s3_class(plot, "ggplot")
  expect_identical(lengths(observed), c(
    longitude = 3L, latitude = 1L, depth = 2L, time = 1L, variable = 1L
  ))
  expect_identical(attr(plot, "oceancube_backend"), "netcdf")
})
