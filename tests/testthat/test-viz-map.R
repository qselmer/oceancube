map_test_cube <- function(lon = c(-80, -79, -78), lat = c(-12, -11),
                          depth = 0, time = as.Date("2020-01-01"),
                          vars = "temperature", units = "degC", data = NULL) {
  if (is.null(data)) {
    data <- array(
      seq_len(length(lon) * length(lat) * length(depth) * length(time) * length(vars)),
      dim = c(length(lon), length(lat), length(depth), length(time), length(vars))
    )
  }
  ocean_cube(
    lon = lon, lat = lat, depth = depth, time = time, vars = vars,
    units = units, data = data
  )
}

test_that("viz.map returns a ggplot for memory and surface cubes", {
  skip_if_not_installed("ggplot2")
  cube <- map_test_cube(depth = NA_real_)

  plot <- viz.map(cube, "temperature")

  expect_s3_class(plot, "ggplot")
  expect_identical(names(plot$data), c("longitude", "latitude", "value"))
  expect_identical(nrow(plot$data), 6L)
})

test_that("single and explicit time and depth selections are exact", {
  skip_if_not_installed("ggplot2")
  single <- map_test_cube(depth = 50)
  multiple <- map_test_cube(
    depth = c(0, 50),
    time = as.Date(c("2020-01-01", "2020-02-01"))
  )

  single_plot <- viz.map(single, "temperature")
  selected <- viz.map(
    multiple,
    "temperature",
    time = as.Date("2020-02-01"),
    depth = 50
  )

  expect_identical(attr(single_plot, "oceancube_depth"), 50)
  expect_identical(attr(selected, "oceancube_time"), as.Date("2020-02-01"))
  expect_identical(attr(selected, "oceancube_depth"), 50)
  expect_error(
    viz.map(multiple, "temperature", depth = 50),
    "time.*exactly one",
    class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.map(multiple, "temperature", time = as.Date("2020-01-01")),
    "depth.*exactly one",
    class = "oceancube_viz_selection_error"
  )
  expect_error(
    viz.map(multiple, "temperature", time = as.Date("1999-01-01"), depth = 0),
    "Could not select",
    class = "oceancube_viz_selection_error"
  )
})

test_that("variables are explicit and scale labels include units", {
  skip_if_not_installed("ggplot2")
  cube <- map_test_cube()

  plot <- viz.map(cube, "temperature")

  expect_identical(plot$scales$get_scales("fill")$name, "temperature (degC)")
  expect_error(
    viz.map(cube, "oxygen"),
    "not present",
    class = "oceancube_viz_selection_error"
  )
  expect_error(viz.map(cube, character()), class = "oceancube_viz_selection_error")
})

test_that("regular grids use raster and irregular grids use tile", {
  skip_if_not_installed("ggplot2")
  regular <- viz.map(map_test_cube(), "temperature")
  irregular <- viz.map(
    map_test_cube(lon = c(-80, -79.25, -78)),
    "temperature"
  )

  expect_true(inherits(regular$layers[[1L]]$geom, "GeomRaster"))
  expect_true(inherits(irregular$layers[[1L]]$geom, "GeomTile"))
  expect_s3_class(regular$coordinates, "CoordCartesian")
  expect_identical(regular$coordinates$ratio, 1)
  expect_false(regular$coordinates$expand)
})

test_that("limits control the scale without dropping data", {
  skip_if_not_installed("ggplot2")
  cube <- map_test_cube()
  plot <- viz.map(cube, "temperature", limits = c(2, 4))

  expect_identical(plot$scales$get_scales("fill")$limits, c(2, 4))
  expect_identical(nrow(plot$data), 6L)
  expect_true(any(plot$data$value < 2 | plot$data$value > 4))
  expect_error(viz.map(cube, "temperature", limits = c(1, 1)), "limits")
  expect_error(viz.map(cube, "temperature", limits = c(1, Inf)), "limits")
  expect_error(viz.map(cube, "temperature", na.rm = NA), "na.rm")
})

test_that("coastline layers are optional and data frames are validated", {
  skip_if_not_installed("ggplot2")
  cube <- map_test_cube()
  coast <- data.frame(
    longitude = c(-80, -79, -78),
    latitude = c(-11.5, -11.25, -11),
    group = 1L
  )

  no_coast <- viz.map(cube, "temperature")
  with_coast <- viz.map(cube, "temperature", coastline = coast)

  expect_identical(length(no_coast$layers), 1L)
  expect_identical(length(with_coast$layers), 2L)
  expect_true(inherits(with_coast$layers[[2L]]$geom, "GeomPath"))
  expect_error(
    viz.map(cube, "temperature", coastline = data.frame(x = 1, y = 2)),
    "coastline data frame",
    class = "oceancube_viz_data_error"
  )
  expect_error(
    viz.map(cube, "temperature", coastline = matrix(1:4, 2)),
    "coastline",
    class = "oceancube_viz_data_error"
  )
})

test_that("sf and sfc coastlines are supported without transformation", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("sf")
  cube <- map_test_cube()
  line <- sf::st_sfc(sf::st_linestring(matrix(
    c(-80, -12, -78, -11), ncol = 2, byrow = TRUE
  )), crs = 4326)

  plot <- viz.map(cube, "temperature", coastline = line)

  expect_true(inherits(plot$layers[[2L]]$geom, "GeomSf"))
  expect_identical(sf::st_crs(line)$epsg, 4326L)
})

test_that("metadata attributes identify the selected layer", {
  skip_if_not_installed("ggplot2")
  cube <- map_test_cube()
  plot <- viz.map(cube, "temperature")

  expect_identical(attr(plot, "oceancube_variable"), "temperature")
  expect_identical(attr(plot, "oceancube_time"), as.Date("2020-01-01"))
  expect_identical(attr(plot, "oceancube_depth"), 0)
  expect_identical(attr(plot, "oceancube_backend"), "memory")
})

test_that("invalid, duplicate, nonnumeric, and empty layers fail clearly", {
  skip_if_not_installed("ggplot2")
  cube <- map_test_cube()
  invalid <- cube
  invalid$lat[1L] <- -100
  duplicated <- map_test_cube(lon = c(-80, -80, -78))

  expect_error(viz.map(invalid, "temperature"), class = "oceancube_validation_error")
  expect_error(
    viz.map(duplicated, "temperature"),
    "duplicate longitude",
    class = "oceancube_viz_data_error"
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
  expect_error(viz.map(cube, "temperature"), "must be numeric",
               class = "oceancube_viz_data_error")
})

test_that("all-missing layers are rejected when na.rm is true", {
  skip_if_not_installed("ggplot2")
  cube <- map_test_cube(data = array(NA_real_, dim = c(3, 2, 1, 1, 1)))

  expect_error(
    viz.map(cube, "temperature"),
    "empty after removing missing",
    class = "oceancube_viz_data_error"
  )
  expect_s3_class(viz.map(cube, "temperature", na.rm = FALSE), "ggplot")
})

test_that("viz.map delegates layer selection to cube_extract", {
  skip_if_not_installed("ggplot2")
  cube <- map_test_cube()
  called <- FALSE
  local_mocked_bindings(
    cube_extract = function(...) {
      called <<- TRUE
      out <- data.frame(
        longitude = c(-80, -79, -78, -80, -79, -78),
        latitude = rep(c(-12, -11), each = 3),
        depth = 0,
        time = as.Date("2020-01-01"),
        variable = "temperature",
        unit = "degC",
        value = 1:6
      )
      attr(out, "oceancube_backend") <- "memory"
      out
    },
    .package = "oceancube"
  )

  viz.map(cube, "temperature")

  expect_true(called)
})

test_that("NetCDF maps read only one selected layer", {
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

  plot <- viz.map(
    cube,
    "temperature",
    time = as.POSIXct("2000-01-01", tz = "UTC"),
    depth = 0
  )

  expect_s3_class(plot, "ggplot")
  expect_identical(lengths(observed), c(
    longitude = 3L, latitude = 2L, depth = 1L, time = 1L, variable = 1L
  ))
  expect_identical(attr(plot, "oceancube_backend"), "netcdf")
})
