test_that("cell areas have stable shape, names, units, and provenance", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  m2 <- cube_cell_area(x, "m2")
  km2 <- cube_cell_area(x, "km2")

  expect_identical(dim(m2), c(2L, 2L))
  expect_identical(names(dimnames(m2)), c("longitude", "latitude"))
  expect_identical(attr(m2, "unit"), "m2")
  expect_identical(attr(m2, "crs"), "EPSG:4326")
  expect_match(attr(m2, "method"), "s2")
  expect_equal(as.numeric(m2) / 1e6, as.numeric(km2), tolerance = 1e-12)
  expect_true(all(m2 > 0))
  expect_lt(m2[1, 2], m2[1, 1])
})

test_that("regular and irregular bounds are inferred from centres", {
  skip_if_not_installed("sf")
  regular <- geometry_test_cube(lon = c(0, 2), lat = c(-2, 0, 2))
  irregular <- geometry_test_cube(
    lon = c(0, 1, 4),
    lat = c(-3, -1, 2)
  )

  regular_area <- cube_cell_area(regular)
  irregular_area <- cube_cell_area(irregular)
  expect_identical(
    unname(attr(regular_area, "bounds_source")),
    rep("inferred_from_centres", 2)
  )
  expect_identical(dim(irregular_area), c(3L, 3L))
  expect_gt(irregular_area[3, 2], irregular_area[1, 2])
})

test_that("ascending and descending axes preserve cube index order", {
  skip_if_not_installed("sf")
  ascending <- geometry_test_cube(
    lon = c(-2, 0, 3),
    lat = c(-1, 2)
  )
  descending <- geometry_test_cube(
    lon = rev(ascending$lon),
    lat = rev(ascending$lat)
  )
  a <- cube_cell_area(ascending)
  d <- cube_cell_area(descending)

  expect_equal(as.numeric(d), as.numeric(a[3:1, 2:1]), tolerance = 1e-12)
  expect_identical(rownames(d), as.character(descending$lon))
  expect_identical(colnames(d), as.character(descending$lat))
})

test_that("explicit interface and paired bounds are equivalent", {
  skip_if_not_installed("sf")
  lon_interface <- structure(c(0, 2), bounds = c(-1, 1, 4))
  lon_pairs <- structure(c(0, 2), bounds = matrix(c(-1, 1, 1, 4), 2, 2,
                                                   byrow = TRUE))
  lat <- structure(c(-1, 1), bounds = c(-2, 0, 2))
  expect_equal(
    cube_cell_area(geometry_test_cube(lon = lon_interface, lat = lat)),
    cube_cell_area(geometry_test_cube(lon = lon_pairs, lat = lat)),
    tolerance = 1e-12
  )
})

test_that("singleton horizontal axes require explicit bounds", {
  skip_if_not_installed("sf")
  no_lon_bounds <- geometry_test_cube(lon = 0)
  no_lat_bounds <- geometry_test_cube(lat = 0)
  explicit <- geometry_test_cube(
    lon = structure(0, bounds = c(-1, 1)),
    lat = structure(0, bounds = matrix(c(-2, 2), 1, 2))
  )

  expect_error(cube_cell_area(no_lon_bounds), "singleton")
  expect_error(cube_cell_area(no_lat_bounds), "singleton")
  expect_identical(dim(cube_cell_area(explicit)), c(1L, 1L))
})

test_that("invalid rectilinear grids and horizontal bounds fail explicitly", {
  skip_if_not_installed("sf")
  expect_error(
    cube_cell_area(geometry_test_cube(lon = c(0, 0, 1))),
    "strictly monotonic"
  )
  expect_error(
    cube_cell_area(geometry_test_cube(lat = c(0, 2, 1))),
    "strictly monotonic"
  )
  bad_count <- structure(c(0, 1), bounds = c(-1, 0, 1, 2))
  expect_error(
    cube_cell_area(geometry_test_cube(lon = bad_count)),
    "length 3"
  )
  overlap <- structure(
    c(0, 1),
    bounds = matrix(c(-1, 0.75, 0.5, 1.5), 2, 2, byrow = TRUE)
  )
  expect_error(
    cube_cell_area(geometry_test_cube(lon = overlap)),
    "overlapping"
  )
})

test_that("poles and antimeridian have explicit safeguards", {
  skip_if_not_installed("sf")
  pole <- geometry_test_cube(
    lat = structure(c(-89.5, -88.5), bounds = c(-90, -89, -88))
  )
  expect_true(all(is.finite(cube_cell_area(pole))))
  expect_error(
    cube_cell_area(geometry_test_cube(lat = c(-90, -89))),
    "\\[-90, 90\\]"
  )
  expect_error(
    cube_cell_area(geometry_test_cube(lon = c(179, -179))),
    "180 degrees|antimeridian"
  )
})

test_that("geodesic area agrees with a spherical rectangle reference", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube(
    lon = structure(0, bounds = c(-0.5, 0.5)),
    lat = structure(0, bounds = c(-0.5, 0.5))
  )
  observed <- unname(cube_cell_area(x)[1, 1])
  radius <- 6371010
  expected <- radius^2 * (pi / 180) *
    (sin(0.5 * pi / 180) - sin(-0.5 * pi / 180))
  expect_equal(observed, expected, tolerance = 5e-4)
})

test_that("layer thickness supports interfaces, pairs, order, and units", {
  x <- geometry_test_cube()
  interfaces <- structure(c(0, 10, 20), units = "m")
  pairs <- structure(
    matrix(c(0, 10, 10, 20), 2, 2, byrow = TRUE),
    unit = "m"
  )
  descending <- geometry_test_cube(
    depth = structure(c(15, 5), units = "m", positive = "up")
  )

  expect_equal(
    as.numeric(cube_layer_thickness(x, interfaces, "m")),
    c(10, 10)
  )
  expect_equal(
    cube_layer_thickness(x, interfaces),
    cube_layer_thickness(x, pairs)
  )
  expect_equal(
    as.numeric(cube_layer_thickness(
      descending,
      structure(c(20, 10, 0), units = "m"),
      "m"
    )),
    c(10, 10)
  )
  km_bounds <- structure(c(0, 0.01, 0.02), units = "km")
  expect_equal(
    as.numeric(cube_layer_thickness(x, km_bounds, "m")),
    c(10, 10)
  )
  expect_equal(
    as.numeric(cube_layer_thickness(x, interfaces, "km")),
    c(0.01, 0.01)
  )
})

test_that("vertical bounds can come from explicit cube metadata", {
  x <- geometry_test_cube()
  x$depth_bounds <- structure(c(0, 10, 20), units = "m")
  thickness <- cube_layer_thickness(x, unit = "m")
  expect_equal(as.numeric(thickness), c(10, 10))
  expect_identical(attr(thickness, "bounds_source"), "depth_bounds")
})

test_that("vertical bounds and depth units fail safely", {
  x <- geometry_test_cube()
  expect_error(cube_layer_thickness(x), "never inferred")
  no_units <- geometry_test_cube(depth = c(5, 15))
  expect_error(
    cube_layer_thickness(no_units, c(0, 10, 20), "m"),
    "declare depth units"
  )
  expect_error(
    cube_layer_thickness(
      x,
      structure(c(0, 10), units = "m")
    ),
    "length 3"
  )
  expect_error(
    cube_layer_thickness(
      x,
      structure(matrix(c(0, 12, 10, 20), 2, 2, byrow = TRUE), units = "m")
    ),
    "overlapping"
  )
  expect_error(
    cube_layer_thickness(
      x,
      structure(c(0, Inf, 20), units = "m")
    ),
    "finite"
  )
  expect_error(
    cube_layer_thickness(
      x,
      structure(c(0, 10, 20), units = "fathom"),
      "m"
    ),
    "unsupported depth unit"
  )
})

test_that("surface cubes cannot produce thickness or volume", {
  x <- geometry_test_cube(depth = NA_real_)
  expect_error(
    cube_layer_thickness(x, structure(c(0, 1), units = "m")),
    "surface cubes"
  )
  expect_error(
    cube_cell_volume(x, structure(c(0, 1), units = "m")),
    "surface cubes"
  )
})

test_that("cell volume is exactly area times thickness with unit conversion", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  bounds <- structure(c(0, 10, 30), units = "m")
  area <- cube_cell_area(x)
  volume <- cube_cell_volume(x, bounds)
  expect_identical(dim(volume), c(2L, 2L, 2L))
  expect_identical(
    names(dimnames(volume)),
    c("longitude", "latitude", "depth")
  )
  expect_equal(
    as.numeric(volume[, , 1]),
    as.numeric(area) * 10,
    tolerance = 1e-12
  )
  expect_equal(
    as.numeric(volume[, , 2]),
    as.numeric(area) * 20,
    tolerance = 1e-12
  )
  expect_equal(
    as.numeric(cube_cell_volume(x, bounds, "km3")),
    as.numeric(volume) / 1e9,
    tolerance = 1e-12
  )
})

test_that("geometry functions do not read memory or NetCDF values", {
  skip_if_not_installed("sf")
  memory <- geometry_test_cube()
  file <- make_netcdf_backend_fixture()
  netcdf <- .new_netcdf_cube(.new_netcdf_storage(file, "temperature"))
  depth_bounds <- structure(c(-25, 25, 75), units = "m")
  local_mocked_bindings(
    .with_netcdf_connection = function(...) stop("NetCDF open is forbidden"),
    .cube_read = function(...) stop("scientific read is forbidden"),
    .cube_read_block = function(...) stop("block read is forbidden"),
    .ncvar_get_block = function(...) stop("NetCDF data read is forbidden"),
    .package = "oceancube"
  )

  expect_silent(cube_cell_area(memory))
  expect_silent(cube_cell_volume(memory, structure(c(0, 10, 20), units = "m")))
  expect_silent(cube_cell_area(netcdf))
  expect_silent(cube_cell_volume(netcdf, depth_bounds))
})

test_that("s2 state is restored after geometry calls", {
  skip_if_not_installed("sf")
  previous <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(!previous))
  on.exit(suppressMessages(sf::sf_use_s2(previous)), add = TRUE)
  before <- sf::sf_use_s2()
  cube_cell_area(geometry_test_cube())
  expect_identical(sf::sf_use_s2(), before)
})
