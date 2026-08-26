.coast_test_line <- function() {
  sf::st_sfc(
    sf::st_linestring(
      matrix(
        c(-81, -13, -81, -10),
        ncol = 2,
        byrow = TRUE
      )
    ),
    crs = 4326
  )
}

.coast_test_use_s2 <- function() {
  old_s2 <- sf::sf_use_s2()
  withr::defer(sf::sf_use_s2(old_s2), envir = parent.frame())
  sf::sf_use_s2(TRUE)
}

test_that("coast_dist preserves the cube and attaches the baseline dc matrix", {
  skip_if_not_installed("sf")
  .coast_test_use_s2()
  cube <- .make_baseline_fixture()$cube
  cube_before <- cube
  values_before <- .cube_read(cube)
  coast <- .coast_test_line()
  coast_before <- coast

  result <- coast_dist(cube, coast)

  expect_identical(names(formals(coast_dist)), c("x", "coast"))
  expect_identical(class(result), class(cube))
  expect_s3_class(result, "ocean_cube")
  expect_identical(.cube_backend(result), "memory")
  expect_identical(.cube_read(result), values_before)
  expect_identical(result$lon, cube$lon)
  expect_identical(result$lat, cube$lat)
  expect_identical(result$depth, cube$depth)
  expect_identical(result$time, cube$time)
  expect_identical(result$vars, cube$vars)
  expect_identical(result$units, cube$units)
  expect_identical(result$source, cube$source)
  expect_identical(result$dataset_id, cube$dataset_id)
  expect_identical(result$qa, cube$qa)
  expect_identical(result$mask, cube$mask)
  expect_identical(result$climatology, cube$climatology)
  expect_identical(result$anomaly, cube$anomaly)
  operation <- tail(result$provenance$history, 1L)[[1L]]
  expect_identical(operation$operation, "coast_dist")
  expect_null(operation$scientific_method)
  expect_identical(
    head(result$provenance$history, -1L),
    cube$provenance$history
  )
  expect_identical(result$provenance$source, cube$provenance$source)
  expect_true(.check_cube(result))
  expect_identical(cube, cube_before)
  expect_identical(coast, coast_before)

  expect_identical(class(result$dc), c("matrix", "array"))
  expect_identical(typeof(result$dc), "double")
  expect_identical(length(result$dc), 6L)
  expect_identical(dim(result$dc), c(3L, 2L))
  expect_null(dimnames(result$dc))
  expect_null(attr(result$dc, "units"))
  expect_true(all(is.finite(result$dc)))
  expect_true(all(result$dc >= 0))
})

test_that("controlled coastline distances follow the grid and an independent sf reference", {
  skip_if_not_installed("sf")
  .coast_test_use_s2()
  cube <- .make_baseline_fixture()$cube
  coast <- .coast_test_line()

  result <- coast_dist(cube, coast)
  grid <- expand.grid(lon = cube$lon, lat = cube$lat)
  points <- sf::st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)
  reference_m <- as.numeric(
    sf::st_distance(points, sf::st_union(sf::st_as_sf(coast)))
  )
  reference_nm <- matrix(
    reference_m * 0.000539957,
    nrow = length(cube$lon),
    ncol = length(cube$lat),
    byrow = FALSE
  )

  expect_equal(result$dc, reference_nm, tolerance = 1e-8)
  expect_true(all(apply(result$dc, 2, function(column) all(diff(column) > 0))))
  expect_equal(
    range(result$dc),
    c(58.728413765375, 176.809431913569),
    tolerance = 1e-8
  )
})

test_that("coast_dist handles CRS, empty, multiple, and containing geometries basally", {
  skip_if_not_installed("sf")
  .coast_test_use_s2()
  cube <- .make_baseline_fixture()$cube
  coast <- .coast_test_line()
  projected <- sf::st_transform(coast, 3857)
  no_crs <- sf::st_set_crs(coast, NA)
  empty <- sf::st_sfc(
    sf::st_linestring(matrix(numeric(), ncol = 2)),
    crs = 4326
  )
  multiple <- sf::st_sfc(
    sf::st_linestring(
      matrix(c(-81, -13, -81, -10), ncol = 2, byrow = TRUE)
    ),
    sf::st_linestring(
      matrix(c(-77, -13, -77, -10), ncol = 2, byrow = TRUE)
    ),
    crs = 4326
  )
  containing_polygon <- sf::st_sfc(
    sf::st_polygon(
      list(
        matrix(
          c(
            -80.5, -12.5,
            -79.5, -12.5,
            -79.5, -11.5,
            -80.5, -11.5,
            -80.5, -12.5
          ),
          ncol = 2,
          byrow = TRUE
        )
      )
    ),
    crs = 4326
  )

  geographic_result <- coast_dist(cube, coast)
  projected_result <- coast_dist(cube, projected)
  empty_result <- coast_dist(cube, empty)
  multiple_result <- coast_dist(cube, multiple)
  containing_result <- coast_dist(cube, containing_polygon)

  expect_equal(projected_result$dc, geographic_result$dc, tolerance = 1e-6)
  expect_error(coast_dist(cube, no_crs), "missing crs")
  expect_true(all(is.na(empty_result$dc)))
  expect_identical(dim(empty_result$dc), c(3L, 2L))
  expect_equal(multiple_result$dc[1, ], multiple_result$dc[3, ], tolerance = 1e-8)
  expect_true(all(multiple_result$dc[2, ] > multiple_result$dc[1, ]))
  expect_equal(containing_result$dc[1, 1], 0, tolerance = 1e-8)
})

test_that("coordinate ordering and longitude conventions retain their sf behaviour", {
  skip_if_not_installed("sf")
  .coast_test_use_s2()
  coast <- .coast_test_line()
  negative <- ocean_cube(
    lon = c(-80, -79, -78),
    lat = c(-12, -11),
    depth = NA_real_,
    time = as.Date("2020-01-01"),
    vars = "v",
    data = array(1:6, dim = c(3, 2, 1, 1, 1))
  )
  longitude_360 <- ocean_cube(
    lon = c(280, 281, 282),
    lat = c(-12, -11),
    depth = NA_real_,
    time = as.Date("2020-01-01"),
    vars = "v",
    data = array(11:16, dim = c(3, 2, 1, 1, 1))
  )
  descending_lat <- ocean_cube(
    lon = c(-80, -79, -78),
    lat = c(-11, -12),
    depth = 0,
    time = as.Date("2020-01-01"),
    vars = "v",
    data = array(21:26, dim = c(3, 2, 1, 1, 1))
  )
  duplicated <- ocean_cube(
    lon = c(-80, -80, -79),
    lat = c(-12, -12),
    depth = 0,
    time = as.Date("2020-01-01"),
    vars = "v",
    data = array(31:36, dim = c(3, 2, 1, 1, 1))
  )
  one_cell <- ocean_cube(
    lon = -81,
    lat = -12,
    depth = NA_real_,
    time = as.Date("2020-01-01"),
    vars = "v",
    data = array(1, dim = c(1, 1, 1, 1, 1))
  )

  negative_result <- coast_dist(negative, coast)
  longitude_360_result <- coast_dist(longitude_360, coast)
  descending_result <- coast_dist(descending_lat, coast)
  duplicated_result <- coast_dist(duplicated, coast)
  one_cell_result <- coast_dist(one_cell, coast)

  expect_equal(longitude_360_result$dc, negative_result$dc, tolerance = 1e-8)
  expect_equal(descending_result$dc, negative_result$dc[, 2:1], tolerance = 1e-8)
  expect_equal(duplicated_result$dc[1, ], duplicated_result$dc[2, ], tolerance = 1e-8)
  expect_identical(dim(one_cell_result$dc), c(1L, 1L))
  expect_equal(one_cell_result$dc[1, 1], 0, tolerance = 1e-8)
  expect_identical(one_cell_result$depth, NA_real_)
  expect_error(
    ocean_cube(
      lon = c(-80, NA),
      lat = -12,
      depth = 0,
      time = as.Date("2020-01-01"),
      vars = "v",
      data = array(1:2, dim = c(2, 1, 1, 1, 1))
    ),
    "Invalid `lon`"
  )
})

test_that("coast_dist is independent of oceanographic values and performs no reads", {
  skip_if_not_installed("sf")
  .coast_test_use_s2()
  cube_a <- .make_baseline_fixture()$cube
  shape <- .cube_shape(cube_a)
  replacement <- array(-999, dim = unname(shape))
  cube_b <- .cube_write_block(
    cube_a,
    replacement,
    start = rep(1L, 5L),
    count = unname(shape)
  )
  coast <- .coast_test_line()

  result_a <- coast_dist(cube_a, coast)
  result_b <- coast_dist(cube_b, coast)
  expect_identical(result_a$dc, result_b$dc)

  local_mocked_bindings(
    .cube_read = function(...) {
      stop("coast_dist must not read cube values", call. = FALSE)
    },
    .cube_read_block = function(...) {
      stop("coast_dist must not read cube blocks", call. = FALSE)
    },
    .package = "oceancube"
  )
  expect_no_error(coast_dist(cube_a, coast))
})

test_that("coast_dist and general methods can use a logical cube with an unknown backend", {
  skip_if_not_installed("sf")
  .coast_test_use_s2()
  unknown <- .make_baseline_fixture()$cube
  unknown[["data"]] <- NULL
  coast <- .coast_test_line()

  local_mocked_bindings(
    .cube_backend = function(x) "unknown",
    .cube_read = function(...) {
      stop("unknown backend values must not be read", call. = FALSE)
    },
    .cube_read_block = function(...) {
      stop("unknown backend blocks must not be read", call. = FALSE)
    },
    .package = "oceancube"
  )

  expect_true(.check_cube(unknown))
  result <- coast_dist(unknown, coast)
  expect_s3_class(result, "ocean_cube")
  expect_identical(.cube_backend(result), "unknown")
  expect_identical(result$lon, unknown$lon)
  expect_identical(result$lat, unknown$lat)
  expect_identical(dim(result$dc), c(3L, 2L))
  expect_true(all(is.finite(result$dc)))
  expect_no_error(capture.output(print(result)))
  expect_no_error(summary(result))
  expect_error(
    .cube_storage_shape(result),
    "Unsupported ocean_cube backend: 'unknown'"
  )
})

test_that("logical and physical memory shapes agree and corruption is diagnosed", {
  cube <- .make_baseline_fixture()$cube
  logical_shape <- .cube_shape(cube)
  storage_shape <- .cube_storage_shape(cube)

  expect_identical(unname(logical_shape), unname(storage_shape))
  expect_identical(names(storage_shape), names(logical_shape))

  corrupted <- cube
  dim(corrupted[["data"]]) <- c(2, 3, 2, 4, 2)
  expect_error(
    .check_cube(corrupted),
    "memory.*longitude.*expected.*3 x 2 x 2 x 4 x 2.*obtained.*2 x 3 x 2 x 4 x 2"
  )
})

test_that("coast_dist retains baseline input errors", {
  skip_if_not_installed("sf")
  cube <- .make_baseline_fixture()$cube

  expect_error(coast_dist(list(), .coast_test_line()), "must be an <ocean_cube>")
  expect_error(coast_dist(cube, 1), "must be an sf/sfc object")
  expect_error(coast_dist(cube, "missing-coast-file.gpkg"), "must be an sf/sfc object")
})
