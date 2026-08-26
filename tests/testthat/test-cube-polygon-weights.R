weights_test_polygon <- function(xmin, ymin, xmax, ymax, crs = 4326) {
  sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(xmin, ymin), c(xmax, ymin), c(xmax, ymax),
      c(xmin, ymax), c(xmin, ymin)
    ))),
    crs = crs
  )
}

test_that("full-cell intersection produces exact self-contained 2d weights", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  polygon <- weights_test_polygon(-0.5, -0.5, 0.5, 0.5)
  weights <- cube_polygon_weights(x, polygon)

  expect_identical(nrow(weights), 1L)
  expect_equal(weights$fraction_cell_covered, 1, tolerance = 1e-12)
  expect_equal(
    weights$overlap_area_m2,
    weights$cell_area_m2,
    tolerance = 1e-12
  )
  expect_identical(weights$effective_area_m2, weights$overlap_area_m2)
  expect_identical(
    c(weights$longitude_index, weights$latitude_index, weights$cell_index),
    c(1L, 1L, 1L)
  )
  expect_false(any(vapply(weights, inherits, logical(1), what = "sfc")))
  expect_null(attr(weights, "cube"))
  expect_identical(attr(weights, "dimension"), "2d")
  expect_match(attr(weights, "area_method"), "s2")
})

test_that("half, quarter, boundary, and vertex contacts are represented", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  half <- cube_polygon_weights(
    x,
    weights_test_polygon(-0.5, -0.5, 0, 0.5)
  )
  quarter <- cube_polygon_weights(
    x,
    weights_test_polygon(-0.5, -0.5, 0, 0)
  )
  boundary <- cube_polygon_weights(
    x,
    weights_test_polygon(-1.5, -0.5, -0.5, 0.5),
    include_zero = TRUE
  )
  vertex <- cube_polygon_weights(
    x,
    weights_test_polygon(-1.5, -1.5, -0.5, -0.5),
    include_zero = TRUE
  )

  expect_equal(half$fraction_cell_covered, 0.5, tolerance = 2e-3)
  expect_equal(quarter$fraction_cell_covered, 0.25, tolerance = 2e-3)
  expect_true(all(boundary$overlap_area_m2 == 0))
  expect_true(all(vertex$overlap_area_m2 == 0))
  expect_identical(nrow(boundary), 4L)
  expect_identical(nrow(vertex), 4L)
})

test_that("include_zero controls cardinality without changing positive rows", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  polygon <- weights_test_polygon(-0.5, -0.5, 0.5, 0.5)
  sparse <- cube_polygon_weights(x, polygon)
  dense <- cube_polygon_weights(x, polygon, include_zero = TRUE)

  expect_identical(nrow(sparse), 1L)
  expect_identical(nrow(dense), 4L)
  expect_identical(sum(dense$overlap_area_m2 > 0), 1L)
  dense_positive <- dense[dense$overlap_area_m2 > 0, names(sparse)]
  for (name in names(sparse)) {
    expect_equal(dense_positive[[name]], sparse[[name]], tolerance = 1e-12)
  }
})

test_that("multiple features retain identifiers, order, duplicates, and overlap", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  geometry <- c(
    weights_test_polygon(-0.5, -0.5, 0.5, 0.5),
    weights_test_polygon(-0.5, -0.5, 0.5, 0.5)
  )
  polygons <- sf::st_sf(
    code = c("same", "same"),
    geometry = geometry
  )
  weights <- cube_polygon_weights(x, polygons, id_col = "code")

  expect_identical(weights$feature_id, c("same", "same"))
  expect_identical(weights$feature_order, c(1L, 2L))
  expect_equal(weights$overlap_area_m2[1], weights$overlap_area_m2[2])
  expect_identical(attr(weights, "n_features"), 2L)
  expect_identical(
    attr(weights, "feature_coverage")$feature_order,
    c(1L, 2L)
  )
})

test_that("holes and multipart polygons remain feature-specific", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  outer <- rbind(
    c(-0.5, -0.5), c(0.5, -0.5), c(0.5, 0.5),
    c(-0.5, 0.5), c(-0.5, -0.5)
  )
  hole <- rbind(
    c(-0.2, -0.2), c(-0.2, 0.2), c(0.2, 0.2),
    c(0.2, -0.2), c(-0.2, -0.2)
  )
  with_hole <- sf::st_sfc(sf::st_polygon(list(outer, hole)), crs = 4326)
  multipart <- sf::st_sfc(
    sf::st_multipolygon(list(
      list(rbind(
        c(-0.5, -0.5), c(0, -0.5), c(0, 0),
        c(-0.5, 0), c(-0.5, -0.5)
      )),
      list(rbind(
        c(0, 0), c(0.5, 0), c(0.5, 0.5),
        c(0, 0.5), c(0, 0)
      ))
    )),
    crs = 4326
  )

  hole_weights <- cube_polygon_weights(x, with_hole)
  multipart_weights <- cube_polygon_weights(x, multipart)
  expect_gt(hole_weights$fraction_cell_covered, 0)
  expect_lt(hole_weights$fraction_cell_covered, 1)
  expect_equal(
    multipart_weights$fraction_cell_covered,
    0.5,
    tolerance = 3e-3
  )
})

test_that("partly and totally outside features report coverage explicitly", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  partial <- cube_polygon_weights(
    x,
    weights_test_polygon(-1, -0.5, 0, 0.5)
  )
  outside_polygon <- weights_test_polygon(10, 10, 11, 11)
  outside_sparse <- cube_polygon_weights(x, outside_polygon)
  outside_dense <- cube_polygon_weights(
    x, outside_polygon, include_zero = TRUE
  )

  partial_coverage <- unique(partial$fraction_polygon_covered_by_grid)
  expect_length(partial_coverage, 1L)
  expect_gt(partial_coverage, 0)
  expect_lt(partial_coverage, 1)
  expect_identical(nrow(outside_sparse), 0L)
  expect_identical(nrow(outside_dense), 4L)
  expect_true(all(outside_dense$overlap_area_m2 == 0))
  coverage <- attr(outside_sparse, "feature_coverage")
  expect_identical(nrow(coverage), 1L)
  expect_identical(coverage$intersected_grid_area_m2, 0)
  expect_identical(coverage$fraction_polygon_covered_by_grid, 0)
})

test_that("candidate cells use bounds rather than centre membership", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  edge <- weights_test_polygon(0.4, -0.4, 0.49, 0.4)
  weights <- cube_polygon_weights(x, edge)
  expect_identical(nrow(weights), 1L)
  expect_gt(weights$overlap_area_m2, 0)
  expect_equal(weights$longitude, 0)
})

test_that("sparse weights do not emit the feature by grid product", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube(lon = 0:11, lat = 0:9)
  weights <- cube_polygon_weights(
    x,
    weights_test_polygon(-0.4, -0.4, 0.4, 0.4)
  )
  qa <- attr(weights, "oceancube_qa")$polygon_weights

  expect_identical(nrow(weights), 1L)
  expect_identical(qa$n_feature_cell_pairs, 120L)
  expect_identical(qa$n_candidates, 1L)
  expect_identical(qa$n_intersections, 1L)
  expect_equal(weights$fraction_cell_covered, 0.64, tolerance = 3e-3)
})

test_that("2d output columns, types, bounds, metrics, and provenance are stable", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  weights <- cube_polygon_weights(
    x,
    weights_test_polygon(-0.5, -0.5, 1.5, 1.5),
    include_zero = TRUE
  )
  required <- c(
    "feature_id", "feature_order", "longitude_index", "latitude_index",
    "cell_index", "longitude", "latitude", "lon_min", "lon_max",
    "lat_min", "lat_max", "cell_area_m2", "overlap_area_m2",
    "fraction_cell_covered", "effective_area_m2", "polygon_area_m2",
    "intersected_grid_area_m2", "fraction_polygon_covered_by_grid"
  )
  expect_true(all(required %in% names(weights)))
  expect_true(all(vapply(
    weights[c("feature_order", "longitude_index", "latitude_index",
              "cell_index")],
    is.integer,
    logical(1)
  )))
  expect_true(all(weights$longitude >= weights$lon_min))
  expect_true(all(weights$longitude <= weights$lon_max))
  expect_true(all(weights$latitude >= weights$lat_min))
  expect_true(all(weights$latitude <= weights$lat_max))
  expect_true(all(weights$fraction_cell_covered >= 0))
  expect_true(all(weights$fraction_cell_covered <= 1))
  expect_equal(
    weights$effective_area_m2,
    weights$cell_area_m2 * weights$fraction_cell_covered,
    tolerance = 1e-12
  )
  provenance <- attr(weights, "provenance")
  operation <- provenance$history[[length(provenance$history)]]
  expect_identical(operation$software$package, "oceancube")
  expect_match(operation$parameters$resolved$role, "no indicator")
  expect_identical(operation$parameters$resolved$intended_consumer,
                   "spatind or another explicit downstream package")
})

test_that("3d weights expand depth fastest and conserve effective volume", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  bounds <- structure(c(0, 10, 30), units = "m")
  weights <- cube_polygon_weights(
    x,
    weights_test_polygon(-0.5, -0.5, 1.5, 1.5),
    dimension = "3d",
    depth_bounds = bounds
  )

  expect_identical(nrow(weights), 8L)
  expect_identical(weights$depth_index, rep(c(1L, 2L), 4L))
  expect_identical(
    weights$cell_index,
    rep(seq_len(4L), each = 2L)
  )
  expect_equal(
    weights$cell_volume_m3,
    weights$cell_area_m2 * weights$layer_thickness_m,
    tolerance = 1e-12
  )
  expect_equal(
    weights$effective_volume_m3,
    weights$cell_volume_m3 * weights$fraction_cell_covered,
    tolerance = 1e-12
  )
  expect_identical(attr(weights, "dimension"), "3d")
  expect_identical(attr(weights, "volume_unit"), "m3")
  expect_identical(attr(weights, "vertical_bounds_source"), "argument")
})

test_that("surface cubes allow 2d but reject 3d polygon weights", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube(depth = NA_real_)
  polygon <- weights_test_polygon(-0.5, -0.5, 0.5, 0.5)
  expect_s3_class(cube_polygon_weights(x, polygon), "data.frame")
  expect_error(
    cube_polygon_weights(
      x,
      polygon,
      dimension = "3d",
      depth_bounds = structure(c(0, 1), units = "m")
    ),
    "surface cubes"
  )
})

test_that("polygon classes, CRS, validity, emptiness, and antimeridian are checked", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  no_crs <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(-0.5, -0.5), c(0.5, -0.5), c(0.5, 0.5),
    c(-0.5, 0.5), c(-0.5, -0.5)
  ))))
  expect_error(cube_polygon_weights(x, no_crs), "has no CRS")
  expect_s3_class(cube_polygon_weights(x, no_crs, crs = 4326), "data.frame")
  projected <- sf::st_transform(weights_test_polygon(-0.5, -0.5, 0.5, 0.5),
                                3857)
  expect_error(cube_polygon_weights(x, projected), "geographic")
  expect_error(
    cube_polygon_weights(
      x,
      weights_test_polygon(-0.5, -0.5, 0.5, 0.5),
      crs = 3857
    ),
    "reinterpret"
  )
  empty <- sf::st_sfc(sf::st_polygon(), crs = 4326)
  expect_error(cube_polygon_weights(x, empty), "empty")
  invalid <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(-0.5, -0.5), c(0.5, 0.5), c(0.5, -0.5),
    c(-0.5, 0.5), c(-0.5, -0.5)
  ))), crs = 4326)
  expect_error(cube_polygon_weights(x, invalid), "invalid")
  dateline <- weights_test_polygon(179, -1, -179, 1)
  expect_error(cube_polygon_weights(x, dateline), "antimeridian")
  point <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  expect_error(cube_polygon_weights(x, point), "Only POLYGON")
})

test_that("id_col and scalar arguments are validated", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  polygon <- weights_test_polygon(-0.5, -0.5, 0.5, 0.5)
  expect_error(
    cube_polygon_weights(x, polygon, id_col = "id"),
    "only when"
  )
  sf_polygon <- sf::st_sf(id = "A", geometry = polygon)
  expect_error(
    cube_polygon_weights(x, sf_polygon, id_col = "missing"),
    "not present"
  )
  expect_error(
    cube_polygon_weights(x, sf_polygon, include_zero = NA),
    "logical"
  )
})

test_that("polygon weights do not read memory or NetCDF values", {
  skip_if_not_installed("sf")
  polygon <- weights_test_polygon(-81, -13, -77, -10)
  memory <- geometry_test_cube()
  file <- make_netcdf_backend_fixture()
  netcdf <- .new_netcdf_cube(.new_netcdf_storage(file, "temperature"))
  local_mocked_bindings(
    .with_netcdf_connection = function(...) stop("NetCDF open is forbidden"),
    .cube_read = function(...) stop("scientific read is forbidden"),
    .cube_read_block = function(...) stop("block read is forbidden"),
    .ncvar_get_block = function(...) stop("NetCDF data read is forbidden"),
    .package = "oceancube"
  )

  expect_silent(cube_polygon_weights(
    memory,
    weights_test_polygon(-0.5, -0.5, 0.5, 0.5)
  ))
  expect_silent(cube_polygon_weights(netcdf, polygon))
  expect_silent(cube_polygon_weights(
    netcdf,
    polygon,
    dimension = "3d",
    depth_bounds = structure(c(-25, 25, 75), units = "m")
  ))
})

test_that("s2 state is restored after polygon weights", {
  skip_if_not_installed("sf")
  previous <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(!previous))
  on.exit(suppressMessages(sf::sf_use_s2(previous)), add = TRUE)
  before <- sf::sf_use_s2()
  cube_polygon_weights(
    geometry_test_cube(),
    weights_test_polygon(-0.5, -0.5, 0.5, 0.5)
  )
  expect_identical(sf::sf_use_s2(), before)
})
