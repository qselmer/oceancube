.mask_rectangle <- function(xmin, ymin, xmax, ymax, crs = 4326) {
  sf::st_sfc(sf::st_polygon(list(matrix(
    c(
      xmin, ymin, xmax, ymin, xmax, ymax,
      xmin, ymax, xmin, ymin
    ),
    ncol = 2, byrow = TRUE
  ))), crs = crs)
}

.mask_plain_data <- function(x) {
  list(
    values = .cube_read(x),
    mask = x$mask,
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    time = x$time,
    vars = x$vars,
    units = x$units,
    spatial_extent = x$spatial_extent,
    temporal_extent = x$temporal_extent,
    depth_extent = x$depth_extent
  )
}

test_that("geometry and CRS validation accepts polygonal sf inputs", {
  skip_if_not_installed("sf")
  x <- .make_baseline_fixture()$cube
  polygon <- .mask_rectangle(-80.5, -12.5, -78.5, -10.5)
  polygon_before <- polygon

  from_sfc <- cube_mask(x, polygon)
  from_sf <- cube_mask(x, sf::st_sf(id = 1L, geometry = polygon))
  from_sfg <- cube_mask(x, polygon[[1L]], crs = 4326)
  expect_equal(.cube_read(from_sf), .cube_read(from_sfc))
  expect_equal(.cube_read(from_sfg), .cube_read(from_sfc))
  expect_identical(polygon, polygon_before)
  expect_identical(.cube_backend(from_sfc), "memory")
  expect_identical(unname(.cube_shape(from_sfc)), unname(.cube_shape(x)))

  multipolygon <- sf::st_sfc(
    sf::st_multipolygon(list(unclass(polygon[[1L]]))),
    crs = 4326
  )
  expect_s3_class(cube_mask(x, multipolygon), "ocean_cube")
  expect_error(cube_mask(x, sf::st_sfc(polygon[[1L]])), "no CRS")
  expect_error(
    cube_mask(x, sf::st_transform(polygon, 3857)),
    "geographic longitude-latitude"
  )
  expect_error(
    cube_mask(x, sf::st_sfc(sf::st_point(c(-79, -11)), crs = 4326)),
    "unsupported geometry"
  )
  empty <- sf::st_sfc(sf::st_polygon(), crs = 4326)
  expect_error(cube_mask(x, empty), "empty geometry")
  invalid <- sf::st_sfc(sf::st_polygon(list(matrix(
    c(-80, -12, -78, -10, -80, -10, -78, -12, -80, -12),
    ncol = 2, byrow = TRUE
  ))), crs = 4326)
  expect_error(cube_mask(x, invalid), "invalid geometry")
})

test_that("rectangular center classification preserves canonical order and 5D values", {
  skip_if_not_installed("sf")
  fixture <- .make_baseline_fixture()
  x <- fixture$cube
  polygon <- .mask_rectangle(-80.5, -12.5, -78.5, -10.5)
  result <- cube_mask(x, polygon, keep = "inside", boundary = "include")
  expected_spatial <- matrix(
    c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
    nrow = 3L, ncol = 2L
  )
  expect_identical(
    unname(result$mask$polygon_keep[, , 1L, drop = TRUE]),
    unname(expected_spatial)
  )
  expect_identical(result$lon, x$lon)
  expect_identical(result$lat, x$lat)
  expect_identical(result$depth, x$depth)
  expect_identical(result$time, x$time)
  expect_identical(result$vars, x$vars)
  expect_identical(result$units, x$units)
  expect_identical(result$spatial_extent, x$spatial_extent)
  expect_identical(result$temporal_extent, x$temporal_extent)
  expect_identical(result$depth_extent, x$depth_extent)
  expect_identical(dim(.cube_read(result)), dim(.cube_read(x)))
  expect_identical(dimnames(.cube_read(result)), dimnames(.cube_read(x)))

  observed <- .cube_read(result)
  original <- .cube_read(x)
  for (i in seq_len(3L)) {
    for (j in seq_len(2L)) {
      for (k in seq_len(2L)) {
        for (l in seq_len(4L)) {
          for (m in seq_len(2L)) {
            if (expected_spatial[i, j]) {
              expect_equal(observed[i, j, k, l, m], original[i, j, k, l, m])
            } else {
              expect_true(is.na(observed[i, j, k, l, m]))
            }
          }
        }
      }
    }
  }
  coverage <- result$mask$coverage
  expect_equal(coverage$n_spatial_cells_total, 6L)
  expect_equal(coverage$n_centers_inside_polygon, 4L)
  expect_equal(coverage$n_spatial_cells_kept, 4L)
  expect_equal(coverage$n_spatial_cells_masked, 2L)
  expect_equal(coverage$fraction_centers_inside, 4 / 6)
  expect_equal(coverage$fraction_cells_kept, 4 / 6)
  expect_identical(coverage$semantics, "cell_center")
  expect_equal(coverage$n_logical_values_total, 96L)
  expect_equal(coverage$n_logical_values_kept_by_geometry, 64L)
})

test_that("boundary policy, outside complement, and no-center policies are exact", {
  skip_if_not_installed("sf")
  x <- .make_baseline_fixture()$cube
  edge <- .mask_rectangle(-80.5, -12.5, -79, -10.5)
  include <- cube_mask(x, edge, boundary = "include")
  exclude <- cube_mask(x, edge, boundary = "exclude")
  expect_equal(
    include$mask$coverage$n_centers_inside_polygon -
      exclude$mask$coverage$n_centers_inside_polygon,
    2L
  )
  outside <- cube_mask(x, edge, keep = "outside")
  expect_identical(
    outside$mask$polygon_keep[, , 1L],
    !include$mask$polygon_keep[, , 1L]
  )
  tiny <- .mask_rectangle(-79.6, -11.6, -79.4, -11.4)
  expect_error(cube_mask(x, tiny, keep = "inside"), "contains no cube cell centers")
  keep_all <- cube_mask(x, tiny, keep = "outside")
  expect_true(all(keep_all$mask$mask))
  all_polygon <- .mask_rectangle(-80.5, -12.5, -77.5, -10.5)
  all_inside <- cube_mask(x, all_polygon, keep = "inside")
  all_outside <- cube_mask(x, all_polygon, keep = "outside")
  expect_equal(.cube_read(all_inside), .cube_read(x))
  expect_true(all(is.na(.cube_read(all_outside))))
})

test_that("holes, multiple features, and overlaps use polygon union", {
  skip_if_not_installed("sf")
  x <- .make_baseline_fixture()$cube
  outer <- matrix(
    c(-80.5, -12.5, -77.5, -12.5, -77.5, -10.5,
      -80.5, -10.5, -80.5, -12.5),
    ncol = 2, byrow = TRUE
  )
  hole <- matrix(
    c(-79.2, -11.2, -79.2, -10.8, -78.8, -10.8,
      -78.8, -11.2, -79.2, -11.2),
    ncol = 2, byrow = TRUE
  )
  polygon_hole <- sf::st_sfc(
    sf::st_polygon(list(outer, hole)), crs = 4326
  )
  holed <- cube_mask(x, polygon_hole)
  expect_false(holed$mask$polygon_keep[2, 2, 1])
  expect_true(holed$mask$polygon_keep[1, 2, 1])
  expect_true(holed$mask$polygon_keep[3, 1, 1])

  feature_a <- .mask_rectangle(-80.2, -12.2, -79.8, -11.8)[[1L]]
  feature_b <- .mask_rectangle(-78.2, -11.2, -77.8, -10.8)[[1L]]
  features <- sf::st_sfc(feature_a, feature_b, crs = 4326)
  union_result <- cube_mask(x, features)
  expect_equal(union_result$mask$coverage$n_polygon_features, 2L)
  expect_equal(union_result$mask$coverage$n_centers_inside_polygon, 2L)
  expect_true(union_result$mask$polygon_keep[1, 1, 1])
  expect_true(union_result$mask$polygon_keep[3, 2, 1])

  overlapping <- sf::st_sfc(feature_a, feature_a, crs = 4326)
  overlap_result <- cube_mask(x, overlapping)
  expect_equal(overlap_result$mask$coverage$n_centers_inside_polygon, 1L)
})

test_that("existing ocean masks combine by AND and derived components are invalidated", {
  skip_if_not_installed("sf")
  fixture <- .make_baseline_fixture()
  x <- fixture$cube
  previous <- stock_mask(x, depth = c(0, 0))
  x$mask <- previous
  x$dc <- matrix(seq_len(6), nrow = 3L)
  x$climatology <- list(old = TRUE)
  x$anomaly <- list(old = TRUE)
  x$qa <- list(label = "scalar")
  before <- x
  polygon <- .mask_rectangle(-80.5, -12.5, -78.5, -10.5)
  result <- cube_mask(x, polygon)
  expected <- previous$mask & result$mask$polygon_keep
  expect_identical(result$mask$mask, expected)
  expect_false(any(result$mask$mask & !previous$mask))
  expect_identical(result$dc, x$dc)
  expect_null(result$climatology)
  expect_null(result$anomaly)
  expect_identical(result$qa, x$qa)
  expect_identical(x, before)

  bad <- x
  bad$mask$mask <- array(TRUE, dim = c(1, 1, 1))
  expect_error(cube_mask(bad, polygon), "incompatible")
})

test_that("NetCDF masking is partial for inside and equivalent to memory", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ncdf4")
  file <- make_netcdf_backend_fixture()
  on.exit(unlink(file), add = TRUE)
  netcdf <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )
  memory <- cube_collect(netcdf)
  polygon <- .mask_rectangle(-80.2, -12.2, -79.8, -11.8)
  file_before <- file.info(file)[c("size", "mtime")]

  inside_memory <- cube_mask(memory, polygon, keep = "inside")
  inside_netcdf <- cube_mask(netcdf, polygon, keep = "inside")
  expect_equal(.mask_plain_data(inside_netcdf), .mask_plain_data(inside_memory))
  metrics <- inside_netcdf$provenance$cube_mask$bounding_rectangle_read
  expect_equal(metrics$longitude_count, 1L)
  expect_equal(metrics$latitude_count, 1L)
  expect_equal(metrics$spatial_cells_in_bbox, 1L)
  expect_equal(metrics$n_open, 1L)
  expect_equal(metrics$n_ncvar_get, 2L)
  expect_equal(metrics$n_values_read, 16L)
  expect_identical(metrics$spatial_read, "bounding_rectangle")

  outside_memory <- cube_mask(memory, polygon, keep = "outside")
  outside_netcdf <- cube_mask(netcdf, polygon, keep = "outside")
  expect_equal(.mask_plain_data(outside_netcdf), .mask_plain_data(outside_memory))
  outside_metrics <-
    outside_netcdf$provenance$cube_mask$bounding_rectangle_read
  expect_identical(outside_metrics$spatial_read, "full")
  expect_equal(outside_metrics$n_open, 1L)

  compare_backends <- function(geometry, ...) {
    from_memory <- cube_mask(memory, geometry, ...)
    from_netcdf <- cube_mask(netcdf, geometry, ...)
    expect_equal(
      .mask_plain_data(from_netcdf),
      .mask_plain_data(from_memory)
    )
  }
  compare_backends(
    .mask_rectangle(-80.5, -12.5, -79, -10.5),
    boundary = "exclude"
  )
  feature_a <- .mask_rectangle(-80.2, -12.2, -79.8, -11.8)[[1L]]
  feature_b <- .mask_rectangle(-78.2, -11.2, -77.8, -10.8)[[1L]]
  compare_backends(sf::st_sfc(feature_a, feature_b, crs = 4326))
  outer <- matrix(
    c(-80.5, -12.5, -77.5, -12.5, -77.5, -10.5,
      -80.5, -10.5, -80.5, -12.5),
    ncol = 2, byrow = TRUE
  )
  hole <- matrix(
    c(-79.2, -11.2, -79.2, -10.8, -78.8, -10.8,
      -78.8, -11.2, -79.2, -11.2),
    ncol = 2, byrow = TRUE
  )
  compare_backends(
    sf::st_sfc(sf::st_polygon(list(outer, hole)), crs = 4326)
  )
  compare_backends(.mask_rectangle(-80.5, -12.5, -77.5, -10.5))

  previous <- stock_mask(memory, depth = c(0, 0))
  memory_previous <- memory
  netcdf_previous <- netcdf
  memory_previous$mask <- previous
  netcdf_previous$mask <- previous
  expect_equal(
    .mask_plain_data(cube_mask(netcdf_previous, polygon)),
    .mask_plain_data(cube_mask(memory_previous, polygon))
  )
  expect_equal(file.info(file)[c("size", "mtime")], file_before)

  rds <- tempfile(fileext = ".rds")
  on.exit(unlink(rds), add = TRUE)
  saveRDS(inside_netcdf, rds)
  restored <- readRDS(rds)
  expect_identical(.cube_backend(restored), "memory")
  expect_identical(.cube_read(restored), .cube_read(inside_netcdf))

  materialized <- inside_netcdf
  unlink(file)
  expect_silent(summary(materialized))
  expect_error(cube_mask(netcdf, polygon))
})

test_that("coverage naming is explicitly cell-center based", {
  skip_if_not_installed("sf")
  x <- .make_baseline_fixture()$cube
  result <- cube_mask(
    x, .mask_rectangle(-80.5, -12.5, -78.5, -10.5)
  )
  coverage_names <- names(result$mask$coverage)
  expect_true("semantics" %in% coverage_names)
  expect_identical(result$mask$coverage$semantics, "cell_center")
  expect_false(any(c(
    "area_fraction", "area_coverage", "weighted_area"
  ) %in% coverage_names))
})
