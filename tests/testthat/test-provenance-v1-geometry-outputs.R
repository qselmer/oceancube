v1_test_polygon <- function(xmin, ymin, xmax, ymax) {
  sf::st_sfc(sf::st_polygon(list(rbind(
    c(xmin, ymin), c(xmax, ymin), c(xmax, ymax), c(xmin, ymax),
    c(xmin, ymin)
  ))), crs = 4326)
}

v1_test_coast <- function() {
  sf::st_sfc(sf::st_linestring(rbind(c(-81, -13), c(-81, -10))),
             crs = 4326)
}

test_that("polygon weights expose canonical V1 plus the documented legacy alias", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  polygon <- v1_test_polygon(-0.5, -0.5, 1.5, 1.5)
  x_before <- serialize(x, NULL)
  polygon_before <- serialize(polygon, NULL)
  previous_s2 <- sf::sf_use_s2()
  weights <- cube_polygon_weights(x, polygon, include_zero = TRUE)
  provenance <- attr(weights, "oceancube_provenance")
  operation <- tail(provenance$history, 1L)[[1L]]

  expect_identical(serialize(x, NULL), x_before)
  expect_identical(serialize(polygon, NULL), polygon_before)
  expect_identical(sf::sf_use_s2(), previous_s2)
  expect_true(.provenance_validate(provenance, strict = TRUE)$valid)
  expect_identical(attr(weights, "provenance"), provenance)
  expect_identical(operation$operation, "cube_polygon_weights")
  expect_identical(operation$scientific_method$id,
                   "oceancube:s2_polygon_cell_intersection")
  expect_identical(operation$parameters$resolved$geometry$n_features, 1L)
  expect_identical(operation$parameters$resolved$geometry$dimension, "2d")
  expect_identical(operation$parameters$resolved$horizontal_bounds_source,
                   attr(weights, "horizontal_bounds_source"))
  expect_true(is.data.frame(attr(weights, "oceancube_qa")$
                            polygon_weights$feature_coverage))
  expect_false("n_candidates" %in% names(operation$parameters$resolved))
})

test_that("polygon V1 is portable and spatind-ready across serialization", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  weights <- cube_polygon_weights(
    x, v1_test_polygon(-0.5, -0.5, 1.5, 1.5), dimension = "3d",
    depth_bounds = structure(c(0, 10, 30), units = "m")
  )
  provenance <- attr(weights, "oceancube_provenance")
  restored <- unserialize(serialize(weights, NULL))
  expect_s3_class(weights, "data.frame")
  expect_true(.provenance_validate(provenance, strict = TRUE)$valid)
  expect_identical(restored, weights)
  path <- tempfile(fileext = ".rds")
  withr::local_file(path)
  saveRDS(weights, path)
  expect_identical(readRDS(path), weights)
  expect_false(any(vapply(unclass(provenance), inherits, logical(1L), "sf")))
  expect_false(any(vapply(unclass(provenance), inherits, logical(1L), "sfc")))
})

test_that("coast provenance records the actual global engine without false s2 method", {
  skip_if_not_installed("sf")
  x <- .make_baseline_fixture()$cube
  coast <- v1_test_coast()
  x_before <- serialize(x, NULL)
  coast_before <- serialize(coast, NULL)
  old_s2 <- sf::sf_use_s2()
  withr::defer(suppressMessages(sf::sf_use_s2(old_s2)))
  suppressMessages(sf::sf_use_s2(TRUE))
  result <- coast_dist(x, coast)
  operation <- tail(result$provenance$history, 1L)[[1L]]

  expect_identical(serialize(x, NULL), x_before)
  expect_identical(serialize(coast, NULL), coast_before)
  expect_true(.provenance_validate(result$provenance, strict = TRUE)$valid)
  expect_identical(operation$operation, "coast_dist")
  expect_true(operation$parameters$resolved$s2_enabled)
  expect_match(operation$parameters$resolved$distance_engine, "s2 enabled")
  expect_null(operation$scientific_method)
  expect_identical(result$provenance$time, x$provenance$time)
})
