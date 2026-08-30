c1_vertical_cube <- function(
    depth = structure(c(1, 4, 8), units = "m", positive = "down"),
    bounds = structure(
      matrix(c(0, 2, 2, 6, 6, 10), ncol = 2, byrow = TRUE),
      units = "m"
    ),
    values = c(10, 20, 40)) {
  x <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = depth,
    time = as.Date("2020-01-01"),
    vars = "temperature",
    data = array(values, dim = c(1, 1, length(depth), 1, 1))
  )
  x$depth_bounds <- bounds
  x
}

test_that("explicit support resolves exact clipped and multi-cell overlaps", {
  x <- c1_vertical_cube()

  clipped <- layer_mean(x, c(1, 2))
  multi <- layer_mean(x, c(1, 7))
  split <- layer_mean(x, c(1, 2, 6, 7))

  expect_equal(as.numeric(clipped$data), 10)
  expect_equal(as.numeric(multi$data), (10 + 4 * 20 + 40) / 6)
  expect_equal(as.numeric(split$data), c(10, 20, 40))
  expect_identical(attr(multi$depth, "bounds"), structure(
    matrix(c(1, 7), nrow = 1), units = "m", positive = "down"
  ))
  expect_match(
    paste(capture.output(str(tail(multi$provenance$history, 1L))), collapse = " "),
    "USER_DECLARED_EXPLICIT_DEPTH_BOUNDS"
  )
})

test_that("coverage policy rejects gaps but permits zero coverage as NA", {
  x <- c1_vertical_cube(
    depth = structure(c(1, 4), units = "m", positive = "down"),
    bounds = structure(matrix(c(0, 2, 3, 5), 2, 2, byrow = TRUE), units = "m"),
    values = c(10, 20)
  )

  expect_error(
    layer_mean(x, c(0, 5)),
    class = "oceancube_vertical_partial_coverage"
  )
  outside <- layer_mean(x, c(10, 12))
  expect_true(all(is.na(outside$data)))
})

test_that("explicit support preserves storage order and m/km equivalence", {
  ascending <- c1_vertical_cube()
  descending <- c1_vertical_cube(
    depth = structure(c(8, 4, 1), units = "m", positive = "down"),
    bounds = structure(
      matrix(c(6, 10, 2, 6, 0, 2), 3, 2, byrow = TRUE),
      units = "m"
    ),
    values = c(40, 20, 10)
  )
  kilometres <- c1_vertical_cube(
    depth = structure(c(0.001, 0.004, 0.008), units = "km", positive = "down"),
    bounds = structure(
      matrix(c(0, 0.002, 0.002, 0.006, 0.006, 0.010), 3, 2, byrow = TRUE),
      units = "km"
    )
  )

  expected <- as.numeric(layer_mean(ascending, c(1, 7))$data)
  expect_equal(as.numeric(layer_mean(descending, c(1, 7))$data), expected)
  expect_equal(as.numeric(layer_mean(kilometres, c(0.001, 0.007))$data), expected)
  expect_equal(as.numeric(cube_layer_thickness(kilometres, unit = "m")), c(2, 4, 4))
})

test_that("legacy layer means retain the exact centre-derived baseline", {
  x <- ocean_cube(
    lon = -80, lat = -12, depth = c(0, 10, 40),
    time = as.Date("2020-01-01"), vars = "temperature",
    data = array(c(10, 20, 40), dim = c(1, 1, 3, 1, 1))
  )
  expected <- (10 * 5 + 20 * 20 + 40 * 15) / 40
  result <- layer_mean(x, c(0, 40))

  expect_equal(as.numeric(result$data), expected)
  expect_null(attr(result$depth, "bounds", exact = TRUE))
  expect_match(
    paste(capture.output(str(tail(result$provenance$history, 1L))), collapse = " "),
    "LEGACY_CENTRE_DERIVED_SUPPORT"
  )
  expect_match(
    paste(capture.output(str(tail(result$provenance$history, 1L))), collapse = " "),
    "UNCERTIFIED"
  )
})

test_that("certified WOA layers use bounds, derive metadata, and reject gaps", {
  path <- test_path("fixtures", "real-data", "noaa-woa23-monthly-vertical-fv1.nc")
  eager <- read_nc(path, vars = "t_an")
  deferred <- cube_open(path, vars = "t_an")

  exact <- layer_mean(eager, c(0, 2.5))
  clipped <- layer_mean(deferred, c(0.5, 2))

  expect_equal(as.numeric(exact$data), as.numeric(eager$data[, , 1, , , drop = FALSE]))
  expect_equal(as.numeric(clipped$data), as.numeric(eager$data[, , 1, , , drop = FALSE]))
  expect_identical(exact$metadata$cf$current$vertical$bounds, list(c(0, 2.5)))
  expect_identical(
    exact$metadata$cf$current$vertical$runtime_status,
    "VERTICAL_RUNTIME_SUPPORTED"
  )
  expect_identical(exact$metadata$cf$source, eager$metadata$cf$source)
  expect_error(
    layer_mean(eager, c(0, 7.5)),
    class = "oceancube_vertical_partial_coverage"
  )
})

test_that("CF metric-depth failures never fall back to inferred centres", {
  missing <- read_nc(make_cf_vertical_fixture(bounds = NULL), vars = "temperature")
  malformed <- read_nc(make_cf_vertical_fixture(
    bounds = rbind(c(-1, 8), c(7, 14), c(14, 24))
  ), vars = "temperature")

  expect_error(
    layer_mean(missing, c(0, 10)),
    class = "oceancube_vertical_geometry_unsupported"
  )
  expect_error(
    layer_mean(malformed, c(0, 10)),
    class = "oceancube_vertical_geometry_unsupported"
  )
})

test_that("explicit coverage is validated before any scientific payload read", {
  path <- test_path("fixtures", "real-data", "noaa-woa23-monthly-vertical-fv1.nc")
  x <- cube_open(path, vars = "t_an")
  testthat::local_mocked_bindings(
    .cube_read_netcdf = function(...) stop("scientific payload read"),
    .package = "oceancube"
  )

  expect_error(
    layer_mean(x, c(0, 7.5)),
    class = "oceancube_vertical_partial_coverage"
  )
})
