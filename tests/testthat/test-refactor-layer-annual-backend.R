.make_layer_metadata_cube <- function() {
  cube <- .make_baseline_fixture()$cube
  cube$source <- "synthetic-layer"
  cube$dataset_id <- "layer-fixture"
  cube$mask <- list(id = "mask-fixture")
  cube$dc <- matrix(seq_len(6), nrow = 3, ncol = 2)
  cube$provenance <- list(step = "input")
  cube$qa <- list(status = "raw")
  cube
}

test_that("layer_mean preserves the baseline weighted result and structure", {
  cube <- .make_layer_metadata_cube()
  before <- cube
  expected_first <- (11111 * 25 + 11211 * 25) / 50

  layer <- layer_mean(cube, depth = c(0, 50))

  expect_equal(layer$data[1, 1, 1, 1, 1], expected_first)
  expect_identical(expected_first, 11161)
  expect_identical(class(layer), c("ocean_cube", "list"))
  expect_identical(names(layer), names(cube))
  expect_identical(unname(dim(layer$data)), c(3L, 2L, 1L, 4L, 2L))
  expect_identical(layer$lon, cube$lon)
  expect_identical(layer$lat, cube$lat)
  expect_identical(layer$depth, 25)
  expect_identical(layer$time, cube$time)
  expect_identical(layer$vars, cube$vars)
  expect_identical(layer$units, cube$units)
  expect_identical(layer$source, cube$source)
  expect_identical(layer$dataset_id, cube$dataset_id)
  expect_identical(layer$spatial_extent, cube$spatial_extent)
  expect_identical(layer$temporal_extent, cube$temporal_extent)
  expect_identical(layer$depth_extent, c(0, 50))
  expect_identical(layer$mask, cube$mask)
  expect_identical(layer$dc, cube$dc)
  expect_identical(layer$provenance$extra$parent, cube$provenance)
  expect_null(layer$qa)
  expect_identical(dimnames(layer$data)[[1]], as.character(cube$lon))
  expect_identical(dimnames(layer$data)[[2]], as.character(cube$lat))
  expect_identical(dimnames(layer$data)[[3]], "25")
  expect_identical(dimnames(layer$data)[[4]], as.character(cube$time))
  expect_identical(dimnames(layer$data)[[5]], cube$vars)
  expect_identical(.cube_backend(layer), "memory")
  expect_identical(cube, before)
})

test_that("layer_mean preserves independently derived irregular vertical weights", {
  cube <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = c(0, 10, 40),
    time = as.Date("2020-01-01"),
    vars = "temperature",
    data = array(c(10, 20, 40), dim = c(1, 1, 3, 1, 1))
  )

  full_interval <- layer_mean(cube, depth = c(0, 40))
  offset_interval <- layer_mean(cube, depth = c(5, 35))
  two_layers <- layer_mean(cube, depth = c(0, 10, 40))

  # Midpoint-derived cells are [0,5], [5,25], [25,55].
  expected_full <- (10 * 5 + 20 * 20 + 40 * 15) / (5 + 20 + 15)
  expected_offset <- (20 * 20 + 40 * 10) / (20 + 10)
  expected_shallow <- (10 * 5 + 20 * 5) / 10
  expected_deep <- (20 * 15 + 40 * 15) / 30

  expect_equal(full_interval$data[1, 1, 1, 1, 1], expected_full)
  expect_equal(expected_full, 26.25)
  expect_equal(offset_interval$data[1, 1, 1, 1, 1], expected_offset)
  expect_equal(expected_offset, 80 / 3)
  expect_equal(as.vector(two_layers$data), c(expected_shallow, expected_deep))
  expect_identical(two_layers$depth, c(5, 25))
  expect_identical(unname(dim(two_layers$data)), c(1L, 1L, 2L, 1L, 1L))
})

test_that("layer_mean renormalizes finite coverage and preserves all-missing cells", {
  data <- array(NA_real_, dim = c(2, 1, 3, 1, 1))
  data[1, 1, , 1, 1] <- c(10, NA_real_, 40)
  cube <- ocean_cube(
    lon = c(-80, -79),
    lat = -12,
    depth = c(0, 10, 40),
    time = as.Date("2020-01-01"),
    vars = "temperature",
    data = data
  )

  layer <- layer_mean(cube, depth = c(0, 40))
  expected_partial <- (10 * 5 + 40 * 15) / (5 + 15)

  expect_equal(layer$data[1, 1, 1, 1, 1], expected_partial)
  expect_equal(expected_partial, 32.5)
  expect_true(is.na(layer$data[2, 1, 1, 1, 1]))
})

test_that("layer_mean preserves selection boundaries and legacy edge behaviour", {
  irregular <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = c(0, 10, 40),
    time = as.Date("2020-01-01"),
    vars = "temperature",
    data = array(c(10, 20, 40), dim = c(1, 1, 3, 1, 1))
  )
  descending <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = c(40, 10, 0),
    time = as.Date("2020-01-01"),
    vars = "temperature",
    data = array(c(40, 20, 10), dim = c(1, 1, 3, 1, 1))
  )
  single <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = 0,
    time = as.Date("2020-01-01"),
    vars = "temperature",
    data = array(10, dim = rep(1, 5))
  )
  surface <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = NA_real_,
    time = as.Date("2020-01-01"),
    vars = "temperature",
    data = array(10, dim = rep(1, 5))
  )

  exact <- layer_mean(irregular, depth = c(0, 10))
  outside <- layer_mean(irregular, depth = c(100, 200))
  descending_result <- layer_mean(descending, depth = c(0, 40))

  expect_equal(exact$data[1, 1, 1, 1, 1], mean(c(10, 20)))
  expect_true(all(is.na(outside$data)))
  expect_identical(outside$depth, 150)
  expect_identical(.cube_backend(outside), "memory")
  expect_true(all(is.na(descending_result$data)))
  expect_error(layer_mean(single, c(0, 10)), "at least two valid depth levels")
  expect_error(layer_mean(surface, c(0, 10)), "at least two valid depth levels")
  expect_error(layer_mean(irregular, 10), "must contain at least two values")
  expect_error(layer_mean(irregular, c(10, 0)), "must be sorted increasingly")
})

test_that("layer_mean reads selected depths through the backend", {
  cube <- .make_baseline_fixture()$cube
  original_read <- .cube_read
  observed_indices <- list()

  local_mocked_bindings(
    .cube_read = function(x, index = NULL, drop = FALSE) {
      observed_indices <<- c(observed_indices, list(index))
      original_read(x, index = index, drop = drop)
    },
    .package = "oceancube"
  )

  layer_mean(cube, c(0, 50))

  expect_identical(observed_indices, list(list(depth = 1:2)))
})

test_that("annual_index preserves baseline statistics, columns, and row order", {
  cube <- .make_baseline_fixture()$cube
  before <- cube
  expected_2020 <- 10000 + mean(c(1000, 2000)) + 100 + mean(c(10, 20)) + mean(1:3)
  expected_2021 <- 10000 + mean(c(3000, 4000)) + 100 + mean(c(10, 20)) + mean(1:3)

  indicators <- annual_index(cube)
  selected <- indicators$var == "temperature" & indicators$depth == 0

  expect_identical(class(indicators), c("ocean_indicators", "data.frame"))
  expect_identical(
    names(indicators),
    c(
      "year", "var", "depth", "n", "mean_value", "median_value", "sd_value",
      "min_value", "max_value", "p10", "p90", "frac_positive",
      "frac_negative", "threshold_pos", "threshold_neg"
    )
  )
  expect_identical(nrow(indicators), 8L)
  expect_identical(
    indicators$year,
    c(2020L, 2021L, 2020L, 2021L, 2020L, 2021L, 2020L, 2021L)
  )
  expect_identical(
    indicators$var,
    c(
      rep("temperature", 4),
      rep("oxygen", 4)
    )
  )
  expect_identical(indicators$depth, rep(c(0, 0, 50, 50), 2))
  expect_equal(indicators$mean_value[selected], c(expected_2020, expected_2021))
  expect_identical(c(expected_2020, expected_2021), c(11617, 13617))
  expect_identical(indicators$n[selected], c(12L, 12L))
  expect_identical(indicators$threshold_pos, rep(0, 8))
  expect_identical(indicators$threshold_neg, rep(0, 8))
  expect_identical(attributes(indicators)$class, c("ocean_indicators", "data.frame"))
  expect_identical(row.names(indicators), as.character(seq_len(8)))
  expect_identical(cube, before)
})

test_that("annual_index preserves strict positive and negative thresholds", {
  cube <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = 0,
    time = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01")),
    vars = "signal",
    data = array(c(-1, 0, 1, 2), dim = c(1, 1, 1, 4, 1))
  )

  defaults <- annual_index(cube)
  strict <- annual_index(cube, threshold_pos = 1, threshold_neg = 0)
  none_above <- annual_index(cube, threshold_pos = 10)

  expect_equal(defaults$frac_positive, 2 / 4)
  expect_equal(defaults$frac_negative, 1 / 4)
  expect_equal(strict$frac_positive, 1 / 4)
  expect_equal(strict$frac_negative, 1 / 4)
  expect_identical(strict$threshold_pos, 1)
  expect_identical(strict$threshold_neg, 0)
  expect_identical(none_above$frac_positive, 0)
})

test_that("annual_index preserves missing-data and non-consecutive-year policies", {
  cube <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = 0,
    time = as.POSIXct(
      c(
        "2018-01-01", "2020-01-01", "2020-02-01",
        "2021-01-01", "2022-01-01"
      ),
      tz = "UTC"
    ),
    vars = "signal",
    data = array(c(1, 2, NA_real_, NA_real_, 4), dim = c(1, 1, 1, 5, 1))
  )

  indicators <- annual_index(cube)

  expect_s3_class(cube$time, "POSIXct")
  expect_identical(attr(cube$time, "tzone"), "UTC")
  expect_identical(indicators$year, c(2018L, 2020L, 2021L, 2022L))
  expect_false(any(indicators$year == 2019L))
  expect_identical(indicators$n, c(1L, 1L, 0L, 1L))
  expect_equal(indicators$mean_value, c(1, 2, NA_real_, 4))
  expect_true(is.na(indicators$sd_value[indicators$year == 2018L]))
  expect_true(is.na(indicators$mean_value[indicators$year == 2021L]))
  expect_true(is.na(indicators$frac_positive[indicators$year == 2021L]))
})

test_that("annual_index handles multiple variables independently", {
  cube <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = c(0, 10),
    time = as.Date(c("2020-01-01", "2021-01-01")),
    vars = c("a", "b"),
    data = array(seq_len(8), dim = c(1, 1, 2, 2, 2))
  )

  indicators <- annual_index(cube)

  expect_identical(nrow(indicators), 8L)
  expect_identical(indicators$var, c(rep("a", 4), rep("b", 4)))
  expect_identical(indicators$depth, rep(c(0, 0, 10, 10), 2))
  expect_identical(indicators$year, rep(c(2020L, 2021L), 4))
  expect_identical(indicators$n, rep(1L, 8))
  expect_equal(indicators$mean_value, c(1, 3, 2, 4, 5, 7, 6, 8))
})

test_that("annual_index performs one complete backend read", {
  cube <- .make_baseline_fixture()$cube
  original_read <- .cube_read
  observed_indices <- list()

  local_mocked_bindings(
    .cube_read = function(x, index = NULL, drop = FALSE) {
      observed_indices <<- c(observed_indices, list(index))
      original_read(x, index = index, drop = drop)
    },
    .package = "oceancube"
  )

  annual_index(cube)

  expect_identical(observed_indices, list(NULL))
})

test_that("layer and annual functions fail through unknown backend dispatch", {
  cube <- .make_baseline_fixture()$cube

  local_mocked_bindings(
    .cube_backend = function(x) "unknown",
    .package = "oceancube"
  )

  expect_error(
    layer_mean(cube, c(0, 50)),
    "Unsupported ocean_cube backend: 'unknown'"
  )
  expect_error(
    annual_index(cube),
    "Unsupported ocean_cube backend: 'unknown'"
  )
})

test_that("layer and annual public signatures remain unchanged", {
  expect_identical(names(formals(layer_mean)), c("x", "depth"))
  expect_identical(
    names(formals(annual_index)),
    c("x", "threshold_pos", "threshold_neg")
  )
})
