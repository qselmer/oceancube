test_that("the current cube representation selects the memory backend", {
  cube <- .make_baseline_fixture()$cube

  expect_identical(.cube_backend(cube), "memory")
  expect_error(.cube_read(list(data = cube$data)), "must be an <ocean_cube>")

  local_mocked_bindings(
    .cube_backend = function(x) "future-store",
    .package = "oceancube"
  )
  expect_error(
    .cube_read(cube),
    "Unsupported ocean_cube backend: 'future-store'"
  )
})

test_that("full memory reads preserve the complete five-dimensional array", {
  cube <- .make_baseline_fixture()$cube
  before <- cube

  result <- .cube_read(cube)

  expect_identical(result, cube$data)
  expect_identical(cube, before)
  expect_identical(unname(dim(result)), c(3L, 2L, 2L, 4L, 2L))
  expect_identical(names(dim(result)), names(dim(cube$data)))
  expect_identical(dimnames(result), dimnames(cube$data))
  expect_identical(typeof(result), typeof(cube$data))
  expect_error(.cube_read(cube, drop = TRUE), "`drop = TRUE` is not supported")
  expect_error(.cube_read(cube, drop = NA), "single non-missing logical")
  expect_error(.cube_read(cube, drop = c(FALSE, FALSE)), "single non-missing logical")
})

test_that("indexed reads use positions and keep canonical axis order", {
  cube <- .make_baseline_fixture()$cube

  one <- .cube_read(
    cube,
    index = list(
      longitude = 1,
      latitude = 1,
      depth = 1,
      time = 1,
      variable = 1
    )
  )
  partial <- .cube_read(cube, index = list(time = 2:3))
  reordered <- .cube_read(
    cube,
    index = list(variable = 2, longitude = c(3, 1), time = c(4, 2))
  )

  expect_identical(unname(dim(one)), rep(1L, 5L))
  expect_equal(one[1, 1, 1, 1, 1], 11111)
  expect_identical(
    partial,
    cube$data[, , , 2:3, , drop = FALSE]
  )
  expect_identical(
    reordered,
    cube$data[c(3, 1), , , c(4, 2), 2, drop = FALSE]
  )
  expect_identical(
    unname(dim(reordered)),
    c(2L, 2L, 2L, 2L, 1L)
  )
  expect_identical(names(dim(reordered)), names(dim(cube$data)))
  expect_identical(
    dimnames(reordered),
    dimnames(cube$data[c(3, 1), , , c(4, 2), 2, drop = FALSE])
  )
})

test_that("indexed reads reject ambiguous or unsafe positions", {
  cube <- .make_baseline_fixture()$cube
  duplicated <- structure(list(1L, 2L), names = c("time", "time"))
  unnamed <- list(1L)
  empty_name <- structure(list(1L), names = "")

  expect_error(.cube_read(cube, list(lon = 1L)), "Unknown axis.*lon")
  expect_error(.cube_read(cube, duplicated), "must not contain duplicate")
  expect_error(.cube_read(cube, unnamed), "fully named")
  expect_error(.cube_read(cube, empty_name), "fully named")
  expect_error(.cube_read(cube, list(longitude = integer())), "must not be empty")
  expect_error(.cube_read(cube, list(longitude = 0L)), "between 1 and 3")
  expect_error(.cube_read(cube, list(latitude = -1L)), "between 1 and 2")
  expect_error(.cube_read(cube, list(depth = NA_integer_)), "finite, non-missing")
  expect_error(.cube_read(cube, list(time = Inf)), "finite, non-missing")
  expect_error(.cube_read(cube, list(variable = 1.5)), "whole-number")
  expect_error(.cube_read(cube, list(time = 5L)), "between 1 and 4")
  expect_error(.cube_read(cube, list(time = matrix(1L))), "must be a vector")
  expect_error(.cube_read(cube, list(time = "1")), "must be numeric")
})

test_that("block reads translate start and count into five positional ranges", {
  cube <- .make_baseline_fixture()$cube
  start <- c(2, 1, 1, 2, 1)
  count <- c(2, 2, 1, 2, 2)

  block <- .cube_read_block(cube, start = start, count = count)
  equivalent <- .cube_read(
    cube,
    index = list(
      longitude = 2:3,
      latitude = 1:2,
      depth = 1,
      time = 2:3,
      variable = 1:2
    )
  )
  one <- .cube_read_block(cube, rep(1, 5), rep(1, 5))
  full <- .cube_read_block(cube, rep(1, 5), dim(cube$data))

  expect_identical(block, equivalent)
  expect_identical(
    block,
    cube$data[2:3, 1:2, 1, 2:3, 1:2, drop = FALSE]
  )
  expect_identical(unname(dim(block)), c(2L, 2L, 1L, 2L, 2L))
  expect_identical(unname(dim(one)), rep(1L, 5L))
  expect_equal(one[1, 1, 1, 1, 1], 11111)
  expect_identical(full, cube$data)
  expect_identical(
    .cube_read_block(
      cube,
      start = c(longitude = 1, latitude = 1, depth = 1, time = 1, variable = 1),
      count = c(longitude = 3, latitude = 2, depth = 2, time = 4, variable = 2)
    ),
    cube$data
  )
})

test_that("block reads validate five-dimensional bounds before access", {
  cube <- .make_baseline_fixture()$cube
  good_count <- c(1, 1, 1, 1, 1)

  expect_error(.cube_read_block(cube, 1:4, good_count), "`start`.*must have length 5")
  expect_error(.cube_read_block(cube, rep(1, 5), 1:4), "`count`.*must have length 5")
  expect_error(.cube_read_block(cube, c(1, 1, NA, 1, 1), good_count), "`start`.*finite")
  expect_error(.cube_read_block(cube, rep(1, 5), c(1, 1, Inf, 1, 1)), "`count`.*finite")
  expect_error(.cube_read_block(cube, c(1, 1, 1.5, 1, 1), good_count), "`start`.*whole-number")
  expect_error(.cube_read_block(cube, rep(1, 5), c(1, 1, 1.5, 1, 1)), "`count`.*whole-number")
  expect_error(.cube_read_block(cube, c(0, 1, 1, 1, 1), good_count), "`start`.*at least 1")
  expect_error(.cube_read_block(cube, rep(1, 5), c(1, 0, 1, 1, 1)), "`count`.*at least 1")
  expect_error(.cube_read_block(cube, rep(1, 5), c(1, -1, 1, 1, 1)), "`count`.*at least 1")
  expect_error(
    .cube_read_block(cube, c(3, 1, 1, 1, 1), c(2, 1, 1, 1, 1)),
    "longitude.*exceeds.*3"
  )
  expect_error(
    .cube_read_block(
      cube,
      start = c(latitude = 1, longitude = 1, depth = 1, time = 1, variable = 1),
      count = good_count
    ),
    "`start`.*names must be exactly"
  )
  expect_error(
    .cube_read_block(
      cube,
      start = rep(1, 5),
      count = c(lon = 1, latitude = 1, depth = 1, time = 1, variable = 1)
    ),
    "`count`.*names must be exactly"
  )
  expect_error(
    .cube_read_block(cube, matrix(rep(1, 5)), good_count),
    "`start`.*must be a vector"
  )
})

test_that("block writes return a changed cube without mutating the input", {
  cube <- .make_baseline_fixture()$cube
  cube$source <- "synthetic"
  cube$dataset_id <- "memory-fixture"
  cube$provenance <- list(step = "backend-test")
  cube$qa <- list(status = "verified")
  before <- cube
  replacement <- array(-999, dim = rep(1, 5))

  changed <- .cube_write_block(
    cube,
    values = replacement,
    start = c(2, 1, 2, 3, 2)
  )

  expect_equal(changed$data[2, 1, 2, 3, 2], -999)
  expect_equal(cube$data[2, 1, 2, 3, 2], before$data[2, 1, 2, 3, 2])
  expect_identical(cube, before)
  expect_s3_class(changed, "ocean_cube")
  expect_identical(class(changed), class(cube))
  expect_identical(names(changed), names(cube))
  expect_identical(changed$lon, cube$lon)
  expect_identical(changed$lat, cube$lat)
  expect_identical(changed$depth, cube$depth)
  expect_identical(changed$time, cube$time)
  expect_identical(changed$vars, cube$vars)
  expect_identical(changed$units, cube$units)
  expect_identical(changed$source, cube$source)
  expect_identical(changed$dataset_id, cube$dataset_id)
  expect_identical(changed$provenance, cube$provenance)
  expect_identical(changed$qa, cube$qa)
  expect_identical(dim(changed$data), dim(cube$data))
  expect_identical(dimnames(changed$data), dimnames(cube$data))
  expect_identical(.cube_backend(changed), "memory")
  expect_true(.check_cube(changed))
})

test_that("block writes replace exactly the requested hyper-rectangle", {
  cube <- .make_baseline_fixture()$cube
  start <- c(2, 1, 1, 2, 1)
  count <- c(2, 2, 1, 2, 2)
  values <- array(as.double(seq_len(prod(count))), dim = count)
  expected <- cube$data
  expected[2:3, 1:2, 1, 2:3, 1:2] <- values

  changed <- .cube_write_block(cube, values, start, count)

  expect_identical(changed$data, expected)
  expect_identical(
    as.vector(.cube_read_block(changed, start, count)),
    as.vector(values)
  )
  expect_identical(
    changed$data[1, , , , , drop = FALSE],
    cube$data[1, , , , , drop = FALSE]
  )
  expect_identical(cube$data, .make_baseline_fixture()$cube$data)
})

test_that("block writes require exact five-dimensional shapes and safe types", {
  double_cube <- .make_baseline_fixture()$cube
  integer_cube <- ocean_cube(
    lon = c(-80, -79),
    lat = -12,
    depth = 0,
    time = as.Date(c("2020-01-01", "2020-01-02")),
    vars = "temperature",
    data = array(1:4, dim = c(2, 1, 1, 2, 1))
  )

  double_changed <- .cube_write_block(
    double_cube,
    array(NA_integer_, dim = rep(1, 5)),
    rep(1, 5)
  )
  integer_changed <- .cube_write_block(
    integer_cube,
    array(NA_integer_, dim = rep(1, 5)),
    rep(1, 5)
  )
  nan_changed <- .cube_write_block(
    double_cube,
    array(NaN, dim = rep(1, 5)),
    rep(1, 5)
  )

  expect_identical(typeof(double_changed$data), "double")
  expect_true(is.na(double_changed$data[1, 1, 1, 1, 1]))
  expect_identical(typeof(integer_changed$data), "integer")
  expect_identical(typeof(.cube_read(integer_cube)), "integer")
  expect_true(is.na(integer_changed$data[1, 1, 1, 1, 1]))
  expect_true(is.nan(nan_changed$data[1, 1, 1, 1, 1]))
  expect_error(
    .cube_write_block(integer_cube, array(1.5, dim = rep(1, 5)), rep(1, 5)),
    "integer backend data requires integer `values`"
  )
  expect_error(
    .cube_write_block(double_cube, array("bad", dim = rep(1, 5)), rep(1, 5)),
    "`values`.*must be a numeric array"
  )
  expect_error(
    .cube_write_block(double_cube, -1, rep(1, 5)),
    "`values`.*must be a numeric array"
  )
  expect_error(
    .cube_write_block(double_cube, array(-1, dim = c(1, 1, 1, 1)), rep(1, 5)),
    "`values`.*must have exactly 5 dimensions"
  )
  expect_error(
    .cube_write_block(
      double_cube,
      array(-1, dim = c(2, 1, 1, 1, 1)),
      rep(1, 5),
      rep(1, 5)
    ),
    "`values` dimensions.*must equal `count`"
  )
  expect_error(
    .cube_write_block(
      double_cube,
      array(-1, dim = rep(1, 5)),
      c(4, 1, 1, 1, 1)
    ),
    "longitude.*exceeds.*3"
  )
})

test_that("memory reads preserve missing values and storage mode", {
  cube <- .make_baseline_fixture()$cube
  cube$data[1, 1, 1, 1, 1] <- NA_real_
  cube$data[2, 1, 1, 1, 1] <- NaN

  result <- .cube_read(cube, list(longitude = 1:2, variable = 1))

  expect_identical(typeof(result), "double")
  expect_true(is.na(result[1, 1, 1, 1, 1]))
  expect_true(is.nan(result[2, 1, 1, 1, 1]))
  expect_identical(unname(dim(result)), c(2L, 2L, 2L, 4L, 1L))
})

test_that("the new backend layer does not alter scientific results", {
  cube <- .make_baseline_fixture()$cube
  clim <- clim_month(cube)
  layer <- layer_mean(cube, c(0, 50))
  linked <- link_events(
    cube,
    data.frame(
      lon = -80,
      lat = -12,
      date = as.Date("2020-01-01")
    ),
    vars = "temperature"
  )

  expect_equal(clim$mean[1, 1, 1, 1, 1], 12111)
  expect_equal(layer$data[1, 1, 1, 1, 1], 11161)
  expect_equal(linked$temperature_value, 11111)
  expect_identical(.cube_read(cube), cube$data)
})
