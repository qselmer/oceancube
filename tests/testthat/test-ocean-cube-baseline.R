test_that("ocean_cube preserves the deterministic 5D public contract", {
  fixture <- .make_baseline_fixture()
  cube <- fixture$cube

  expect_s3_class(cube, "ocean_cube")
  expect_identical(unname(dim(cube$data)), c(3L, 2L, 2L, 4L, 2L))
  expect_identical(names(dimnames(cube$data)), c("lon", "lat", "depth", "time", "var"))
  expect_identical(cube$lon, fixture$longitude)
  expect_identical(cube$lat, fixture$latitude)
  expect_identical(cube$depth, fixture$depth)
  expect_identical(cube$time, fixture$time)
  expect_identical(cube$vars, fixture$variable)
  expect_equal(cube$data[1, 1, 1, 1, 1], 11111)
  expect_equal(cube$data[3, 2, 2, 4, 2], 24223)
  expect_length(cube$data, 96)
  expect_false(anyNA(cube$data))
  expect_true(is.numeric(cube$data))
  expect_identical(fixture$values, fixture$values_before)
  expect_no_error(print(cube))
  expect_no_error(summary(cube))
})

test_that("ocean_cube rejects incompatible coordinate dimensions", {
  expect_error(
    ocean_cube(
      lon = c(-80, -79),
      lat = -12,
      depth = 0,
      time = as.Date("2020-01-01"),
      data = array(1, dim = c(1, 1, 1, 1, 1)),
      vars = "temperature"
    ),
    "length must match"
  )
})

test_that("surface cubes have an unknown finite-safe depth extent", {
  surface_data <- array(1:4, dim = c(2, 1, 2, 1))

  expect_no_warning(
    surface <- ocean_cube(
      lon = c(-80, -79),
      lat = -12,
      time = as.Date(c("2020-01-01", "2020-01-02")),
      data = surface_data,
      vars = "surface"
    )
  )

  expect_identical(surface$depth, NA_real_)
  expect_identical(surface$depth_extent, c(NA_real_, NA_real_))
  expect_no_warning(print(surface))
  expect_no_warning(surface_summary <- summary(surface))
  expect_true(all(is.na(surface_summary[surface_summary$field == "depth", c("min", "max")])))
})

test_that("ocean_cube rejects invalid depth coordinates", {
  expect_error(
    ocean_cube(
      lon = c(-80, -79),
      lat = c(-12, -11),
      depth = c(NA_real_, NA_real_),
      time = as.Date("2020-01-01"),
      data = array(1:8, dim = c(2, 2, 2, 1, 1)),
      vars = "temperature"
    ),
    "single NA"
  )
  expect_error(
    ocean_cube(
      lon = c(-80, -79),
      lat = c(-12, -11),
      depth = c(0, NA_real_, 50),
      time = as.Date("2020-01-01"),
      data = array(1:12, dim = c(2, 2, 3, 1, 1)),
      vars = "temperature"
    ),
    "single NA"
  )
  expect_error(
    ocean_cube(
      lon = c(-80, -79),
      lat = c(-12, -11),
      depth = numeric(0),
      time = as.Date("2020-01-01"),
      data = array(numeric(0), dim = c(2, 2, 0, 1, 1)),
      vars = "temperature"
    ),
    "must not be empty"
  )
  expect_error(
    ocean_cube(
      lon = c(-80, -79),
      lat = c(-12, -11),
      depth = c(-Inf, Inf),
      time = as.Date("2020-01-01"),
      data = array(1:8, dim = c(2, 2, 2, 1, 1)),
      vars = "temperature"
    ),
    "finite"
  )
})

test_that("print and summary report coordinate depth rather than optional metadata", {
  cube <- ocean_cube(
    lon = c(-80, -79),
    lat = c(-12, -11),
    depth = c(0, 50),
    time = as.Date("2020-01-01"),
    data = array(1:8, dim = c(2, 2, 2, 1, 1)),
    vars = "temperature",
    depth_extent = c(-10, 100)
  )

  expect_identical(cube$depth_extent, c(-10, 100))
  printed <- capture.output(print(cube))
  expect_true(any(grepl("depth      : 0 to 50", printed, fixed = TRUE)))
  depth_summary <- summary(cube)
  depth_summary <- depth_summary[depth_summary$field == "depth", , drop = FALSE]
  expect_equal(as.numeric(depth_summary$min), 0)
  expect_equal(as.numeric(depth_summary$max), 50)
})
