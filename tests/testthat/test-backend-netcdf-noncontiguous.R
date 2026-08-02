noncontiguous_plan_fixture <- function() {
  file <- make_netcdf_backend_fixture()
  storage <- .new_netcdf_storage(
    file,
    c("temperature", "oxygen")
  )
  list(
    file = file,
    cube = .new_netcdf_cube(storage)
  )
}

noncontiguous_expected_variable <- function(variable) {
  variable_index <- switch(
    variable,
    temperature = 1L,
    oxygen = 2L,
    stop("Unknown fixture variable.")
  )
  values <- array(NA_real_, dim = c(3L, 2L, 2L, 4L))
  for (time_index in seq_len(4L)) {
    for (depth_index in seq_len(2L)) {
      for (latitude_index in seq_len(2L)) {
        for (longitude_index in seq_len(3L)) {
          values[
            longitude_index,
            latitude_index,
            depth_index,
            time_index
          ] <- 10000 * variable_index +
            1000 * time_index +
            100 * depth_index +
            10 * latitude_index +
            longitude_index
        }
      }
    }
  }
  if (identical(variable, "temperature")) {
    values[1, 2, 2, 2] <- NA_real_
    values[3, 1, 2, 3] <- NA_real_
  } else {
    values[1, 2, 1, 3] <- NA_real_
  }
  values
}

noncontiguous_equivalent_memory <- function(x) {
  data <- array(
    NA_real_,
    dim = unname(.cube_shape(x)),
    dimnames = stats::setNames(
      list(
        as.character(x$lon),
        as.character(x$lat),
        as.character(x$depth),
        as.character(x$time),
        x$vars
      ),
      .cube_axis_names()
    )
  )
  for (variable_index in seq_along(x$vars)) {
    data[, , , , variable_index] <-
      noncontiguous_expected_variable(x$vars[[variable_index]])
  }
  memory <- ocean_cube(
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    time = as.Date(x$time),
    vars = x$vars,
    data = data
  )
  dimnames(memory$data) <- dimnames(data)
  memory
}

test_that("plan calculates the minimum envelope and local positions", {
  fixture <- noncontiguous_plan_fixture()
  withr::local_file(fixture$file)
  plan <- .plan_cube_index_read(
    fixture$cube,
    list(
      longitude = c(3L, 1L),
      time = c(4L, 2L),
      variable = c(2L, 1L)
    )
  )

  expect_identical(
    plan$physical_start,
    c(longitude = 1L, latitude = 1L, depth = 1L, time = 2L)
  )
  expect_identical(
    plan$physical_count,
    c(longitude = 3L, latitude = 2L, depth = 2L, time = 3L)
  )
  expect_identical(plan$local_index$longitude, c(3L, 1L))
  expect_identical(plan$local_index$latitude, 1:2)
  expect_identical(plan$local_index$depth, 1:2)
  expect_identical(plan$local_index$time, c(3L, 1L))
  expect_identical(plan$variable_index, c(2L, 1L))
  expect_identical(
    plan$output_shape,
    c(longitude = 2L, latitude = 2L, depth = 2L, time = 2L, variable = 2L)
  )
  expect_identical(plan$values_requested, 32)
  expect_identical(plan$values_in_envelope, 72)
  expect_identical(plan$amplification, 2.25)
})

test_that("plan preserves arbitrary order and duplicate positions", {
  fixture <- noncontiguous_plan_fixture()
  withr::local_file(fixture$file)
  plan <- .plan_cube_index_read(
    fixture$cube,
    list(
      longitude = c(3L, 1L, 3L),
      latitude = c(2L, 1L, 2L),
      depth = c(2L, 1L, 2L),
      time = c(4L, 2L, 4L),
      variable = c(2L, 1L, 2L)
    )
  )

  expect_identical(plan$local_index$longitude, c(3L, 1L, 3L))
  expect_identical(plan$local_index$latitude, c(2L, 1L, 2L))
  expect_identical(plan$local_index$depth, c(2L, 1L, 2L))
  expect_identical(plan$local_index$time, c(3L, 1L, 3L))
  expect_identical(plan$variable_index, c(2L, 1L, 2L))
  expect_identical(unname(plan$output_shape), rep(3L, 5L))
})

test_that("plan treats the logical variable axis outside the envelope", {
  data <- array(
    as.double(seq_len(3 * 2 * 2 * 4 * 3)),
    dim = c(3L, 2L, 2L, 4L, 3L)
  )
  cube <- ocean_cube(
    lon = c(-80, -79, -78),
    lat = c(-12, -11),
    depth = c(0, 50),
    time = as.Date("2020-01-01") + 0:3,
    vars = c("first", "middle", "last"),
    data = data
  )
  plan <- .plan_cube_index_read(
    cube,
    list(variable = c(3L, 1L))
  )

  expect_identical(plan$variable_index, c(3L, 1L))
  expect_identical(
    plan$physical_count,
    c(longitude = 3L, latitude = 2L, depth = 2L, time = 4L)
  )
  expect_identical(plan$values_in_envelope, 96)
  expect_identical(plan$values_requested, 96)
  expect_identical(plan$amplification, 1)
})

test_that("plan completes omitted axes and full selections", {
  fixture <- noncontiguous_plan_fixture()
  withr::local_file(fixture$file)
  full <- .plan_cube_index_read(fixture$cube)
  partial <- .plan_cube_index_read(
    fixture$cube,
    list(depth = 2L, longitude = 1L)
  )

  expect_identical(full$requested$longitude, 1:3)
  expect_identical(full$requested$latitude, 1:2)
  expect_identical(full$requested$depth, 1:2)
  expect_identical(full$requested$time, 1:4)
  expect_identical(full$requested$variable, 1:2)
  expect_identical(full$amplification, 1)
  expect_identical(
    partial$output_shape,
    c(longitude = 1L, latitude = 2L, depth = 1L, time = 4L, variable = 2L)
  )
})

test_that("noncontiguous read is identical to memory across selections", {
  fixture <- noncontiguous_plan_fixture()
  withr::local_file(fixture$file)
  memory <- noncontiguous_equivalent_memory(fixture$cube)
  selections <- list(
    full = list(),
    longitude = list(longitude = c(1L, 3L)),
    latitude = list(latitude = c(2L, 1L)),
    depth = list(depth = c(2L, 1L)),
    time = list(time = c(1L, 4L)),
    variables = list(variable = c(2L, 1L)),
    multiple = list(
      longitude = c(3L, 1L),
      latitude = 2L,
      time = c(4L, 2L),
      variable = c(2L, 1L)
    ),
    arbitrary = list(
      longitude = c(3L, 1L, 2L),
      latitude = c(2L, 1L),
      depth = c(2L, 1L),
      time = c(4L, 1L, 3L),
      variable = c(2L, 1L)
    ),
    duplicated = list(
      longitude = c(1L, 1L, 3L),
      time = c(4L, 2L, 4L),
      variable = c(2L, 1L, 2L)
    ),
    cell = list(
      longitude = 3L,
      latitude = 2L,
      depth = 2L,
      time = 4L,
      variable = 2L
    ),
    partial = list(variable = 2L, longitude = c(3L, 1L))
  )

  for (selection in selections) {
    netcdf <- .cube_read(fixture$cube, index = selection)
    expected <- .cube_read(memory, index = selection)
    expect_identical(netcdf, expected)
    expect_length(dim(netcdf), 5L)
    expect_identical(names(dimnames(netcdf)), .cube_axis_names())
  }
})

test_that("noncontiguous read preserves requested dimname order", {
  fixture <- noncontiguous_plan_fixture()
  withr::local_file(fixture$file)
  index <- list(
    longitude = c(3L, 1L),
    time = c(4L, 2L),
    variable = c(2L, 1L)
  )
  result <- .cube_read(fixture$cube, index = index)

  expect_identical(
    dimnames(result)$longitude,
    as.character(fixture$cube$lon[index$longitude])
  )
  expect_identical(
    dimnames(result)$time,
    as.character(fixture$cube$time[index$time])
  )
  expect_identical(
    dimnames(result)$variable,
    fixture$cube$vars[index$variable]
  )
  expect_identical(unname(dim(result)), c(2L, 2L, 2L, 2L, 2L))
})

test_that("noncontiguous read requests only the minimum physical envelope", {
  fixture <- noncontiguous_plan_fixture()
  withr::local_file(fixture$file)
  observed <- list()
  original <- .ncvar_get_block
  local_mocked_bindings(
    .ncvar_get_block = function(nc, variable, start, count) {
      observed[[length(observed) + 1L]] <<- list(
        variable = variable,
        start = start,
        count = count
      )
      original(nc, variable, start, count)
    },
    .package = "oceancube"
  )

  result <- .cube_read(
    fixture$cube,
    index = list(
      longitude = c(2L, 1L),
      latitude = 2L,
      depth = 1L,
      time = c(3L, 2L),
      variable = 2L
    )
  )

  expect_length(observed, 1L)
  expect_identical(observed[[1L]]$variable, "oxygen")
  expect_identical(
    observed[[1L]]$start,
    c(time = 2L, depth = 1L, latitude = 2L, longitude = 1L)
  )
  expect_identical(
    observed[[1L]]$count,
    c(time = 2L, depth = 1L, latitude = 1L, longitude = 2L)
  )
  expect_equal(prod(observed[[1L]]$count), 4)
  expect_identical(length(result), 4L)
})

test_that("noncontiguous read opens once and reads requested variables only", {
  fixture <- noncontiguous_plan_fixture()
  withr::local_file(fixture$file)
  openings <- 0L
  variables <- character()
  original_connection <- .with_netcdf_connection
  original_read <- .ncvar_get_block
  local_mocked_bindings(
    .with_netcdf_connection = function(file, code) {
      openings <<- openings + 1L
      original_connection(file, code)
    },
    .ncvar_get_block = function(nc, variable, start, count) {
      variables <<- c(variables, variable)
      original_read(nc, variable, start, count)
    },
    .package = "oceancube"
  )

  result <- .cube_read(
    fixture$cube,
    index = list(
      longitude = c(3L, 1L),
      time = c(4L, 2L),
      variable = c(2L, 1L, 2L)
    )
  )

  expect_identical(openings, 1L)
  expect_identical(
    variables,
    c("oxygen", "temperature", "oxygen")
  )
  expect_identical(
    dimnames(result)$variable,
    c("oxygen", "temperature", "oxygen")
  )
})

test_that("invalid indices fail before opening or reading NetCDF", {
  fixture <- noncontiguous_plan_fixture()
  withr::local_file(fixture$file)
  memory <- noncontiguous_equivalent_memory(fixture$cube)
  before <- fixture$cube$storage
  openings <- 0L
  reads <- 0L
  local_mocked_bindings(
    .with_netcdf_connection = function(file, code) {
      openings <<- openings + 1L
      stop("connection must not open")
    },
    .ncvar_get_block = function(nc, variable, start, count) {
      reads <<- reads + 1L
      stop("physical read must not occur")
    },
    .package = "oceancube"
  )
  invalid <- list(
    list(longitude = 0L),
    list(latitude = -1L),
    list(depth = NA_integer_),
    list(time = 100L),
    list(variable = numeric()),
    list(longitude = 1.5),
    list(unknown = 1L),
    list(1L, 2L)
  )

  for (index in invalid) {
    memory_error <- tryCatch(
      .cube_read(memory, index = index),
      error = conditionMessage
    )
    netcdf_error <- tryCatch(
      .cube_read(fixture$cube, index = index),
      error = conditionMessage
    )
    expect_identical(netcdf_error, memory_error)
  }
  expect_identical(openings, 0L)
  expect_identical(reads, 0L)
  expect_identical(fixture$cube$storage, before)
})

test_that("noncontiguous plan reports maximum fixture amplification", {
  fixture <- noncontiguous_plan_fixture()
  withr::local_file(fixture$file)
  plan <- .plan_cube_index_read(
    fixture$cube,
    list(
      longitude = c(1L, 3L),
      time = c(1L, 4L)
    )
  )

  expect_identical(plan$values_requested, 32)
  expect_identical(plan$values_in_envelope, 96)
  expect_identical(plan$amplification, 3)
})
