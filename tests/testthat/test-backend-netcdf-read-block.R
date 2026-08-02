netcdf_read_expected_variable <- function(variable) {
  variable_index <- switch(
    variable,
    temperature = 1L,
    oxygen = 2L,
    sst = 1L,
    stop("Unknown fixture variable.")
  )
  include_depth <- !identical(variable, "sst")
  dimensions <- if (include_depth) {
    c(3L, 2L, 2L, 4L)
  } else {
    c(3L, 2L, 4L)
  }
  values <- array(NA_real_, dim = dimensions)

  if (include_depth) {
    for (time_index in seq_len(dimensions[[4L]])) {
      for (depth_index in seq_len(dimensions[[3L]])) {
        for (latitude_index in seq_len(dimensions[[2L]])) {
          for (longitude_index in seq_len(dimensions[[1L]])) {
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
  } else {
    for (time_index in seq_len(dimensions[[3L]])) {
      for (latitude_index in seq_len(dimensions[[2L]])) {
        for (longitude_index in seq_len(dimensions[[1L]])) {
          values[
            longitude_index,
            latitude_index,
            time_index
          ] <- 10000 * variable_index +
            1000 * time_index +
            100 +
            10 * latitude_index +
            longitude_index
        }
      }
    }
  }

  if (identical(variable, "temperature")) {
    values[1, 2, 2, 2] <- NA_real_
    values[3, 1, 2, 3] <- NA_real_
  }
  if (identical(variable, "oxygen")) {
    values[1, 2, 1, 3] <- NA_real_
  }
  values
}

netcdf_read_expected_cube <- function(x) {
  dimensions <- unname(.cube_shape(x))
  result <- array(
    NA_real_,
    dim = dimensions,
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
    variable <- x$vars[[variable_index]]
    values <- netcdf_read_expected_variable(variable)
    if (identical(variable, "sst")) {
      result[, , 1, , variable_index] <- values
    } else {
      result[, , , , variable_index] <- values
    }
  }
  result
}

netcdf_read_fixture <- function(variables = c("temperature", "oxygen")) {
  file <- make_netcdf_backend_fixture()
  storage <- .new_netcdf_storage(file, variables)
  list(
    file = file,
    storage = storage,
    cube = .new_netcdf_cube(storage)
  )
}

test_that("translate maps canonical and reversed physical blocks", {
  fixture <- netcdf_read_fixture()
  withr::local_file(fixture$file)
  canonical_start <- c(2L, 1L, 1L, 2L)
  canonical_count <- c(2L, 2L, 1L, 2L)

  temperature <- .translate_netcdf_block(
    fixture$storage$variables$map$temperature,
    canonical_start,
    canonical_count
  )
  expect_identical(
    temperature$source_start,
    c(longitude = 2L, latitude = 1L, depth = 1L, time = 2L)
  )
  expect_identical(
    temperature$source_count,
    c(longitude = 2L, latitude = 2L, depth = 1L, time = 2L)
  )
  expect_identical(temperature$permutation, 1:4)
  expect_identical(temperature$singleton_axes, character())

  oxygen <- .translate_netcdf_block(
    fixture$storage$variables$map$oxygen,
    canonical_start,
    canonical_count
  )
  expect_identical(
    oxygen$source_start,
    c(time = 2L, depth = 1L, latitude = 1L, longitude = 2L)
  )
  expect_identical(
    oxygen$source_count,
    c(time = 2L, depth = 1L, latitude = 2L, longitude = 2L)
  )
  expect_identical(
    oxygen$source_axes,
    c("time", "depth", "latitude", "longitude")
  )
  expect_identical(oxygen$permutation, c(4L, 3L, 2L, 1L))
})

test_that("translate records singleton insertion for a surface variable", {
  fixture <- netcdf_read_fixture("sst")
  withr::local_file(fixture$file)
  translated <- .translate_netcdf_block(
    fixture$storage$variables$map$sst,
    c(1L, 1L, 1L, 2L),
    c(2L, 1L, 1L, 2L)
  )

  expect_identical(
    translated$source_start,
    c(longitude = 1L, latitude = 1L, time = 2L)
  )
  expect_identical(
    translated$source_count,
    c(longitude = 2L, latitude = 1L, time = 2L)
  )
  expect_identical(translated$singleton_axes, "depth")
})

test_that("read block returns canonical values, dimensions, and dimnames", {
  fixture <- netcdf_read_fixture()
  withr::local_file(fixture$file)
  block <- .cube_read_block(
    fixture$cube,
    start = c(
      longitude = 2L,
      latitude = 1L,
      depth = 1L,
      time = 2L,
      variable = 1L
    ),
    count = c(
      longitude = 2L,
      latitude = 2L,
      depth = 1L,
      time = 2L,
      variable = 2L
    )
  )

  expect_identical(unname(dim(block)), c(2L, 2L, 1L, 2L, 2L))
  expect_identical(names(dimnames(block)), .cube_axis_names())
  expect_identical(dimnames(block)$longitude, c("-79", "-78"))
  expect_identical(dimnames(block)$latitude, c("-12", "-11"))
  expect_identical(dimnames(block)$depth, "0")
  expect_identical(
    dimnames(block)$time,
    c("2000-01-02", "2000-01-03")
  )
  expect_identical(
    dimnames(block)$variable,
    c("temperature", "oxygen")
  )
  expect_identical(block[1, 1, 1, 1, 1], 12112)
  expect_identical(block[2, 2, 1, 2, 2], 23123)
})

test_that("read complete reuses blocks and respects the deterministic formula", {
  fixture <- netcdf_read_fixture()
  withr::local_file(fixture$file)
  full <- .cube_read(fixture$cube)
  expected <- netcdf_read_expected_cube(fixture$cube)

  expect_identical(full, expected)
  expect_identical(typeof(full), "double")
  expect_identical(unname(dim(full)), c(3L, 2L, 2L, 4L, 2L))
  expect_identical(full[1, 1, 1, 1, 1], 11111)
  expect_identical(full[3, 2, 2, 4, 2], 24223)
})

test_that("read preserves the logical variable order stored in the descriptor", {
  fixture <- netcdf_read_fixture(c("oxygen", "temperature"))
  withr::local_file(fixture$file)
  full <- .cube_read(fixture$cube)

  expect_identical(
    dimnames(full)$variable,
    c("oxygen", "temperature")
  )
  expect_identical(full[3, 2, 2, 4, 1], 24223)
  expect_identical(full[1, 1, 1, 1, 2], 11111)
  expect_identical(full, netcdf_read_expected_cube(fixture$cube))
})

test_that("read block matches cells, corners, interior, full, and variables", {
  fixture <- netcdf_read_fixture()
  withr::local_file(fixture$file)
  expected <- netcdf_read_expected_cube(fixture$cube)
  cases <- list(
    cell = list(start = c(1, 1, 1, 1, 1), count = c(1, 1, 1, 1, 1)),
    initial = list(start = c(1, 1, 1, 1, 1), count = c(2, 1, 1, 2, 2)),
    final = list(start = c(3, 2, 2, 4, 2), count = c(1, 1, 1, 1, 1)),
    interior = list(start = c(2, 1, 1, 2, 1), count = c(2, 2, 1, 2, 2)),
    one_variable = list(start = c(1, 1, 1, 1, 2), count = c(3, 2, 2, 4, 1)),
    one_time = list(start = c(1, 1, 1, 3, 1), count = c(3, 2, 2, 1, 2)),
    one_depth = list(start = c(1, 1, 2, 1, 1), count = c(3, 2, 1, 4, 2)),
    full = list(start = rep(1L, 5L), count = unname(dim(expected)))
  )

  for (case in cases) {
    actual <- .cube_read_block(
      fixture$cube,
      case$start,
      case$count
    )
    index <- Map(
      function(first, count) seq.int(first, length.out = count),
      case$start,
      case$count
    )
    wanted <- do.call(
      `[`,
      c(list(expected), index, list(drop = FALSE))
    )
    expect_identical(actual, wanted)
    expect_length(dim(actual), 5L)
  }
})

test_that("read handles fill, missing, scale, offset, and numeric type once", {
  fixture <- netcdf_read_fixture()
  withr::local_file(fixture$file)
  full <- .cube_read(fixture$cube)

  expect_identical(full[1, 1, 1, 1, 1], 11111)
  expect_true(is.na(full[1, 2, 2, 2, 1]))
  expect_true(is.na(full[3, 1, 2, 3, 1]))
  expect_true(is.na(full[1, 2, 1, 3, 2]))
  expect_false(any(full == -32767, na.rm = TRUE))
  expect_false(any(full == -32766, na.rm = TRUE))
  expect_identical(typeof(full), "double")

  .with_netcdf_connection(fixture$file, function(nc) {
    packed <- .ncvar_get_block(
      nc,
      "temperature",
      c(1L, 1L, 1L, 1L),
      rep(1L, 4L)
    )
    expect_identical(as.integer(packed), 222L)
  })
})

test_that("read surface inserts exactly one logical depth dimension", {
  fixture <- netcdf_read_fixture("sst")
  withr::local_file(fixture$file)
  full <- .cube_read(fixture$cube)
  expected <- netcdf_read_expected_cube(fixture$cube)

  expect_identical(full, expected)
  expect_identical(unname(dim(full)), c(3L, 2L, 1L, 4L, 1L))
  expect_identical(dimnames(full)$depth, NA_character_)
  expect_identical(full[3, 2, 1, 4, 1], 14123)
})

test_that("read index accepts contiguous partial lists in any order", {
  fixture <- netcdf_read_fixture()
  withr::local_file(fixture$file)
  expected <- netcdf_read_expected_cube(fixture$cube)

  first <- .cube_read(
    fixture$cube,
    index = list(time = 2:4)
  )
  expect_identical(first, expected[, , , 2:4, , drop = FALSE])

  second <- .cube_read(
    fixture$cube,
    index = list(variable = 1:2, longitude = 1:2)
  )
  expect_identical(second, expected[1:2, , , , 1:2, drop = FALSE])

  third <- .cube_read(
    fixture$cube,
    index = list(depth = 1L)
  )
  expect_identical(third, expected[, , 1, , , drop = FALSE])
})

test_that("read index accepts non-contiguous positions like memory", {
  fixture <- netcdf_read_fixture()
  withr::local_file(fixture$file)
  memory_data <- netcdf_read_expected_cube(fixture$cube)
  memory <- ocean_cube(
    lon = fixture$cube$lon,
    lat = fixture$cube$lat,
    depth = fixture$cube$depth,
    time = as.Date(fixture$cube$time),
    vars = fixture$cube$vars,
    data = memory_data
  )
  dimnames(memory$data) <- dimnames(memory_data)
  expect_identical(
    .cube_read(
      fixture$cube,
      index = list(
        longitude = c(1L, 3L),
        time = c(3L, 2L),
        variable = c(2L, 1L)
      )
    ),
    .cube_read(
      memory,
      index = list(
        longitude = c(1L, 3L),
        time = c(3L, 2L),
        variable = c(2L, 1L)
      )
    )
  )
})

test_that("read block reuses shared validation for all five axes", {
  fixture <- netcdf_read_fixture()
  withr::local_file(fixture$file)
  expect_error(
    .cube_read_block(fixture$cube, rep(1L, 4L), rep(1L, 5L)),
    "length 5"
  )
  expect_error(
    .cube_read_block(fixture$cube, c(1, 1, NA, 1, 1), rep(1L, 5L)),
    "finite, non-missing"
  )
  expect_error(
    .cube_read_block(fixture$cube, rep(0L, 5L), rep(1L, 5L)),
    "at least 1"
  )
  expect_error(
    .cube_read_block(fixture$cube, rep(1L, 5L), c(4, 1, 1, 1, 1)),
    "longitude.*exceeds axis size",
    class = "oceancube_bad_block"
  )
  expect_error(
    .cube_read_block(
      fixture$cube,
      stats::setNames(rep(1L, 5L), rev(.cube_axis_names())),
      rep(1L, 5L)
    ),
    "names must be exactly"
  )
})

test_that("read block requests only the physical rectangle needed", {
  fixture <- netcdf_read_fixture()
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

  block <- .cube_read_block(
    fixture$cube,
    start = c(2L, 1L, 1L, 2L, 1L),
    count = c(2L, 1L, 1L, 2L, 2L)
  )
  expect_identical(unname(dim(block)), c(2L, 1L, 1L, 2L, 2L))
  expect_length(observed, 2L)
  expect_identical(observed[[1L]]$variable, "temperature")
  expect_identical(
    observed[[1L]]$start,
    c(longitude = 2L, latitude = 1L, depth = 1L, time = 2L)
  )
  expect_identical(
    observed[[1L]]$count,
    c(longitude = 2L, latitude = 1L, depth = 1L, time = 2L)
  )
  expect_identical(observed[[2L]]$variable, "oxygen")
  expect_identical(
    observed[[2L]]$start,
    c(time = 2L, depth = 1L, latitude = 1L, longitude = 2L)
  )
  expect_identical(
    observed[[2L]]$count,
    c(time = 2L, depth = 1L, latitude = 1L, longitude = 2L)
  )
  expect_true(all(vapply(
    observed,
    function(read) prod(read$count) == 4,
    logical(1)
  )))
})

test_that("read uses one connection and one physical call per variable", {
  fixture <- netcdf_read_fixture()
  withr::local_file(fixture$file)
  openings <- 0L
  physical_reads <- 0L
  original_connection <- .with_netcdf_connection
  original_read <- .ncvar_get_block
  local_mocked_bindings(
    .with_netcdf_connection = function(file, code) {
      openings <<- openings + 1L
      original_connection(file, code)
    },
    .ncvar_get_block = function(nc, variable, start, count) {
      physical_reads <<- physical_reads + 1L
      original_read(nc, variable, start, count)
    },
    .package = "oceancube"
  )

  result <- .cube_read_block(
    fixture$cube,
    c(1L, 1L, 1L, 1L, 1L),
    c(2L, 1L, 1L, 2L, 2L)
  )
  expect_identical(openings, 1L)
  expect_identical(physical_reads, 2L)
  expect_length(result, 8L)
})

test_that("read always closes the connection after success and error", {
  fixture <- netcdf_read_fixture()
  moved <- paste0(fixture$file, ".moved")
  withr::defer(unlink(c(fixture$file, moved)))

  expect_silent(.cube_read_block(
    fixture$cube,
    rep(1L, 5L),
    rep(1L, 5L)
  ))
  expect_true(file.rename(fixture$file, moved))
  expect_true(file.rename(moved, fixture$file))

  original <- .ncvar_get_block
  local_mocked_bindings(
    .ncvar_get_block = function(nc, variable, start, count) {
      if (identical(variable, "oxygen")) {
        stop("forced second-variable failure")
      }
      original(nc, variable, start, count)
    },
    .package = "oceancube"
  )
  expect_error(
    .cube_read_block(
      fixture$cube,
      rep(1L, 5L),
      c(1L, 1L, 1L, 1L, 2L)
    ),
    "forced second-variable failure"
  )
  expect_true(file.rename(fixture$file, moved))
  expect_true(file.rename(moved, fixture$file))
})

test_that("read rejects corrupt permutations and physical schema mismatches", {
  fixture <- netcdf_read_fixture()
  withr::local_file(fixture$file)
  corrupt <- fixture$cube
  corrupt$storage$variables$map$oxygen$
    source_to_canonical_permutation <- c(1L, 1L, 3L, 4L)
  expect_error(
    .cube_read(corrupt),
    "Invalid NetCDF permutation",
    class = "oceancube_bad_storage"
  )

  .with_netcdf_connection(fixture$file, function(nc) {
    incompatible <- nc
    incompatible$var$oxygen <- NULL
    expect_error(
      .validate_netcdf_physical_schema(
        incompatible,
        fixture$storage,
        c("temperature", "oxygen")
      ),
      "oxygen.*no longer present",
      class = "oceancube_netcdf_missing_variable"
    )
  })
})

test_that("read survives RDS serialization without a saved connection", {
  fixture <- netcdf_read_fixture()
  withr::local_file(fixture$file)
  rds <- tempfile(tmpdir = tempdir(), fileext = ".rds")
  withr::local_file(rds)
  saveRDS(fixture$cube, rds)
  restored <- readRDS(rds)

  block <- .cube_read_block(
    restored,
    rep(1L, 5L),
    rep(1L, 5L)
  )
  expect_identical(unname(block), array(11111, dim = rep(1L, 5L)))
  expect_false(.netcdf_contains_forbidden_object(restored$storage))
})

test_that("read detects a removed or modified source before physical access", {
  removed <- netcdf_read_fixture()
  expect_identical(unlink(removed$file), 0L)
  expect_error(
    .cube_read(removed$cube),
    "no longer exists",
    class = "oceancube_netcdf_changed_file"
  )

  modified <- netcdf_read_fixture()
  withr::local_file(modified$file)
  connection <- file(modified$file, open = "ab")
  writeBin(as.raw(0), connection)
  close(connection)
  expect_error(
    .cube_read(modified$cube),
    "expected size.*found",
    class = "oceancube_netcdf_changed_file"
  )
})

test_that("read-only policy leaves source size and modification time unchanged", {
  fixture <- netcdf_read_fixture()
  withr::local_file(fixture$file)
  before <- file.info(fixture$file)
  expect_silent(.cube_read(fixture$cube))
  expect_error(
    .cube_write_block(
      fixture$cube,
      array(0, dim = rep(1L, 5L)),
      rep(1L, 5L)
    ),
    "NetCDF backend is read-only",
    class = "oceancube_netcdf_read_only"
  )
  after <- file.info(fixture$file)
  expect_identical(as.double(after$size), as.double(before$size))
  expect_identical(as.numeric(after$mtime), as.numeric(before$mtime))
})

test_that("read NetCDF and memory backends are structurally equivalent", {
  fixture <- netcdf_read_fixture()
  withr::local_file(fixture$file)
  expected <- netcdf_read_expected_cube(fixture$cube)
  memory <- ocean_cube(
    lon = fixture$cube$lon,
    lat = fixture$cube$lat,
    depth = fixture$cube$depth,
    time = as.Date(fixture$cube$time),
    vars = fixture$cube$vars,
    data = expected
  )
  dimnames(memory$data) <- dimnames(expected)

  expect_identical(.cube_read(fixture$cube), .cube_read(memory))
  expect_identical(
    .cube_read_block(
      fixture$cube,
      c(2L, 1L, 1L, 2L, 1L),
      c(2L, 2L, 1L, 2L, 2L)
    ),
    .cube_read_block(
      memory,
      c(2L, 1L, 1L, 2L, 1L),
      c(2L, 2L, 1L, 2L, 2L)
    )
  )
})

test_that("read matches read_nc after classified baseline differences", {
  fixture <- netcdf_read_fixture()
  withr::local_file(fixture$file)
  legacy <- read_nc(
    fixture$file,
    vars = c("temperature", "oxygen")
  )
  lazy <- .cube_read(fixture$cube)

  expect_identical(unname(dim(legacy$data)), unname(dim(lazy)))
  expect_identical(legacy$data[1, 1, 1, 1, 1], 11111)
  expect_identical(legacy$data[3, 2, 2, 4, 2], 24223)
  expect_identical(legacy$data[1, 2, 2, 2, 1], -5383.5)
  expect_true(is.na(lazy[1, 2, 2, 2, 1]))
  expect_identical(names(dimnames(legacy$data)), c(
    "lon", "lat", "depth", "time", "var"
  ))
  expect_identical(names(dimnames(lazy)), .cube_axis_names())

  repaired_legacy <- legacy$data
  repaired_legacy[1, 2, 2, 2, 1] <- NA_real_
  dimnames(repaired_legacy) <- dimnames(lazy)
  expect_identical(repaired_legacy, lazy)
})
