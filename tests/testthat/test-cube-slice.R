slice_baseline_cube <- function() {
  cube <- .make_baseline_fixture()$cube
  cube$source <- "synthetic"
  cube$dataset_id <- "slice-001"
  cube$provenance <- list(previous_step = "fixture")
  cube$qa <- list(reviewed = TRUE)
  cube$dc <- matrix(seq_len(6L), nrow = 3L, ncol = 2L)
  cube$mask <- stock_mask(cube, stock = "fixture")
  cube$climatology <- list(scale = "month")
  cube$anomaly <- list(method = "difference")
  cube
}

slice_netcdf_fixture <- function() {
  file <- make_netcdf_backend_fixture(
    time_units = "days since 2020-01-01 00:00:00",
    time_values = c(0, 31, 366, 397)
  )
  storage <- .new_netcdf_storage(
    file,
    c("temperature", "oxygen"),
    source = "synthetic",
    dataset_id = "slice-001"
  )
  list(
    file = file,
    storage = storage,
    cube = .new_netcdf_cube(
      storage,
      provenance = list(previous_step = "fixture"),
      qa = list(reviewed = TRUE)
    )
  )
}

slice_equivalent_memory <- function(netcdf_cube) {
  values <- array(
    NA_real_,
    dim = unname(.cube_shape(netcdf_cube)),
    dimnames = stats::setNames(
      list(
        as.character(netcdf_cube$lon),
        as.character(netcdf_cube$lat),
        as.character(netcdf_cube$depth),
        as.character(netcdf_cube$time),
        netcdf_cube$vars
      ),
      .cube_axis_names()
    )
  )
  for (m in seq_along(netcdf_cube$vars)) {
    for (l in seq_along(netcdf_cube$time)) {
      for (k in seq_along(netcdf_cube$depth)) {
        for (j in seq_along(netcdf_cube$lat)) {
          for (i in seq_along(netcdf_cube$lon)) {
            values[i, j, k, l, m] <-
              10000 * m + 1000 * l + 100 * k + 10 * j + i
          }
        }
      }
    }
  }
  values[1, 2, 2, 2, 1] <- NA_real_
  values[3, 1, 2, 3, 1] <- NA_real_
  values[1, 2, 1, 3, 2] <- NA_real_
  memory <- ocean_cube(
    lon = netcdf_cube$lon,
    lat = netcdf_cube$lat,
    depth = netcdf_cube$depth,
    time = as.Date(netcdf_cube$time),
    vars = netcdf_cube$vars,
    units = netcdf_cube$units,
    data = values
  )
  dimnames(memory$data) <- dimnames(values)
  memory$time <- netcdf_cube$time
  memory$temporal_extent <- range(netcdf_cube$time)
  dimnames(memory$data)$time <- as.character(netcdf_cube$time)
  memory$units <- netcdf_cube$units
  memory$source <- netcdf_cube$source
  memory$dataset_id <- netcdf_cube$dataset_id
  memory$provenance <- netcdf_cube$provenance
  memory$qa <- netcdf_cube$qa
  .check_cube(memory)
  memory
}

test_that("cube_slice is exported with the intended public signature", {
  expect_true("cube_slice" %in% getNamespaceExports("oceancube"))
  expect_identical(
    names(formals(cube_slice)),
    c(
      "x", "longitude", "latitude", "depth", "time", "variable",
      "by", "match", "tolerance"
    )
  )
})

test_that("index mode preserves non-time order and canonical time", {
  cube <- slice_baseline_cube()
  before <- cube
  result <- cube_slice(
    cube,
    longitude = c(3L, 1L, 3L),
    latitude = 2L,
    depth = c(2L, 1L),
    time = c(2L, 4L),
    variable = c(2L, 1L),
    by = "index"
  )
  expected <- .cube_read(
    cube,
    list(
      longitude = c(3L, 1L, 3L),
      latitude = 2L,
      depth = c(2L, 1L),
      time = c(2L, 4L),
      variable = c(2L, 1L)
    )
  )

  expect_identical(.cube_read(result), expected)
  expect_identical(unname(dim(result$data)), c(3L, 1L, 2L, 2L, 2L))
  expect_identical(result$lon, c(-78, -80, -78))
  expect_identical(result$time, cube$time[c(2L, 4L)])
  expect_identical(result$vars, c("oxygen", "temperature"))
  expect_identical(.cube_backend(result), "memory")
  expect_identical(cube, before)
})

test_that("index mode completes omitted axes and validates positions", {
  cube <- slice_baseline_cube()
  full <- cube_slice(cube, by = "index")
  cell <- cube_slice(
    cube,
    longitude = 1L,
    latitude = 1L,
    depth = 1L,
    time = 1L,
    variable = 1L,
    by = "index"
  )

  expect_identical(.cube_read(full), .cube_read(cube))
  expect_identical(unname(dim(cell$data)), rep(1L, 5L))
  expect_identical(cell$data[1, 1, 1, 1, 1], 11111)
  expect_error(cube_slice(cube, longitude = 0L, by = "index"), "between 1 and 3")
  expect_error(cube_slice(cube, latitude = -1L, by = "index"), "between 1 and 2")
  expect_error(cube_slice(cube, depth = NA_integer_, by = "index"), "finite, non-missing")
  expect_error(cube_slice(cube, time = Inf, by = "index"), "finite, non-missing")
  expect_error(cube_slice(cube, variable = 1.5, by = "index"), "whole-number")
  expect_error(cube_slice(cube, time = numeric(), by = "index"), "must not be empty")
  expect_error(cube_slice(cube, time = matrix(1L), by = "index"), "must be a vector")
  expect_error(cube_slice(cube, variable = "oxygen", by = "index"), "must be numeric")
})

test_that("index mode rejects incompatible matching arguments and duplicate variables", {
  cube <- slice_baseline_cube()
  expect_error(
    cube_slice(cube, longitude = 1L, by = "index", match = "exact"),
    "do not supply `match` or `tolerance`"
  )
  expect_error(
    cube_slice(cube, longitude = 1L, by = "index",
      tolerance = list(longitude = 1)
    ),
    "do not supply `match` or `tolerance`"
  )
  expect_error(
    cube_slice(cube, variable = c(2L, 2L), by = "index"),
    "unique-variable"
  )
})

test_that("exact value mode reproduces the deterministic baseline selection", {
  cube <- slice_baseline_cube()
  result <- cube_slice(
    cube,
    longitude = c(-78, -80),
    latitude = -11,
    depth = 50,
    time = as.Date(c("2020-01-01", "2021-02-01")),
    variable = c("oxygen", "temperature"),
    by = "value",
    match = "exact"
  )

  expect_identical(unname(dim(result$data)), c(2L, 1L, 1L, 2L, 2L))
  expect_identical(result$lon, c(-78, -80))
  expect_identical(result$lat, -11)
  expect_identical(result$depth, 50)
  expect_identical(
    result$time,
    as.Date(c("2020-01-01", "2021-02-01"))
  )
  expect_identical(result$vars, c("oxygen", "temperature"))
  expect_identical(result$data[1, 1, 1, 1, 1], 21223)
  expect_identical(result$data[2, 1, 1, 1, 1], 21221)
  expect_identical(result$data[1, 1, 1, 2, 2], 14223)
  expect_identical(result$data[2, 1, 1, 2, 2], 14221)
})

test_that("exact value mode preserves spatial order and duplicates", {
  cube <- slice_baseline_cube()
  result <- cube_slice(
    cube,
    longitude = c(-78, -80, -78),
    depth = c(50, 0, 50),
    time = as.Date(c("2020-01-01", "2021-02-01")),
    variable = c("oxygen", "temperature"),
    by = "value"
  )

  expect_identical(result$lon, c(-78, -80, -78))
  expect_identical(result$depth, c(50, 0, 50))
  expect_identical(
    result$time,
    as.Date(c("2020-01-01", "2021-02-01"))
  )
  expect_identical(
    result$provenance$cube_slice$resolved_indices$longitude,
    c(3L, 1L, 3L)
  )
})

test_that("exact value mode rejects absent, empty, and malformed selectors", {
  cube <- slice_baseline_cube()
  expect_error(cube_slice(cube, longitude = -77, by = "value"), "not found.*-77")
  expect_error(cube_slice(cube, latitude = numeric(), by = "value"), "must not be empty")
  expect_error(cube_slice(cube, depth = NA_real_, by = "value"), "finite, non-missing")
  expect_error(cube_slice(cube, longitude = matrix(-80), by = "value"), "not an array")
  expect_error(cube_slice(cube, time = as.Date(NA), by = "value"), "missing")
  expect_error(cube_slice(cube, time = "2020-01-01", by = "value"), "must inherit from Date")
  expect_error(cube_slice(cube, variable = "unknown", by = "value"), "Unknown variable")
  expect_error(cube_slice(cube, variable = character(), by = "value"), "must not be empty")
  expect_error(cube_slice(cube, variable = NA_character_, by = "value"), "non-empty")
  expect_error(
    cube_slice(cube, variable = c("oxygen", "oxygen"), by = "value"),
    "must not contain duplicates"
  )
  expect_error(
    cube_slice(cube, longitude = -80, by = "value",
      tolerance = list(longitude = 0)
    ),
    "only available.*nearest"
  )
})

test_that("surface depth is selectable only by exact NA or index", {
  cube <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = NA_real_,
    time = as.Date("2020-01-01"),
    vars = "sst",
    data = array(20, dim = rep(1L, 5L))
  )
  by_value <- cube_slice(
    cube,
    depth = NA_real_,
    by = "value",
    match = "exact"
  )
  by_index <- cube_slice(cube, depth = 1L, by = "index")

  expect_identical(by_value$depth, NA_real_)
  expect_identical(by_value$depth_extent, c(NA_real_, NA_real_))
  expect_identical(.cube_read(by_value), .cube_read(by_index))
  expect_error(
    cube_slice(cube, depth = 0, by = "value"),
    "surface cube"
  )
  expect_error(
    cube_slice(cube, depth = NA_real_, by = "value", match = "nearest"),
    "surface cube"
  )
})

test_that("nearest selection is deterministic and does not interpolate", {
  cube <- slice_baseline_cube()
  result <- cube_slice(
    cube,
    longitude = -79.4,
    latitude = -11.2,
    depth = 40,
    time = as.Date("2021-01-10"),
    variable = "temperature",
    by = "value",
    match = "nearest",
    tolerance = list(
      longitude = 0.5,
      latitude = 0.5,
      depth = 15,
      time = as.difftime(15, units = "days")
    )
  )
  record <- result$provenance$cube_slice

  expect_identical(result$lon, -79)
  expect_identical(result$lat, -11)
  expect_identical(result$depth, 50)
  expect_identical(result$time, as.Date("2021-01-01"))
  expect_identical(result$data[1, 1, 1, 1, 1], 13222)
  expect_equal(record$distances$longitude, 0.4)
  expect_equal(record$distances$latitude, 0.2)
  expect_equal(record$distances$depth, 10)
  expect_equal(
    as.numeric(record$distances$time, units = "days"),
    9
  )
})

test_that("nearest preserves first-position ties on all coordinate axes", {
  cube <- ocean_cube(
    lon = c(-80, -79),
    lat = c(-12, -11),
    depth = c(0, 50),
    time = as.Date(c("2020-01-01", "2020-01-03")),
    vars = "temperature",
    data = array(seq_len(16), dim = c(2, 2, 2, 2, 1))
  )
  result <- cube_slice(
    cube,
    longitude = -79.5,
    latitude = -11.5,
    depth = 25,
    time = as.Date("2020-01-02"),
    by = "value",
    match = "nearest"
  )

  expect_identical(
    result$provenance$cube_slice$resolved_indices[1:4],
    list(longitude = 1L, latitude = 1L, depth = 1L, time = 1L)
  )
  expect_identical(result$lon, -80)
  expect_identical(result$lat, -12)
  expect_identical(result$depth, 0)
  expect_identical(result$time, as.Date("2020-01-01"))
})

test_that("nearest works on descending spatial axes without reordering them", {
  cube <- ocean_cube(
    lon = c(-78, -79, -80),
    lat = c(-11, -12),
    depth = c(50, 0),
    time = as.Date(c("2021-01-01", "2021-01-02", "2021-01-03")),
    vars = "temperature",
    data = array(seq_len(36), dim = c(3, 2, 2, 3, 1))
  )
  result <- cube_slice(
    cube,
    longitude = -79.8,
    latitude = -11.8,
    depth = 5,
    time = as.Date("2021-01-01"),
    by = "value",
    match = "nearest"
  )

  expect_identical(result$lon, -80)
  expect_identical(result$lat, -12)
  expect_identical(result$depth, 0)
  expect_identical(result$time, as.Date("2021-01-01"))
  expect_identical(
    result$provenance$cube_slice$resolved_indices[1:4],
    list(longitude = 3L, latitude = 2L, depth = 2L, time = 1L)
  )
})

test_that("nearest enforces domain and inclusive tolerance boundaries", {
  cube <- slice_baseline_cube()
  accepted <- cube_slice(
    cube,
    longitude = -79.5,
    time = as.Date("2020-01-16"),
    by = "value",
    match = "nearest",
    tolerance = list(
      longitude = 0.5,
      time = as.difftime(15, units = "days")
    )
  )

  expect_identical(accepted$lon, -80)
  expect_identical(accepted$time, as.Date("2020-01-01"))
  expect_error(
    cube_slice(cube, longitude = -70, by = "value", match = "nearest"),
    "outside the cube domain.*-80.*-78"
  )
  expect_error(
    cube_slice(cube, time = as.Date("2019-12-31"),
      by = "value", match = "nearest"
    ),
    "outside the cube domain"
  )
  expect_error(
    cube_slice(cube, time = as.Date("2022-01-01"),
      by = "value", match = "nearest"
    ),
    "outside the cube domain"
  )
  expect_error(
    cube_slice(cube, depth = 40, by = "value", match = "nearest",
      tolerance = list(depth = 9)
    ),
    "exceeding tolerance"
  )
})

test_that("tolerance validation is strict and temporal units are explicit", {
  cube <- slice_baseline_cube()
  expect_error(
    cube_slice(cube, longitude = -79.4, by = "value", match = "nearest",
      tolerance = 0.5
    ),
    "fully named list"
  )
  expect_error(
    cube_slice(cube, longitude = -79.4, by = "value", match = "nearest",
      tolerance = list(bad_axis = 1)
    ),
    "unknown or unsupported"
  )
  expect_error(
    cube_slice(cube, variable = "temperature", by = "value", match = "nearest",
      tolerance = list(variable = 1)
    ),
    "variable.*never uses nearest"
  )
  expect_error(
    cube_slice(cube, longitude = -79.4, by = "value", match = "nearest",
      tolerance = list(longitude = -1)
    ),
    "non-negative"
  )
  expect_error(
    cube_slice(cube, time = as.Date("2020-01-15"),
      by = "value", match = "nearest", tolerance = list(time = 15)
    ),
    "difftime"
  )
  expect_error(
    cube_slice(cube, longitude = -79.4, by = "value", match = "nearest",
      tolerance = list(latitude = 1)
    ),
    "unselected axis"
  )
})

test_that("variable names remain exact even with nearest matching", {
  cube <- slice_baseline_cube()
  result <- cube_slice(
    cube,
    longitude = -79.4,
    variable = "oxygen",
    by = "value",
    match = "nearest"
  )
  expect_identical(result$vars, "oxygen")
  expect_error(
    cube_slice(cube, variable = "oxyg", by = "value", match = "nearest"),
    "Unknown variable"
  )
})

test_that("time matching preserves Date and POSIXct semantics", {
  date_cube <- slice_baseline_cube()
  posix_cube <- slice_baseline_cube()
  posix_cube$time <- as.POSIXct(posix_cube$time, tz = "UTC")
  posix_cube$temporal_extent <- range(posix_cube$time)
  dimnames(posix_cube$data)$time <- as.character(posix_cube$time)
  .check_cube(posix_cube)

  date_result <- cube_slice(
    date_cube,
    time = as.Date("2020-02-01"),
    by = "value"
  )
  posix_result <- cube_slice(
    posix_cube,
    time = as.POSIXct("2020-02-01 00:00:00", tz = "UTC"),
    by = "value"
  )

  expect_s3_class(date_result$time, "Date")
  expect_s3_class(posix_result$time, "POSIXct")
  expect_error(
    cube_slice(posix_cube, time = as.Date("2020-02-01"), by = "value"),
    "must inherit from POSIXct"
  )
  expect_error(
    cube_slice(date_cube,
      time = as.POSIXct("2020-02-01", tz = "UTC"), by = "value"
    ),
    "must inherit from Date"
  )
})

test_that("result units, extents, dimnames, classes, and metadata are aligned", {
  cube <- slice_baseline_cube()
  result <- cube_slice(
    cube,
    longitude = c(-78, -80),
    latitude = -11,
    depth = 50,
    time = as.Date(c("2020-01-01", "2021-02-01")),
    variable = "oxygen",
    by = "value"
  )

  expect_identical(class(result), c("ocean_cube", "list"))
  expect_identical(.cube_backend(result), "memory")
  expect_identical(result$units, c(oxygen = "mmol m-3"))
  expect_identical(
    unname(result$spatial_extent),
    c(-80, -78, -11, -11)
  )
  expect_identical(
    result$temporal_extent,
    as.Date(c("2020-01-01", "2021-02-01"))
  )
  expect_identical(result$depth_extent, c(50, 50))
  expect_identical(dimnames(result$data), dimnames(.cube_read(result)))
  expect_identical(result$source, cube$source)
  expect_identical(result$dataset_id, cube$dataset_id)
  expect_identical(result$qa, cube$qa)
  expect_true(.check_cube(result))
})

test_that("aligned dc and ocean_mask are subset while unsafe components are recorded", {
  cube <- slice_baseline_cube()
  result <- cube_slice(
    cube,
    longitude = c(3L, 1L),
    latitude = 2L,
    depth = 2L,
    by = "index"
  )

  expect_identical(result$dc, cube$dc[c(3L, 1L), 2L, drop = FALSE])
  expect_s3_class(result$mask, "ocean_mask")
  expect_identical(
    result$mask$mask,
    cube$mask$mask[c(3L, 1L), 2L, 2L, drop = FALSE]
  )
  expect_identical(result$mask$lon, c(-78, -80))
  expect_null(result$climatology)
  expect_null(result$anomaly)
  expect_identical(result$qa, cube$qa)
  expect_setequal(
    result$provenance$cube_slice$discarded_components,
    c("climatology", "anomaly")
  )
})

test_that("incompatible auxiliary metadata is discarded without misalignment", {
  cube <- slice_baseline_cube()
  cube$dc <- matrix(1, nrow = 1, ncol = 1)
  cube$mask <- list(kind = "unknown-shape")
  cube$qa <- list(grid = matrix(1, nrow = 3, ncol = 2))
  result <- cube_slice(cube, longitude = 1L, by = "index")

  expect_null(result$dc)
  expect_null(result$mask)
  expect_null(result$qa)
  expect_setequal(
    result$provenance$cube_slice$discarded_components,
    c("dc", "mask", "climatology", "anomaly", "qa")
  )
})

test_that("resolution performs no backend read", {
  cube <- slice_baseline_cube()
  local_mocked_bindings(
    .cube_read = function(...) stop("data read is forbidden during resolution"),
    .cube_read_block = function(...) stop("block read is forbidden during resolution"),
    .package = "oceancube"
  )

  resolved <- .resolve_cube_slice(
    cube,
    selectors = list(
      longitude = -79.4,
      latitude = NULL,
      depth = NULL,
      time = as.Date("2021-01-10"),
      variable = "temperature"
    ),
    by = "value",
    method = "nearest",
    tolerance = NULL
  )
  expect_identical(resolved$index$longitude, 2L)
  expect_identical(resolved$index$time, 3L)
})

test_that("cube_slice calls cube_read once after resolving all axes", {
  cube <- slice_baseline_cube()
  reads <- 0L
  original <- .cube_read
  local_mocked_bindings(
    .cube_read = function(x, index = NULL, drop = FALSE) {
      reads <<- reads + 1L
      original(x, index = index, drop = drop)
    },
    .package = "oceancube"
  )

  result <- cube_slice(
    cube,
    longitude = c(-78, -80),
    time = as.Date(c("2020-01-01", "2021-02-01")),
    variable = "temperature",
    by = "value"
  )
  expect_identical(reads, 1L)
  expect_identical(unname(dim(result$data)), c(2L, 2L, 2L, 2L, 1L))
})

test_that("memory and NetCDF slices are logically equivalent", {
  fixture <- slice_netcdf_fixture()
  withr::local_file(fixture$file)
  memory <- slice_equivalent_memory(fixture$cube)
  selectors <- list(
    longitude = c(-78, -80),
    latitude = -11,
    depth = 50,
    time = as.POSIXct(c("2020-01-01", "2021-02-01"), tz = "UTC"),
    variable = c("oxygen", "temperature"),
    by = "value",
    match = "exact"
  )

  memory_slice <- do.call(cube_slice, c(list(x = memory), selectors))
  netcdf_slice <- do.call(cube_slice, c(list(x = fixture$cube), selectors))

  expect_identical(.cube_read(memory_slice), .cube_read(netcdf_slice))
  expect_identical(memory_slice$lon, netcdf_slice$lon)
  expect_identical(memory_slice$lat, netcdf_slice$lat)
  expect_identical(memory_slice$depth, netcdf_slice$depth)
  expect_identical(memory_slice$time, netcdf_slice$time)
  expect_identical(memory_slice$vars, netcdf_slice$vars)
  expect_identical(memory_slice$units, netcdf_slice$units)
  expect_identical(.cube_shape(memory_slice), .cube_shape(netcdf_slice))
  expect_identical(class(memory_slice), class(netcdf_slice))
  expect_identical(.cube_backend(memory_slice), "memory")
  expect_identical(.cube_backend(netcdf_slice), "memory")
  expect_identical(
    memory_slice$provenance$cube_slice$backend_from,
    "memory"
  )
  expect_identical(
    netcdf_slice$provenance$cube_slice$backend_from,
    "netcdf"
  )
})

test_that("NetCDF slicing reads one requested envelope and variable", {
  fixture <- slice_netcdf_fixture()
  withr::local_file(fixture$file)
  openings <- 0L
  observed <- list()
  original_connection <- .with_netcdf_connection
  original_read <- .ncvar_get_block
  local_mocked_bindings(
    .with_netcdf_connection = function(file, code) {
      openings <<- openings + 1L
      original_connection(file, code)
    },
    .ncvar_get_block = function(nc, variable, start, count) {
      observed[[length(observed) + 1L]] <<- list(
        variable = variable,
        start = start,
        count = count
      )
      original_read(nc, variable, start, count)
    },
    .package = "oceancube"
  )

  result <- cube_slice(
    fixture$cube,
    longitude = c(-80, -78),
    time = as.POSIXct(c("2020-01-01", "2021-02-01"), tz = "UTC"),
    variable = "temperature",
    by = "value"
  )
  read_record <- result$provenance$cube_slice$netcdf_read

  expect_identical(openings, 1L)
  expect_length(observed, 1L)
  expect_identical(observed[[1L]]$variable, "temperature")
  expect_identical(
    observed[[1L]]$start,
    c(longitude = 1L, latitude = 1L, depth = 1L, time = 1L)
  )
  expect_identical(
    observed[[1L]]$count,
    c(longitude = 3L, latitude = 2L, depth = 2L, time = 4L)
  )
  expect_identical(read_record$values_requested, 16)
  expect_identical(read_record$values_in_envelope, 48)
  expect_identical(read_record$amplification, 3)
  expect_identical(read_record$variables, "temperature")
})

test_that("NetCDF slice is independent after source deletion", {
  fixture <- slice_netcdf_fixture()
  source_before <- file.info(fixture$file)
  result <- cube_slice(
    fixture$cube,
    variable = "temperature",
    by = "value"
  )
  expected <- .cube_read(result)
  source_after <- file.info(fixture$file)

  expect_identical(as.double(source_after$size), as.double(source_before$size))
  expect_identical(as.numeric(source_after$mtime), as.numeric(source_before$mtime))
  expect_identical(unlink(fixture$file), 0L)
  expect_identical(.cube_backend(result), "memory")
  expect_identical(.cube_read(result), expected)
  expect_s3_class(clim_month(result), "ocean_clim")
  expect_error(
    .cube_read(fixture$cube),
    "no longer exists",
    class = "oceancube_netcdf_changed_file"
  )
})

test_that("slice without selectors matches collect values for both backends", {
  memory <- slice_baseline_cube()
  memory_slice <- cube_slice(memory)
  expect_identical(.cube_read(memory_slice), .cube_read(memory))

  fixture <- slice_netcdf_fixture()
  withr::local_file(fixture$file)
  netcdf_slice <- cube_slice(fixture$cube)
  collected <- cube_collect(fixture$cube)
  expect_identical(.cube_read(netcdf_slice), .cube_read(collected))
  expect_identical(.cube_shape(netcdf_slice), .cube_shape(collected))
})

test_that("invalid cubes and unsupported backends fail informatively", {
  expect_error(
    cube_slice(list()),
    "must be an <ocean_cube>",
    class = "oceancube_bad_cube"
  )
  cube <- slice_baseline_cube()
  local_mocked_bindings(
    .cube_backend = function(x) "future-store",
    .package = "oceancube"
  )
  expect_error(
    cube_slice(cube),
    "Unsupported ocean_cube backend: 'future-store'",
    class = "oceancube_unsupported_backend"
  )
})
