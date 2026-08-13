crop_baseline_cube <- function() {
  cube <- .make_baseline_fixture()$cube
  cube$source <- "synthetic"
  cube$dataset_id <- "crop-001"
  cube$provenance <- list(previous_step = "fixture")
  cube$qa <- list(reviewed = TRUE)
  cube$dc <- matrix(seq_len(6L), nrow = 3L, ncol = 2L)
  cube$mask <- stock_mask(cube, stock = "fixture")
  cube$climatology <- list(scale = "month")
  cube$anomaly <- list(method = "difference")
  cube
}

crop_netcdf_fixture <- function() {
  file <- make_netcdf_backend_fixture(
    time_units = "days since 2020-01-01 00:00:00",
    time_values = c(0, 31, 366, 397)
  )
  storage <- .new_netcdf_storage(
    file,
    c("temperature", "oxygen"),
    source = "synthetic",
    dataset_id = "crop-001"
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

crop_equivalent_memory <- function(netcdf_cube) {
  shape <- unname(.cube_shape(netcdf_cube))
  values <- array(NA_real_, dim = shape)
  for (m in seq_len(shape[[5L]])) {
    for (l in seq_len(shape[[4L]])) {
      for (k in seq_len(shape[[3L]])) {
        for (j in seq_len(shape[[2L]])) {
          for (i in seq_len(shape[[1L]])) {
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
  dimnames(values) <- stats::setNames(
    list(
      as.character(netcdf_cube$lon),
      as.character(netcdf_cube$lat),
      as.character(netcdf_cube$depth),
      as.character(netcdf_cube$time),
      netcdf_cube$vars
    ),
    .cube_axis_names()
  )
  memory <- ocean_cube(
    lon = netcdf_cube$lon,
    lat = netcdf_cube$lat,
    depth = netcdf_cube$depth,
    time = as.Date(netcdf_cube$time),
    vars = netcdf_cube$vars,
    units = netcdf_cube$units,
    data = values
  )
  memory$time <- netcdf_cube$time
  memory$temporal_extent <- range(netcdf_cube$time)
  dimnames(memory$data) <- dimnames(values)
  memory$source <- netcdf_cube$source
  memory$dataset_id <- netcdf_cube$dataset_id
  memory$provenance <- netcdf_cube$provenance
  memory$qa <- netcdf_cube$qa
  .check_cube(memory)
  memory
}

test_that("cube_crop is exported with the intended public signature", {
  expect_true("cube_crop" %in% getNamespaceExports("oceancube"))
  expect_identical(
    names(formals(cube_crop)),
    c(
      "x", "longitude", "latitude", "depth", "time", "variable",
      "bbox", "outside"
    )
  )
})

test_that("closed ranges include cell centres on both limits", {
  cube <- crop_baseline_cube()
  longitude <- cube_crop(cube, longitude = c(-80, -79))
  latitude <- cube_crop(cube, latitude = c(-12, -11))
  one <- cube_crop(cube, longitude = c(-79, -79))
  between <- cube_crop(cube, longitude = c(-79.8, -78.8))

  expect_identical(longitude$lon, c(-80, -79))
  expect_identical(latitude$lat, c(-12, -11))
  expect_identical(one$lon, -79)
  expect_identical(unname(dim(one$data)), c(1L, 2L, 2L, 4L, 2L))
  expect_identical(between$lon, -79)
})

test_that("numeric ranges validate type, length, values, and order", {
  cube <- crop_baseline_cube()
  expect_error(
    cube_crop(cube, longitude = c("west", "east")),
    "numeric range"
  )
  expect_error(
    cube_crop(cube, latitude = -12),
    "exactly two"
  )
  expect_error(
    cube_crop(cube, depth = c(0, 25, 50)),
    "exactly two"
  )
  expect_error(
    cube_crop(cube, longitude = c(NA_real_, -79)),
    "finite, non-missing"
  )
  expect_error(
    cube_crop(cube, latitude = c(-Inf, -11)),
    "finite, non-missing"
  )
  expect_error(
    cube_crop(cube, longitude = c(-78, -80)),
    "ordered.*antimeridian"
  )
  expect_error(
    cube_crop(cube, longitude = matrix(c(-80, -79))),
    "numeric range"
  )
})

test_that("ranges preserve non-time axis order and canonical time order", {
  cube <- ocean_cube(
    lon = c(-78, -79, -80),
    lat = c(-10, -11, -12),
    depth = c(50, 0, -50),
    time = as.Date(c("2021-01-01", "2021-01-02", "2021-01-03")),
    vars = "temperature",
    data = array(seq_len(81), dim = c(3, 3, 3, 3, 1))
  )
  result <- cube_crop(
    cube,
    longitude = c(-80, -79),
    latitude = c(-12, -10),
    depth = c(-50, 0),
    time = as.Date(c("2021-01-01", "2021-01-02"))
  )

  expect_identical(result$lon, c(-79, -80))
  expect_identical(result$lat, c(-10, -11, -12))
  expect_identical(result$depth, c(0, -50))
  expect_identical(
    result$time,
    as.Date(c("2021-01-01", "2021-01-02"))
  )
  expect_identical(
    result$provenance$cube_crop$resolved_indices[1:4],
    list(
      longitude = 2:3,
      latitude = 1:3,
      depth = 2:3,
      time = 1:2
    )
  )
})

test_that("ranges intersecting the domain without centres are rejected", {
  cube <- crop_baseline_cube()
  expect_error(
    cube_crop(cube, longitude = c(-79.8, -79.6)),
    "contains no cube coordinates"
  )
  expect_error(
    cube_crop(
      cube,
      time = as.Date(c("2020-03-01", "2020-12-01"))
    ),
    "contains no cube coordinates"
  )
})

test_that("bbox is equivalent to explicit spatial ranges", {
  cube <- crop_baseline_cube()
  bbox <- c(xmin = -80, ymin = -12, xmax = -79, ymax = -11)
  reordered <- bbox[c("ymax", "xmax", "ymin", "xmin")]
  by_bbox <- cube_crop(cube, bbox = bbox)
  by_reordered_bbox <- cube_crop(cube, bbox = reordered)
  explicit <- cube_crop(
    cube,
    longitude = c(-80, -79),
    latitude = c(-12, -11)
  )

  expect_identical(.cube_read(by_bbox), .cube_read(explicit))
  expect_identical(.cube_read(by_reordered_bbox), .cube_read(explicit))
  expect_identical(by_bbox$lon, explicit$lon)
  expect_identical(by_bbox$lat, explicit$lat)
  expect_identical(
    by_bbox$provenance$cube_crop$bbox_requested,
    bbox
  )
})

test_that("bbox combines with vertical, temporal, and variable ranges", {
  cube <- crop_baseline_cube()
  result <- cube_crop(
    cube,
    bbox = c(xmin = -80, ymin = -12, xmax = -79, ymax = -11),
    depth = c(50, 50),
    time = as.Date(c("2020-02-01", "2021-01-01")),
    variable = "oxygen"
  )

  expect_identical(unname(dim(result$data)), c(2L, 2L, 1L, 2L, 1L))
  expect_identical(result$depth, 50)
  expect_identical(result$vars, "oxygen")
})

test_that("bbox rejects ambiguity, malformed names, and unverifiable sf bbox", {
  cube <- crop_baseline_cube()
  expect_error(
    cube_crop(
      cube,
      bbox = c(xmin = -80, ymin = -12, xmax = -79, ymax = -11),
      longitude = c(-80, -79)
    ),
    "cannot be combined"
  )
  expect_error(
    cube_crop(
      cube,
      bbox = c(xmin = -80, ymin = -12, xmax = -79, ymax = -11),
      latitude = c(-12, -11)
    ),
    "cannot be combined"
  )
  expect_error(cube_crop(cube, bbox = c(-80, -12, -79, -11)), "names must")
  expect_error(
    cube_crop(cube, bbox = c(xmin = -80, ymin = -12, xmax = -79)),
    "exactly four"
  )
  expect_error(
    cube_crop(
      cube,
      bbox = c(left = -80, bottom = -12, right = -79, top = -11)
    ),
    "names must be exactly"
  )
  expect_error(
    cube_crop(
      cube,
      bbox = c(xmin = -80, ymin = -12, xmax = Inf, ymax = -11)
    ),
    "finite, non-missing"
  )
  sf_bbox <- structure(
    c(xmin = -80, ymin = -12, xmax = -79, ymax = -11),
    class = "bbox",
    crs = list(input = "EPSG:4326")
  )
  expect_error(cube_crop(cube, bbox = sf_bbox), "not supported.*CRS")
})

test_that("outside error rejects every domain excess before reading", {
  cube <- crop_baseline_cube()
  expect_error(
    cube_crop(cube, longitude = c(-81, -79)),
    "exceeds the cube domain.*outside = \"clip\""
  )
  expect_error(
    cube_crop(cube, longitude = c(-79, -77)),
    "exceeds the cube domain"
  )
  expect_error(
    cube_crop(cube, latitude = c(-20, 0)),
    "exceeds the cube domain"
  )
  expect_error(
    cube_crop(cube, time = as.Date(c("2019-01-01", "2020-01-01"))),
    "exceeds the cube domain"
  )
  expect_error(cube_crop(cube, outside = "nearest"), "should be one of")
})

test_that("outside clip intersects ranges and records each changed axis", {
  cube <- crop_baseline_cube()
  result <- cube_crop(
    cube,
    longitude = c(-81, -79),
    latitude = c(-12.5, -11),
    depth = c(0, 100),
    time = as.Date(c("2019-01-01", "2020-02-01")),
    outside = "clip"
  )
  record <- result$provenance$cube_crop

  expect_identical(result$lon, c(-80, -79))
  expect_identical(result$lat, c(-12, -11))
  expect_identical(result$depth, c(0, 50))
  expect_identical(
    result$time,
    as.Date(c("2020-01-01", "2020-02-01"))
  )
  expect_identical(record$ranges_requested$longitude, c(-81, -79))
  expect_identical(record$ranges_applied$longitude, c(-80, -79))
  expect_identical(
    record$clipped,
    c(longitude = TRUE, latitude = TRUE, depth = TRUE, time = TRUE)
  )
})

test_that("outside clip rejects ranges with no domain intersection", {
  cube <- crop_baseline_cube()
  expect_error(
    cube_crop(cube, longitude = c(-70, -69), outside = "clip"),
    "does not intersect"
  )
  expect_error(
    cube_crop(cube, depth = c(100, 200), outside = "clip"),
    "does not intersect"
  )
  expect_error(
    cube_crop(
      cube,
      time = as.Date(c("2022-01-01", "2023-01-01")),
      outside = "clip"
    ),
    "does not intersect"
  )
})

test_that("surface cubes retain NULL depth and reject numeric depth ranges", {
  surface <- ocean_cube(
    lon = c(-80, -79),
    lat = -12,
    depth = NA_real_,
    time = as.Date(c("2020-01-01", "2020-02-01")),
    vars = "sst",
    data = array(1:4, dim = c(2, 1, 1, 2, 1))
  )
  result <- cube_crop(surface, longitude = c(-80, -80))

  expect_identical(result$depth, NA_real_)
  expect_identical(result$depth_extent, c(NA_real_, NA_real_))
  expect_error(
    cube_crop(surface, depth = c(0, 0)),
    "surface cube.*depth = NULL"
  )
  expect_error(
    cube_crop(surface, depth = c(NA_real_, NA_real_)),
    "surface cube.*depth = NULL"
  )
})

test_that("Date ranges preserve exact temporal semantics", {
  cube <- crop_baseline_cube()
  full <- cube_crop(cube, time = range(cube$time))
  partial <- cube_crop(
    cube,
    time = as.Date(c("2020-02-01", "2021-01-01"))
  )
  one <- cube_crop(
    cube,
    time = as.Date(c("2021-02-01", "2021-02-01"))
  )

  expect_identical(full$time, cube$time)
  expect_identical(
    partial$time,
    as.Date(c("2020-02-01", "2021-01-01"))
  )
  expect_identical(one$time, as.Date("2021-02-01"))
  expect_error(
    cube_crop(cube, time = c("2020-01-01", "2021-01-01")),
    "two-value Date range"
  )
  expect_error(
    cube_crop(
      cube,
      time = as.Date(c("2021-01-01", "2020-01-01"))
    ),
    "ordered"
  )
})

test_that("POSIXct ranges preserve timezone and real instants", {
  base <- crop_baseline_cube()
  cube <- ocean_cube(
    lon = base$lon, lat = base$lat, depth = base$depth,
    time = as.POSIXct(base$time, tz = "UTC"), vars = base$vars,
    units = base$units, data = base$data
  )
  result <- cube_crop(
    cube,
    time = as.POSIXct(
      c("2020-02-01 00:00:00", "2021-01-01 00:00:00"),
      tz = "UTC"
    )
  )

  expect_s3_class(result$time, "POSIXct")
  expect_identical(attr(result$time, "tzone"), "UTC")
  expect_identical(length(result$time), 2L)
  equivalent_zone <- cube_crop(
    cube,
    time = as.POSIXct(
      c("2020-01-31 19:00:00", "2020-12-31 19:00:00"),
      tz = "America/Lima"
    )
  )
  expect_identical(equivalent_zone$time, result$time)
  expect_error(
    cube_crop(cube, time = as.Date(c("2020-02-01", "2021-01-01"))),
    "two-value POSIXct range"
  )
})

test_that("undecoded temporal axes report positional alternative", {
  undecoded <- structure(c(0, 1), class = "cf_time")
  expect_error(
    .resolve_time_crop_range(undecoded, undecoded, "error"),
    "not safely decoded.*cube_slice"
  )
})

test_that("variables are exact, ordered, unique, and aligned with units", {
  cube <- crop_baseline_cube()
  oxygen <- cube_crop(cube, variable = "oxygen")
  reordered <- cube_crop(
    cube,
    variable = c("oxygen", "temperature")
  )

  expect_identical(oxygen$vars, "oxygen")
  expect_identical(oxygen$units, c(oxygen = "mmol m-3"))
  expect_identical(reordered$vars, c("oxygen", "temperature"))
  expect_identical(
    reordered$units,
    c(oxygen = "mmol m-3", temperature = "degC")
  )
  expect_error(cube_crop(cube, variable = "unknown"), "Unknown variable")
  expect_error(cube_crop(cube, variable = character()), "must not be empty")
  expect_error(cube_crop(cube, variable = NA_character_), "non-empty")
  expect_error(
    cube_crop(cube, variable = c("oxygen", "oxygen")),
    "must not contain duplicates"
  )
})

test_that("unit subsetting handles named lists, unnamed vectors, and NULL", {
  base <- .make_baseline_fixture()$cube
  list_cube <- base
  list_cube$units <- list(temperature = "degC", oxygen = "mmol m-3")
  unnamed_cube <- base
  unnamed_cube$units <- unname(base$units)
  null_cube <- base
  null_cube$units <- NULL

  expect_identical(
    cube_crop(list_cube, variable = "oxygen")$units,
    list(oxygen = "mmol m-3")
  )
  expect_identical(
    cube_crop(unnamed_cube, variable = "oxygen")$units,
    "mmol m-3"
  )
  expect_null(cube_crop(null_cube, variable = "oxygen")$units)
})

test_that("deterministic crop has expected shape, coordinates, and values", {
  cube <- crop_baseline_cube()
  before <- cube
  result <- cube_crop(
    cube,
    longitude = c(-79.5, -78),
    latitude = c(-12, -11),
    depth = c(0, 50),
    time = as.Date(c("2020-02-01", "2021-01-01")),
    variable = "oxygen"
  )

  expect_identical(unname(dim(result$data)), c(2L, 2L, 2L, 2L, 1L))
  expect_identical(result$lon, c(-79, -78))
  expect_identical(result$lat, c(-12, -11))
  expect_identical(result$depth, c(0, 50))
  expect_identical(
    result$time,
    as.Date(c("2020-02-01", "2021-01-01"))
  )
  expect_identical(result$vars, "oxygen")
  expect_identical(result$data[1, 1, 1, 1, 1], 22112)
  expect_identical(result$data[2, 1, 1, 1, 1], 22113)
  expect_identical(result$data[1, 2, 2, 2, 1], 23222)
  expect_identical(result$data[2, 2, 2, 2, 1], 23223)
  expect_identical(cube, before)
})

test_that("single-centre crops retain five dimensions and aligned dimnames", {
  cube <- crop_baseline_cube()
  result <- cube_crop(
    cube,
    longitude = c(-79, -79),
    latitude = c(-11, -11),
    depth = c(50, 50),
    time = as.Date(c("2021-02-01", "2021-02-01")),
    variable = "oxygen"
  )

  expect_identical(unname(dim(result$data)), rep(1L, 5L))
  expect_identical(names(dim(result$data)), names(dim(cube$data)))
  expect_identical(dimnames(result$data), dimnames(.cube_read(result)))
  expect_identical(.cube_backend(result), "memory")
  expect_identical(class(result), c("ocean_cube", "list"))
  expect_true(.check_cube(result))
})

test_that("crop recalculates extents and preserves product metadata", {
  cube <- crop_baseline_cube()
  result <- cube_crop(
    cube,
    longitude = c(-79, -78),
    latitude = c(-11, -11),
    depth = c(50, 50),
    time = as.Date(c("2020-02-01", "2021-01-01")),
    variable = "oxygen"
  )

  expect_identical(unname(result$spatial_extent), c(-79, -78, -11, -11))
  expect_identical(result$depth_extent, c(50, 50))
  expect_identical(
    result$temporal_extent,
    as.Date(c("2020-02-01", "2021-01-01"))
  )
  expect_identical(result$source, cube$source)
  expect_identical(result$dataset_id, cube$dataset_id)
  expect_identical(result$qa, cube$qa)
  expect_identical(result$provenance$parent, cube$provenance)
  expect_identical(result$provenance$cube_crop$operation, "cube_crop")
})

test_that("dc and compatible ocean masks are cropped safely", {
  cube <- crop_baseline_cube()
  result <- cube_crop(
    cube,
    longitude = c(-79, -78),
    latitude = c(-11, -11),
    depth = c(50, 50)
  )

  expect_identical(result$dc, cube$dc[2:3, 2L, drop = FALSE])
  expect_s3_class(result$mask, "ocean_mask")
  expect_identical(
    result$mask$mask,
    cube$mask$mask[2:3, 2L, 2L, drop = FALSE]
  )
  expect_identical(result$mask$lon, c(-79, -78))
  expect_identical(result$mask$lat, -11)
  expect_identical(result$mask$depth, 50)
  expect_null(result$climatology)
  expect_null(result$anomaly)
  expect_setequal(
    result$provenance$cube_crop$discarded_components,
    c("climatology", "anomaly")
  )
})

test_that("incompatible auxiliary components are discarded and recorded", {
  cube <- crop_baseline_cube()
  cube$dc <- matrix(1, 1, 1)
  cube$mask <- list(kind = "unknown")
  cube$qa <- list(grid = matrix(1, 3, 2))
  result <- cube_crop(cube, longitude = c(-80, -79))

  expect_null(result$dc)
  expect_null(result$mask)
  expect_null(result$qa)
  expect_setequal(
    result$provenance$cube_crop$discarded_components,
    c("dc", "mask", "climatology", "anomaly", "qa")
  )
})

test_that("range resolution performs no data read or NetCDF opening", {
  cube <- crop_baseline_cube()
  local_mocked_bindings(
    .cube_read = function(...) stop("data read is forbidden"),
    .cube_read_block = function(...) stop("block read is forbidden"),
    .with_netcdf_connection = function(...) stop("connection is forbidden"),
    .package = "oceancube"
  )
  resolved <- .resolve_cube_crop(
    cube,
    ranges = list(
      longitude = c(-79, -78),
      latitude = NULL,
      depth = c(50, 50),
      time = NULL
    ),
    variable = "oxygen",
    outside = "error"
  )

  expect_identical(resolved$index$longitude, 2:3)
  expect_identical(resolved$index$depth, 2L)
  expect_identical(resolved$index$variable, 2L)
})

test_that("cube_crop reads exactly once after resolving all ranges", {
  cube <- crop_baseline_cube()
  reads <- 0L
  original <- .cube_read
  local_mocked_bindings(
    .cube_read = function(x, index = NULL, drop = FALSE) {
      reads <<- reads + 1L
      original(x, index = index, drop = drop)
    },
    .package = "oceancube"
  )
  result <- cube_crop(
    cube,
    longitude = c(-79, -78),
    variable = "temperature"
  )

  expect_identical(reads, 1L)
  expect_identical(unname(dim(result$data)), c(2L, 2L, 2L, 4L, 1L))
})

test_that("crop and slice agree when selecting the same stored centres", {
  cube <- crop_baseline_cube()
  crop <- cube_crop(
    cube,
    longitude = c(-80, -78),
    latitude = c(-12, -11)
  )
  slice <- cube_slice(
    cube,
    longitude = c(-80, -79, -78),
    latitude = c(-12, -11),
    by = "value",
    match = "exact"
  )

  expect_identical(.cube_read(crop), .cube_read(slice))
  expect_identical(.cube_shape(crop), .cube_shape(slice))
  expect_identical(crop$lon, slice$lon)
  expect_identical(
    cube_crop(cube, longitude = c(-79.8, -78.8))$lon,
    -79
  )
  expect_error(
    cube_slice(cube, longitude = c(-79.8, -78.8), by = "value"),
    "not found"
  )
})

test_that("memory and NetCDF crops are equivalent", {
  fixture <- crop_netcdf_fixture()
  withr::local_file(fixture$file)
  memory <- crop_equivalent_memory(fixture$cube)
  time_range <- as.POSIXct(
    c("2020-02-01", "2021-01-01"),
    tz = "UTC"
  )
  memory_crop <- cube_crop(
    memory,
    longitude = c(-79, -78),
    latitude = c(-12, -11),
    depth = c(0, 50),
    time = time_range,
    variable = c("oxygen", "temperature")
  )
  netcdf_crop <- cube_crop(
    fixture$cube,
    longitude = c(-79, -78),
    latitude = c(-12, -11),
    depth = c(0, 50),
    time = time_range,
    variable = c("oxygen", "temperature")
  )

  expect_identical(.cube_read(memory_crop), .cube_read(netcdf_crop))
  expect_identical(memory_crop$lon, netcdf_crop$lon)
  expect_identical(memory_crop$lat, netcdf_crop$lat)
  expect_identical(memory_crop$depth, netcdf_crop$depth)
  expect_identical(memory_crop$time, netcdf_crop$time)
  expect_identical(memory_crop$vars, netcdf_crop$vars)
  expect_identical(memory_crop$units, netcdf_crop$units)
  expect_identical(.cube_shape(memory_crop), .cube_shape(netcdf_crop))
  expect_identical(class(memory_crop), class(netcdf_crop))
  expect_identical(.cube_backend(netcdf_crop), "memory")
})

test_that("NetCDF crop reads one exact physical block and requested variable", {
  fixture <- crop_netcdf_fixture()
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
  result <- cube_crop(
    fixture$cube,
    longitude = c(-80, -79),
    latitude = c(-12, -12),
    time = as.POSIXct(c("2020-01-01", "2020-02-01"), tz = "UTC"),
    variable = "temperature"
  )
  read_record <- result$provenance$cube_crop$netcdf_read

  expect_identical(openings, 1L)
  expect_length(observed, 1L)
  expect_identical(observed[[1L]]$variable, "temperature")
  expect_identical(
    observed[[1L]]$start,
    c(longitude = 1L, latitude = 1L, depth = 1L, time = 1L)
  )
  expect_identical(
    observed[[1L]]$count,
    c(longitude = 2L, latitude = 1L, depth = 2L, time = 2L)
  )
  expect_identical(read_record$values_requested, 8)
  expect_identical(read_record$values_in_envelope, 8)
  expect_identical(read_record$variables, "temperature")
  expect_identical(.cube_backend(result), "memory")
})

test_that("NetCDF connection closes and source remains unchanged", {
  fixture <- crop_netcdf_fixture()
  moved <- paste0(fixture$file, ".moved")
  withr::defer(unlink(c(fixture$file, moved)))
  before <- file.info(fixture$file)
  result <- cube_crop(
    fixture$cube,
    longitude = c(-80, -79),
    variable = "temperature"
  )
  after <- file.info(fixture$file)

  expect_identical(as.double(after$size), as.double(before$size))
  expect_identical(as.numeric(after$mtime), as.numeric(before$mtime))
  expect_true(file.rename(fixture$file, moved))
  expect_true(file.rename(moved, fixture$file))
  expect_identical(.cube_backend(result), "memory")
})

test_that("NetCDF crop remains usable after deleting the source file", {
  fixture <- crop_netcdf_fixture()
  result <- cube_crop(
    fixture$cube,
    longitude = c(-80, -79),
    depth = c(0, 50),
    variable = "temperature"
  )
  expected <- .cube_read(result)
  expect_identical(unlink(fixture$file), 0L)

  expect_identical(.cube_backend(result), "memory")
  expect_identical(.cube_read(result), expected)
  expect_no_error(summary(result))
  expect_s3_class(
    layer_mean(result, depth = range(result$depth)),
    "ocean_cube"
  )
  expect_error(
    .cube_read(fixture$cube),
    "no longer exists",
    class = "oceancube_netcdf_changed_file"
  )
})

test_that("crop without ranges matches slice and collect values", {
  memory <- crop_baseline_cube()
  crop_memory <- cube_crop(memory)
  slice_memory <- cube_slice(memory)
  expect_identical(.cube_read(crop_memory), .cube_read(slice_memory))

  fixture <- crop_netcdf_fixture()
  withr::local_file(fixture$file)
  crop_netcdf <- cube_crop(fixture$cube)
  collected <- cube_collect(fixture$cube)
  expect_identical(.cube_read(crop_netcdf), .cube_read(collected))
  expect_identical(.cube_shape(crop_netcdf), .cube_shape(collected))
})

test_that("invalid cubes and unsupported backends fail informatively", {
  expect_error(
    cube_crop(list()),
    "must be an <ocean_cube>",
    class = "oceancube_bad_cube"
  )
  cube <- crop_baseline_cube()
  local_mocked_bindings(
    .cube_backend = function(x) "future-store",
    .package = "oceancube"
  )
  expect_error(
    cube_crop(cube),
    "Unsupported ocean_cube backend: 'future-store'",
    class = "oceancube_unsupported_backend"
  )
})
