test_that("cube_open exposes the approved public signature and descriptor", {
  expect_identical(
    names(formals(cube_open)),
    c(
      "file", "vars", "lon_name", "lat_name", "depth_name", "time_name",
      "source", "dataset_id"
    )
  )
  expect_null(formals(cube_open)$vars)
  expect_identical(formals(cube_open)$source, "netcdf")

  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  before <- tools::md5sum(file)
  scientific_reads <- 0L

  local_mocked_bindings(
    .read_netcdf_variable_block = function(...) {
      scientific_reads <<- scientific_reads + 1L
      stop("scientific data read during metadata-only construction")
    },
    .package = "oceancube"
  )

  x <- cube_open(
    file,
    vars = c("oxygen", "temperature"),
    source = "fixture",
    dataset_id = "cube-open-public"
  )
  printed <- capture.output(print(x))
  summarized <- summary(x)
  inspected_none <- cube_inspect(x, missing = "none")
  inspected_auto <- cube_inspect(x, missing = "auto")
  validation <- cube_validate(x)

  expect_identical(class(x), c("ocean_cube", "list"))
  expect_identical(x$vars, c("oxygen", "temperature"))
  expect_identical(.cube_backend(x), "netcdf")
  expect_false("data" %in% names(x))
  expect_true("storage" %in% names(x))
  expect_identical(x$storage$version, 1L)
  expect_identical(x$storage$backend, "netcdf")
  expect_true(x$storage$read_only)
  expect_false(.netcdf_contains_forbidden_object(x$storage))
  expect_identical(scientific_reads, 0L)
  expect_identical(tools::md5sum(file), before)
  expect_true(any(grepl("backend    : netcdf", printed, fixed = TRUE)))
  expect_true(any(grepl("source     : fixture", printed, fixed = TRUE)))
  expect_false(any(grepl(x$storage$file$normalized_path, printed, fixed = TRUE)))
  expect_s3_class(summarized, "data.frame")
  expect_s3_class(inspected_none, "ocean_cube_inspection")
  expect_s3_class(inspected_auto, "ocean_cube_inspection")
  expect_false(any(validation$status == "FAIL"))
})

test_that("cube_open vars NULL discovers OISST data variables in source order", {
  file <- test_path(
    "fixtures", "real-data", "noaa-oisst21-surface-time-fv1.nc"
  )
  scientific_reads <- 0L
  local_mocked_bindings(
    .read_netcdf_variable_block = function(...) {
      scientific_reads <<- scientific_reads + 1L
      stop("scientific data read during vars discovery")
    },
    .package = "oceancube"
  )

  x <- cube_open(
    file,
    vars = NULL,
    depth_name = "zlev",
    source = "NOAA OISST v2.1",
    dataset_id = "FIXTURE-SURFACE-TIME-001"
  )

  expect_identical(x$vars, c("sst", "anom", "err", "ice"))
  expect_false(any(x$vars %in% c("lon", "lat", "zlev", "time")))
  expect_identical(scientific_reads, 0L)
  expect_identical(
    .cube_shape(x),
    c(longitude = 36L, latitude = 48L, depth = 1L, time = 4L, variable = 4L)
  )
})

test_that("cube_open public OISST collection has exact eager scientific parity", {
  file <- test_path(
    "fixtures", "real-data", "noaa-oisst21-surface-time-fv1.nc"
  )
  vars <- c("sst", "anom", "err", "ice")
  eager <- read_nc(
    file, vars = vars, depth_name = "zlev",
    source = "NOAA OISST v2.1", dataset_id = "FIXTURE-SURFACE-TIME-001"
  )
  deferred <- cube_open(
    file, vars = NULL, depth_name = "zlev",
    source = "NOAA OISST v2.1", dataset_id = "FIXTURE-SURFACE-TIME-001"
  )
  collected <- cube_collect(deferred)

  expect_identical(collected$lon, eager$lon)
  expect_identical(collected$lat, eager$lat)
  expect_identical(collected$depth, eager$depth)
  expect_identical(collected$time, eager$time)
  expect_identical(collected$vars, eager$vars)
  expect_identical(
    as.vector(is.na(collected$data)), as.vector(is.na(eager$data))
  )
  expect_equal(
    as.vector(collected$data), as.vector(eager$data), tolerance = 0
  )
  expect_identical(.cube_backend(collected), "memory")
  expect_true("data" %in% names(collected))
  expect_identical(
    vapply(deferred$provenance$history, `[[`, character(1), "operation"),
    "read_nc"
  )
  expect_identical(
    vapply(collected$provenance$history, `[[`, character(1), "operation"),
    c("read_nc", "cube_collect")
  )
  expect_true(.provenance_validate(deferred$provenance, strict = TRUE)$valid)
  expect_identical(cube_collect(collected), collected)
})

test_that("cube_open routes public bounded operations through the backend", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  deferred <- cube_open(file, vars = c("temperature", "oxygen"))
  memory <- cube_collect(deferred)

  slice_args <- list(
    longitude = c(1L, 3L), latitude = c(1L, 2L), depth = c(1L, 2L),
    time = c(2L, 4L), variable = c(2L, 1L), by = "index"
  )
  from_deferred <- do.call(cube_slice, c(list(x = deferred), slice_args))
  from_memory <- do.call(cube_slice, c(list(x = memory), slice_args))
  expect_equal(from_deferred$data, from_memory$data, tolerance = 0)
  expect_identical(.cube_backend(from_deferred), "memory")

  crop_args <- list(
    longitude = c(-80, -79), latitude = c(-12, -11), depth = c(0, 50),
    time = range(deferred$time), variable = "oxygen"
  )
  from_deferred <- do.call(cube_crop, c(list(x = deferred), crop_args))
  from_memory <- do.call(cube_crop, c(list(x = memory), crop_args))
  expect_equal(from_deferred$data, from_memory$data, tolerance = 0)

  extract_args <- list(
    longitude = c(1L, 3L), latitude = c(2L, 1L), depth = c(2L, 1L),
    time = c(4L, 1L), variable = c(2L, 1L), by = "index",
    mode = "table", keep_index = TRUE
  )
  from_deferred <- do.call(cube_extract, c(list(x = deferred), extract_args))
  from_memory <- do.call(cube_extract, c(list(x = memory), extract_args))
  attributes(from_deferred) <- attributes(from_deferred)[c(
    "names", "row.names", "class"
  )]
  attributes(from_memory) <- attributes(from_memory)[c(
    "names", "row.names", "class"
  )]
  expect_equal(
    as.data.frame(from_deferred), as.data.frame(from_memory), tolerance = 0
  )

  path <- data.frame(
    station = c("A", "B"), longitude = c(-80, -79), latitude = c(-12, -11)
  )
  transect_args <- list(
    path = path, id_col = "station", depth = c(0, 50),
    time = deferred$time[[2L]], variable = c("oxygen", "temperature"),
    match = "exact", mode = "section", keep_index = TRUE
  )
  from_deferred <- do.call(
    cube_transect, c(list(x = deferred), transect_args)
  )
  from_memory <- do.call(cube_transect, c(list(x = memory), transect_args))
  attributes(from_deferred) <- attributes(from_deferred)[c(
    "names", "row.names", "class"
  )]
  attributes(from_memory) <- attributes(from_memory)[c(
    "names", "row.names", "class"
  )]
  expect_equal(
    as.data.frame(from_deferred), as.data.frame(from_memory), tolerance = 0
  )
})

test_that("cube_open preserves temporal operation parity", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  deferred <- cube_open(file, vars = c("temperature", "oxygen"))
  memory <- cube_collect(deferred)

  deferred_aggregate <- cube_aggregate_time(deferred, by = "day")
  memory_aggregate <- cube_aggregate_time(memory, by = "day")
  expect_equal(deferred_aggregate$data, memory_aggregate$data, tolerance = 0)

  deferred_clim <- suppressWarnings(cube_climatology(deferred, by = "day"))
  memory_clim <- suppressWarnings(cube_climatology(memory, by = "day"))
  expect_equal(deferred_clim$mean, memory_clim$mean, tolerance = 0)

  deferred_anom <- cube_anomaly(deferred, deferred_clim)
  memory_anom <- cube_anomaly(memory, memory_clim)
  expect_equal(deferred_anom$data, memory_anom$data, tolerance = 0)

  deferred_trend <- cube_trend(deferred, time_unit = "day")
  memory_trend <- cube_trend(memory, time_unit = "day")
  expect_equal(deferred_trend$data, memory_trend$data, tolerance = 0)
})

test_that("cube_open validates variables and unsupported resources publicly", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)

  expect_error(cube_open(file, vars = character()), "at least one")
  expect_error(cube_open(file, vars = NA_character_), "non-empty")
  expect_error(cube_open(file, vars = ""), "non-empty")
  expect_error(cube_open(file, vars = c("oxygen", "oxygen")), "duplicates")
  expect_error(cube_open(file, vars = "missing"), "not present")
  expect_error(cube_open(file, vars = "longitude"), "Coordinate variable")
  expect_error(cube_open(c(file, file), vars = "temperature"), "`file`")
  expect_error(cube_open(tempdir(), vars = "temperature"), "directory")
  expect_error(
    cube_open("https://example.org/ocean.nc", vars = "temperature"),
    "local files only"
  )
  expect_error(
    cube_open(file, vars = c("temperature", "sst")),
    "vertical axes are incompatible"
  )
  expect_error(
    cube_open(file, vars = NULL, lat_name = "latitude", depth_name = "depth"),
    "cannot use the initial rectilinear backend"
  )
  expect_error(
    cube_open(file, vars = "temperature", lon_name = "not_lon"),
    "does not exist"
  )

  ambiguous <- make_netcdf_backend_fixture(ambiguous_longitude = TRUE)
  withr::local_file(ambiguous)
  expect_error(
    cube_open(ambiguous, vars = c("temperature", "longitude_probe")),
    "multiple CF candidates"
  )

  unsupported <- make_netcdf_backend_fixture(calendar = "360_day")
  withr::local_file(unsupported)
  expect_error(cube_open(unsupported, vars = "temperature"), "unsupported")
})

test_that("cube_open vars NULL rejects a coordinate-only NetCDF", {
  file <- tempfile(tmpdir = tempdir(), fileext = ".nc")
  withr::local_file(file)
  dimension <- ncdf4::ncdim_def(
    "lon", "", 1:2, create_dimvar = FALSE
  )
  coordinate <- ncdf4::ncvar_def("lon", "degrees_east", list(dimension))
  nc <- ncdf4::nc_create(file, list(coordinate))
  ncdf4::ncvar_put(nc, "lon", c(-80, -79))
  ncdf4::nc_close(nc)

  expect_error(
    cube_open(file, vars = NULL),
    "No eligible NetCDF data variables remain"
  )
})

test_that("cube_open serialization preserves descriptors and file identity", {
  file <- make_netcdf_backend_fixture()
  moved <- paste0(file, ".moved")
  rds <- tempfile(tmpdir = tempdir(), fileext = ".rds")
  withr::defer(unlink(c(file, moved, rds)))
  x <- cube_open(file, vars = "temperature")
  saveRDS(x, rds)
  restored <- readRDS(rds)

  expect_identical(restored$storage, x$storage)
  expect_false(.netcdf_contains_forbidden_object(restored$storage))
  expect_equal(.cube_read(restored), .cube_read(x), tolerance = 0)
  expect_true(file.rename(file, moved))
  expect_error(
    .cube_read(restored), "no longer exists",
    class = "oceancube_netcdf_changed_file"
  )
  expect_true(file.rename(moved, file))

  expect_true(Sys.setFileTime(file, file.info(file)$mtime + 10))
  expect_error(
    .cube_read(restored), "expected modified time.*found",
    class = "oceancube_netcdf_changed_file"
  )
})

test_that("coast_dist preserves a public deferred cube", {
  skip_if_not_installed("sf")
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  x <- cube_open(file, vars = "temperature")
  coast <- sf::st_sfc(sf::st_linestring(matrix(
    c(-81, -13, -81, -10), ncol = 2, byrow = TRUE
  )), crs = 4326)

  y <- coast_dist(x, coast)
  expect_identical(.cube_backend(y), "netcdf")
  expect_false("data" %in% names(y))
  expect_true("storage" %in% names(y))
  expect_true("dc" %in% names(y))
  expect_identical(
    vapply(y$provenance$history, `[[`, character(1), "operation"),
    c("read_nc", "coast_dist")
  )
})
