collect_netcdf_fixture <- function() {
  file <- make_netcdf_backend_fixture()
  storage <- .new_netcdf_storage(
    file,
    c("temperature", "oxygen"),
    source = "collect-fixture",
    dataset_id = "collect-001"
  )
  cube <- .new_netcdf_cube(
    storage,
    provenance = list(previous_step = "descriptor"),
    qa = list(reviewed = TRUE)
  )
  cube$mask <- list(kind = "fixture-mask")
  cube$dc <- matrix(1:6, nrow = 3L, ncol = 2L)
  cube$climatology <- list(method = "fixture")
  cube$anomaly <- list(reference = "fixture")
  list(file = file, storage = storage, cube = cube)
}

test_that("estimated bytes uses logical double shape without reading", {
  memory <- .make_baseline_fixture()$cube
  expect_identical(.cube_estimated_bytes(memory), 768)

  fixture <- collect_netcdf_fixture()
  withr::local_file(fixture$file)
  expect_identical(.cube_estimated_bytes(fixture$cube), 768)

  local_mocked_bindings(
    .cube_shape = function(x) rep(1e100, 4L),
    .package = "oceancube"
  )
  expect_error(
    .cube_estimated_bytes(fixture$cube),
    "overflow",
    class = "oceancube_size_overflow"
  )
})

test_that("cube_collect returns an existing memory cube unchanged", {
  memory <- .make_baseline_fixture()$cube
  expect_true("cube_collect" %in% getNamespaceExports("oceancube"))
  local_mocked_bindings(
    .cube_read = function(...) {
      stop("memory data must not be reread")
    },
    .package = "oceancube"
  )

  collected <- cube_collect(memory)
  expect_identical(collected, memory)
  expect_identical(.cube_backend(collected), "memory")
  expect_true(.check_cube(collected))
})

test_that("cube_collect materializes NetCDF with identical values and shape", {
  fixture <- collect_netcdf_fixture()
  withr::local_file(fixture$file)
  expected <- .cube_read(fixture$cube)
  before <- file.info(fixture$file)

  collected <- cube_collect(fixture$cube)
  after <- file.info(fixture$file)

  expect_identical(.cube_backend(collected), "memory")
  expect_identical(.cube_shape(collected), .cube_shape(fixture$cube))
  expect_identical(.cube_read(collected), expected)
  expect_identical(collected$lon, fixture$cube$lon)
  expect_identical(collected$lat, fixture$cube$lat)
  expect_identical(collected$depth, fixture$cube$depth)
  expect_identical(collected$time, fixture$cube$time)
  expect_identical(collected$vars, fixture$cube$vars)
  expect_identical(collected$units, fixture$cube$units)
  expect_identical(as.double(after$size), as.double(before$size))
  expect_identical(as.numeric(after$mtime), as.numeric(before$mtime))
  expect_false("storage" %in% names(collected))
  expect_true(.check_cube(collected))
})

test_that("cube_collect preserves scientific metadata and adds provenance", {
  fixture <- collect_netcdf_fixture()
  withr::local_file(fixture$file)
  collected <- cube_collect(fixture$cube)
  record <- collected$provenance$cube_collect

  expect_identical(collected$source, fixture$cube$source)
  expect_identical(collected$dataset_id, fixture$cube$dataset_id)
  expect_identical(
    collected$spatial_extent,
    fixture$cube$spatial_extent
  )
  expect_identical(
    collected$temporal_extent,
    fixture$cube$temporal_extent
  )
  expect_identical(collected$depth_extent, fixture$cube$depth_extent)
  expect_identical(collected$mask, fixture$cube$mask)
  expect_identical(collected$dc, fixture$cube$dc)
  expect_identical(
    collected$climatology,
    fixture$cube$climatology
  )
  expect_identical(collected$anomaly, fixture$cube$anomaly)
  expect_identical(collected$qa, fixture$cube$qa)
  expect_identical(
    collected$provenance$parent,
    fixture$cube$provenance
  )
  expect_identical(record$operation, "cube_collect")
  expect_identical(record$source_backend, "netcdf")
  expect_identical(record$target_backend, "memory")
  expect_identical(
    record$source_file,
    fixture$storage$file$normalized_path
  )
  expect_identical(record$variables, fixture$cube$vars)
  expect_identical(record$shape, .cube_shape(fixture$cube))
  expect_identical(record$estimated_bytes, 768)
  expect_s3_class(record$collected_utc, "POSIXct")
})

test_that("collected cube remains usable after deleting the source file", {
  fixture <- collect_netcdf_fixture()
  collected <- cube_collect(fixture$cube)
  expected <- .cube_read(collected)
  expect_identical(unlink(fixture$file), 0L)

  expect_identical(.cube_backend(collected), "memory")
  expect_identical(.cube_read(collected), expected)
  expect_s3_class(clim_month(collected), "ocean_clim")
  expect_s3_class(
    layer_mean(collected, depth = range(collected$depth)),
    "ocean_cube"
  )
  expect_error(
    .cube_read(fixture$cube),
    "no longer exists",
    class = "oceancube_netcdf_changed_file"
  )
})

test_that("collected cube is writable while NetCDF remains read-only", {
  fixture <- collect_netcdf_fixture()
  withr::local_file(fixture$file)
  before <- file.info(fixture$file)
  collected <- cube_collect(fixture$cube)
  replacement <- array(999, dim = rep(1L, 5L))

  expect_error(
    .cube_write_block(
      fixture$cube,
      replacement,
      rep(1L, 5L)
    ),
    "NetCDF backend is read-only",
    class = "oceancube_netcdf_read_only"
  )
  modified <- .cube_write_block(
    collected,
    replacement,
    rep(1L, 5L)
  )
  after <- file.info(fixture$file)

  expect_identical(.cube_read(modified)[1, 1, 1, 1, 1], 999)
  expect_identical(.cube_read(collected)[1, 1, 1, 1, 1], 11111)
  expect_identical(as.double(after$size), as.double(before$size))
  expect_identical(as.numeric(after$mtime), as.numeric(before$mtime))
})

test_that("cube_collect closes connections and adds failure context", {
  fixture <- collect_netcdf_fixture()
  moved <- paste0(fixture$file, ".moved")
  withr::defer(unlink(c(fixture$file, moved)))
  original <- .ncvar_get_block
  local_mocked_bindings(
    .ncvar_get_block = function(nc, variable, start, count) {
      if (identical(variable, "oxygen")) {
        stop("forced oxygen failure")
      }
      original(nc, variable, start, count)
    },
    .package = "oceancube"
  )

  expect_error(
    cube_collect(fixture$cube),
    "Failed to collect NetCDF cube.*oxygen",
    class = "oceancube_collect_error"
  )
  expect_true(file.rename(fixture$file, moved))
  expect_true(file.rename(moved, fixture$file))
})

test_that("cube_collect rejects invalid inputs and unavailable sources", {
  expect_error(
    cube_collect(list()),
    "must be an <ocean_cube>",
    class = "oceancube_bad_cube"
  )

  missing <- collect_netcdf_fixture()
  expect_identical(unlink(missing$file), 0L)
  expect_error(
    cube_collect(missing$cube),
    "Failed to collect NetCDF cube.*no longer exists",
    class = "oceancube_collect_error"
  )

  corrupt <- collect_netcdf_fixture()
  withr::local_file(corrupt$file)
  corrupt$cube$storage$backend <- "unknown"
  expect_error(
    cube_collect(corrupt$cube),
    "backend.*netcdf",
    class = "oceancube_bad_storage"
  )
})

test_that("collected cubes survive RDS serialization as memory", {
  fixture <- collect_netcdf_fixture()
  withr::local_file(fixture$file)
  collected <- cube_collect(fixture$cube)
  rds <- tempfile(tmpdir = tempdir(), fileext = ".rds")
  withr::local_file(rds)
  saveRDS(collected, rds)
  restored <- readRDS(rds)

  expect_identical(.cube_backend(restored), "memory")
  expect_identical(.cube_read(restored), .cube_read(collected))
  expect_identical(restored$provenance, collected$provenance)
  expect_true(.check_cube(restored))
})
