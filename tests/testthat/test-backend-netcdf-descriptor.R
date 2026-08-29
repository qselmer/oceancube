test_that("a local NetCDF file produces a serializable storage descriptor", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)

  storage <- .new_netcdf_storage(
    file,
    variables = c("temperature", "oxygen"),
    source = "controlled-fixture",
    dataset_id = "fixture-001"
  )

  expect_type(storage, "list")
  expect_identical(class(storage), "list")
  expect_identical(storage$version, 1L)
  expect_identical(storage$backend, "netcdf")
  expect_true(storage$read_only)
  expect_identical(storage$file$path, file)
  expect_true(grepl("/", storage$file$normalized_path, fixed = TRUE))
  expect_true(storage$file$size_bytes > 0)
  expect_s3_class(storage$file$modified_utc, "POSIXct")
  expect_identical(attr(storage$file$modified_utc, "tzone"), "UTC")
  expect_identical(storage$file$identity_policy, "size_mtime_error")
  expect_identical(storage$options$source, "controlled-fixture")
  expect_identical(storage$options$dataset_id, "fixture-001")
  expect_true(.validate_netcdf_storage(storage))
  expect_type(serialize(storage, NULL), "raw")
  expect_false(.netcdf_contains_forbidden_object(storage))
})

test_that("file paths are normalized without requiring an nc extension", {
  absolute <- make_netcdf_backend_fixture()
  withr::local_file(absolute)

  old <- withr::local_dir(dirname(absolute))
  relative_storage <- .new_netcdf_storage(
    basename(absolute),
    variables = "temperature"
  )
  expect_identical(
    relative_storage$file$normalized_path,
    normalizePath(absolute, winslash = "/", mustWork = TRUE)
  )

  spaced <- file.path(
    tempdir(),
    paste0("oceancube descriptor con espacios-", basename(absolute))
  )
  expect_true(file.rename(absolute, spaced))
  withr::local_file(spaced)
  spaced_storage <- .new_netcdf_storage(spaced, variables = "temperature")
  expect_identical(
    spaced_storage$file$normalized_path,
    normalizePath(spaced, winslash = "/", mustWork = TRUE)
  )

  other_extension <- sub("\\.nc$", ".bin", spaced)
  expect_true(file.rename(spaced, other_extension))
  withr::local_file(other_extension)
  expect_silent(
    storage <- .new_netcdf_storage(
      other_extension,
      variables = "temperature"
    )
  )
  expect_identical(storage$backend, "netcdf")

  unicode_source <- make_netcdf_backend_fixture()
  unicode_path <- file.path(
    tempdir(),
    paste0("oceancube-", intToUtf8(233), "-descriptor.nc")
  )
  withr::local_file(unicode_source)
  expect_true(file.rename(unicode_source, unicode_path))
  withr::local_file(unicode_path)
  expect_silent(
    unicode_storage <- .new_netcdf_storage(
      unicode_path,
      variables = "temperature"
    )
  )
  expect_identical(unicode_storage$backend, "netcdf")
  rm(old)
})

test_that("invalid local resources fail with differentiated errors", {
  missing <- tempfile(tmpdir = tempdir(), fileext = ".nc")

  expect_error(
    .new_netcdf_storage(missing, "temperature"),
    "does not exist",
    class = "oceancube_netcdf_file_error"
  )
  expect_error(
    .new_netcdf_storage(tempdir(), "temperature"),
    "is a directory",
    class = "oceancube_netcdf_file_error"
  )
  expect_error(
    .new_netcdf_storage("https://example.org/ocean.nc", "temperature"),
    "local files only",
    class = "oceancube_netcdf_file_error"
  )
  expect_error(.new_netcdf_storage(NA_character_, "temperature"), "`file`")
  expect_error(.new_netcdf_storage(character(), "temperature"), "`file`")
})

test_that("CF attributes, explicit names, and known names resolve dimensions", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)

  cf <- .new_netcdf_storage(file, "temperature")
  expect_identical(
    cf$dimensions$canonical$longitude$detection$method,
    "cf_attributes"
  )
  expect_identical(
    cf$dimensions$canonical$latitude$detection$method,
    "cf_attributes"
  )

  explicit <- .new_netcdf_storage(
    file,
    "temperature",
    lon_name = "longitude",
    lat_name = "latitude",
    depth_name = "depth",
    time_name = "time"
  )
  expect_true(all(vapply(
    explicit$dimensions$canonical,
    function(x) identical(x$detection$method, "explicit"),
    logical(1)
  )))

  known_file <- make_netcdf_backend_fixture(
    add_cf_attributes = FALSE,
    neutral_spatial_units = TRUE
  )
  withr::local_file(known_file)
  known <- .new_netcdf_storage(known_file, "temperature")
  expect_identical(
    known$dimensions$canonical$longitude$detection$method,
    "known_name"
  )
  expect_identical(
    known$dimensions$canonical$latitude$detection$method,
    "known_name"
  )
  expect_identical(
    known$dimensions$canonical$depth$detection$method,
    "known_name"
  )
})

test_that("ambiguous and unresolved dimensions require explicit guidance", {
  ambiguous <- make_netcdf_backend_fixture(ambiguous_longitude = TRUE)
  withr::local_file(ambiguous)
  expect_error(
    .new_netcdf_storage(
      ambiguous,
      c("temperature", "longitude_probe")
    ),
    "longitude.*multiple CF candidates.*longitude_aux.*explicitly"
  )

  unknown <- make_netcdf_backend_fixture(
    add_cf_attributes = FALSE,
    neutral_spatial_units = TRUE,
    coordinate_names = c(
      longitude = "ucoord",
      latitude = "vcoord",
      depth = "zcoord",
      time = "epoch"
    )
  )
  withr::local_file(unknown)
  expect_error(
    .new_netcdf_storage(unknown, "temperature"),
    "Cannot resolve the longitude dimension.*ucoord.*specify"
  )
  expect_silent(
    explicitly_resolved <- .new_netcdf_storage(
      unknown,
      "temperature",
      lon_name = "ucoord",
      lat_name = "vcoord",
      depth_name = "zcoord",
      time_name = "epoch"
    )
  )
  expect_identical(
    explicitly_resolved$dimensions$canonical$longitude$source_dimension,
    "ucoord"
  )
})

test_that("requested data variables are strict and preserve order", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)

  storage <- .new_netcdf_storage(
    file,
    variables = c("oxygen", "temperature")
  )
  expect_identical(storage$variables$order, c("oxygen", "temperature"))
  expect_identical(names(storage$variables$map), c("oxygen", "temperature"))

  expect_error(.new_netcdf_storage(file, character()), "at least one")
  expect_error(.new_netcdf_storage(file, NA_character_), "non-empty")
  expect_error(.new_netcdf_storage(file, 1), "character vector")
  expect_error(
    .new_netcdf_storage(file, c("oxygen", "oxygen")),
    "duplicates"
  )
  expect_error(
    .new_netcdf_storage(file, "salinity"),
    "not present.*Available data variables"
  )
  expect_error(
    .new_netcdf_storage(file, "longitude"),
    "Coordinate variable.*cannot be selected"
  )
})

test_that("variable maps retain physical orders and decoding metadata", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  storage <- .new_netcdf_storage(
    file,
    variables = c("temperature", "oxygen")
  )
  temperature <- storage$variables$map$temperature
  oxygen <- storage$variables$map$oxygen

  expect_identical(
    temperature$source_dimension_names,
    c("longitude", "latitude", "depth", "time")
  )
  expect_identical(
    temperature$source_to_canonical_permutation,
    1:4
  )
  expect_identical(
    oxygen$source_dimension_names,
    c("time", "depth", "latitude", "longitude")
  )
  expect_identical(
    oxygen$source_to_canonical_permutation,
    c(4L, 3L, 2L, 1L)
  )
  expect_identical(
    oxygen$canonical_to_source_permutation,
    c(4L, 3L, 2L, 1L)
  )
  expect_identical(temperature$source_type, "short")
  expect_equal(temperature$fill_value, -32767)
  expect_equal(temperature$missing_value, -32766)
  expect_equal(temperature$scale_factor, 0.5)
  expect_equal(temperature$add_offset, 11000)
  expect_identical(temperature$units, "degree_Celsius")
  expect_identical(
    temperature$standard_name,
    "sea_water_potential_temperature"
  )

  physical <- array(1:48, dim = c(4, 2, 2, 3))
  canonical <- aperm(
    physical,
    oxygen$source_to_canonical_permutation
  )
  expect_identical(dim(canonical), c(3L, 2L, 2L, 4L))
  expect_identical(
    aperm(canonical, oxygen$canonical_to_source_permutation),
    physical
  )
})

test_that("surface variables use a singleton depth and do not mix with 3D", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)

  surface <- .new_netcdf_storage(file, "sst")
  map <- surface$variables$map$sst
  expect_identical(surface$dimensions$canonical$depth$values, NA_real_)
  expect_identical(surface$dimensions$canonical$depth$length, 1L)
  expect_identical(map$canonical_axes, c("longitude", "latitude", "time"))
  expect_identical(map$singleton_axes_inserted, "depth")
  expect_identical(
    surface$dimensions$shape,
    c(longitude = 3L, latitude = 2L, depth = 1L, time = 4L, variable = 1L)
  )

  expect_error(
    .new_netcdf_storage(file, c("temperature", "sst")),
    "vertical axes are incompatible.*separate cubes"
  )
  expect_error(
    .new_netcdf_storage(
      file,
      c("temperature", "chlorophyll"),
      lat_name = "latitude"
    ),
    "chlorophyll.*latitude_chlorophyll.*do not map"
  )
})

test_that("coordinates preserve source values, orientation, and attributes", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  storage <- .new_netcdf_storage(file, "temperature")
  dimensions <- storage$dimensions$canonical

  expect_identical(dimensions$longitude$values, c(-80, -79, -78))
  expect_identical(dimensions$longitude$units, "degrees_east")
  expect_identical(dimensions$latitude$values, c(-12, -11))
  expect_identical(dimensions$latitude$units, "degrees_north")
  expect_identical(dimensions$depth$values, c(0, 50))
  expect_identical(dimensions$depth$positive, "down")
  expect_identical(dimensions$depth$standard_name, "depth")
  expect_identical(dimensions$depth$axis_attribute, "Z")
  expect_identical(storage$time$raw_values, c(0, 1, 2, 3))
  expect_s3_class(storage$time$decoded_values, "POSIXct")
})

test_that("time decoding is strict and preserves source metadata", {
  gregorian <- make_netcdf_backend_fixture(calendar = "gregorian")
  withr::local_file(gregorian)
  gregorian_storage <- .new_netcdf_storage(gregorian, "temperature")
  expect_identical(gregorian_storage$time$calendar, "gregorian")
  expect_identical(gregorian_storage$time$decode_status, "decoded")
  expect_identical(
    as.character(gregorian_storage$time$decoded_values),
    c(
      "2000-01-01",
      "2000-01-02",
      "2000-01-03",
      "2000-01-04"
    )
  )

  standard <- make_netcdf_backend_fixture(calendar = "standard")
  withr::local_file(standard)
  expect_identical(
    .new_netcdf_storage(standard, "temperature")$time$calendar,
    "standard"
  )

  absent <- make_netcdf_backend_fixture(add_calendar = FALSE)
  withr::local_file(absent)
  expect_identical(
    .new_netcdf_storage(absent, "temperature")$time$calendar,
    "standard"
  )

  hours <- make_netcdf_backend_fixture(
    time_units = "hours since 2000-01-01 00:00:00"
  )
  withr::local_file(hours)
  hours_storage <- .new_netcdf_storage(hours, "temperature")
  expect_equal(
    as.numeric(diff(hours_storage$time$decoded_values), units = "secs"),
    rep(3600, 3)
  )

  calendar_aware <- make_netcdf_backend_fixture(calendar = "360_day")
  withr::local_file(calendar_aware)
  calendar_storage <- .new_netcdf_storage(calendar_aware, "temperature")
  expect_s3_class(calendar_storage$time$decoded_values, "oceancube_cf_time")
  expect_identical(
    attr(calendar_storage$time$decoded_values, "calendar", exact = TRUE),
    "360_day"
  )
})

test_that("logical NetCDF cubes expose metadata without resident data", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  storage <- .new_netcdf_storage(
    file,
    c("temperature", "oxygen"),
    source = "fixture",
    dataset_id = "logical-cube"
  )
  cube <- .new_netcdf_cube(storage)

  expect_s3_class(cube, "ocean_cube")
  expect_identical(class(cube), c("ocean_cube", "list"))
  expect_identical(.cube_backend(cube), "netcdf")
  expect_identical(
    .cube_shape(cube),
    c(longitude = 3L, latitude = 2L, depth = 2L, time = 4L, variable = 2L)
  )
  expect_identical(.cube_storage_shape(cube), .cube_shape(cube))
  expect_identical(cube$lon, c(-80, -79, -78))
  expect_identical(cube$lat, c(-12, -11))
  expect_identical(cube$depth, c(0, 50))
  expect_identical(cube$vars, c("temperature", "oxygen"))
  expect_identical(cube$source, "fixture")
  expect_identical(cube$dataset_id, "logical-cube")
  expect_false("data" %in% names(cube))
  expect_true("storage" %in% names(cube))
  expect_true(.check_cube(cube))

  printed <- capture.output(print(cube))
  summarized <- summary(cube)
  expect_match(printed[[1L]], "<ocean_cube>", fixed = TRUE)
  expect_true(any(grepl("3 x 2 x 2 x 4 x 2", printed, fixed = TRUE)))
  expect_false(any(grepl(storage$file$normalized_path, printed, fixed = TRUE)))
  expect_s3_class(summarized, "data.frame")
  expect_identical(summarized$n, c(3L, 2L, 2L, 4L, 2L))
})

test_that("NetCDF cubes survive RDS serialization without connections", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  cube <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )
  rds <- tempfile(tmpdir = tempdir(), fileext = ".rds")
  withr::local_file(rds)

  saveRDS(cube, rds)
  restored <- readRDS(rds)

  expect_identical(.cube_backend(restored), "netcdf")
  expect_identical(.cube_shape(restored), .cube_shape(cube))
  expect_identical(restored$storage, cube$storage)
  expect_false(.netcdf_contains_forbidden_object(restored$storage))
})

test_that("corrupt descriptors and mismatched headers fail explicitly", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  storage <- .new_netcdf_storage(file, "temperature")
  cube <- .new_netcdf_cube(storage)

  bad_backend <- storage
  bad_backend$backend <- "memory"
  expect_error(
    .validate_netcdf_storage(bad_backend, check_file = FALSE),
    "backend.*netcdf",
    class = "oceancube_bad_storage"
  )

  bad_shape <- storage
  bad_shape$dimensions$shape[["time"]] <- 5L
  expect_error(
    .validate_netcdf_storage(bad_shape, check_file = FALSE),
    "Invalid NetCDF storage shape",
    class = "oceancube_bad_storage"
  )

  bad_permutation <- storage
  bad_permutation$variables$map$temperature$
    source_to_canonical_permutation <- c(1L, 1L, 3L, 4L)
  expect_error(
    .validate_netcdf_storage(bad_permutation, check_file = FALSE),
    "Invalid NetCDF permutation"
  )

  bad_cube <- cube
  bad_cube$storage$backend <- "broken"
  expect_error(.cube_backend(bad_cube), "backend.*netcdf")

  mismatched <- cube
  mismatched$lon <- mismatched$lon[-1]
  expect_error(.cube_shape(mismatched), "header shape")
})

test_that("data reads are available and writes remain read-only", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  cube <- .new_netcdf_cube(.new_netcdf_storage(file, "temperature"))
  before <- unname(file.info(file)$size)

  expect_identical(dim(.cube_read(cube)), c(3L, 2L, 2L, 4L, 1L))
  expect_identical(
    unname(.cube_read_block(
      cube,
      rep(1L, 5L),
      rep(1L, 5L)
    )),
    array(11111, dim = rep(1L, 5L))
  )
  expect_error(
    .cube_write_block(
      cube,
      array(0, dim = rep(1L, 5L)),
      rep(1L, 5L)
    ),
    "NetCDF backend is read-only.*Collect the cube into memory",
    class = "oceancube_netcdf_read_only"
  )
  expect_identical(unname(file.info(file)$size), before)
})

test_that("descriptor construction reads coordinate variables only", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  observed <- character()
  original <- .read_netcdf_coordinate

  local_mocked_bindings(
    .read_netcdf_coordinate = function(nc, variable) {
      observed <<- c(observed, variable)
      original(nc, variable)
    },
    .package = "oceancube"
  )

  storage <- .new_netcdf_storage(
    file,
    variables = c("temperature", "oxygen")
  )
  expect_setequal(observed, c("longitude", "latitude", "depth", "time"))
  expect_false(any(storage$variables$order %in% observed))
  expect_false(.netcdf_contains_forbidden_object(storage))
})

test_that("connections close after success and after schema errors", {
  file <- make_netcdf_backend_fixture()
  moved <- paste0(file, ".moved")
  withr::defer(unlink(c(file, moved)))
  before <- rownames(showConnections(all = TRUE))

  storage <- .new_netcdf_storage(file, "temperature")
  after_success <- rownames(showConnections(all = TRUE))
  expect_setequal(after_success, before)
  expect_true(file.rename(file, moved))
  expect_true(file.rename(moved, file))
  expect_true(.validate_netcdf_storage(storage))

  expect_error(.new_netcdf_storage(file, "salinity"), "not present")
  after_error <- rownames(showConnections(all = TRUE))
  expect_setequal(after_error, before)
  expect_true(file.rename(file, moved))
  expect_true(file.rename(moved, file))
})

test_that("file identity detects disappearance, size, and mtime changes", {
  missing_file <- make_netcdf_backend_fixture()
  missing_storage <- .new_netcdf_storage(missing_file, "temperature")
  expect_identical(unlink(missing_file), 0L)
  expect_error(
    .validate_netcdf_storage(missing_storage),
    "no longer exists",
    class = "oceancube_netcdf_changed_file"
  )

  size_file <- make_netcdf_backend_fixture()
  withr::local_file(size_file)
  size_storage <- .new_netcdf_storage(size_file, "temperature")
  connection <- file(size_file, open = "ab")
  writeBin(as.raw(0), connection)
  close(connection)
  expect_error(
    .validate_netcdf_storage(size_storage),
    "expected size.*found",
    class = "oceancube_netcdf_changed_file"
  )

  time_file <- make_netcdf_backend_fixture()
  withr::local_file(time_file)
  time_storage <- .new_netcdf_storage(time_file, "temperature")
  expect_true(Sys.setFileTime(
    time_file,
    file.info(time_file)$mtime + 10
  ))
  expect_error(
    .validate_netcdf_storage(time_storage),
    "expected modified time.*found",
    class = "oceancube_netcdf_changed_file"
  )
})

test_that("the memory backend remains unchanged", {
  cube <- .make_baseline_fixture()$cube

  expect_identical(.cube_backend(cube), "memory")
  expect_identical(.cube_storage_shape(cube), .cube_shape(cube))
  expect_identical(.cube_read(cube), cube$data)
  expect_identical(
    .cube_read_block(cube, rep(1L, 5L), rep(1L, 5L)),
    cube$data[1, 1, 1, 1, 1, drop = FALSE]
  )
})
