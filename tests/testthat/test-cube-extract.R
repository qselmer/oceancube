extract_baseline_cube <- function() {
  cube <- .make_baseline_fixture()$cube
  cube$source <- "synthetic"
  cube$dataset_id <- "extract-001"
  cube$provenance <- list(previous_step = "fixture")
  cube
}

extract_netcdf_fixture <- function() {
  file <- make_netcdf_backend_fixture(
    time_units = "days since 2020-01-01 00:00:00",
    time_values = c(0, 31, 366, 397)
  )
  storage <- .new_netcdf_storage(
    file,
    c("temperature", "oxygen"),
    source = "synthetic",
    dataset_id = "extract-001"
  )
  list(
    file = file,
    cube = .new_netcdf_cube(
      storage,
      provenance = list(previous_step = "fixture")
    )
  )
}

extract_equivalent_memory <- function(netcdf_cube) {
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
  memory <- ocean_cube(
    lon = netcdf_cube$lon,
    lat = netcdf_cube$lat,
    depth = netcdf_cube$depth,
    time = as.Date(netcdf_cube$time),
    vars = netcdf_cube$vars,
    units = netcdf_cube$units,
    data = values,
    source = netcdf_cube$source,
    dataset_id = netcdf_cube$dataset_id,
    provenance = netcdf_cube$provenance
  )
  memory$time <- netcdf_cube$time
  memory$temporal_extent <- range(netcdf_cube$time)
  dimnames(memory$data) <- stats::setNames(
    list(
      as.character(memory$lon),
      as.character(memory$lat),
      as.character(memory$depth),
      as.character(memory$time),
      memory$vars
    ),
    .cube_axis_names()
  )
  .check_cube(memory)
  memory
}

extract_data_only <- function(x) {
  attributes(x) <- attributes(x)[c("names", "row.names", "class")]
  x
}

test_that("cube_extract is exported with the intended public signature", {
  expect_true("cube_extract" %in% getNamespaceExports("oceancube"))
  expect_identical(
    names(formals(cube_extract)),
    c(
      "x", "longitude", "latitude", "depth", "time", "variable",
      "by", "match", "tolerance", "mode", "format", "keep_index",
      "keep_distance"
    )
  )
  expect_identical(eval(formals(cube_extract)$format), c("long", "wide"))
  expect_identical(
    eval(formals(cube_extract)$mode),
    c("point", "profile", "series", "table")
  )
})

test_that("point extraction returns the deterministic observation", {
  cube <- extract_baseline_cube()
  before <- cube
  result <- cube_extract(
    cube,
    longitude = -79,
    latitude = -11,
    depth = 50,
    time = as.Date("2021-02-01"),
    variable = "oxygen",
    mode = "point"
  )

  expect_s3_class(result, "data.frame")
  expect_false(inherits(result, "ocean_cube"))
  expect_identical(
    names(result),
    c("longitude", "latitude", "depth", "time", "variable", "unit", "value")
  )
  expect_identical(nrow(result), 1L)
  expect_identical(result$longitude, -79)
  expect_identical(result$latitude, -11)
  expect_identical(result$depth, 50)
  expect_identical(result$time, as.Date("2021-02-01"))
  expect_identical(result$variable, "oxygen")
  expect_identical(result$unit, "mmol m-3")
  expect_identical(result$value, 24222)
  expect_identical(cube, before)
})

test_that("profile mode varies depth and permits several variables", {
  result <- cube_extract(
    extract_baseline_cube(),
    longitude = -79,
    latitude = -11,
    time = as.Date("2021-02-01"),
    variable = c("temperature", "oxygen"),
    mode = "profile"
  )

  expect_identical(nrow(result), 4L)
  expect_identical(result$depth, c(0, 50, 0, 50))
  expect_identical(
    result$variable,
    c("temperature", "temperature", "oxygen", "oxygen")
  )
  expect_identical(result$value, c(14122, 14222, 24122, 24222))
  expect_identical(result$unit, c("degC", "degC", "mmol m-3", "mmol m-3"))
})

test_that("profile mode validates fixed axes", {
  cube <- extract_baseline_cube()
  expect_error(
    cube_extract(
      cube,
      longitude = c(-80, -79),
      latitude = -11,
      time = as.Date("2021-02-01"),
      mode = "profile"
    ),
    "profile.*longitude.*2",
    class = "oceancube_extract_mode"
  )
  expect_error(
    cube_extract(
      cube,
      longitude = -79,
      latitude = c(-12, -11),
      time = as.Date("2021-02-01"),
      mode = "profile"
    ),
    "profile.*latitude.*2"
  )
  expect_error(
    cube_extract(
      cube,
      longitude = -79,
      latitude = -11,
      time = as.Date(c("2020-01-01", "2021-02-01")),
      mode = "profile"
    ),
    "profile.*time.*2"
  )
})

test_that("series mode varies time in original order", {
  result <- cube_extract(
    extract_baseline_cube(),
    longitude = -79,
    latitude = -11,
    depth = 0,
    variable = "temperature",
    mode = "series",
    keep_index = TRUE
  )

  expect_identical(nrow(result), 4L)
  expect_s3_class(result$time, "Date")
  expect_identical(
    result$time,
    as.Date(c("2020-01-01", "2020-02-01", "2021-01-01", "2021-02-01"))
  )
  expect_identical(result$value, c(11122, 12122, 13122, 14122))
  expect_identical(result$time_index, 1:4)
})

test_that("series mode validates fixed axes", {
  cube <- extract_baseline_cube()
  expect_error(
    cube_extract(
      cube,
      longitude = c(-80, -79),
      latitude = -11,
      depth = 0,
      mode = "series"
    ),
    "series.*longitude.*2"
  )
  expect_error(
    cube_extract(
      cube,
      longitude = -79,
      latitude = c(-12, -11),
      depth = 0,
      mode = "series"
    ),
    "series.*latitude.*2"
  )
  expect_error(
    cube_extract(
      cube,
      longitude = -79,
      latitude = -11,
      depth = c(0, 50),
      mode = "series"
    ),
    "series.*depth.*2"
  )
})

test_that("point and table modes retain Cartesian-product semantics", {
  cube <- extract_baseline_cube()
  point <- cube_extract(
    cube,
    longitude = c(-80, -78),
    latitude = c(-12, -11),
    depth = 0,
    time = as.Date("2020-01-01"),
    variable = "temperature",
    mode = "point"
  )
  table <- cube_extract(
    cube,
    longitude = c(-80, -78),
    latitude = c(-12, -11),
    depth = 0,
    time = as.Date("2020-01-01"),
    variable = "temperature",
    mode = "table"
  )

  expect_identical(nrow(point), 4L)
  expect_identical(extract_data_only(point), extract_data_only(table))
})

test_that("index mode preserves requested order, duplicates, and global indices", {
  result <- cube_extract(
    extract_baseline_cube(),
    longitude = c(3L, 1L, 3L),
    latitude = c(2L, 1L),
    depth = 2L,
    time = c(4L, 1L),
    variable = c(2L, 1L, 2L),
    by = "index",
    mode = "table",
    keep_index = TRUE
  )

  expect_identical(nrow(result), 36L)
  expect_identical(unique(result$longitude_index), c(3L, 1L))
  expect_identical(result$longitude_index[1:3], c(3L, 1L, 3L))
  expect_identical(unique(result$variable_index), c(2L, 1L))
  expect_identical(
    result$variable[c(1L, 13L, 25L)],
    c("oxygen", "temperature", "oxygen")
  )
  expect_identical(result$time_index[c(1L, 7L)], c(4L, 1L))
})

test_that("long value mode preserves repeated variables", {
  result <- cube_extract(
    extract_baseline_cube(),
    longitude = -79,
    latitude = -11,
    depth = 50,
    time = as.Date("2021-02-01"),
    variable = c("oxygen", "temperature", "oxygen")
  )
  expect_identical(
    result$variable,
    c("oxygen", "temperature", "oxygen")
  )
  expect_identical(result$value, c(24222, 14222, 24222))
  expect_identical(result$unit, c("mmol m-3", "degC", "mmol m-3"))
})

test_that("exact value mode shares cube_slice selector errors", {
  cube <- extract_baseline_cube()
  expect_error(cube_extract(cube, longitude = -79.5), "Exact longitude")
  expect_error(cube_extract(cube, longitude = numeric()), "must not be empty")
  expect_error(cube_extract(cube, longitude = NA_real_), "finite")
  expect_error(cube_extract(cube, variable = "unknown"), "Unknown variable")
  expect_error(cube_extract(cube, variable = character()), "must not be empty")
  expect_error(cube_extract(cube, variable = NA_character_), "non-empty")
  expect_error(
    cube_extract(cube, longitude = 1L, by = "index", match = "exact"),
    "do not supply"
  )
})

test_that("nearest diagnostics distinguish requested and selected coordinates", {
  result <- cube_extract(
    extract_baseline_cube(),
    longitude = -79.4,
    latitude = -11.2,
    depth = 40,
    time = as.Date("2021-01-10"),
    variable = "temperature",
    match = "nearest",
    tolerance = list(
      longitude = 0.5,
      latitude = 0.5,
      depth = 15,
      time = as.difftime(15, units = "days")
    ),
    keep_index = TRUE,
    keep_distance = TRUE
  )

  expect_identical(result$longitude, -79)
  expect_identical(result$latitude, -11)
  expect_identical(result$depth, 50)
  expect_identical(result$time, as.Date("2021-01-01"))
  expect_identical(result$longitude_index, 2L)
  expect_identical(result$latitude_index, 2L)
  expect_identical(result$depth_index, 2L)
  expect_identical(result$time_index, 3L)
  expect_identical(result$variable_index, 1L)
  expect_equal(result$longitude_requested, -79.4)
  expect_equal(result$longitude_distance, 0.4)
  expect_equal(result$latitude_requested, -11.2)
  expect_equal(result$latitude_distance, 0.2)
  expect_identical(result$depth_requested, 40)
  expect_identical(result$depth_distance, 10)
  expect_identical(result$time_requested, as.Date("2021-01-10"))
  expect_s3_class(result$time_distance, "difftime")
  expect_equal(as.numeric(result$time_distance, units = "days"), 9)
  expect_identical(result$value, 13222)
})

test_that("nearest multiple selectors retain diagnostic correspondence", {
  result <- cube_extract(
    extract_baseline_cube(),
    longitude = c(-78.1, -79.9),
    latitude = -11.1,
    depth = 0,
    time = as.Date("2020-01-01"),
    variable = "temperature",
    match = "nearest",
    keep_distance = TRUE
  )

  expect_identical(result$longitude, c(-78, -80))
  expect_equal(result$longitude_requested, c(-78.1, -79.9))
  expect_equal(result$longitude_distance, c(0.1, 0.1))
  expect_equal(result$latitude_requested, c(-11.1, -11.1))
})

test_that("keep_distance rejects incompatible matching combinations", {
  cube <- extract_baseline_cube()
  expect_error(
    cube_extract(cube, longitude = 1L, by = "index", keep_distance = TRUE),
    "nearest"
  )
  expect_error(
    cube_extract(cube, longitude = -79, keep_distance = TRUE),
    "nearest"
  )
  expect_error(
    cube_extract(
      cube,
      longitude = -79,
      match = "exact",
      tolerance = list(longitude = 1)
    ),
    "tolerance"
  )
})

test_that("nearest respects domains, tolerances, ties, and descending axes", {
  cube <- extract_baseline_cube()
  expect_error(
    cube_extract(cube, longitude = -81, match = "nearest"),
    "outside the cube domain"
  )
  expect_error(
    cube_extract(
      cube,
      longitude = -79.4,
      match = "nearest",
      tolerance = list(longitude = 0.3)
    ),
    "exceeding tolerance"
  )
  tie <- cube_extract(
    cube,
    longitude = -79.5,
    match = "nearest",
    keep_distance = TRUE
  )
  expect_identical(unique(tie$longitude), -80)

  descending <- cube
  descending$lat <- rev(cube$lat)
  descending$data <- .cube_read(cube, index = list(latitude = 2:1))
  dimnames(descending$data)[[2L]] <- as.character(descending$lat)
  descending$spatial_extent <- cube$spatial_extent
  result <- cube_extract(
    descending,
    latitude = c(-11.1, -11.9),
    match = "nearest"
  )
  expect_identical(unique(result$latitude), c(-11, -12))
})

test_that("long format preserves complete R array order", {
  arr <- array(
    seq_len(2 * 2 * 2 * 2 * 2),
    dim = c(2, 2, 2, 2, 2)
  )
  cube <- ocean_cube(
    lon = c(-80, -79),
    lat = c(-12, -11),
    depth = c(0, 50),
    time = as.Date(c("2020-01-01", "2020-01-02")),
    vars = c("a", "b"),
    units = c(a = "ua", b = "ub"),
    data = arr
  )
  result <- cube_extract(cube, keep_index = TRUE)

  expect_identical(nrow(result), 32L)
  for (row in seq_len(nrow(result))) {
    expect_identical(
      result$value[[row]],
      arr[
        result$longitude_index[[row]],
        result$latitude_index[[row]],
        result$depth_index[[row]],
        result$time_index[[row]],
        result$variable_index[[row]]
      ]
    )
  }
  expect_identical(result$value, seq_len(32L))
})

test_that("general deterministic table preserves all selector orders", {
  result <- cube_extract(
    extract_baseline_cube(),
    longitude = c(-78, -80),
    latitude = c(-11, -12),
    depth = 50,
    time = as.Date(c("2021-02-01", "2020-01-01")),
    variable = c("oxygen", "temperature"),
    mode = "table",
    keep_index = TRUE
  )

  expect_identical(attr(result, "oceancube_shape"), c(
    longitude = 2L, latitude = 2L, depth = 1L, time = 2L, variable = 2L
  ))
  expect_identical(nrow(result), 16L)
  expect_identical(result$value[c(1L, 2L, 3L, 4L)], c(24223, 24221, 24213, 24211))
  expect_identical(result$value[c(5L, 9L, 13L)], c(21223, 14223, 11223))
  expect_identical(unique(result$time_index), c(4L, 1L))
  expect_identical(unique(result$variable_index), c(2L, 1L))
})

test_that("wide format produces one row per four-axis key", {
  result <- cube_extract(
    extract_baseline_cube(),
    longitude = -79,
    latitude = -11,
    depth = 0,
    mode = "series",
    format = "wide"
  )

  expect_identical(
    names(result),
    c("longitude", "latitude", "depth", "time", "temperature", "oxygen")
  )
  expect_identical(nrow(result), 4L)
  expect_identical(result$temperature, c(11122, 12122, 13122, 14122))
  expect_identical(result$oxygen, c(21122, 22122, 23122, 24122))
  expect_identical(
    attr(result, "units"),
    c(temperature = "degC", oxygen = "mmol m-3")
  )
})

test_that("wide keep_index is unambiguous for one variable", {
  result <- cube_extract(
    extract_baseline_cube(),
    longitude = -79,
    latitude = -11,
    depth = 0,
    time = as.Date(c("2020-02-01", "2021-02-01")),
    variable = "oxygen",
    format = "wide",
    keep_index = TRUE
  )
  expect_identical(nrow(result), 2L)
  expect_identical(result$longitude_index, c(2L, 2L))
  expect_identical(result$time_index, c(2L, 4L))
  expect_identical(result$variable_index, c(2L, 2L))
})

test_that("wide format preserves non-syntactic variable names", {
  cube <- extract_baseline_cube()
  cube$vars <- c("sea temp", "O2%")
  names(cube$units) <- cube$vars
  dimnames(cube$data)$var <- cube$vars
  .check_cube(cube)

  result <- cube_extract(
    cube,
    longitude = -79,
    latitude = -11,
    depth = 0,
    time = as.Date("2020-01-01"),
    format = "wide"
  )
  expect_true("sea temp" %in% names(result))
  expect_true("O2%" %in% names(result))
  expect_identical(result[["sea temp"]], 11122)
  expect_identical(result[["O2%"]], 21122)
})

test_that("wide format rejects duplicate variables and coordinate keys", {
  cube <- extract_baseline_cube()
  expect_error(
    cube_extract(cube, variable = c("oxygen", "oxygen"), format = "wide"),
    "duplicate variables"
  )
  expect_error(
    cube_extract(cube, longitude = c(-78, -78), format = "wide"),
    "duplicate coordinate keys"
  )
  expect_error(
    cube_extract(
      cube,
      longitude = -79,
      variable = c("temperature", "oxygen"),
      format = "wide",
      keep_index = TRUE
    ),
    "variable_index"
  )
})

test_that("wide output retains rows containing only missing values", {
  cube <- extract_baseline_cube()
  cube$data[2, 2, 1, 1, ] <- NA_real_
  result <- cube_extract(
    cube,
    longitude = -79,
    latitude = -11,
    depth = 0,
    time = as.Date("2020-01-01"),
    format = "wide"
  )

  expect_identical(nrow(result), 1L)
  expect_true(is.na(result$temperature))
  expect_true(is.na(result$oxygen))
})

test_that("missing data remain rows while invalid selections fail", {
  cube <- extract_baseline_cube()
  cube$data[2, 2, 2, 4, 2] <- NA_real_
  result <- cube_extract(
    cube,
    longitude = -79,
    latitude = -11,
    depth = 50,
    time = as.Date("2021-02-01"),
    variable = "oxygen"
  )
  expect_identical(nrow(result), 1L)
  expect_true(is.na(result$value))
  expect_error(cube_extract(cube, longitude = NA_real_), "finite")
  expect_error(cube_extract(cube, time = as.Date(NA)), "must not contain missing")
  expect_error(cube_extract(cube, longitude = 0L, by = "index"), "between 1 and")
})

test_that("integer, NA, and NaN values are not coerced to character or removed", {
  values <- array(c(1L, NA_integer_, NaN), dim = c(3, 1, 1, 1, 1))
  cube <- ocean_cube(
    lon = c(-80, -79, -78),
    lat = -12,
    depth = 0,
    time = as.Date("2020-01-01"),
    vars = "v",
    data = values
  )
  result <- cube_extract(cube)
  expect_true(is.numeric(result$value))
  expect_identical(nrow(result), 3L)
  expect_identical(result$value[[1L]], 1)
  expect_true(is.na(result$value[[2L]]))
  expect_true(is.nan(result$value[[3L]]))
})

test_that("surface depth remains an explicit NA column", {
  cube <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = NA_real_,
    time = as.Date(c("2020-01-01", "2020-01-02")),
    vars = "surface",
    units = c(surface = "degC"),
    data = array(c(1, 2), dim = c(1, 1, 1, 2, 1))
  )
  table <- cube_extract(cube)
  series <- cube_extract(
    cube,
    longitude = -80,
    latitude = -12,
    variable = "surface",
    mode = "series"
  )

  expect_identical(nrow(table), 2L)
  expect_true(all(is.na(table$depth)))
  expect_identical(nrow(series), 2L)
  expect_true(all(is.na(series$depth)))
  expect_error(cube_extract(cube, depth = 0), "surface cube")
})

test_that("POSIXct time class and timezone survive tabulation", {
  cube <- extract_baseline_cube()
  cube$time <- as.POSIXct(cube$time, tz = "UTC")
  attr(cube$time, "tzone") <- "UTC"
  cube$temporal_extent <- range(cube$time)
  result <- cube_extract(
    cube,
    time = cube$time[c(4L, 1L)],
    keep_index = TRUE
  )

  expect_s3_class(result$time, "POSIXct")
  expect_identical(attr(result$time, "tzone"), "UTC")
  expect_identical(unique(result$time_index), c(4L, 1L))
})

test_that("index extraction preserves undecoded temporal representation", {
  cube <- extract_baseline_cube()
  class(cube$time) <- c("oceancube_test_time", "Date")
  result <- cube_extract(cube, time = c(4L, 1L), by = "index")
  expect_true(inherits(result$time, "oceancube_test_time"))
  expect_identical(as.numeric(unique(result$time)), as.numeric(cube$time[c(4L, 1L)]))
})

test_that("unit resolution handles NULL, unnamed vectors, and lists", {
  cube <- extract_baseline_cube()
  cube$units <- NULL
  expect_true(all(is.na(cube_extract(cube, variable = "oxygen")$unit)))

  cube$units <- c("degC", "mmol m-3")
  expect_identical(
    unique(cube_extract(cube, variable = c("oxygen", "temperature"))$unit),
    c("mmol m-3", "degC")
  )

  cube$units <- list(temperature = "degC", oxygen = "mmol m-3")
  expect_identical(
    unique(cube_extract(cube, variable = "oxygen")$unit),
    "mmol m-3"
  )
})

test_that("lightweight attributes describe selection, size, and provenance", {
  result <- cube_extract(
    extract_baseline_cube(),
    longitude = c(-80, -78),
    latitude = -11,
    depth = 50,
    time = as.Date("2021-02-01"),
    variable = "oxygen"
  )
  selection <- attr(result, "oceancube_selection")
  provenance <- attr(result, "oceancube_provenance")

  expect_identical(attr(result, "oceancube_backend"), "memory")
  expect_identical(attr(result, "oceancube_shape"), c(
    longitude = 2L, latitude = 1L, depth = 1L, time = 1L, variable = 1L
  ))
  expect_identical(selection$requested$longitude, c(-80, -78))
  expect_identical(selection$indices$longitude, c(1L, 3L))
  expect_identical(selection$selected$longitude, c(-80, -78))
  expect_identical(provenance$operation, "cube_extract")
  expect_identical(provenance$mode, "point")
  expect_identical(provenance$format, "long")
  expect_identical(provenance$rows_expected_long, 2)
  expect_identical(provenance$rows_expected_wide, 2)
  expect_identical(provenance$approximate_array_bytes, 16)
  expect_identical(provenance$rows_returned, 2L)
  expect_identical(provenance$source, "synthetic")
  expect_identical(provenance$dataset_id, "extract-001")
  expect_null(provenance$netcdf_read)
})

test_that("extraction without selectors returns the complete long table", {
  cube <- extract_baseline_cube()
  result <- cube_extract(cube)
  expect_identical(nrow(result), as.integer(prod(.cube_shape(cube))))
  expect_identical(result$value, as.vector(.cube_read(cube)))
  expect_identical(attr(result, "oceancube_shape"), .cube_shape(cube))
})

test_that("mode, format, flags, cubes, and backends validate informatively", {
  cube <- extract_baseline_cube()
  expect_error(cube_extract(cube, mode = "bad"), "arg")
  expect_error(cube_extract(cube, format = "bad"), "arg")
  expect_error(cube_extract(cube, keep_index = NA), "keep_index")
  expect_error(cube_extract(cube, keep_distance = 1), "keep_distance")
  expect_error(cube_extract(list()), "ocean_cube")

  local_mocked_bindings(
    .cube_backend = function(x) "future-store",
    .package = "oceancube"
  )
  expect_error(
    cube_extract(cube),
    "Unsupported ocean_cube backend: 'future-store'",
    class = "oceancube_unsupported_backend"
  )
})

test_that("resolution and mode validation occur before any cube read", {
  cube <- extract_baseline_cube()
  reads <- 0L
  local_mocked_bindings(
    .cube_read = function(...) {
      reads <<- reads + 1L
      stop("unexpected read")
    },
    .package = "oceancube"
  )

  expect_error(cube_extract(cube, longitude = -79.5), "Exact longitude")
  expect_error(
    cube_extract(
      cube,
      longitude = c(-80, -79),
      latitude = -11,
      time = as.Date("2020-01-01"),
      mode = "profile"
    ),
    "profile"
  )
  expect_identical(reads, 0L)
})

test_that("a valid extraction performs exactly one cube read", {
  cube <- extract_baseline_cube()
  reads <- 0L
  original <- .cube_read
  local_mocked_bindings(
    .cube_read = function(...) {
      reads <<- reads + 1L
      original(...)
    },
    .package = "oceancube"
  )
  result <- cube_extract(
    cube,
    longitude = c(-80, -78),
    variable = "oxygen"
  )
  expect_identical(reads, 1L)
  expect_identical(nrow(result), 32L)
})

test_that("memory and NetCDF results are equivalent across modes and formats", {
  fixture <- extract_netcdf_fixture()
  withr::local_file(fixture$file)
  memory <- extract_equivalent_memory(fixture$cube)
  cases <- list(
    list(
      longitude = -79, latitude = -11, depth = 50,
      time = fixture$cube$time[[4L]], variable = "oxygen", mode = "point"
    ),
    list(
      longitude = -79, latitude = -11, time = fixture$cube$time[[4L]],
      variable = c("temperature", "oxygen"), mode = "profile"
    ),
    list(
      longitude = -79, latitude = -11, depth = 0,
      variable = "temperature", mode = "series"
    ),
    list(
      longitude = c(-78, -80), latitude = c(-11, -12), depth = 50,
      time = fixture$cube$time[c(4L, 1L)],
      variable = c("oxygen", "temperature"), mode = "table"
    )
  )
  for (case in cases) {
    memory_result <- do.call(cube_extract, c(list(x = memory), case))
    netcdf_result <- do.call(cube_extract, c(list(x = fixture$cube), case))
    expect_identical(extract_data_only(memory_result), extract_data_only(netcdf_result))
    expect_identical(attr(memory_result, "units"), attr(netcdf_result, "units"))
    expect_identical(
      attr(memory_result, "oceancube_shape"),
      attr(netcdf_result, "oceancube_shape")
    )
  }

  memory_wide <- cube_extract(
    memory,
    longitude = -79,
    latitude = -11,
    depth = 0,
    format = "wide"
  )
  netcdf_wide <- cube_extract(
    fixture$cube,
    longitude = -79,
    latitude = -11,
    depth = 0,
    format = "wide"
  )
  expect_identical(extract_data_only(memory_wide), extract_data_only(netcdf_wide))
})

test_that("memory and NetCDF nearest diagnostics are equivalent", {
  fixture <- extract_netcdf_fixture()
  withr::local_file(fixture$file)
  memory <- extract_equivalent_memory(fixture$cube)
  args <- list(
    longitude = -79.4,
    latitude = -11.2,
    depth = 40,
    time = fixture$cube$time[[3L]] + 9 * 86400,
    variable = "temperature",
    match = "nearest",
    tolerance = list(
      longitude = 0.5,
      latitude = 0.5,
      depth = 15,
      time = as.difftime(15, units = "days")
    ),
    keep_index = TRUE,
    keep_distance = TRUE
  )
  memory_result <- do.call(cube_extract, c(list(x = memory), args))
  netcdf_result <- do.call(cube_extract, c(list(x = fixture$cube), args))
  expect_identical(extract_data_only(memory_result), extract_data_only(netcdf_result))
})

test_that("NetCDF point reads one variable and one logical cell", {
  fixture <- extract_netcdf_fixture()
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
  result <- cube_extract(
    fixture$cube,
    longitude = -79,
    latitude = -11,
    depth = 50,
    time = fixture$cube$time[[4L]],
    variable = "oxygen"
  )
  record <- attr(result, "oceancube_provenance")$netcdf_read

  expect_identical(nrow(result), 1L)
  expect_identical(result$value, 24222)
  expect_identical(openings, 1L)
  expect_length(observed, 1L)
  expect_identical(observed[[1L]]$variable, "oxygen")
  expect_identical(
    observed[[1L]]$start,
    c(time = 4L, depth = 2L, latitude = 2L, longitude = 2L)
  )
  expect_identical(
    observed[[1L]]$count,
    c(time = 1L, depth = 1L, latitude = 1L, longitude = 1L)
  )
  expect_identical(record$physical_count, c(
    longitude = 1L, latitude = 1L, depth = 1L, time = 1L
  ))
  expect_identical(record$variables, "oxygen")
  expect_identical(record$values_requested, 1)
  expect_identical(record$values_in_envelope, 1)
})

test_that("NetCDF profile and series read only their logical shapes", {
  fixture <- extract_netcdf_fixture()
  withr::local_file(fixture$file)
  profile <- cube_extract(
    fixture$cube,
    longitude = -79,
    latitude = -11,
    time = fixture$cube$time[[4L]],
    variable = c("temperature", "oxygen"),
    mode = "profile"
  )
  series <- cube_extract(
    fixture$cube,
    longitude = -79,
    latitude = -11,
    depth = 0,
    variable = "temperature",
    mode = "series"
  )
  expect_identical(
    attr(profile, "oceancube_shape"),
    c(longitude = 1L, latitude = 1L, depth = 2L, time = 1L, variable = 2L)
  )
  expect_identical(nrow(profile), 4L)
  expect_identical(
    attr(series, "oceancube_shape"),
    c(longitude = 1L, latitude = 1L, depth = 1L, time = 4L, variable = 1L)
  )
  expect_identical(nrow(series), 4L)
})

test_that("NetCDF source remains unchanged and result is file independent", {
  fixture <- extract_netcdf_fixture()
  before_cube <- fixture$cube
  before_file <- file.info(fixture$file)
  result <- cube_extract(
    fixture$cube,
    longitude = -79,
    latitude = -11,
    depth = 50,
    variable = "oxygen"
  )
  after_file <- file.info(fixture$file)
  expected <- extract_data_only(result)

  expect_identical(fixture$cube, before_cube)
  expect_identical(as.double(after_file$size), as.double(before_file$size))
  expect_identical(as.numeric(after_file$mtime), as.numeric(before_file$mtime))
  expect_identical(unlink(fixture$file), 0L)
  expect_identical(extract_data_only(result), expected)
  expect_no_error(summary(result))
  expect_error(
    .cube_read(fixture$cube),
    "no longer exists",
    class = "oceancube_netcdf_changed_file"
  )
})

test_that("cube_extract contains no direct cube data access", {
  contains_direct_access <- function(expr) {
    if (!is.call(expr) && !is.expression(expr) && !is.pairlist(expr)) {
      return(FALSE)
    }
    if (is.call(expr)) {
      call_name <- if (is.symbol(expr[[1L]])) as.character(expr[[1L]]) else ""
      if (call_name %in% c("$", "[[") &&
          length(expr) >= 3L &&
          identical(expr[[2L]], as.name("x")) &&
          identical(as.character(expr[[3L]]), "data")) {
        return(TRUE)
      }
    }
    any(vapply(as.list(expr), contains_direct_access, logical(1)))
  }
  extract_functions <- list(
    cube_extract,
    .cube_extract_long,
    .cube_extract_wide,
    .extract_selected_coordinates
  )
  expect_false(any(vapply(
    extract_functions,
    function(fun) contains_direct_access(body(fun)),
    logical(1)
  )))
})
