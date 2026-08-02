test_that("link_events preserves its public structure and deterministic baseline", {
  fixture <- .make_baseline_fixture()
  cube <- fixture$cube
  events <- data.frame(
    event_id = c("inside", "edge", "outside"),
    lon = c(-80, -78, -70),
    lat = c(-12, -11, 0),
    date = as.Date(c("2020-01-01", "2021-02-01", "2020-01-01")),
    row.names = c("event-a", "event-b", "event-c")
  )
  cube_before <- cube
  events_before <- events

  result <- link_events(cube, events, vars = "temperature")

  expect_identical(class(result), class(events))
  expect_identical(nrow(result), 3L)
  expect_identical(rownames(result), rownames(events))
  expect_identical(result$event_id, events$event_id)
  expect_identical(result[names(events)], events)
  expect_identical(
    names(result),
    c(
      names(events),
      "temperature_value",
      ".oceancube_lon",
      ".oceancube_lat",
      ".oceancube_depth",
      ".oceancube_date",
      ".oceancube_dt_days"
    )
  )
  expect_identical(result$temperature_value, c(11111, 14123, 11123))
  expect_identical(result$.oceancube_lon, c(-80, -78, -78))
  expect_identical(result$.oceancube_lat, c(-12, -11, -11))
  expect_identical(result$.oceancube_depth, c(0, 0, 0))
  expect_identical(
    result$.oceancube_date,
    as.Date(c("2020-01-01", "2021-02-01", "2020-01-01"))
  )
  expect_identical(result$.oceancube_dt_days, c(0L, 0L, 0L))
  expect_type(result$temperature_value, "double")
  expect_s3_class(result$.oceancube_date, "Date")
  expect_type(result$.oceancube_dt_days, "integer")
  expect_identical(cube, cube_before)
  expect_identical(events, events_before)
})

test_that("link_events keeps its signature and baseline input validation", {
  cube <- .make_baseline_fixture()$cube
  event <- data.frame(
    lon = -80,
    lat = -12,
    date = as.Date("2020-01-01")
  )

  expect_identical(
    names(formals(link_events)),
    c(
      "x", "events", "lon_col", "lat_col", "date_col", "depth_col",
      "vars", "prefix", "time_tolerance", "keep_grid"
    )
  )
  expect_identical(.cube_backend(cube), "memory")
  expect_error(link_events(list(), event), "must be an <ocean_cube>")
  expect_error(link_events(cube, as.matrix(event)), "must be a data frame")
  expect_error(
    link_events(cube, event[c("lat", "date")]),
    "Missing columns.*lon"
  )
  expect_error(
    link_events(cube, event, depth_col = "sample_depth"),
    "Missing depth column"
  )

  character_event <- data.frame(
    x = "-80",
    y = "-12",
    when = "2020-01-01"
  )
  character_result <- link_events(
    cube,
    character_event,
    lon_col = "x",
    lat_col = "y",
    date_col = "when",
    vars = "temperature"
  )
  expect_identical(character_result$temperature_value, 11111)
})

test_that("spatial localization remains per-axis nearest neighbour", {
  fixture <- .make_baseline_fixture()
  cube <- fixture$cube
  events <- data.frame(
    case = c("exact", "interior", "edge", "outside", "far", "tie"),
    lon = c(-80, -79.4, -78, -70, 150, -79.5),
    lat = c(-12, -11.4, -11, 0, 80, -11.5),
    date = rep(as.Date("2020-01-01"), 6)
  )

  result <- link_events(cube, events, vars = "temperature")

  expect_identical(result$case, events$case)
  expect_identical(result$.oceancube_lon, c(-80, -79, -78, -78, -78, -80))
  expect_identical(result$.oceancube_lat, c(-12, -11, -11, -11, -11, -12))
  expect_identical(
    result$temperature_value,
    c(11111, 11122, 11123, 11123, 11123, 11111)
  )

  descending <- ocean_cube(
    lon = c(-78, -79, -80),
    lat = c(-11, -12),
    depth = 0,
    time = as.Date("2020-01-01"),
    vars = "v",
    data = array(c(1, 2, 3, 4, 5, 6), dim = c(3, 2, 1, 1, 1))
  )
  descending_result <- link_events(
    descending,
    data.frame(
      lon = c(-79.4, -79.5),
      lat = c(-11.6, -11.5),
      date = as.Date(c("2020-01-01", "2020-01-01"))
    ),
    vars = "v"
  )

  expect_identical(descending_result$.oceancube_lon, c(-79, -79))
  expect_identical(descending_result$.oceancube_lat, c(-12, -11))
  expect_identical(descending_result$v_value, c(5, 2))
})

test_that("temporal localization keeps nearest dates, ties, and day tolerance", {
  cube <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = 0,
    time = as.Date(c("2020-01-01", "2020-01-03", "2020-01-06")),
    vars = "v",
    data = array(c(10, 30, 60), dim = c(1, 1, 1, 3, 1))
  )
  dates <- data.frame(
    case = c("exact", "tie", "before", "after", "within", "outside", "missing"),
    lon = -80,
    lat = -12,
    date = as.Date(c(
      "2020-01-03", "2020-01-02", "2019-12-31", "2020-01-08",
      "2020-01-05", "2020-01-09", NA
    ))
  )

  exact_only <- link_events(cube, dates, vars = "v", time_tolerance = 0L)
  tolerant <- link_events(cube, dates, vars = "v", time_tolerance = 2L)

  expect_identical(exact_only$v_value, c(30, NA, NA, NA, NA, NA, NA))
  expect_identical(
    tolerant$v_value,
    c(30, 10, 10, 60, 60, NA, NA)
  )
  expect_identical(
    tolerant$.oceancube_date,
    as.Date(c(
      "2020-01-03", "2020-01-01", "2020-01-01", "2020-01-06",
      "2020-01-06", NA, NA
    ))
  )
  expect_identical(tolerant$.oceancube_dt_days, c(0L, 1L, 1L, 2L, 1L, NA, NA))

  posix_event <- data.frame(
    lon = -80,
    lat = -12,
    date = as.POSIXct("2020-01-03 12:00:00", tz = "UTC")
  )
  expect_identical(
    link_events(cube, posix_event, vars = "v")$v_value,
    30
  )
})

test_that("unusual tolerance inputs retain their baseline coercion behaviour", {
  cube <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = 0,
    time = as.Date(c("2020-01-01", "2020-01-04")),
    vars = "v",
    data = array(c(10, 40), dim = c(1, 1, 1, 2, 1))
  )
  event <- data.frame(
    lon = -80,
    lat = -12,
    date = as.Date("2020-01-02")
  )

  expect_error(link_events(cube, event, time_tolerance = NA))
  expect_true(is.na(link_events(cube, event, time_tolerance = -1)$v_value))
  expect_identical(
    link_events(cube, event, time_tolerance = 1.9)$v_value,
    10
  )
  # Preserve the historical first-element coercion under strict R logic checks.
  expect_warning(
    length_two <- link_events(cube, event, time_tolerance = c(0, 1)),
    "coercion to 'logical\\(1\\)'"
  )
  expect_true(is.na(length_two$v_value))
})

test_that("depth localization preserves first-level and nearest policies", {
  fixture <- .make_baseline_fixture()
  cube <- fixture$cube
  events <- data.frame(
    case = c("exact", "nearest", "tie", "outside", "missing"),
    lon = -80,
    lat = -12,
    date = rep(as.Date("2020-01-01"), 5),
    sample_depth = c(0, 40, 25, 500, NA)
  )

  with_depth <- link_events(
    cube,
    events,
    depth_col = "sample_depth",
    vars = "temperature"
  )
  without_depth <- link_events(cube, events, vars = "temperature")

  expect_identical(with_depth$.oceancube_depth, c(0, 50, 0, 50, NA))
  expect_identical(
    with_depth$temperature_value,
    c(11111, 11211, 11111, 11211, NA)
  )
  expect_identical(without_depth$.oceancube_depth, rep(0, 5))
  expect_identical(without_depth$temperature_value, rep(11111, 5))

  descending <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = c(100, 50, 0),
    time = as.Date("2020-01-01"),
    vars = "v",
    data = array(c(100, 50, 0), dim = c(1, 1, 3, 1, 1))
  )
  descending_result <- link_events(
    descending,
    data.frame(
      lon = -80,
      lat = -12,
      date = as.Date("2020-01-01"),
      z = 25
    ),
    depth_col = "z",
    vars = "v"
  )
  expect_identical(descending_result$.oceancube_depth, 50)
  expect_identical(descending_result$v_value, 50)

  surface <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = NA_real_,
    time = as.Date("2020-01-01"),
    vars = "v",
    data = array(7, dim = c(1, 1, 1, 1, 1))
  )
  surface_event <- data.frame(
    lon = -80,
    lat = -12,
    date = as.Date("2020-01-01"),
    z = 0
  )
  expect_identical(
    link_events(surface, surface_event, vars = "v")$v_value,
    7
  )
  expect_error(
    link_events(surface, surface_event, depth_col = "z", vars = "v"),
    "no valid depth coordinate"
  )
})

test_that("variable selection, names, prefixes, and collisions remain stable", {
  fixture <- .make_baseline_fixture()
  cube <- fixture$cube
  event <- data.frame(
    lon = -80,
    lat = -12,
    date = as.Date("2020-01-01"),
    temperature_value = -1
  )
  plain_event <- event[c("lon", "lat", "date")]

  all_variables <- link_events(cube, plain_event)
  reordered <- link_events(cube, plain_event, vars = c("oxygen", "temperature"))
  duplicated <- link_events(cube, plain_event, vars = c("oxygen", "oxygen"))
  prefixed <- link_events(
    cube,
    plain_event,
    vars = c("temperature", "oxygen"),
    prefix = "raw"
  )
  empty <- link_events(cube, plain_event, vars = character())
  collision <- link_events(cube, event, vars = "temperature")

  expect_identical(all_variables$temperature_value, 11111)
  expect_identical(all_variables$oxygen_value, 21111)
  expect_identical(
    names(reordered)[4:5],
    c("oxygen_value", "temperature_value")
  )
  expect_identical(reordered$oxygen_value, 21111)
  expect_identical(reordered$temperature_value, 11111)
  expect_identical(sum(names(duplicated) == "oxygen_value"), 1L)
  expect_identical(duplicated$oxygen_value, 21111)
  expect_true(all(c("raw_temperature", "raw_oxygen") %in% names(prefixed)))
  expect_false(any(c("temperature_value", "oxygen_value") %in% names(empty)))
  expect_identical(sum(names(collision) == "temperature_value"), 1L)
  expect_identical(collision$temperature_value, 11111)
  expect_identical(names(collision)[4], "temperature_value")
  expect_error(link_events(cube, event, vars = "salinity"), "Variables not found")

  special <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = 0,
    time = as.Date("2020-01-01"),
    vars = c("sea temp", "oxygen%"),
    data = array(c(8, 9), dim = c(1, 1, 1, 1, 2))
  )
  special_result <- link_events(
    special,
    event[1:3],
    vars = c("oxygen%", "sea temp")
  )
  expect_identical(special_result[["oxygen%_value"]], 9)
  expect_identical(special_result[["sea temp_value"]], 8)
})

test_that("custom columns and keep_grid retain the naming contract", {
  cube <- .make_baseline_fixture()$cube
  events <- data.frame(
    id = c("b", "a"),
    longitude_event = c(-78, -80),
    latitude_event = c(-11, -12),
    when = as.Date(c("2021-02-01", "2020-01-01")),
    z = c(50, 0)
  )

  with_grid <- link_events(
    cube,
    events,
    lon_col = "longitude_event",
    lat_col = "latitude_event",
    date_col = "when",
    depth_col = "z",
    vars = "oxygen",
    prefix = "obs",
    keep_grid = TRUE
  )
  without_grid <- link_events(
    cube,
    events,
    lon_col = "longitude_event",
    lat_col = "latitude_event",
    date_col = "when",
    depth_col = "z",
    vars = "oxygen",
    prefix = "obs",
    keep_grid = FALSE
  )

  expect_identical(with_grid$id, c("b", "a"))
  expect_identical(with_grid$obs_oxygen, c(24223, 21111))
  expect_identical(
    names(with_grid)[(ncol(with_grid) - 4L):ncol(with_grid)],
    c(
      ".oceancube_lon", ".oceancube_lat", ".oceancube_depth",
      ".oceancube_date", ".oceancube_dt_days"
    )
  )
  expect_identical(names(without_grid), c(names(events), "obs_oxygen"))
  expect_identical(without_grid$obs_oxygen, with_grid$obs_oxygen)
})

test_that("missing event fields and missing cube values remain row-local", {
  cube <- .make_baseline_fixture()$cube
  cube$data[1, 1, 1, 1, 1] <- NA_real_
  cube$data[, , , , 2] <- NA_real_
  events <- data.frame(
    case = c("cube-na", "lon-na", "lat-na", "date-na", "depth-na"),
    lon = c(-80, NA, -80, -80, -80),
    lat = c(-12, -12, NA, -12, -12),
    date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01", NA, "2020-01-01")),
    z = c(0, 0, 0, 0, NA)
  )

  result <- link_events(
    cube,
    events,
    depth_col = "z",
    vars = c("temperature", "oxygen")
  )

  expect_true(all(is.na(result$temperature_value)))
  expect_true(all(is.na(result$oxygen_value)))
  expect_true(is.na(result$.oceancube_lon[2]))
  expect_true(is.na(result$.oceancube_lat[3]))
  expect_true(is.na(result$.oceancube_date[4]))
  expect_true(is.na(result$.oceancube_depth[5]))
  expect_identical(result$.oceancube_dt_days, c(0L, 0L, 0L, NA, 0L))
})

test_that("zero-row inputs return typed empty value and diagnostic columns", {
  cube <- .make_baseline_fixture()$cube
  events <- data.frame(
    id = integer(),
    lon = numeric(),
    lat = numeric(),
    date = as.Date(character())
  )

  result <- link_events(cube, events, vars = c("temperature", "oxygen"))

  expect_identical(nrow(result), 0L)
  expect_identical(names(result)[1:4], names(events))
  expect_type(result$temperature_value, "double")
  expect_type(result$oxygen_value, "double")
  expect_s3_class(result$.oceancube_date, "Date")
  expect_type(result$.oceancube_dt_days, "integer")
})

test_that("link_events reads one five-dimensional point per valid event", {
  cube <- .make_baseline_fixture()$cube
  events <- data.frame(
    event_id = c("first", "invalid", "third"),
    lon = c(-80, NA, -78),
    lat = c(-12, -12, -11),
    date = as.Date(c("2020-01-01", "2020-01-01", "2021-02-01"))
  )
  memory_read <- .cube_read
  calls <- list()
  returned_dimensions <- list()

  local_mocked_bindings(
    .cube_read = function(x, index = NULL, drop = FALSE) {
      value <- memory_read(x, index = index, drop = drop)
      calls[[length(calls) + 1L]] <<- index
      returned_dimensions[[length(returned_dimensions) + 1L]] <<- unname(dim(value))
      value
    },
    .package = "oceancube"
  )

  result <- link_events(
    cube,
    events,
    vars = c("oxygen", "temperature")
  )

  expect_identical(length(calls), 2L)
  expect_identical(
    calls,
    list(
      list(
        longitude = 1L,
        latitude = 1L,
        depth = 1L,
        time = 1L,
        variable = c(2L, 1L)
      ),
      list(
        longitude = 3L,
        latitude = 2L,
        depth = 1L,
        time = 4L,
        variable = c(2L, 1L)
      )
    )
  )
  expect_identical(
    returned_dimensions,
    list(c(1L, 1L, 1L, 1L, 2L), c(1L, 1L, 1L, 1L, 2L))
  )
  expect_identical(result$oxygen_value, c(21111, NA, 24123))
  expect_identical(result$temperature_value, c(11111, NA, 14123))
})

test_that("link_events keeps memory and NetCDF daily results identical", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  storage <- .new_netcdf_storage(file, c("temperature", "oxygen"))
  netcdf_cube <- .new_netcdf_cube(storage)
  memory_cube <- ocean_cube(
    lon = netcdf_cube$lon,
    lat = netcdf_cube$lat,
    depth = netcdf_cube$depth,
    time = as.Date(netcdf_cube$time),
    vars = netcdf_cube$vars,
    units = netcdf_cube$units,
    data = .cube_read(netcdf_cube)
  )
  events <- data.frame(
    id = c("inside", "edge", "outside", "nearest", "missing", "duplicate"),
    lon = c(-80, -78, -70, -79, -80, -80),
    lat = c(-12, -11, 0, -11, NA, -12),
    date = as.Date(c(
      "2000-01-01", "2000-01-04", "2000-01-01", "2000-01-05",
      "2000-01-01", "2000-01-01"
    )),
    row.names = paste0("event-", 1:6)
  )
  events_before <- events
  netcdf_before <- netcdf_cube
  file_before <- list(
    size = file.info(file)$size,
    mtime = file.info(file)$mtime,
    md5 = unname(tools::md5sum(file))
  )
  connection_count <- 0L
  netcdf_connection <- .with_netcdf_connection

  local_mocked_bindings(
    .with_netcdf_connection = function(file, code) {
      connection_count <<- connection_count + 1L
      netcdf_connection(file, code)
    },
    .package = "oceancube"
  )

  memory_result <- link_events(
    memory_cube,
    events,
    vars = c("oxygen", "temperature"),
    prefix = "cube_",
    time_tolerance = 1L
  )
  netcdf_result <- link_events(
    netcdf_cube,
    events,
    vars = c("oxygen", "temperature"),
    prefix = "cube_",
    time_tolerance = 1L
  )
  file_after <- list(
    size = file.info(file)$size,
    mtime = file.info(file)$mtime,
    md5 = unname(tools::md5sum(file))
  )
  reopened <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(reopened), add = TRUE)

  # Protect daily Date normalization in the lazy installed backend.
  expect_true(isTRUE(all.equal(
    netcdf_result,
    memory_result,
    check.attributes = TRUE
  )))
  expect_identical(
    netcdf_result$cube__temperature,
    c(11111, 14123, 11123, 14122, NA, 11111)
  )
  expect_s3_class(netcdf_result$.oceancube_date, "Date")
  expect_identical(rownames(netcdf_result), rownames(events))
  expect_identical(connection_count, 5L)
  expect_identical(events, events_before)
  expect_identical(netcdf_cube, netcdf_before)
  expect_identical(file_after, file_before)
  expect_s3_class(reopened, "ncdf4")
})

test_that("link_events cannot bypass an unsupported backend", {
  cube <- .make_baseline_fixture()$cube
  event <- data.frame(
    lon = -80,
    lat = -12,
    date = as.Date("2020-01-01")
  )

  local_mocked_bindings(
    .cube_backend = function(x) "unknown",
    .package = "oceancube"
  )

  expect_error(
    link_events(cube, event, vars = "temperature"),
    "Unsupported ocean_cube backend: 'unknown'"
  )
})

test_that("link_events implementation contains no direct data access", {
  # Inspect the installed function body because package source files are absent.
  expect_false(.contains_direct_cube_data_access(body(link_events)))
})
