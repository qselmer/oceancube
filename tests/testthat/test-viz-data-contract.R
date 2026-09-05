test_that("oceancube_viz_data v1 is internal, bounded, and valid", {
  cube <- viz_data_test_cube()
  path <- viz_data_test_path()
  prepared <- list(
    .viz_prepare_map(cube, "temperature", cube$time[[1L]], 0),
    .viz_prepare_profile(cube, "temperature", -79, -11, cube$time[[1L]]),
    .viz_prepare_section(cube, "temperature", time = cube$time[[1L]],
                         latitude = -11),
    .viz_prepare_transect(cube, path, "temperature", time = cube$time[[1L]],
                          mode = "section"),
    .viz_prepare_transect(cube, path, "temperature", time = cube$time[[1L]],
                          depth = 0, mode = "horizontal"),
    .viz_prepare_timeseries(cube, "temperature", -79, -11, 0)
  )

  expect_identical(
    vapply(prepared, `[[`, character(1), "kind"),
    c("MAP_LAYER", "PROFILE", "SECTION", "TRANSECT_SECTION",
      "TRANSECT_LINE", "TIMESERIES")
  )
  expect_true(all(vapply(prepared, inherits, logical(1), "oceancube_viz_data")))
  expect_true(all(vapply(prepared, function(x) {
    identical(x$schema_name, "oceancube_viz_data") &&
      identical(x$schema_version, "1.0.0") &&
      isTRUE(.validate_oceancube_viz_data(x))
  }, logical(1))))
  expect_false("oceancube_viz_data" %in% getNamespaceExports("oceancube"))
  expect_identical(length(getNamespaceExports("oceancube")), 48L)
})

test_that("validator rejects malformed schema components deterministically", {
  cube <- viz_data_test_cube()
  prepared <- .viz_prepare_profile(
    cube, "temperature", -79, -11, cube$time[[1L]]
  )
  malformed <- list(
    missing_key = within(prepared, rm(geometry)),
    version = within(prepared, schema_version <- "2.0.0"),
    kind = within(prepared, kind <- "HOVMOLLER"),
    data = within(prepared, data <- list(value = 1)),
    role = within(prepared, roles$x <- "absent"),
    coordinate = within(prepared, coordinates$depth$n <- 999L),
    depth = within(prepared, depth$scientific_positive <- "up"),
    time = within(prepared, time$class <- NA_character_),
    source = within(prepared, source_semantics$rendered_from <- "GUESSED"),
    scale = within(prepared, scale$classification <- "DIVERGING"),
    projection = within(prepared, projection$status <- "EPSG_GUESSED"),
    provenance = within(prepared, provenance <- "invalid")
  )
  for (item in malformed) {
    expect_error(
      .validate_oceancube_viz_data(item),
      class = "oceancube_viz_data_error"
    )
  }
})

test_that("prepared state round-trips without live renderer or backend state", {
  cube <- viz_data_test_cube()
  prepared <- .viz_prepare_section(
    cube, "temperature", time = cube$time[[1L]], latitude = -11
  )
  restored <- unserialize(serialize(prepared, NULL))
  file <- tempfile(fileext = ".rds")
  withr::local_file(file)
  saveRDS(prepared, file)
  restored_file <- readRDS(file)

  expect_identical(restored, prepared)
  expect_identical(restored_file, prepared)
  expect_true(.validate_oceancube_viz_data(restored))
  expect_identical(viz_plot_semantics(.viz_render_ggplot(restored)),
                   viz_plot_semantics(.viz_render_ggplot(prepared)))
  expect_false(any(vapply(prepared, inherits, logical(1), "ggplot")))
  expect_false(any(vapply(prepared, is.environment, logical(1))))
})

test_that("source, scale, projection, roles, provenance, and QA are explicit", {
  cube <- viz_data_test_cube()
  prepared <- .viz_prepare_map(cube, "temperature", cube$time[[1L]], 0)

  expect_true(is.na(prepared$source_semantics$rendered_from))
  expect_identical(prepared$source_semantics$classification_status, "UNRESOLVED")
  expect_identical(prepared$scale$classification, "UNSPECIFIED_CONTINUOUS")
  expect_identical(prepared$projection$status, "UNKNOWN")
  expect_identical(prepared$roles$x, "longitude")
  expect_identical(prepared$roles$y, "latitude")
  expect_identical(prepared$roles$value, "value")
  expect_true(is.list(prepared$provenance) || is.null(prepared$provenance))
  expect_true(is.list(prepared$qa) || is.null(prepared$qa))
})

test_that("renderer hints never mutate prepared science, provenance, or QA", {
  cube <- viz_data_test_cube()
  path <- viz_data_test_path()
  plain <- .viz_prepare_profile(
    cube, "temperature", -79, -11, cube$time[[1L]],
    reverse_depth = FALSE, points = FALSE
  )
  styled <- .viz_prepare_profile(
    cube, "temperature", -79, -11, cube$time[[1L]],
    limits = c(20, 180), reverse_depth = TRUE, points = TRUE,
    title = "Title", subtitle = "Subtitle", caption = "Caption"
  )
  line_plain <- .viz_prepare_transect(
    cube, path, "temperature", time = cube$time[[1L]], depth = 0,
    mode = "horizontal", points = FALSE
  )
  line_points <- .viz_prepare_transect(
    cube, path, "temperature", time = cube$time[[1L]], depth = 0,
    mode = "horizontal", points = TRUE
  )

  expect_identical(styled$data, plain$data)
  expect_identical(styled$provenance, plain$provenance)
  expect_identical(styled$qa, plain$qa)
  expect_identical(styled$depth$values, plain$depth$values)
  expect_false(plain$depth$display_reverse)
  expect_true(styled$depth$display_reverse)
  expect_identical(line_plain$data, line_points$data)
  expect_length(.viz_render_ggplot(line_plain)$layers, 1L)
  expect_length(.viz_render_ggplot(line_points)$layers, 2L)
})

test_that("coastline context and display limits preserve map field values", {
  cube <- viz_data_test_cube()
  coastline <- data.frame(
    longitude = c(-80, -77), latitude = c(-12, -12), group = c(1, 1)
  )
  plain <- .viz_prepare_map(cube, "temperature", cube$time[[1L]], 0)
  contextual <- .viz_prepare_map(
    cube, "temperature", cube$time[[1L]], 0,
    limits = c(10, 100), coastline = coastline
  )

  expect_identical(contextual$data, plain$data)
  expect_identical(contextual$provenance, plain$provenance)
  expect_identical(contextual$qa, plain$qa)
  expect_length(.viz_render_ggplot(contextual)$layers, 2L)
})

test_that("prepared objects disclose no private local paths", {
  prepared <- .viz_prepare_map(
    viz_data_test_cube(), "temperature", as.Date("2020-01-01"), 0
  )
  rendered <- paste(capture.output(str(prepared)), collapse = "\n")
  expect_false(grepl("[A-Za-z]:[/\\\\]", rendered))
  expect_false(grepl(Sys.info()[["nodename"]], rendered, fixed = TRUE))
})

test_that("NetCDF preparation is bounded and rendering needs no live source", {
  file <- make_netcdf_backend_fixture()
  cube <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )
  prepared <- .viz_prepare_map(
    cube, "temperature", cube$time[[1L]], cube$depth[[1L]]
  )
  read_evidence <- prepared$qa$extraction$netcdf_read
  expect_identical(read_evidence$physical_count,
                   c(longitude = 3L, latitude = 2L, depth = 1L, time = 1L))
  serialized_state <- paste(capture.output(str(prepared)), collapse = "\n")
  expect_false(grepl("[A-Za-z]:[/\\\\]", serialized_state))
  expect_false(grepl(normalizePath(tempdir(), winslash = "/"),
                     serialized_state, fixed = TRUE))
  expect_identical(unlink(file), 0L)
  expect_s3_class(.viz_render_ggplot(prepared), "ggplot")
})

test_that("memory and NetCDF prepared states agree after equivalent selection", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  netcdf <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )
  memory <- cube_collect(netcdf)
  netcdf_prepared <- .viz_prepare_map(
    netcdf, "temperature", netcdf$time[[1L]], netcdf$depth[[1L]]
  )
  memory_prepared <- .viz_prepare_map(
    memory, "temperature", memory$time[[1L]], memory$depth[[1L]]
  )

  expect_identical(netcdf_prepared$data, memory_prepared$data)
  expect_identical(netcdf_prepared$roles, memory_prepared$roles)
  expect_identical(netcdf_prepared$geometry, memory_prepared$geometry)
  expect_identical(netcdf_prepared$scale, memory_prepared$scale)
  expect_identical(netcdf_prepared$source_semantics,
                   memory_prepared$source_semantics)
  expect_identical(netcdf_prepared$variables, memory_prepared$variables)
})
