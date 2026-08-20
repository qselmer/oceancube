a2_validation_contract <- function(report) {
  report[order(report$check), c(
    "check", "status", "severity", "component", "repairable",
    "suggested_action"
  )]
}

test_that("cube_validate has exact common memory and NetCDF contract parity", {
  skip_if_not_installed("ncdf4")
  pair <- a2_make_netcdf_pair()
  withr::local_file(pair$file)

  mutators <- list(
    valid = identity,
    longitude_out_of_bounds = function(x) {
      x$lon[[1L]] <- 400
      x
    },
    duplicate_time = function(x) {
      x$time[[2L]] <- x$time[[1L]]
      x$temporal_extent <- range(x$time)
      x
    },
    variable_shape_mismatch = function(x) {
      x$vars <- x$vars[[1L]]
      x
    },
    invalid_units = function(x) {
      x$units <- "degC"
      x
    }
  )

  for (name in names(mutators)) {
    memory <- mutators[[name]](pair$memory)
    netcdf <- mutators[[name]](pair$netcdf)
    memory_report <- cube_validate(memory)
    netcdf_report <- cube_validate(netcdf)

    expect_identical(
      a2_validation_contract(netcdf_report),
      a2_validation_contract(memory_report),
      info = name
    )
  }

  corrupt_descriptor <- pair$netcdf
  corrupt_descriptor$storage$backend <- "unknown"
  report <- cube_validate(corrupt_descriptor)
  expect_identical(report$status[report$check == "backend"], "FAIL")
  expect_match(report$message[report$check == "backend"], "unknown")
})

test_that("all visualization APIs have scientific memory and NetCDF parity", {
  skip_if_not_installed("ncdf4")
  pair <- a2_make_netcdf_pair()
  withr::local_file(pair$file)
  time <- pair$netcdf$time[[1L]]
  path <- data.frame(
    longitude = c(-80, -79, -78),
    latitude = c(-12, -11, -12)
  )
  memory_before <- a2_scientific_snapshot(pair$memory)
  netcdf_before <- a2_scientific_snapshot(pair$netcdf)

  memory_plots <- list(
    map = viz.map(
      pair$memory, "temperature", time = time, depth = 0
    ),
    section = viz.section(
      pair$memory, "temperature", section = "longitude-depth",
      latitude = -12, time = time
    ),
    profile = viz.profile(
      pair$memory, "temperature", longitude = -80, latitude = -12,
      time = time
    ),
    transect = viz.transect(
      pair$memory, path, variable = "temperature", time = time,
      depth = pair$memory$depth, match = "exact", mode = "section"
    ),
    timeseries = viz.timeseries(
      pair$memory, "temperature", longitude = -80, latitude = -12,
      depth = 0, match = "exact"
    )
  )
  netcdf_plots <- list(
    map = viz.map(
      pair$netcdf, "temperature", time = time, depth = 0
    ),
    section = viz.section(
      pair$netcdf, "temperature", section = "longitude-depth",
      latitude = -12, time = time
    ),
    profile = viz.profile(
      pair$netcdf, "temperature", longitude = -80, latitude = -12,
      time = time
    ),
    transect = viz.transect(
      pair$netcdf, path, variable = "temperature", time = time,
      depth = pair$netcdf$depth, match = "exact", mode = "section"
    ),
    timeseries = viz.timeseries(
      pair$netcdf, "temperature", longitude = -80, latitude = -12,
      depth = 0, match = "exact"
    )
  )

  for (name in names(memory_plots)) {
    expect_s3_class(memory_plots[[name]], "ggplot")
    expect_s3_class(netcdf_plots[[name]], "ggplot")
    expect_equal(
      a2_plot_payload(netcdf_plots[[name]]),
      a2_plot_payload(memory_plots[[name]]),
      tolerance = A2_TOLERANCE$visualization_absolute,
      info = name
    )
    expect_equal(
      a2_plot_layer_xy(netcdf_plots[[name]]),
      a2_plot_layer_xy(memory_plots[[name]]),
      tolerance = A2_TOLERANCE$visualization_absolute,
      info = paste(name, "mapped x/y")
    )
  }

  a2_expect_source_unchanged(pair$memory, memory_before)
  a2_expect_source_unchanged(pair$netcdf, netcdf_before)
})
