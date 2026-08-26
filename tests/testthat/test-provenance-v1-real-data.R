test_that("offline OISST workflow normalizes without paths or host identity", {
  fixture <- testthat::test_path(
    "fixtures", "real-data", "noaa-oisst21-surface-time-fv1.nc"
  )
  raw <- read_nc(fixture, depth_name = "zlev")
  cropped <- cube_crop(
    raw, longitude = range(raw$lon), latitude = range(raw$lat)
  )
  aggregated <- suppressWarnings(cube_aggregate_time(cropped, by = "month"))
  trend <- cube_trend(cropped, time_unit = "day", min_n = 3L)
  context <- list(
    source = aggregated$source, dataset_id = aggregated$dataset_id,
    time = aggregated$time, backend = "memory",
    shape = oceancube:::.cube_shape(aggregated), variables = aggregated$vars
  )
  provenance <- oceancube:::.provenance_normalize(aggregated$provenance, context)

  expect_identical(vapply(provenance$history, `[[`, character(1), "operation"),
                   c("read_nc", "cube_crop", "cube_aggregate_time"))
  expect_identical(provenance$time$current$kind, "historical")
  expect_identical(provenance$source$identity$label, "netcdf")
  expect_true(all(c(
    "source_class", "source_timezone", "source_offset", "calendar",
    "calendar_defaulted", "cf_units", "cf_origin", "decoder",
    "decode_status", "normalization"
  ) %in% names(provenance$time$source)))
  expect_true(oceancube:::.provenance_validate(provenance, strict = TRUE)$valid)
  expect_identical(provenance_operations(trend),
                   c("read_nc", "cube_crop", "cube_trend"))
  expect_identical(trend$provenance$time$current$kind, "trend_anchor")
  expect_true(oceancube:::.provenance_validate(
    trend$provenance, strict = TRUE
  )$valid)
  text <- paste(capture.output(dput(
    oceancube:::.provenance_semantic(provenance)
  )), collapse = "")
  expect_false(grepl(normalizePath(fixture, winslash = "/"), text, fixed = TRUE))
  info <- Sys.info()
  expect_false(grepl(info[["user"]], text, fixed = TRUE))
  expect_false(grepl(info[["nodename"]], text, fixed = TRUE))
})
