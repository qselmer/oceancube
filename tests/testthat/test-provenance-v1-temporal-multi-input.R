temporal_v1_cube <- function(provenance = NULL) {
  time <- seq(as.Date("2018-01-01"), as.Date("2021-12-01"), by = "month")
  month <- as.integer(format(time, "%m"))
  year <- as.integer(format(time, "%Y")) - 2018L
  base <- 10 + sin(2 * pi * (month - 1) / 12) + 0.25 * year
  values <- rep(base, each = 4L) + rep(c(0, 0.5, 1, 1.5), length(base))
  ocean_cube(
    lon = c(-80, -79), lat = c(-12, -11), depth = 0,
    time = time, vars = "sst",
    data = array(values, dim = c(2, 2, 1, length(time), 1)),
    units = c(sst = "degC"), source = "synthetic-temporal-v1",
    dataset_id = "a4b3a-synthetic", provenance = provenance
  )
}

temporal_legacy_provenance <- function() {
  out <- .make_provenance("read_nc", args = list(vars = "sst"))
  out$time <- list(
    canonical_class = "Date", canonical_timezone = NA_character_,
    source_class = "Date", source_timezone = NA_character_,
    source_offset = NA_character_, calendar = "proleptic_gregorian",
    calendar_defaulted = TRUE, decoder = "a4b3a-test",
    decode_status = "decoded", normalization = "none"
  )
  out
}

temporal_v1_pipeline <- function() {
  raw <- temporal_v1_cube()
  cropped <- cube_crop(raw, longitude = range(raw$lon), latitude = range(raw$lat))
  aggregate <- suppressWarnings(cube_aggregate_time(cropped, "month"))
  climatology <- suppressWarnings(cube_climatology(
    aggregate, "month", diagnostics = TRUE
  ))
  anomaly <- cube_anomaly(aggregate, climatology, "difference")
  trend <- cube_trend(anomaly, time_unit = "year", diagnostics = TRUE)
  list(
    raw = raw, cropped = cropped, aggregate = aggregate,
    climatology = climatology, anomaly = anomaly, trend = trend
  )
}

test_that("temporal producers append canonical V1 operations and time kinds", {
  pipeline <- temporal_v1_pipeline()
  for (name in names(pipeline)[-1L]) {
    expect_true(.provenance_validate(
      pipeline[[name]]$provenance, strict = TRUE
    )$valid, info = name)
    expect_null(pipeline[[name]]$provenance$parent, info = name)
  }

  expect_identical(
    provenance_operations(pipeline$aggregate),
    c("cube_crop", "cube_aggregate_time")
  )
  expect_identical(
    provenance_operations(pipeline$climatology),
    c("cube_crop", "cube_aggregate_time", "cube_climatology")
  )
  expect_identical(
    provenance_operations(pipeline$anomaly),
    c("cube_crop", "cube_aggregate_time", "cube_anomaly")
  )
  expect_identical(
    provenance_operations(pipeline$trend),
    c("cube_crop", "cube_aggregate_time", "cube_anomaly", "cube_trend")
  )
  expect_identical(pipeline$aggregate$provenance$time$current$kind, "historical")
  expect_identical(
    pipeline$climatology$provenance$time$current$kind,
    "recurring_climatology"
  )
  expect_identical(pipeline$anomaly$provenance$time$current$kind, "historical")
  expect_identical(pipeline$trend$provenance$time$current$kind, "trend_anchor")
  expect_identical(
    pipeline$aggregate$provenance$time$source,
    pipeline$trend$provenance$time$source
  )
  expect_identical(
    pipeline$raw$provenance$source$identity,
    pipeline$trend$provenance$source$identity
  )
})

test_that("temporal methods parameters and QA stay in their certified roles", {
  pipeline <- temporal_v1_pipeline()
  aggregate <- provenance_last_operation(pipeline$aggregate)
  climatology <- provenance_last_operation(pipeline$climatology)
  anomaly <- provenance_last_operation(pipeline$anomaly)
  trend <- provenance_last_operation(pipeline$trend)

  expect_identical(aggregate$scientific_method$id,
                   "oceancube:equal_observation_weighted_mean")
  expect_identical(climatology$scientific_method$id,
                   "oceancube:two_stage_equal_year_weighting")
  expect_identical(anomaly$scientific_method$id, "oceancube:difference")
  expect_identical(trend$scientific_method$id,
                   "oceancube:ols_elapsed_time_linear")
  expect_identical(trend$parameters$resolved$inference, "none")
  expect_true(is.list(pipeline$aggregate$qa$temporal_aggregation))
  expect_true(is.list(pipeline$climatology$qa$climatology))
  expect_true(is.list(pipeline$anomaly$qa$anomaly))
  expect_true(is.list(pipeline$trend$qa$trend))
  expect_false(any(vapply(aggregate$parameters, is.array, logical(1L))))
  expect_false(any(vapply(climatology$parameters, is.array, logical(1L))))
  expect_false(any(vapply(anomaly$parameters, is.array, logical(1L))))
  expect_false(any(vapply(trend$parameters, is.array, logical(1L))))
})

test_that("anomaly registers one secondary lineage with explicit roles", {
  pipeline <- temporal_v1_pipeline()
  source_before <- serialize(pipeline$aggregate, NULL)
  climatology_before <- serialize(pipeline$climatology, NULL)
  anomaly <- cube_anomaly(pipeline$aggregate, pipeline$climatology, "z")
  record <- provenance_last_operation(anomaly)

  expect_identical(serialize(pipeline$aggregate, NULL), source_before)
  expect_identical(serialize(pipeline$climatology, NULL), climatology_before)
  expect_identical(length(anomaly$provenance$history),
                   length(pipeline$aggregate$provenance$history) + 1L)
  expect_length(anomaly$provenance$lineages, 1L)
  expect_named(anomaly$provenance$lineages, "lineage_001")
  expect_identical(vapply(record$inputs, `[[`, character(1L), "role"),
                   c("source", "climatology"))
  expect_identical(vapply(record$inputs, `[[`, character(1L), "lineage_ref"),
                   c("primary", "lineage_001"))
  expect_identical(record$scientific_method$id, "oceancube:standardized_z")
  expect_identical(anomaly$provenance$time$current$kind, "historical")
  expect_identical(
    anomaly$provenance$lineages$lineage_001$time$current$kind,
    "recurring_climatology"
  )
  repeated <- .provenance_merge_lineages(
    anomaly$provenance, pipeline$climatology$provenance, "climatology"
  )
  expect_length(repeated$provenance$lineages, 1L)
  expect_identical(repeated$refs[[1L]]$lineage_ref, "lineage_001")
})

test_that("temporal compatibility wrappers do not duplicate canonical engines", {
  x <- temporal_v1_cube()
  monthly <- suppressWarnings(to_month(x))
  month_clim <- suppressWarnings(clim_month(x))
  day_x <- ocean_cube(
    lon = -80, lat = -12, depth = 0,
    time = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01")),
    vars = "sst", data = array(c(1, 2, 3), c(1, 1, 1, 3, 1)),
    units = "degC", source = "daily-wrapper"
  )
  day_clim <- suppressWarnings(clim_day(day_x, leap = "drop"))
  difference <- anom_diff(x, month_clim)
  z <- anom_z(x, month_clim)
  magnitude <- signal_noise(x, month_clim, signed = FALSE)
  signed <- signal_noise(x, month_clim, signed = TRUE)

  expect_identical(provenance_operations(monthly), "cube_aggregate_time")
  expect_identical(provenance_operations(list(provenance = month_clim$provenance)),
                   "cube_climatology")
  expect_identical(provenance_operations(list(provenance = day_clim$provenance)),
                   "cube_climatology")
  expect_identical(provenance_operations(difference), "cube_anomaly")
  expect_identical(provenance_operations(z), "cube_anomaly")
  expect_identical(provenance_operations(magnitude),
                   c("cube_anomaly", "signal_noise"))
  expect_identical(provenance_operations(signed),
                   c("cube_anomaly", "signal_noise"))
  expect_identical(
    provenance_last_operation(magnitude)$parameters$resolved$transformation,
    "absolute_value"
  )
  expect_identical(
    provenance_last_operation(signed)$parameters$resolved$transformation,
    "identity"
  )
  expect_length(magnitude$provenance$lineages, 1L)
  expect_length(signed$provenance$lineages, 1L)
  expect_null(difference$provenance$extra$parent)
})

test_that("legacy temporal inputs migrate to V1 without mutation", {
  x <- temporal_v1_cube()
  x$provenance <- temporal_legacy_provenance()
  before <- serialize(x, NULL)
  aggregate <- suppressWarnings(cube_aggregate_time(x, "month"))
  climatology <- suppressWarnings(cube_climatology(x, "month", diagnostics = TRUE))
  anomaly <- cube_anomaly(x, climatology, "difference")
  trend <- cube_trend(x, time_unit = "year")
  wrapper_clim <- suppressWarnings(clim_month(x))
  noise <- signal_noise(x, wrapper_clim)

  expect_identical(serialize(x, NULL), before)
  for (value in list(aggregate, climatology, anomaly, trend, noise)) {
    expect_true(.provenance_validate(value$provenance, strict = TRUE)$valid)
    expect_null(value$provenance$parent)
  }
  expect_identical(tail(provenance_operations(aggregate), 1L),
                   "cube_aggregate_time")
  expect_identical(tail(provenance_operations(climatology), 1L),
                   "cube_climatology")
  expect_identical(tail(provenance_operations(anomaly), 1L), "cube_anomaly")
  expect_identical(tail(provenance_operations(trend), 1L), "cube_trend")
  expect_identical(tail(provenance_operations(noise), 2L),
                   c("cube_anomaly", "signal_noise"))
})

test_that("temporal semantics are deterministic private and serializable", {
  first <- temporal_v1_pipeline()
  second <- temporal_v1_pipeline()
  targets <- c("aggregate", "climatology", "anomaly", "trend")
  for (name in targets) {
    a <- first[[name]]
    b <- second[[name]]
    expect_identical(.provenance_semantic(a$provenance),
                     .provenance_semantic(b$provenance), info = name)
    restored <- unserialize(serialize(a, NULL))
    expect_true(.provenance_validate(restored$provenance, strict = TRUE)$valid)
    expect_identical(.provenance_semantic(restored$provenance),
                     .provenance_semantic(a$provenance), info = name)
  }

  path <- tempfile(fileext = ".rds")
  withr::local_file(path)
  saveRDS(first$anomaly, path)
  anomaly <- readRDS(path)
  expect_identical(names(anomaly$provenance$lineages),
                   names(first$anomaly$provenance$lineages))
  expect_identical(vapply(anomaly$provenance$history, `[[`, character(1L), "id"),
                   vapply(first$anomaly$provenance$history, `[[`, character(1L), "id"))
  expect_identical(.provenance_semantic(anomaly$provenance),
                   .provenance_semantic(first$anomaly$provenance))

  semantic_text <- paste(capture.output(dput(
    .provenance_semantic(first$trend$provenance)
  )), collapse = "")
  forbidden <- c(Sys.info()[["user"]], Sys.info()[["nodename"]],
                 normalizePath(tempdir(), winslash = "/"), "credential", "signed_url")
  forbidden <- forbidden[!is.na(forbidden) & nzchar(forbidden)]
  expect_false(any(vapply(forbidden, grepl, logical(1L),
                          x = semantic_text, fixed = TRUE)))
})

test_that("multi-input provenance stores the secondary history once", {
  pipeline <- temporal_v1_pipeline()
  anomaly <- pipeline$anomaly$provenance
  secondary_bytes <- length(serialize(
    anomaly$lineages$lineage_001, NULL
  ))
  v1_bytes <- length(serialize(anomaly, NULL))
  expect_length(anomaly$lineages, 1L)
  expect_false("parent" %in% names(anomaly))
  expect_gt(v1_bytes, secondary_bytes)
  expect_lt(v1_bytes, 4 * secondary_bytes)
  expect_identical(length(anomaly$history), 3L)
})
