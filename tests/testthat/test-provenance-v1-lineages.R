test_that("secondary lineages are flat, deterministic and reused", {
  primary <- provenance_test_v1("read_nc")
  context <- provenance_test_context(time_kind = "recurring_climatology")
  secondary <- provenance_test_v1("cube_climatology", context)

  first <- oceancube:::.provenance_merge_lineages(
    primary, secondary, roles = "climatology"
  )
  second <- oceancube:::.provenance_merge_lineages(
    first$provenance, secondary, roles = "climatology"
  )
  expect_named(first$provenance$lineages, "lineage_001")
  expect_named(second$provenance$lineages, "lineage_001")
  expect_named(first$provenance$lineages[[1]], c("source", "time", "history"),
               ignore.order = FALSE)
  expect_identical(first$refs[[1]]$lineage_ref, "lineage_001")
  expect_identical(second$refs[[1]]$lineage_ref, "lineage_001")
  expect_true(oceancube:::.provenance_validate(second$provenance, strict = TRUE)$valid)
})

test_that("anomaly graph has one primary operation and two input references", {
  primary <- provenance_test_v1("read_nc")
  primary <- oceancube:::.provenance_append(
    primary, "cube_crop", context = provenance_test_context()
  )
  clim_context <- provenance_test_context(time_kind = "recurring_climatology")
  climatology <- provenance_test_v1("cube_climatology", clim_context)
  merged <- oceancube:::.provenance_merge_lineages(
    primary, climatology, roles = "climatology"
  )
  inputs <- c(list(list(
    role = "source", lineage_ref = "primary", entity_ref = "op_002:output",
    summary = oceancube:::.provenance_summary(provenance_test_context())
  )), merged$refs)
  anomaly <- oceancube:::.provenance_append(
    merged$provenance, "cube_anomaly", inputs = inputs,
    output = oceancube:::.provenance_summary(provenance_test_context()),
    scientific_method = list(id = "oceancube:difference", version = "1"),
    context = provenance_test_context()
  )
  record <- anomaly$history[[3]]
  expect_identical(record$operation, "cube_anomaly")
  expect_identical(vapply(record$inputs, `[[`, character(1), "role"),
                   c("source", "climatology"))
  expect_identical(vapply(record$inputs, `[[`, character(1), "lineage_ref"),
                   c("primary", "lineage_001"))
  expect_length(anomaly$lineages, 1L)
  expect_identical(vapply(anomaly$history, `[[`, character(1), "operation"),
                   c("read_nc", "cube_crop", "cube_anomaly"))
  expect_true(oceancube:::.provenance_validate(anomaly, strict = TRUE)$valid)
})

test_that("a secondary graph with its own lineage is flattened and rewritten", {
  primary <- provenance_test_v1("read_nc")
  tertiary <- provenance_test_v1("cube_climatology")
  secondary <- provenance_test_v1("read_nc")
  inner <- oceancube:::.provenance_merge_lineages(
    secondary, tertiary, roles = "baseline"
  )
  secondary <- oceancube:::.provenance_append(
    inner$provenance, "cube_anomaly",
    inputs = c(list(list(
      role = "source", lineage_ref = "primary", entity_ref = "op_001:output",
      summary = list()
    )), inner$refs),
    scientific_method = list(id = "oceancube:difference", version = "1")
  )
  outer <- oceancube:::.provenance_merge_lineages(
    primary, secondary, roles = "derived"
  )
  expect_named(outer$provenance$lineages, c("lineage_001", "lineage_002"))
  expect_false(any(vapply(outer$provenance$lineages, function(x) {
    "lineages" %in% names(x)
  }, logical(1))))
  refs <- outer$provenance$lineages$lineage_001$history[[2]]$inputs
  expect_identical(refs[[2]]$lineage_ref, "lineage_002")
  expect_true(oceancube:::.provenance_validate(
    outer$provenance, strict = TRUE
  )$valid)
  repeated <- oceancube:::.provenance_merge_lineages(
    outer$provenance, secondary, roles = "derived"
  )
  expect_length(repeated$provenance$lineages, 2L)
  expect_identical(repeated$refs[[1]]$lineage_ref, "lineage_001")
})

test_that("multi-lineage provenance survives serialization semantically", {
  primary <- provenance_test_v1("read_nc")
  secondary <- provenance_test_v1("cube_climatology")
  merged <- oceancube:::.provenance_merge_lineages(primary, secondary, "baseline")
  merged$provenance$extensions$user <- list(note = "safe")
  merged$provenance$history[[1]]$execution <- list(
    recorded_at = as.POSIXct("2026-08-20", tz = "UTC")
  )
  restored <- unserialize(serialize(merged$provenance, NULL))
  expect_true(oceancube:::.provenance_validate(restored, strict = TRUE)$valid)
  expect_identical(oceancube:::.provenance_semantic(restored),
                   oceancube:::.provenance_semantic(merged$provenance))
})

test_that("current legacy anomaly normalizes to a flat two-lineage graph", {
  time <- as.Date(c("2020-01-01", "2021-01-01"))
  x <- ocean_cube(
    lon = -80, lat = -12, depth = 0, time = time,
    data = array(c(1, 3), c(1, 1, 1, 2, 1)), vars = "sst",
    units = "degC", source = "anomaly-test"
  )
  climatology <- suppressWarnings(cube_climatology(x, "month", diagnostics = TRUE))
  anomaly <- cube_anomaly(x, climatology, "difference")
  context <- list(
    source = anomaly$source, time = anomaly$time, backend = "memory",
    shape = oceancube:::.cube_shape(anomaly), variables = anomaly$vars
  )
  migrated <- oceancube:::.provenance_normalize(anomaly$provenance, context)
  expect_identical(vapply(migrated$history, `[[`, character(1), "operation"),
                   "cube_anomaly")
  expect_named(migrated$lineages, "lineage_001")
  expect_identical(vapply(migrated$history[[1]]$inputs, `[[`, character(1), "role"),
                   c("source", "climatology"))
  expect_true(oceancube:::.provenance_validate(migrated, strict = TRUE)$valid)
  expect_null(anomaly$provenance$schema_version)
  expect_identical(length(migrated$history), 1L)
  expect_identical(length(migrated$lineages[[1]]$history), 1L)
  expect_lt(length(serialize(migrated, NULL)),
            2 * length(serialize(anomaly$provenance, NULL)))
})
