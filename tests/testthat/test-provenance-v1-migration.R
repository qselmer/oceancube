test_that("legacy parent chains migrate oldest to newest without large payloads", {
  context <- provenance_test_context()
  legacy <- provenance_test_legacy_chain()
  frozen <- unserialize(serialize(legacy, NULL))
  migrated <- oceancube:::.provenance_normalize(legacy, context)

  expect_identical(legacy, frozen)
  expect_identical(vapply(migrated$history, `[[`, character(1), "operation"),
                   c("read_nc", "cube_crop"))
  expect_identical(vapply(migrated$history, `[[`, character(1), "id"),
                   c("op_001", "op_002"))
  expect_s3_class(migrated$history[[1]]$execution$recorded_at, "POSIXct")
  expect_identical(attr(migrated$history[[1]]$execution$recorded_at, "tzone"), "UTC")
  expect_true(oceancube:::.provenance_validate(migrated, strict = TRUE)$valid)
  expect_identical(
    migrated$extensions$legacy$provider_note,
    "legacy note retained non-semantically"
  )
  text <- paste(capture.output(dput(migrated)), collapse = "")
  expect_false(grepl("C:/private", text, fixed = TRUE))
  expect_false(grepl("nodename|private execution detail", text))
  expect_lt(as.numeric(object.size(migrated)), 100000)
})

test_that("opaque user metadata is preserved only under extensions", {
  user <- list(provider_note = "curated by science team", revision = 2L)
  migrated <- oceancube:::.provenance_normalize(user)
  expect_identical(migrated$extensions$user, user)
  expect_length(migrated$history, 0L)
  expect_true(oceancube:::.provenance_validate(migrated)$valid)
})

test_that("legacy temporal aliases become canonical operations exactly once", {
  legacy <- list(
    parent = list(cube_aggregate_time = list(
      operation = "temporal_aggregation", by = "month", method = "mean"
    )),
    cube_trend = list(operation = "trend", method = "linear")
  )
  migrated <- oceancube:::.provenance_normalize(legacy, provenance_test_context())
  expect_identical(vapply(migrated$history, `[[`, character(1), "operation"),
                   c("cube_aggregate_time", "cube_trend"))
})

test_that("legacy compatibility wrappers retain delegated core operations once", {
  clim <- list(
    package = "oceancube", package_version = "0.2.0.9000",
    function_name = "clim_month", arguments = list(period = as.Date(c(
      "2020-01-01", "2021-12-01"
    ))),
    extra = list(
      parent = list(time = list(calendar = "proleptic_gregorian")),
      core = list(by = "month", requested_period = as.Date(c(
        "2020-01-01", "2021-12-01"
      )))
    )
  )
  migrated <- oceancube:::.provenance_normalize(
    clim, provenance_test_context(time_kind = "recurring_climatology")
  )
  expect_identical(vapply(migrated$history, `[[`, character(1), "operation"),
                   c("cube_climatology", "clim_month"))
  expect_identical(sum(vapply(migrated$history, function(record) {
    identical(record$operation, "cube_climatology")
  }, logical(1))), 1L)
})

test_that("legacy and V1 provenance round-trip through base serialization", {
  legacy <- provenance_test_legacy_chain()
  v1 <- oceancube:::.provenance_normalize(legacy, provenance_test_context())
  expect_identical(unserialize(serialize(legacy, NULL)), legacy)
  expect_identical(unserialize(serialize(v1, NULL)), v1)
  path <- tempfile(fileext = ".rds")
  saveRDS(v1, path)
  expect_identical(readRDS(path), v1)
  expect_identical(oceancube:::.provenance_semantic(readRDS(path)),
                   oceancube:::.provenance_semantic(v1))
})
