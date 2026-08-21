test_that("ocean_cube initializes and normalizes V1 without a fake operation", {
  fresh <- core_runtime_cube()
  expect_true(.provenance_validate(fresh$provenance, strict = TRUE)$valid)
  expect_identical(fresh$provenance$schema_version, "1.0.0")
  expect_length(fresh$provenance$history, 0L)
  expect_identical(fresh$provenance$source$identity$label, "fixture")
  expect_identical(
    fresh$provenance$source$identity$dataset_id,
    "core-runtime"
  )
  expect_identical(fresh$provenance$time$current$class, "Date")

  supplied_v1 <- fresh$provenance
  supplied_v1$extensions$user <- list(note = "preserve")
  before_v1 <- supplied_v1
  rebuilt <- core_runtime_cube(supplied_v1)
  expect_identical(supplied_v1, before_v1)
  expect_identical(rebuilt$provenance$extensions$user, list(note = "preserve"))
  expect_length(rebuilt$provenance$history, 0L)

  legacy <- list(
    cube_crop = list(
      ranges_requested = list(longitude = c(-80, -79)),
      ranges_applied = list(longitude = c(-80, -79)),
      outside = "clip"
    ),
    safe_note = "legacy"
  )
  before_legacy <- legacy
  migrated <- core_runtime_cube(legacy)
  expect_identical(legacy, before_legacy)
  expect_identical(migrated$provenance$history[[1L]]$operation, "cube_crop")
  expect_identical(migrated$provenance$extensions$legacy$safe_note, "legacy")

  opaque <- list(owner_note = "safe")
  user <- core_runtime_cube(opaque)
  expect_identical(user$provenance$extensions$user, opaque)
  expect_error(
    core_runtime_cube(list(api_token = "do-not-store")),
    class = "oceancube_provenance_unsafe"
  )
})

test_that("read_nc emits exactly one canonical ingestion operation", {
  fixture <- testthat::test_path(
    "fixtures", "real-data", "noaa-oisst21-surface-time-fv1.nc"
  )
  cube <- read_nc(
    fixture, vars = "sst", depth_name = "zlev",
    source = "NOAA OISST", dataset_id = "oisst-v2.1"
  )
  provenance <- cube$provenance
  expect_true(.provenance_validate(provenance, strict = TRUE)$valid)
  expect_identical(
    vapply(provenance$history, `[[`, character(1), "operation"),
    "read_nc"
  )
  expect_identical(provenance$source$identity$label, "NOAA OISST")
  expect_identical(provenance$source$identity$dataset_id, "oisst-v2.1")
  expect_identical(provenance$source$locator$type, "file")
  expect_false(provenance$source$locator$portable)
  expect_identical(provenance$source$locator$basename, basename(fixture))
  expect_identical(provenance$time$current$class, "POSIXct")
  expect_identical(provenance$time$current$timezone, "UTC")
  expect_true(all(c("cf_units", "cf_origin", "decoder") %in%
                    names(provenance$time$source)))

  record <- provenance$history[[1L]]
  expect_identical(record$parameters$requested$vars, "sst")
  expect_identical(record$parameters$requested$depth_name, "zlev")
  expect_identical(record$output$backend, "memory")
  expect_identical(record$output$shape, .cube_shape(cube))
  expect_identical(record$output$variables, "sst")
  expect_identical(record$output$time_kind, "historical")
  expect_identical(record$software$version, "0.2.0.9000")
  expect_false("parent" %in% names(provenance))

  semantic_text <- paste(capture.output(dput(
    .provenance_semantic(provenance)
  )), collapse = "")
  expect_false(grepl(normalizePath(fixture, winslash = "/"),
                     semantic_text, fixed = TRUE))
})
