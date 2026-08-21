test_that("provenance V1 has an exact schema and deterministic append records", {
  context <- provenance_test_context()
  empty <- oceancube:::.provenance_normalize(NULL, context)
  expect_named(empty, c("schema_version", "source", "time", "history",
                        "lineages", "extensions"), ignore.order = FALSE)
  expect_identical(empty$schema_version, "1.0.0")
  expect_true(oceancube:::.provenance_validate(empty)$valid)

  original <- unserialize(serialize(empty, NULL))
  one <- oceancube:::.provenance_append(
    empty, "read_nc",
    parameters = list(request = list(vars = "sst"), resolved = list()),
    output = oceancube:::.provenance_summary(context), context = context
  )
  two <- oceancube:::.provenance_append(one, "cube_crop", context = context)
  expect_identical(empty, original)
  expect_identical(vapply(two$history, `[[`, character(1), "id"),
                   c("op_001", "op_002"))
  expect_named(one$history[[1]]$parameters, c("requested", "resolved"))
  expect_false("request" %in% names(one$history[[1]]$parameters))
  expect_true(oceancube:::.provenance_validate(two, strict = TRUE)$valid)
})

test_that("provenance classification separates V1, legacy, opaque and future", {
  v1 <- provenance_test_v1()
  future <- v1
  future$schema_version <- "2.0.0"
  malformed <- v1
  malformed$history[[1]]$id <- "random"

  expect_identical(oceancube:::.provenance_validate(NULL)$kind, "null")
  expect_identical(oceancube:::.provenance_validate(v1)$kind, "v1")
  expect_identical(oceancube:::.provenance_validate(list(parent = list()))$kind,
                   "legacy")
  expect_identical(oceancube:::.provenance_validate(list(note = "user"))$kind,
                   "opaque_user")
  expect_identical(oceancube:::.provenance_validate(future)$kind, "future_schema")
  expect_identical(oceancube:::.provenance_normalize(future), future)
  expect_error(oceancube:::.provenance_append(future, "cube_crop"),
               class = "oceancube_provenance_future_schema")
  expect_error(oceancube:::.provenance_normalize(malformed),
               class = "oceancube_provenance_malformed")
})

test_that("provenance rejects unsafe values, credentials and invalid times", {
  expect_error(oceancube:::.provenance_normalize(list(value = globalenv())),
               class = "oceancube_provenance_unsafe")
  expect_error(oceancube:::.provenance_normalize(list(value = identity)),
               class = "oceancube_provenance_unsafe")
  connection <- textConnection("buffer", open = "w", local = TRUE)
  on.exit(close(connection), add = TRUE)
  expect_error(oceancube:::.provenance_normalize(list(value = connection)),
               class = "oceancube_provenance_unsafe")
  expect_error(oceancube:::.provenance_normalize(list(api_token = "redacted")),
               class = "oceancube_provenance_unsafe")

  v1 <- provenance_test_v1()
  v1$source$locator <- list(
    type = "url", value = "https://example.test/x?access_token=redacted",
    basename = "x", portable = TRUE
  )
  expect_error(oceancube:::.provenance_validate(v1, strict = TRUE),
               class = "oceancube_provenance_unsafe")
  v1 <- provenance_test_v1()
  v1$time$current$start <- as.POSIXct("2020-01-01", tz = "America/Lima")
  expect_error(oceancube:::.provenance_validate(v1, strict = TRUE),
               class = "oceancube_provenance_unsafe")
  v1 <- provenance_test_v1()
  v1$extensions$value <- Inf
  expect_error(oceancube:::.provenance_validate(v1, strict = TRUE),
               class = "oceancube_provenance_unsafe")
  v1 <- provenance_test_v1()
  v1$time$source <- list(ad_hoc = "not canonical")
  expect_error(oceancube:::.provenance_validate(v1, strict = TRUE),
               class = "oceancube_provenance_malformed")
  v1 <- provenance_test_v1()
  v1$time$current$class <- "Date"
  v1$time$current$start <- as.POSIXct("2020-01-01", tz = "UTC")
  expect_error(oceancube:::.provenance_validate(v1, strict = TRUE),
               class = "oceancube_provenance_malformed")
})

test_that("provenance software version is loaded package metadata", {
  expect_identical(oceancube:::.provenance_software_version(), "0.2.0.9000")
  expect_identical(provenance_test_v1()$history[[1]]$software,
                   list(package = "oceancube", version = "0.2.0.9000"))
})

test_that("optional source identity and metadata fields validate when known", {
  provenance <- oceancube:::.provenance_normalize(NULL)
  provenance$source$identity <- list(
    label = "fixture", dataset_id = "dataset", provider = "provider",
    product = "product", version = "1", doi = "10.0000/example",
    fixture_id = "fixture-1",
    checksum = list(algorithm = "sha256", value = "sentinel")
  )
  provenance$source$metadata <- list(edition = "final")
  expect_true(oceancube:::.provenance_validate(provenance, strict = TRUE)$valid)
})

test_that("append normalizes legacy once without mutating its input", {
  legacy <- provenance_test_legacy_chain()
  frozen <- unserialize(serialize(legacy, NULL))
  appended <- oceancube:::.provenance_append(
    legacy, "cube_aggregate_time", context = provenance_test_context()
  )
  expect_identical(legacy, frozen)
  expect_identical(length(appended$history), 3L)
  expect_identical(appended$history[[3]]$operation, "cube_aggregate_time")
})
