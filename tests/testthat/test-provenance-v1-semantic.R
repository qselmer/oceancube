test_that("semantic projection excludes locator, backend, execution and extensions", {
  left <- provenance_test_v1()
  right <- left
  left$source$locator <- list(
    type = "file", value = "a.nc", basename = "a.nc", portable = FALSE
  )
  right$source$locator <- list(
    type = "file", value = "b.nc", basename = "b.nc", portable = FALSE
  )
  left$history[[1]]$execution <- list(
    recorded_at = as.POSIXct("2026-08-20 10:00:00", tz = "UTC")
  )
  right$history[[1]]$execution <- list(
    recorded_at = as.POSIXct("2026-08-21 10:00:00", tz = "UTC")
  )
  left$history[[1]]$output$backend <- "memory"
  right$history[[1]]$output$backend <- "netcdf"
  left$extensions$user <- list(note = "left")
  right$extensions$user <- list(note = "right")
  expect_identical(oceancube:::.provenance_semantic(left),
                   oceancube:::.provenance_semantic(right))
})

test_that("semantic projection retains scientific parameters and methods", {
  left <- provenance_test_v1("cube_aggregate_time")
  right <- left
  right$history[[1]]$parameters$requested$variable <- "temperature"
  expect_false(identical(oceancube:::.provenance_semantic(left),
                         oceancube:::.provenance_semantic(right)))
  right <- left
  right$history[[1]]$scientific_method$id <- "oceancube:other_method"
  expect_false(identical(oceancube:::.provenance_semantic(left),
                         oceancube:::.provenance_semantic(right)))
})

test_that("append-only V1 growth remains linear through fifty operations", {
  sizes <- vapply(c(1L, 5L, 10L, 25L, 50L), function(n) {
    provenance <- oceancube:::.provenance_normalize(NULL, provenance_test_context())
    for (i in seq_len(n)) {
      provenance <- oceancube:::.provenance_append(
        provenance, "cube_crop",
        parameters = list(requested = list(step = i), resolved = list()),
        context = provenance_test_context()
      )
    }
    length(serialize(provenance, NULL))
  }, integer(1))
  expect_true(all(diff(sizes) > 0L))
  expect_lt(sizes[[5]], sizes[[1]] * 30)
  increments <- diff(sizes) / diff(c(1L, 5L, 10L, 25L, 50L))
  expect_lt(max(increments) / min(increments), 2)
})
