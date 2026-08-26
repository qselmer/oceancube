test_that("OISST core pipeline is deterministic, private, and serializable", {
  fixture <- testthat::test_path(
    "fixtures", "real-data", "noaa-oisst21-surface-time-fv1.nc"
  )
  run <- function() {
    source <- read_nc(
      fixture, vars = "sst", depth_name = "zlev",
      source = "NOAA OISST", dataset_id = "oisst-v2.1"
    )
    cropped <- cube_crop(
      source,
      longitude = range(source$lon), latitude = range(source$lat)
    )
    cube_slice(cropped, time = source$time[[1L]], by = "value")
  }
  first <- run()
  second <- run()
  expect_identical(
    vapply(first$provenance$history, `[[`, character(1), "operation"),
    c("read_nc", "cube_crop", "cube_slice")
  )
  expect_identical(.provenance_semantic(first$provenance),
                   .provenance_semantic(second$provenance))
  expect_identical(first$lon, second$lon)
  expect_identical(.cube_read(first), .cube_read(second))
  expect_true(all(first$lon >= 0 & first$lon <= 360))

  semantic_text <- paste(capture.output(dput(
    .provenance_semantic(first$provenance)
  )), collapse = "")
  expect_false(grepl(normalizePath(fixture, winslash = "/"),
                     semantic_text, fixed = TRUE))
  expect_false(grepl(Sys.info()[["user"]], semantic_text, fixed = TRUE))
  expect_false(grepl(Sys.info()[["nodename"]], semantic_text, fixed = TRUE))

  path <- tempfile(fileext = ".rds")
  withr::local_file(path)
  saveRDS(first, path)
  restored <- readRDS(path)
  expect_true(.provenance_validate(restored$provenance, strict = TRUE)$valid)
  expect_identical(.provenance_semantic(restored$provenance),
                   .provenance_semantic(first$provenance))
  expect_identical(.cube_read(restored), .cube_read(first))
})

test_that("core temporal output stays V1 and normalizable", {
  source <- core_runtime_cube()
  aggregated <- suppressWarnings(cube_aggregate_time(source, by = "month"))
  expect_identical(.provenance_validate(aggregated$provenance)$kind, "v1")
  expect_identical(provenance_operations(aggregated), "cube_aggregate_time")
  expect_null(aggregated$provenance$parent)
  context <- .provenance_cube_context(
    source = aggregated$source, dataset_id = aggregated$dataset_id,
    time = aggregated$time, shape = .cube_shape(aggregated),
    variables = aggregated$vars, backend = "memory",
    provenance = source$provenance
  )
  normalized <- .provenance_normalize(aggregated$provenance, context)
  expect_true(.provenance_validate(normalized, strict = TRUE)$valid)
  expect_identical(
    vapply(normalized$history, `[[`, character(1), "operation"),
    "cube_aggregate_time"
  )
  expect_identical(normalized$source, source$provenance$source)
})

test_that("flat V1 history grows linearly through repeated core operations", {
  cube <- core_runtime_cube()
  sizes <- numeric(10L)
  for (i in seq_len(10L)) {
    cube <- cube_slice(cube, by = "index")
    sizes[[i]] <- length(serialize(cube$provenance, NULL))
  }
  expect_identical(length(cube$provenance$history), 10L)
  expect_identical(
    vapply(cube$provenance$history, `[[`, character(1), "id"),
    sprintf("op_%03d", seq_len(10L))
  )
  increments <- diff(sizes)
  expect_lt(max(increments) / min(increments), 1.2)
  expect_false("parent" %in% names(cube$provenance))
})
