test_that("deferred construction and collect form flat canonical history", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  deferred <- .new_netcdf_cube(
    .new_netcdf_storage(
      file, c("temperature", "oxygen"),
      source = "fixture", dataset_id = "deferred-v1"
    )
  )
  expect_identical(deferred$provenance$history[[1L]]$operation, "read_nc")
  expect_identical(deferred$provenance$history[[1L]]$output$backend, "netcdf")
  expect_identical(deferred$provenance$source$locator$value,
                   deferred$storage$file$normalized_path)
  expect_true(.provenance_validate(deferred$provenance, strict = TRUE)$valid)

  before <- deferred
  collected <- cube_collect(deferred)
  expect_identical(deferred, before)
  expect_identical(
    vapply(collected$provenance$history, `[[`, character(1), "operation"),
    c("read_nc", "cube_collect")
  )
  expect_identical(.cube_read(collected), .cube_read(deferred))
  expect_identical(collected$time, deferred$time)
  expect_identical(collected$units, deferred$units)
  expect_true("netcdf_collect" %in% names(collected$qa))
  expect_false("parent" %in% names(collected$provenance))

  memory_before <- collected
  memory_again <- cube_collect(collected)
  expect_identical(memory_again, memory_before)

  deferred$provenance <- list(
    cube_crop = list(
      ranges_requested = list(longitude = range(deferred$lon)),
      ranges_applied = list(longitude = range(deferred$lon)), outside = "clip"
    ),
    note = "collect legacy"
  )
  legacy_before <- deferred
  legacy_collected <- cube_collect(deferred)
  expect_identical(deferred, legacy_before)
  expect_identical(
    vapply(legacy_collected$provenance$history, `[[`, character(1), "operation"),
    c("cube_crop", "cube_collect")
  )
})

test_that("cube_mask appends once and preserves time and scientific payload", {
  skip_if_not_installed("sf")
  cube <- cube_slice(
    cube_crop(core_runtime_cube(), longitude = c(-80, -79)),
    by = "index"
  )
  polygon <- sf::st_sfc(sf::st_polygon(list(matrix(
    c(-80.5, -12.5, -78.5, -12.5, -78.5, -10.5,
      -80.5, -10.5, -80.5, -12.5),
    ncol = 2, byrow = TRUE
  ))), crs = 4326)
  before <- cube
  masked <- cube_mask(cube, polygon)
  expect_identical(cube, before)
  expect_identical(
    vapply(masked$provenance$history, `[[`, character(1), "operation"),
    c("cube_crop", "cube_slice", "cube_mask")
  )
  expect_identical(masked$time, cube$time)
  expect_identical(masked$provenance$time, cube$provenance$time)
  expect_identical(masked$provenance$source, cube$provenance$source)
  expect_true(.provenance_validate(masked$provenance, strict = TRUE)$valid)
  expect_null(masked$provenance$history[[3L]]$parameters$resolved[["mask"]])
  expect_true("coverage" %in% names(masked$qa$mask))

  legacy <- core_runtime_cube()
  legacy$provenance <- list(
    cube_crop = list(
      ranges_requested = list(longitude = range(legacy$lon)),
      ranges_applied = list(longitude = range(legacy$lon)), outside = "clip"
    )
  )
  legacy_before <- legacy
  legacy_masked <- cube_mask(legacy, polygon)
  expect_identical(legacy, legacy_before)
  expect_identical(
    vapply(legacy_masked$provenance$history, `[[`, character(1), "operation"),
    c("cube_crop", "cube_mask")
  )
})
