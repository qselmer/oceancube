test_that("slice and crop append once for V1, legacy, and NULL inputs", {
  v1 <- core_runtime_cube(list(user_note = "preserved"))
  before_v1 <- v1
  cropped <- cube_crop(v1, longitude = c(-80, -79))
  expect_identical(v1, before_v1)
  expect_identical(
    vapply(cropped$provenance$history, `[[`, character(1), "operation"),
    "cube_crop"
  )
  expect_identical(
    cropped$provenance$extensions$user,
    list(user_note = "preserved")
  )

  legacy <- core_runtime_cube()
  legacy$provenance <- list(
    cube_crop = list(
      ranges_requested = list(longitude = c(-80, -79)),
      ranges_applied = list(longitude = c(-80, -79)), outside = "clip"
    ),
    note = "legacy"
  )
  before_legacy <- legacy
  sliced <- cube_slice(legacy, longitude = -80, by = "value")
  expect_identical(legacy, before_legacy)
  expect_identical(
    vapply(sliced$provenance$history, `[[`, character(1), "operation"),
    c("cube_crop", "cube_slice")
  )
  expect_identical(sliced$provenance$extensions$legacy$note, "legacy")

  absent <- core_runtime_cube()
  absent$provenance <- NULL
  from_null <- cube_slice(absent, latitude = -12, by = "value")
  expect_true(.provenance_validate(from_null$provenance, strict = TRUE)$valid)
  expect_identical(from_null$provenance$history[[1L]]$operation, "cube_slice")
  expect_false("parent" %in% names(from_null$provenance))

  legacy_crop_input <- core_runtime_cube()
  legacy_crop_input$provenance <- list(
    cube_slice = list(by = "index", match = "index", requested = list()),
    note = "crop legacy"
  )
  before_legacy_crop <- legacy_crop_input
  legacy_crop <- cube_crop(legacy_crop_input)
  expect_identical(legacy_crop_input, before_legacy_crop)
  expect_identical(
    vapply(legacy_crop$provenance$history, `[[`, character(1), "operation"),
    c("cube_slice", "cube_crop")
  )

  null_crop_input <- core_runtime_cube()
  null_crop_input$provenance <- NULL
  null_crop <- cube_crop(null_crop_input)
  expect_identical(null_crop$provenance$history[[1L]]$operation, "cube_crop")
})

test_that("selection provenance is bounded and diagnostics remain QA", {
  cube <- core_runtime_cube()
  cropped <- cube_crop(cube, longitude = c(-80, -79), outside = "clip")
  sliced <- cube_slice(
    cropped, longitude = -79, time = as.Date("2020-02-01"),
    by = "value", match = "nearest",
    tolerance = list(longitude = 0.5,
                     time = as.difftime(1, units = "days"))
  )
  operations <- vapply(
    sliced$provenance$history, `[[`, character(1), "operation"
  )
  expect_identical(operations, c("cube_crop", "cube_slice"))
  expect_identical(
    vapply(sliced$provenance$history, `[[`, character(1), "id"),
    c("op_001", "op_002")
  )
  expect_false(any(c("parent", "cube_crop", "cube_slice") %in%
                     names(sliced$provenance)))
  expect_null(sliced$provenance$history[[2L]]$parameters$resolved$resolved_indices)
  expect_true("resolved_indices" %in% names(sliced$qa$selection))
  expect_identical(sliced$provenance$time$current$count, 1L)
  expect_identical(sliced$provenance$time$current$class, "Date")
  expect_identical(sliced$qa$marker, "kept")
})
