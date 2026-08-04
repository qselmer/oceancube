test_that("cube_validate reports valid memory and surface cubes", {
  cube <- .make_baseline_fixture()$cube
  surface <- ocean_cube(
    lon = c(-80, -79),
    lat = c(-12, -11),
    depth = NA_real_,
    time = as.Date("2020-01-01"),
    data = array(1:4, dim = c(2, 2, 1, 1, 1)),
    vars = "temperature",
    units = "degC"
  )

  memory_report <- cube_validate(cube)
  surface_report <- cube_validate(surface)

  expect_false(any(memory_report$status == "FAIL"))
  expect_false(any(surface_report$status == "FAIL"))
  expect_identical(
    surface_report$status[surface_report$check == "depth"],
    "PASS"
  )
})

test_that("cube_validate accepts malformed inputs and reports their failures", {
  cube <- .make_baseline_fixture()$cube
  no_class <- unclass(cube)
  missing_lon <- cube
  missing_lon$lon <- NULL

  expect_identical(cube_validate(no_class)$status[1L], "FAIL")
  expect_s3_class(cube_validate(42), "oceancube_validation")
  expect_true(any(cube_validate(42)$check == "list" & cube_validate(42)$status == "FAIL"))
  expect_true(any(
    cube_validate(missing_lon)$check == "required_components" &
      cube_validate(missing_lon)$status == "FAIL"
  ))
})

test_that("coordinate failures identify longitude, latitude, and time", {
  cube <- .make_baseline_fixture()$cube
  bad_lon <- cube
  bad_lon$lon[1L] <- 400
  bad_lat <- cube
  bad_lat$lat[1L] <- -91
  bad_time <- cube
  bad_time$time <- as.character(bad_time$time)

  expect_identical(
    cube_validate(bad_lon)$status[cube_validate(bad_lon)$check == "longitude_bounds"],
    "FAIL"
  )
  expect_identical(
    cube_validate(bad_lat)$status[cube_validate(bad_lat)$check == "latitude_bounds"],
    "FAIL"
  )
  expect_identical(
    cube_validate(bad_time)$status[cube_validate(bad_time)$check == "time_class"],
    "FAIL"
  )
})

test_that("variables, dimensions, units, and extents are checked", {
  cube <- .make_baseline_fixture()$cube
  duplicated <- cube
  duplicated$vars[2L] <- duplicated$vars[1L]
  mismatched <- cube
  dim(mismatched$data) <- c(2, 3, 2, 4, 2)
  bad_units <- cube
  bad_units$units <- "degC"
  bad_extent <- cube
  bad_extent$spatial_extent <- c(-79, -78, -20, 0)

  expect_identical(
    cube_validate(duplicated)$status[cube_validate(duplicated)$check == "variables"],
    "FAIL"
  )
  expect_identical(
    cube_validate(mismatched)$status[
      cube_validate(mismatched)$check == "dimension_compatibility"
    ],
    "FAIL"
  )
  expect_identical(
    cube_validate(bad_units)$status[cube_validate(bad_units)$check == "units"],
    "FAIL"
  )
  expect_identical(
    cube_validate(bad_extent)$status[
      cube_validate(bad_extent)$check == "spatial_extent"
    ],
    "FAIL"
  )
})

test_that("unsupported backends are reported without dispatching into them", {
  cube <- .make_baseline_fixture()$cube
  cube$data <- NULL
  cube$storage <- list(backend = "unknown")

  report <- cube_validate(cube)

  expect_identical(report$status[report$check == "backend"], "FAIL")
  expect_match(report$message[report$check == "backend"], "unknown")
})

test_that("strict controls reporting versus validation errors", {
  cube <- .make_baseline_fixture()$cube
  cube$lat[1L] <- -100

  report <- cube_validate(cube, strict = FALSE)
  condition <- tryCatch(
    cube_validate(cube, strict = TRUE),
    oceancube_validation_error = identity
  )

  expect_s3_class(report, "oceancube_validation")
  expect_s3_class(condition, "oceancube_validation_error")
  expect_s3_class(condition$report, "oceancube_validation")
  expect_error(cube_validate(cube, strict = NA), class = "oceancube_bad_argument")
  expect_error(cube_validate(cube, strict = c(TRUE, FALSE)), class = "oceancube_bad_argument")
})

test_that("validation schema, values, classes, and printing are stable", {
  report <- cube_validate(.make_baseline_fixture()$cube)

  expect_identical(class(report), c("oceancube_validation", "data.frame"))
  expect_identical(
    names(report),
    c(
      "check", "status", "severity", "component", "message",
      "repairable", "suggested_action"
    )
  )
  expect_true(all(report$status %in% c("PASS", "WARN", "FAIL")))
  expect_true(all(report$severity %in% c("info", "warning", "error")))
  expect_identical(withVisible(print(report))$value, report)
  expect_false(withVisible(print(report))$visible)
  expect_match(capture.output(print(report))[[1L]], "<oceancube_validation>", fixed = TRUE)
})
