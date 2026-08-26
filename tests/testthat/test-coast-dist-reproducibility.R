a4r_coast_line <- function() {
  sf::st_sfc(
    sf::st_linestring(rbind(c(-81, -13), c(-81, -10))),
    crs = 4326
  )
}

a4r_coast_polygon <- function() {
  sf::st_sfc(sf::st_polygon(list(rbind(
    c(-81, -13), c(-79, -13), c(-79, -10), c(-81, -10), c(-81, -13)
  ))), crs = 4326)
}

a4r_run_from_state <- function(state, cube, coast) {
  suppressMessages(sf::sf_use_s2(state))
  result <- coast_dist(cube, coast)
  list(result = result, final_state = sf::sf_use_s2())
}

test_that("coast_dist controls S2 and restores either initial state", {
  skip_if_not_installed("sf")
  cube <- .make_baseline_fixture()$cube
  coast <- a4r_coast_line()
  old_s2 <- sf::sf_use_s2()
  withr::defer(suppressMessages(sf::sf_use_s2(old_s2)))

  from_true <- a4r_run_from_state(TRUE, cube, coast)
  from_false <- a4r_run_from_state(FALSE, cube, coast)

  expect_true(from_true$final_state)
  expect_false(from_false$final_state)
  expect_identical(from_true$result$dc, from_false$result$dc)
  expect_identical(
    .provenance_semantic(from_true$result$provenance),
    .provenance_semantic(from_false$result$provenance)
  )
})

test_that("the controlled S2 helper restores state after an error", {
  skip_if_not_installed("sf")
  old_s2 <- sf::sf_use_s2()
  withr::defer(suppressMessages(sf::sf_use_s2(old_s2)))

  for (initial in c(TRUE, FALSE)) {
    suppressMessages(sf::sf_use_s2(initial))
    expect_error(
      .with_s2_geometry(function() stop("a4r controlled failure")),
      "a4r controlled failure"
    )
    expect_identical(sf::sf_use_s2(), initial)
  }
})

test_that("coast geometry forms retain shortest-distance and zero semantics", {
  skip_if_not_installed("sf")
  old_s2 <- sf::sf_use_s2()
  withr::defer(suppressMessages(sf::sf_use_s2(old_s2)))
  suppressMessages(sf::sf_use_s2(FALSE))
  cube <- ocean_cube(
    lon = c(-81, -80, -78), lat = -12, depth = 0,
    time = as.Date("2020-01-01"), vars = "v",
    data = array(1:3, dim = c(3, 1, 1, 1, 1))
  )
  line <- a4r_coast_line()
  polygon <- a4r_coast_polygon()
  multiple <- c(
    line,
    sf::st_sfc(
      sf::st_linestring(rbind(c(-77, -13), c(-77, -10))), crs = 4326
    )
  )

  line_result <- coast_dist(cube, line)
  polygon_result <- coast_dist(cube, polygon)
  multiple_result <- coast_dist(cube, multiple)

  expect_equal(line_result$dc[1, 1], 0, tolerance = 1e-12)
  expect_equal(polygon_result$dc[1:2, 1], c(0, 0), tolerance = 1e-12)
  expect_true(polygon_result$dc[3, 1] > 0)
  expect_equal(multiple_result$dc[1, 1], 0, tolerance = 1e-12)
  expect_true(multiple_result$dc[2, 1] > 0)
  expect_true(all(multiple_result$dc <= line_result$dc))
  expect_equal(multiple_result$dc[3, 1], line_result$dc[2, 1],
               tolerance = 1e-10)
  expect_false(sf::sf_use_s2())
})

test_that("coast distance preserves inputs and the nautical-mile contract", {
  skip_if_not_installed("sf")
  cube <- .make_baseline_fixture()$cube
  coast <- a4r_coast_line()
  cube_before <- serialize(cube, NULL)
  coast_before <- serialize(coast, NULL)
  old_s2 <- sf::sf_use_s2()
  withr::defer(suppressMessages(sf::sf_use_s2(old_s2)))
  suppressMessages(sf::sf_use_s2(TRUE))

  result <- coast_dist(cube, coast)
  points <- sf::st_as_sf(
    expand.grid(lon = cube$lon, lat = cube$lat),
    coords = c("lon", "lat"), crs = 4326
  )
  metres <- as.numeric(sf::st_distance(points, sf::st_union(coast)))
  historical_s2_nm <- matrix(
    metres * 0.000539957,
    nrow = length(cube$lon), ncol = length(cube$lat), byrow = FALSE
  )
  expected_nm <- matrix(
    metres / 1852,
    nrow = length(cube$lon), ncol = length(cube$lat), byrow = FALSE
  )

  expect_identical(serialize(cube, NULL), cube_before)
  expect_identical(serialize(coast, NULL), coast_before)
  expect_identical(result$dc, historical_s2_nm)
  expect_equal(result$dc, expected_nm, tolerance = 5e-7)
  operation <- tail(result$provenance$history, 1L)[[1L]]
  expect_identical(operation$parameters$resolved$input_distance_unit, "m")
  expect_identical(
    operation$parameters$resolved$output_distance_unit, "nautical_mile"
  )
})

test_that("coast V1 is deterministic, serializable, and stock-compatible", {
  skip_if_not_installed("sf")
  cube <- .make_baseline_fixture()$cube
  coast <- a4r_coast_line()
  old_s2 <- sf::sf_use_s2()
  withr::defer(suppressMessages(sf::sf_use_s2(old_s2)))

  first <- a4r_run_from_state(TRUE, cube, coast)$result
  Sys.sleep(0.01)
  second <- a4r_run_from_state(FALSE, cube, coast)$result
  operation <- tail(first$provenance$history, 1L)[[1L]]

  expect_true(.provenance_validate(first$provenance, strict = TRUE)$valid)
  expect_identical(length(first$provenance$history),
                   length(cube$provenance$history) + 1L)
  expect_identical(first$source, cube$source)
  expect_identical(first$dataset_id, cube$dataset_id)
  expect_identical(first$time, cube$time)
  expect_identical(first$provenance$time, cube$provenance$time)
  expect_identical(
    operation$scientific_method,
    list(id = "oceancube:s2_coast_distance", version = "1")
  )
  expect_identical(first$dc, second$dc)
  expect_identical(.provenance_semantic(first$provenance),
                   .provenance_semantic(second$provenance))

  restored <- unserialize(serialize(first, NULL))
  expect_identical(restored$dc, first$dc)
  expect_true(.provenance_validate(restored$provenance, strict = TRUE)$valid)
  expect_identical(.provenance_semantic(restored$provenance),
                   .provenance_semantic(first$provenance))
  path <- tempfile(fileext = ".rds")
  withr::local_file(path)
  saveRDS(first, path)
  from_rds <- readRDS(path)
  expect_identical(from_rds$dc, first$dc)
  expect_identical(.provenance_semantic(from_rds$provenance),
                   .provenance_semantic(first$provenance))

  selected <- range(first$dc, finite = TRUE)
  mask <- stock_mask(first, stock = "a4r", dc = selected)
  stock <- crop_stock(first, mask)
  expect_s3_class(mask, "ocean_mask")
  expect_s3_class(stock, "stock_cube")
  expect_identical(stock$source, cube$source)
  expect_identical(stock$time, cube$time)
  expect_identical(stock$dc, first$dc)
  expect_identical(stock$mask, mask)
  expect_true(.provenance_validate(stock$provenance, strict = TRUE)$valid)
  expect_identical(
    vapply(tail(stock$provenance$history, 2L), `[[`, character(1L), "operation"),
    c("coast_dist", "crop_stock")
  )
})
