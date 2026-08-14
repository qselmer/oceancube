.make_temporal_metadata_cube <- function() {
  cube <- .make_baseline_fixture()$cube
  cube$source <- "synthetic-temporal"
  cube$dataset_id <- "temporal-fixture"
  cube$mask <- list(id = "mask-fixture")
  cube$dc <- matrix(seq_len(6), nrow = 3, ncol = 2)
  cube$provenance$step <- "input"
  cube$qa <- list(status = "raw")
  cube
}

test_that("cube shape comes only from the canonical coordinate header", {
  cube <- .make_baseline_fixture()$cube
  data_independent <- cube
  data_independent$data <- "not a resident array"

  expect_identical(
    .cube_shape(cube),
    c(
      longitude = 3L,
      latitude = 2L,
      depth = 2L,
      time = 4L,
      variable = 2L
    )
  )
  expect_identical(.cube_shape(data_independent), .cube_shape(cube))
  expect_error(.cube_shape(list()), "must be an <ocean_cube>")
})

test_that("to_month preserves monthly calculations, structure, and metadata policy", {
  cube <- .make_temporal_metadata_cube()
  before <- cube

  monthly <- to_month(cube)

  expect_s3_class(monthly, "ocean_cube")
  expect_identical(class(monthly), c("ocean_cube", "list"))
  expect_identical(
    names(monthly),
    c(
      "lon", "lat", "depth", "time", "vars", "data", "units", "source",
      "dataset_id", "spatial_extent", "temporal_extent", "depth_extent",
      "mask", "dc", "climatology", "anomaly", "provenance", "qa"
    )
  )
  expect_identical(unname(dim(monthly$data)), c(3L, 2L, 2L, 14L, 2L))
  expect_identical(
    monthly$time,
    seq(as.Date("2020-01-01"), as.Date("2021-02-01"), by = "month")
  )
  expect_identical(monthly$lon, cube$lon)
  expect_identical(monthly$lat, cube$lat)
  expect_identical(monthly$depth, cube$depth)
  expect_identical(monthly$vars, cube$vars)
  expect_identical(monthly$units, cube$units)
  expect_identical(monthly$source, cube$source)
  expect_identical(monthly$dataset_id, cube$dataset_id)
  expect_identical(monthly$spatial_extent, cube$spatial_extent)
  expect_identical(monthly$depth_extent, cube$depth_extent)
  expect_identical(monthly$mask, cube$mask)
  expect_identical(monthly$dc, cube$dc)
  expect_identical(monthly$provenance$extra$parent, cube$provenance)
  expect_identical(monthly$qa$parent, cube$qa)
  expect_true(monthly$qa$to_month$core_delegated)
  expect_identical(
    monthly$qa$temporal_aggregation$periods$n_total,
    c(1L, 1L, rep(0L, 10L), 1L, 1L)
  )
  expect_identical(.cube_backend(monthly), "memory")
  expect_identical(cube, before)
})

test_that("to_month applies the supplied function to repeated monthly dates", {
  cube <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = 0,
    time = as.Date(c("2020-01-01", "2020-01-15", "2020-02-01")),
    data = array(c(1, 3, 10), dim = c(1, 1, 1, 3, 1)),
    vars = "temperature"
  )

  averaged <- to_month(cube)
  maximized <- to_month(cube, fun = max)

  expect_identical(averaged$time, as.Date(c("2020-01-01", "2020-02-01")))
  expect_equal(as.vector(averaged$data), c(mean(c(1, 3)), 10))
  expect_equal(as.vector(maximized$data), c(max(c(1, 3)), 10))
  expect_identical(unname(dim(averaged$data)), c(1L, 1L, 1L, 2L, 1L))
  expect_identical(.cube_backend(averaged), "memory")
})

test_that("clim_month preserves independent monthly statistics and structure", {
  cube <- .make_temporal_metadata_cube()
  before <- cube
  january <- c(11111, 13111)

  clim <- suppressWarnings(clim_month(cube))

  expect_s3_class(clim, "ocean_clim")
  expect_identical(class(clim), c("ocean_clim", "list"))
  expect_identical(
    names(clim),
    c(
      "lon", "lat", "depth", "vars", "period", "mean", "sd", "n",
      "units", "source", "dataset_id", "provenance"
    )
  )
  expect_null(clim$scale)
  expect_identical(unname(dim(clim$mean)), c(3L, 2L, 2L, 12L, 2L))
  expect_identical(dim(clim$sd), dim(clim$mean))
  expect_identical(dim(clim$n), dim(clim$mean))
  expect_equal(clim$mean[1, 1, 1, 1, 1], mean(january))
  expect_equal(clim$sd[1, 1, 1, 1, 1], stats::sd(january))
  expect_identical(clim$n[1, 1, 1, 1, 1], 2L)
  expect_true(is.na(clim$mean[1, 1, 1, 3, 1]))
  expect_identical(clim$n[1, 1, 1, 3, 1], 0L)
  expect_identical(dimnames(clim$mean)[[4]], sprintf("%02d", 1:12))
  expect_identical(clim$period, range(cube$time))
  expect_identical(clim$lon, cube$lon)
  expect_identical(clim$lat, cube$lat)
  expect_identical(clim$depth, cube$depth)
  expect_identical(clim$vars, cube$vars)
  expect_identical(clim$units, cube$units)
  expect_identical(clim$source, cube$source)
  expect_identical(clim$dataset_id, cube$dataset_id)
  expect_identical(clim$provenance$extra$parent, cube$provenance)
  expect_match(capture.output(print(clim))[1], "month climatology", fixed = TRUE)
  expect_identical(cube, before)
})

test_that("clim_day preserves feb28, drop, and keep leap policies", {
  time <- as.Date(c("2019-02-28", "2020-02-28", "2020-02-29", "2021-02-28"))
  values <- c(10, 20, 30, 40)
  cube <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = 0,
    time = time,
    data = array(values, dim = c(1, 1, 1, 4, 1)),
    vars = "temperature",
    units = "degC",
    source = "daily-fixture",
    dataset_id = "leap-policy"
  )
  before <- cube

  feb28 <- suppressWarnings(clim_day(cube, leap = "feb28"))
  drop <- suppressWarnings(clim_day(cube, leap = "drop"))
  keep <- suppressWarnings(clim_day(cube, leap = "keep"))
  j_feb28 <- match("02-28", feb28$day)
  j_drop <- match("02-28", drop$day)
  j_keep28 <- match("02-28", keep$day)
  j_keep29 <- match("02-29", keep$day)

  expect_identical(class(feb28), c("ocean_clim", "list"))
  expect_identical(
    names(feb28),
    c(
      "scale", "lon", "lat", "depth", "vars", "period", "day", "leap",
      "min_n", "mean", "sd", "n", "units", "source", "dataset_id",
      "provenance"
    )
  )
  expect_identical(feb28$scale, "day")
  expect_identical(feb28$leap, "feb28")
  expect_identical(drop$leap, "drop")
  expect_identical(keep$leap, "keep")
  expect_identical(length(feb28$day), 365L)
  expect_identical(length(drop$day), 365L)
  expect_identical(length(keep$day), 366L)
  expect_equal(feb28$mean[1, 1, 1, j_feb28, 1], mean(values))
  expect_equal(feb28$sd[1, 1, 1, j_feb28, 1], stats::sd(c(10, 25, 40)))
  expect_identical(feb28$n[1, 1, 1, j_feb28, 1], 3L)
  expect_equal(drop$mean[1, 1, 1, j_drop, 1], mean(c(10, 20, 40)))
  expect_identical(drop$n[1, 1, 1, j_drop, 1], 3L)
  expect_equal(keep$mean[1, 1, 1, j_keep28, 1], mean(c(10, 20, 40)))
  expect_equal(keep$mean[1, 1, 1, j_keep29, 1], 30)
  expect_identical(keep$n[1, 1, 1, j_keep29, 1], 1L)
  expect_true(is.na(keep$sd[1, 1, 1, j_keep29, 1]))
  expect_true(is.na(anom_diff(cube, drop)$data[1, 1, 1, 3, 1]))
  expect_identical(feb28$units, cube$units)
  expect_identical(feb28$source, cube$source)
  expect_identical(feb28$dataset_id, cube$dataset_id)
  expect_identical(cube, before)
})

test_that("absolute and standardized anomalies remain structurally and numerically equivalent", {
  cube <- .make_temporal_metadata_cube()
  before <- cube
  clim <- suppressWarnings(clim_month(cube))
  january <- c(11111, 13111)
  expected_diff <- january[[1]] - mean(january)
  expected_z <- expected_diff / stats::sd(january)

  difference <- anom_diff(cube, clim)
  z_score <- anom_z(cube, clim)

  expect_identical(class(difference), c("ocean_anom", "ocean_cube", "list"))
  expect_identical(class(z_score), class(difference))
  expect_identical(names(difference), names(cube))
  expect_identical(unname(dim(difference$data)), c(3L, 2L, 2L, 4L, 2L))
  expect_identical(dim(difference$data), dim(cube$data))
  expect_identical(dimnames(difference$data), dimnames(cube$data))
  expect_equal(difference$data[1, 1, 1, 1, 1], expected_diff)
  expect_equal(z_score$data[1, 1, 1, 1, 1], expected_z)
  expect_equal(
    as.vector(apply(difference$data, c(1, 2, 3, 5), mean)),
    rep(0, 24)
  )
  expect_identical(difference$lon, cube$lon)
  expect_identical(difference$lat, cube$lat)
  expect_identical(difference$depth, cube$depth)
  expect_identical(difference$time, cube$time)
  expect_identical(difference$vars, cube$vars)
  expect_identical(difference$units, cube$units)
  expect_identical(
    z_score$units,
    stats::setNames(rep("1", length(cube$vars)), cube$vars)
  )
  expect_identical(difference$source, cube$source)
  expect_identical(difference$dataset_id, cube$dataset_id)
  expect_identical(difference$spatial_extent, cube$spatial_extent)
  expect_identical(difference$temporal_extent, cube$temporal_extent)
  expect_identical(difference$depth_extent, cube$depth_extent)
  expect_identical(difference$mask, cube$mask)
  expect_identical(difference$dc, cube$dc)
  expect_identical(difference$climatology, clim)
  expect_identical(difference$anomaly$method, "difference")
  expect_identical(z_score$anomaly$method, "z_score")
  expect_identical(difference$provenance$extra$parent, cube$provenance)
  expect_true(is.list(difference$qa))
  expect_true(is.list(difference$qa$anomaly))
  expect_identical(.cube_backend(difference), "memory")
  expect_identical(.cube_backend(z_score), "memory")
  expect_identical(cube, before)
})

test_that("signal_noise preserves absolute and signed scientific meanings", {
  cube <- .make_baseline_fixture()$cube
  clim <- suppressWarnings(clim_month(cube))
  january <- c(11111, 13111)
  expected_signed <- (january[[1]] - mean(january)) / stats::sd(january)

  absolute <- signal_noise(cube, clim)
  signed <- signal_noise(cube, clim, signed = TRUE)

  expect_identical(class(absolute), c("ocean_anom", "ocean_cube", "list"))
  expect_identical(class(signed), class(absolute))
  expect_equal(absolute$data[1, 1, 1, 1, 1], abs(expected_signed))
  expect_equal(signed$data[1, 1, 1, 1, 1], expected_signed)
  expect_identical(
    absolute$anomaly$method,
    "standardized_anomaly_magnitude"
  )
  expect_identical(signed$anomaly$method, "standardized_anomaly")
  expected_units <- stats::setNames(rep("1", length(cube$vars)), cube$vars)
  expect_identical(absolute$units, expected_units)
  expect_identical(signed$units, expected_units)
  expect_identical(dim(absolute$data), dim(cube$data))
  expect_identical(dimnames(absolute$data), dimnames(cube$data))
  expect_identical(.cube_backend(absolute), "memory")
  expect_identical(.cube_backend(signed), "memory")
})

test_that("zero climatological variation stays missing rather than non-finite", {
  time <- as.Date(c("2020-01-01", "2020-02-01", "2021-01-01", "2021-02-01"))
  cube <- ocean_cube(
    lon = -80,
    lat = -12,
    depth = 0,
    time = time,
    data = array(c(5, 7, 5, 7), dim = c(1, 1, 1, 4, 1)),
    vars = "temperature"
  )

  z_score <- suppressWarnings(
    anom_z(cube, suppressWarnings(clim_month(cube)))
  )

  expect_true(all(is.na(z_score$data)))
  expect_false(any(is.infinite(z_score$data)))
  expect_false(any(is.nan(z_score$data)))
})

test_that("temporal functions fail through backend dispatch for an unknown backend", {
  cube <- .make_baseline_fixture()$cube
  monthly_clim <- suppressWarnings(clim_month(cube))

  local_mocked_bindings(
    .cube_backend = function(x) "unknown",
    .package = "oceancube"
  )

  expect_error(to_month(cube), "Unsupported ocean_cube backend: 'unknown'")
  expect_error(clim_month(cube), "Unsupported ocean_cube backend: 'unknown'")
  expect_error(clim_day(cube), "Unsupported ocean_cube backend: 'unknown'")
  expect_error(anom_diff(cube, monthly_clim), "Unsupported ocean_cube backend: 'unknown'")
  expect_error(anom_z(cube, monthly_clim), "Unsupported ocean_cube backend: 'unknown'")
  expect_error(signal_noise(cube, monthly_clim), "Unsupported ocean_cube backend: 'unknown'")
})

test_that("public temporal signatures remain unchanged", {
  expect_identical(names(formals(to_month)), c("x", "fun"))
  expect_identical(names(formals(clim_month)), c("x", "period"))
  expect_identical(names(formals(clim_day)), c("x", "period", "leap", "min_n"))
  expect_identical(names(formals(anom_diff)), c("x", "clim"))
  expect_identical(names(formals(anom_z)), c("x", "clim"))
  expect_identical(names(formals(signal_noise)), c("x", "clim", "signed"))
})
