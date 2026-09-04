c5_gradient <- function(
    gradients, unit = "m", support_relation = NULL, support_gap_m = NULL) {
  n <- length(gradients)
  physical_depths <- seq(0, by = 10, length.out = n + 1L)
  source_values <- c(0, cumsum(seq_len(n) * 10))
  source_depths <- if (identical(unit, "km")) physical_depths / 1000 else physical_depths
  path <- make_cf_vertical_fixture(
    values = source_depths, units = unit, cell_methods = "z: point",
    data_values = rep(source_values, each = 4L), variable_units = "K"
  )
  result <- depth_gradient(read_nc(path, vars = "temperature"))
  for (i in seq_len(n)) result$data[, , i, , ] <- gradients[[i]]
  descriptor <- result$metadata$cf$current$vertical_gradient
  if (!is.null(support_relation)) {
    descriptor$support_relation <- support_relation
    descriptor$support_gap_m <- support_gap_m
    result$metadata$cf$current$vertical_gradient <- descriptor
  }
  result
}

c5_physical_gradient <- function(unit = "m", descending = FALSE) {
  physical_depths <- c(0, 10, 20, 30, 40)
  gradients <- c(1, 4, -7, 2)
  source_values <- c(0, cumsum(gradients * diff(physical_depths)))
  source_depths <- if (identical(unit, "km")) physical_depths / 1000 else physical_depths
  if (isTRUE(descending)) {
    source_depths <- rev(source_depths)
    source_values <- rev(source_values)
  }
  path <- make_cf_vertical_fixture(
    values = source_depths, units = unit, cell_methods = "z: point",
    data_values = rep(source_values, each = 4L), variable_units = "K"
  )
  depth_gradient(read_nc(path, vars = "temperature"))
}

c5_cell_gradient <- function(
    gradients = c(2, 10, 3),
    bounds = rbind(c(0, 10), c(10, 20), c(30, 40), c(40, 50))) {
  centres <- rowMeans(bounds)
  values <- c(0, cumsum(gradients * diff(centres)))
  path <- make_cf_vertical_fixture(
    values = centres, units = "m", bounds = bounds, bounds_units = "m",
    cell_methods = "z: mean", data_values = rep(values, each = 4L),
    variable_units = "K"
  )
  depth_gradient(read_nc(path, vars = "temperature"))
}

c5_multi_path <- function() {
  path <- tempfile("oceancube-c5-multi-", fileext = ".nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", c(-80, -79))
  lat <- ncdf4::ncdim_def("lat", "degrees_north", c(-12, -11))
  z <- ncdf4::ncdim_def("z", "m", c(0, 10, 20, 30))
  time <- ncdf4::ncdim_def("time", "days since 2000-01-01", c(0, 31))
  nc <- ncdf4::nc_create(path, list(
    ncdf4::ncvar_def("temperature", "K", list(lon, lat, z, time)),
    ncdf4::ncvar_def("oxygen", "mmol m-3", list(lon, lat, z, time))
  ))
  arrays <- lapply(0:1, function(variable_offset) {
    out <- array(NA_real_, c(2, 2, 4, 2))
    for (tt in 1:2) for (jj in 1:2) for (ii in 1:2) {
      feature <- 1L + ((ii - 1L) + 2L * (jj - 1L) +
        4L * (tt - 1L) + variable_offset) %% 3L
      gradient <- rep(1, 3L)
      gradient[[feature]] <- 10
      out[ii, jj, , tt] <- c(0, cumsum(gradient * 10))
    }
    out
  })
  ncdf4::ncvar_put(nc, "temperature", arrays[[1L]])
  ncdf4::ncvar_put(nc, "oxygen", arrays[[2L]])
  for (variable in c("temperature", "oxygen")) {
    ncdf4::ncatt_put(nc, variable, "cell_methods", "z: point")
  }
  ncdf4::ncatt_put(nc, "z", "standard_name", "depth")
  ncdf4::ncatt_put(nc, "z", "positive", "down")
  ncdf4::ncatt_put(nc, "z", "axis", "Z")
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.13")
  ncdf4::nc_close(nc)
  path
}

test_that("C5 exposes one exact public signature", {
  expect_identical(
    names(formals(depth_feature)), c("x", "polarity", "support")
  )
  expect_identical(length(getNamespaceExports("oceancube")), 48L)
})

test_that("absolute detection preserves sign and complete C4 diagnostics", {
  gradient <- c5_gradient(c(1, -5, 3))
  result <- depth_feature(gradient)
  required <- c(
    "longitude", "latitude", "time", "variable", "variable_unit",
    "polarity", "support_policy", "feature_depth", "depth_unit",
    "feature_depth_m", "source_depth_1", "source_depth_2",
    "source_depth_1_m", "source_depth_2_m", "gradient",
    "gradient_magnitude", "gradient_unit", "spacing_m",
    "support_relation", "support_gap_m", "localization_half_span_m",
    "n_support_eligible", "n_finite_gradient", "gradient_completeness",
    "n_matching_candidates", "n_tied", "status", "certification_status"
  )

  expect_s3_class(result, "data.frame", exact = TRUE)
  expect_equal(nrow(result), 4L)
  expect_true(all(required %in% names(result)))
  expect_equal(result$gradient, rep(-5, 4L))
  expect_equal(result$gradient_magnitude, rep(5, 4L))
  expect_equal(result$feature_depth, rep(15, 4L))
  expect_equal(result$feature_depth_m, rep(15, 4L))
  expect_equal(result$source_depth_1, rep(10, 4L))
  expect_equal(result$source_depth_2, rep(20, 4L))
  expect_equal(result$spacing_m, rep(10, 4L))
  expect_equal(result$localization_half_span_m, rep(5, 4L))
  expect_identical(unique(result$gradient_unit), "K m-1")
  expect_identical(unique(result$depth_unit), "m")
  expect_identical(unique(result$status), "LOCAL_POINT_BRACKET_CANDIDATE")
  expect_identical(result$longitude, c(-80, -79, -80, -79))
  expect_identical(result$latitude, c(-12, -12, -11, -11))
  expect_identical(class(result$time), class(gradient$time))
})

test_that("absolute positive and negative polarity rank independently", {
  gradient <- c5_gradient(c(1, 4, -7, 2))
  absolute <- depth_feature(gradient, "absolute")
  positive <- depth_feature(gradient, "positive")
  negative <- depth_feature(gradient, "negative")

  expect_equal(unique(absolute$gradient), -7)
  expect_equal(unique(absolute$feature_depth_m), 25)
  expect_equal(unique(positive$gradient), 4)
  expect_equal(unique(positive$feature_depth_m), 15)
  expect_equal(unique(negative$gradient), -7)
  expect_equal(unique(negative$feature_depth_m), 25)
  expect_identical(unique(positive$n_matching_candidates), 3L)
  expect_identical(unique(negative$n_matching_candidates), 1L)
})

test_that("flat and polarity-mismatched profiles receive no feature", {
  flat <- depth_feature(c5_gradient(c(0, 0, 0)))
  effective_zero <- depth_feature(c5_gradient(c(1e-9, 0, -1e-9)))
  positive <- depth_feature(c5_gradient(c(-1, -2, -3)), "positive")
  negative <- depth_feature(c5_gradient(c(1, 2, 3)), "negative")

  expect_identical(unique(flat$status), "FLAT_PROFILE")
  expect_true(all(is.na(flat$feature_depth)))
  expect_identical(unique(flat$n_matching_candidates), 0L)
  expect_identical(unique(effective_zero$status), "FLAT_PROFILE")
  expect_identical(unique(positive$status), "NO_MATCHING_POLARITY")
  expect_identical(unique(negative$status), "NO_MATCHING_POLARITY")
  expect_true(all(is.na(positive$gradient)))
  expect_true(all(is.na(negative$gradient)))
})

test_that("positive absolute and negative ties remain ambiguous", {
  positive <- depth_feature(c5_gradient(c(1, 5, 5, 2)), "positive")
  absolute <- depth_feature(c5_gradient(c(1, -5, 5, 2)), "absolute")
  negative <- depth_feature(c5_gradient(c(-4, -1, -4)), "negative")
  near <- depth_feature(c5_gradient(c(5, 5 + 1e-8, 1)), "positive")

  for (result in list(positive, absolute, negative, near)) {
    expect_identical(unique(result$status), "AMBIGUOUS_TIE")
    expect_identical(unique(result$n_tied), 2L)
    expect_true(all(is.na(result$feature_depth)))
    expect_true(all(is.na(result$gradient)))
  }
})

test_that("missing gradients retain observed candidates and completeness", {
  incomplete <- depth_feature(c5_gradient(c(1, NA, 4)))
  missing <- depth_feature(c5_gradient(c(NA, NA, NA)))

  expect_equal(unique(incomplete$gradient), 4)
  expect_equal(unique(incomplete$gradient_completeness), 2 / 3)
  expect_identical(unique(incomplete$n_finite_gradient), 2L)
  expect_identical(
    unique(incomplete$certification_status),
    "OBSERVED_CANDIDATE_INCOMPLETE_PROFILE"
  )
  expect_identical(unique(missing$status), "NO_FINITE_GRADIENT")
  expect_equal(unique(missing$gradient_completeness), 0)
  expect_true(all(is.na(missing$feature_depth)))
})

test_that("local support excludes gaps while all support labels them", {
  gradient <- c5_gradient(
    c(2, 10, 3),
    support_relation = c(
      "CONTIGUOUS_SUPPORT", "GAPPED_SUPPORT", "CONTIGUOUS_SUPPORT"
    ),
    support_gap_m = c(0, 5, 0)
  )
  local <- depth_feature(gradient, support = "local")
  all <- depth_feature(gradient, support = "all")

  expect_equal(unique(local$gradient), 3)
  expect_identical(unique(local$support_relation), "CONTIGUOUS_SUPPORT")
  expect_identical(unique(local$n_support_eligible), 2L)
  expect_equal(unique(all$gradient), 10)
  expect_identical(unique(all$support_relation), "GAPPED_SUPPORT")
  expect_equal(unique(all$support_gap_m), 5)
  expect_identical(unique(all$status), "GAPPED_SECANT_CANDIDATE")
  expect_identical(
    unique(all$certification_status), "CERTIFIED_GAPPED_SECANT_CANDIDATE"
  )
})

test_that("only-gapped local profiles have no support and points remain distinct", {
  gapped <- c5_gradient(
    c(2, 10, 3),
    support_relation = rep("GAPPED_SUPPORT", 3L),
    support_gap_m = c(5, 10, 15)
  )
  local <- depth_feature(gapped, support = "local")
  point <- depth_feature(c5_gradient(c(1, 5, 3)), support = "local")

  expect_identical(unique(local$status), "NO_LOCAL_SUPPORT")
  expect_identical(unique(local$n_support_eligible), 0L)
  expect_true(all(is.na(local$gradient_completeness)))
  expect_true(all(is.na(local$feature_depth)))
  expect_identical(unique(point$support_relation), "POINT_SUPPORT_UNBOUNDED")
  expect_identical(unique(point$status), "LOCAL_POINT_BRACKET_CANDIDATE")
})

test_that("real cell geometry preserves contiguous and gapped support", {
  gradient <- c5_cell_gradient()
  local <- depth_feature(gradient, support = "local")
  all <- depth_feature(gradient, support = "all")

  expect_identical(
    gradient$metadata$cf$current$vertical_gradient$support_relation,
    c("CONTIGUOUS_SUPPORT", "GAPPED_SUPPORT", "CONTIGUOUS_SUPPORT")
  )
  expect_equal(unique(local$gradient), 3)
  expect_equal(unique(all$gradient), 10)
  expect_identical(unique(all$status), "GAPPED_SECANT_CANDIDATE")
})

test_that("storage order and metre-kilometre encoding preserve the feature", {
  metres <- depth_feature(c5_physical_gradient())
  kilometres <- depth_feature(c5_physical_gradient("km"))
  descending <- depth_feature(c5_physical_gradient(descending = TRUE))

  expect_equal(kilometres$feature_depth_m, metres$feature_depth_m)
  expect_equal(kilometres$gradient, metres$gradient)
  expect_equal(kilometres$gradient_magnitude, metres$gradient_magnitude)
  expect_equal(descending$feature_depth_m, metres$feature_depth_m)
  expect_equal(descending$gradient, metres$gradient)
  expect_equal(descending$gradient_magnitude, metres$gradient_magnitude)
  expect_equal(unique(kilometres$feature_depth), 0.025)
  expect_equal(unique(descending$feature_depth), 25)
  expect_equal(unique(descending$source_depth_1), 30)
  expect_equal(unique(descending$source_depth_2), 20)
})

test_that("multiple locations times and variables resolve independently in order", {
  x <- read_nc(c5_multi_path(), vars = c("temperature", "oxygen"))
  gradient <- depth_gradient(x)
  result <- depth_feature(gradient, polarity = "positive")
  temperature <- vapply(1:2, function(tt) {
    unlist(lapply(1:2, function(jj) {
      vapply(1:2, function(ii) {
        1L + ((ii - 1L) + 2L * (jj - 1L) + 4L * (tt - 1L)) %% 3L
      }, integer(1L))
    }))
  }, integer(4L))
  oxygen <- 1L + (temperature %% 3L)

  expect_equal(nrow(result), 16L)
  expect_identical(result$variable, rep(c("temperature", "oxygen"), each = 8L))
  expect_identical(result$gradient_index, c(as.vector(temperature), as.vector(oxygen)))
  expect_true(all(result$gradient == 10))
  expect_identical(result$longitude[1:8], rep(c(-80, -79), 4L))
  expect_identical(result$latitude[1:8], rep(c(-12, -12, -11, -11), 2L))
  expect_identical(result$time[1:8], rep(x$time, each = 4L))

  gradient$data[1, 1, 1, 1, 1] <- NA_real_
  changed <- depth_feature(gradient, polarity = "positive")
  expect_lt(changed$gradient_completeness[[1L]], 1)
  expect_true(all(changed$gradient_completeness[-1L] == 1))
})

test_that("supported gradient selections preserve aligned C4 descriptors", {
  gradient <- depth_gradient(read_nc(
    c5_multi_path(), vars = c("temperature", "oxygen")
  ))
  selected <- cube_slice(
    gradient,
    longitude = -79,
    time = gradient$time[[2L]],
    variable = c("oxygen", "temperature"),
    by = "value"
  )
  selected_variables <- vapply(
    selected$metadata$cf$current$vertical_gradient$variables,
    `[[`, character(1L), "variable"
  )
  depth_selected <- cube_slice(
    gradient, depth = gradient$depth[c(1L, 3L)], by = "value"
  )
  cropped <- cube_crop(gradient, depth = range(gradient$depth[c(1L, 2L)]))

  expect_identical(selected_variables, c("oxygen", "temperature"))
  expect_equal(nrow(depth_feature(selected)), 4L)
  expect_equal(nrow(depth_feature(depth_selected)), 16L)
  expect_identical(
    depth_selected$metadata$cf$current$vertical_gradient$output_depths,
    gradient$depth[c(1L, 3L)]
  )
  expect_identical(
    depth_selected$metadata$cf$current$vertical_gradient$source_pair_indices,
    gradient$metadata$cf$current$vertical_gradient$source_pair_indices[c(1L, 3L)]
  )
  expect_equal(nrow(depth_feature(cropped)), 16L)
})

test_that("stale descriptors fail before feature calculation", {
  gradient <- c5_gradient(c(1, 4, -7))
  bad_depth <- gradient
  bad_depth$metadata$cf$current$vertical_gradient$output_depths[[2L]] <- 999
  bad_variable <- gradient
  bad_variable$metadata$cf$current$vertical_gradient$variables[[1L]]$variable <-
    "not_temperature"

  expect_error(
    depth_feature(bad_depth),
    class = "oceancube_vertical_feature_descriptor_mismatch"
  )
  expect_error(
    depth_feature(bad_variable),
    class = "oceancube_vertical_feature_descriptor_mismatch"
  )
})

test_that("one gradient level is valid C5 input", {
  result <- depth_feature(c5_gradient(-2), polarity = "negative")

  expect_equal(result$gradient, rep(-2, 4L))
  expect_identical(result$n_support_eligible, rep(1L, 4L))
  expect_identical(result$n_tied, rep(1L, 4L))
})

test_that("raw and non-gradient products are rejected without implicit work", {
  point_path <- make_cf_vertical_fixture(
    values = c(0, 10, 20), units = "m", cell_methods = "z: point",
    data_values = rep(c(1, 21, 41), each = 4L), variable_units = "K"
  )
  point <- read_nc(point_path, vars = "temperature")
  sampled <- depth_sample(point, 5)
  woa_path <- test_path(
    "fixtures", "real-data", "noaa-woa23-monthly-vertical-fv1.nc"
  )
  woa <- read_nc(woa_path, vars = "t_an")
  mean_cube <- layer_mean(woa, c(0, 2.5))
  integral <- layer_integral(woa, c(0, 2.5))
  oisst <- read_nc(test_path(
    "fixtures", "real-data", "noaa-oisst21-surface-time-fv1.nc"
  ), vars = "sst")

  for (value in list(point, sampled, woa, mean_cube, integral, oisst)) {
    expect_error(depth_feature(value), class = "oceancube_vertical_feature_input")
  }
})

test_that("C5 performs zero NetCDF payload reads after C4 materialization", {
  path <- test_path(
    "fixtures", "real-data", "noaa-woa23-monthly-vertical-fv1.nc"
  )
  gradient <- depth_gradient(cube_open(path, vars = c("t_an", "s_an")))
  testthat::local_mocked_bindings(
    .cube_read_netcdf = function(...) stop("unexpected NetCDF read"),
    .package = "oceancube"
  )
  result <- depth_feature(gradient, polarity = "negative", support = "all")

  expect_s3_class(result, "data.frame", exact = TRUE)
  expect_identical(
    attr(result, "oceancube_qa")$vertical_feature$netcdf_scientific_payload_reads,
    0L
  )
})

test_that("WOA local/all policies and independent manual ranking agree", {
  path <- test_path(
    "fixtures", "real-data", "noaa-woa23-monthly-vertical-fv1.nc"
  )
  gradient <- depth_gradient(read_nc(path, vars = c("t_an", "s_an")))
  local <- depth_feature(gradient, polarity = "negative", support = "local")
  all <- depth_feature(gradient, polarity = "negative", support = "all")
  resolved_row <- which(!is.na(all$gradient_index))[[1L]]
  row <- all[resolved_row, , drop = FALSE]
  li <- match(row$longitude, gradient$lon)
  la <- match(row$latitude, gradient$lat)
  ti <- match(row$time, gradient$time)
  vi <- match(row$variable, gradient$vars)
  profile <- as.numeric(gradient$data[li, la, , ti, vi])
  tolerance <- 8 * sqrt(.Machine$double.eps) *
    max(1, abs(profile[is.finite(profile)]))
  candidates <- which(is.finite(profile) & profile < -tolerance)
  selected <- candidates[[which.min(profile[candidates])]]
  descriptor <- gradient$metadata$cf$current$vertical_gradient

  expect_true(all(local$status == "NO_LOCAL_SUPPORT"))
  expect_true(all(is.na(local$feature_depth)))
  expect_true(any(all$status == "GAPPED_SECANT_CANDIDATE"))
  expect_identical(class(all$time), class(gradient$time))
  expect_identical(row$support_relation, "GAPPED_SUPPORT")
  expect_identical(row$gradient_index, as.integer(selected))
  expect_equal(row$feature_depth, gradient$depth[[selected]])
  expect_equal(row$gradient, profile[[selected]])
  expect_equal(row$gradient_magnitude, abs(profile[[selected]]))
  expect_equal(row$spacing_m, descriptor$spacing_m[[selected]])
  expect_equal(row$support_gap_m, descriptor$support_gap_m[[selected]])
})

test_that("output provenance QA serialization determinism and privacy hold", {
  gradient <- c5_gradient(c(1, -5, 3))
  first <- depth_feature(gradient)
  second <- depth_feature(gradient)
  restored <- unserialize(serialize(first, NULL))
  path <- tempfile("oceancube-c5-roundtrip-", fileext = ".rds")
  saveRDS(first, path)
  from_rds <- readRDS(path)
  provenance <- attr(first, "oceancube_provenance")
  qa <- attr(first, "oceancube_qa")$vertical_feature
  step <- tail(provenance$history, 1L)[[1L]]

  expect_identical(first, second)
  expect_identical(restored, first)
  expect_identical(from_rds, first)
  expect_identical(step$operation, "depth_feature")
  expect_identical(
    step$scientific_method$id,
    "oceancube:strongest_vertical_gradient_candidate"
  )
  expect_identical(qa$profiles_total, 4L)
  expect_identical(qa$features_resolved, 4L)
  expect_identical(qa$netcdf_scientific_payload_reads, 0L)
  expect_identical(qa$memory_cube_reads, 1L)
  expect_gt(qa$input_gradient_bytes, 0)
  expect_gt(qa$output_table_bytes, 0)
  canonical <- paste(capture.output(str(list(step$parameters, qa))), collapse = " ")
  expect_false(grepl("[A-Za-z]:[/\\\\]", canonical))
  expect_false(grepl("oquispe|hostname|tempfile", canonical, ignore.case = TRUE))
})
