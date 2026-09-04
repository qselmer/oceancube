c10_variable <- function(values, standard_name, unit) {
  list(values = values, standard_name = standard_name, unit = unit)
}

c10_source <- function(
    sa = c(34.7118, 34.8915, 35.0256, 34.8472, 34.7366, 34.7324),
    ct = c(28.8099, 28.4392, 22.7862, 10.2262, 6.8272, 4.3236),
    pressure = c(10, 50, 125, 250, 600, 1000),
    depths = c(10, 50, 125, 250, 600, 1000),
    depth_unit = "m", latitude = 4) {
  path <- tempfile("oceancube-c10-", fileext = ".nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", 188)
  lat <- ncdf4::ncdim_def("lat", "degrees_north", latitude)
  z <- ncdf4::ncdim_def("z", depth_unit, depths)
  time <- ncdf4::ncdim_def("time", "days since 2000-01-01", 0)
  items <- list(
    sa = c10_variable(sa, "sea_water_absolute_salinity", "g kg-1"),
    ct = c10_variable(ct, "sea_water_conservative_temperature", "degree_Celsius"),
    pressure = c10_variable(pressure, "sea_water_pressure", "dbar")
  )
  definitions <- lapply(names(items), function(name) {
    ncdf4::ncvar_def(name, items[[name]]$unit, list(lon, lat, z, time),
                     missval = -9999, prec = "double")
  })
  nc <- ncdf4::nc_create(path, definitions)
  for (name in names(items)) {
    payload <- items[[name]]$values
    if (length(payload) == length(depths) && length(latitude) > 1L) {
      payload <- rep(payload, each = length(latitude))
    }
    ncdf4::ncvar_put(nc, name, payload)
    ncdf4::ncatt_put(nc, name, "standard_name", items[[name]]$standard_name)
    ncdf4::ncatt_put(nc, name, "cell_methods", "z: point")
  }
  ncdf4::ncatt_put(nc, "z", "standard_name", "depth")
  ncdf4::ncatt_put(nc, "z", "positive", "down")
  ncdf4::ncatt_put(nc, "z", "axis", "Z")
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.13")
  ncdf4::nc_close(nc)
  read_nc(path, vars = names(items))
}

c10_state <- function(...) {
  thermodynamic_state(
    c10_source(...), salinity = "sa", temperature = "ct",
    pressure = "pressure", reference_pressure_dbar = 0
  )
}

c10_replace <- function(state, variable, values) {
  index <- match(variable, state$vars)
  state$data[1, 1, , 1, index] <- values
  state
}

c10_with_bounds <- function(state, bounds) {
  vertical <- state$metadata$cf$current$vertical
  vertical$geometry_status <- "GEOMETRY_METRIC_BOUNDS_SUPPORTED"
  vertical$bounds_target <- "test:z_bnds"
  vertical$bounds_status <- "BOUNDS_VALID"
  vertical$bounds_units_raw <- vertical$units_raw
  vertical$bounds_unit <- vertical$normalized_unit
  vertical$bounds_shape <- as.integer(c(nrow(bounds), 2L))
  vertical$bounds <- lapply(seq_len(nrow(bounds)), function(i) bounds[i, ])
  lower <- pmin(bounds[, 1L], bounds[, 2L])
  upper <- pmax(bounds[, 1L], bounds[, 2L])
  vertical$coverage_contiguous <- all(
    abs(lower[-1L] - upper[-length(upper)]) <= 1e-8
  )
  state$metadata$cf$current$vertical <- vertical
  state
}

test_that("C10 public surface is bounded and legacy signatures do not change", {
  expect_identical(length(getNamespaceExports("oceancube")), 48L)
  expect_true("stratification" %in% getNamespaceExports("oceancube"))
  expect_identical(
    names(formals(stratification)), c("x", "metric", "support")
  )
  expect_identical(
    names(formals(mixed_layer_depth)),
    c("x", "method", "variable", "reference_depth_m", "threshold", "support")
  )
  expect_identical(
    names(formals(transition_layer)), c("x", "diagnostic", "variable", "support")
  )
  expect_error(stratification(c10_state(), metric = "N"),
               class = "oceancube_stratification_metric")
})

test_that("density MLD applies method-aware defaults and positive departures", {
  state <- c10_state(depths = c(10, 20, 30, 40),
                     sa = rep(35, 4), ct = rep(10, 4),
                     pressure = c(10, 20, 30, 40))
  state <- c10_replace(
    state, "sea_water_potential_density", c(1025, 1024.99, 1025.02, 1025.08)
  )
  default <- mixed_layer_depth(state, method = "density_threshold")
  expect_equal(default$threshold_effective, 0.03)
  expect_identical(default$threshold_source, "METHOD_DEFAULT")
  expect_true(default$density_inversion_encountered)
  expect_equal(default$mld_depth_m, 31 + 2 / 3, tolerance = 1e-10)
  explicit <- mixed_layer_depth(
    state, method = "density_threshold", threshold = 0.02
  )
  expect_identical(explicit$threshold_source, "EXPLICIT")
  expect_equal(explicit$mld_depth_m, 30)
  point_two <- mixed_layer_depth(
    state, method = "density_threshold", threshold = 0.2
  )
  expect_equal(point_two$threshold_effective, 0.2)
  expect_identical(point_two$status, "MLD_OPEN_AT_PROFILE_BOTTOM")
  expect_true(is.na(point_two$mld_depth_m))
  expect_equal(
    attr(default, "oceancube_qa")$mixed_layer_depth$
      netcdf_scientific_payload_reads,
    0L
  )
})

test_that("density diagnostics reject uncertified and wrong-reference state", {
  expect_error(
    mixed_layer_depth(c10_source(), method = "density_threshold"),
    class = "oceancube_c10_state_input"
  )
  state_1000 <- thermodynamic_state(
    c10_source(), salinity = "sa", temperature = "ct", pressure = "pressure",
    reference_pressure_dbar = 1000
  )
  expect_error(
    mixed_layer_depth(state_1000, method = "density_threshold"),
    class = "oceancube_c10_state_input"
  )
  expect_error(
    transition_layer(state_1000, diagnostic = "pycnocline"),
    class = "oceancube_c10_state_input"
  )
  expect_error(
    mixed_layer_depth(c10_state(), method = "density_threshold",
                      variable = "sea_water_density"),
    class = "oceancube_mld_density_variable"
  )
})

test_that("pycnocline composes C4 and C5 exactly", {
  state <- c10_state()
  selected <- cube_slice(state, variable = "sea_water_potential_density")
  expected <- depth_feature(
    depth_gradient(selected), polarity = "positive", support = "local"
  )
  result <- transition_layer(state, diagnostic = "pycnocline")
  expect_equal(result$gradient, expected$gradient)
  expect_equal(result$feature_depth_m, expected$feature_depth_m)
  expect_identical(result$diagnostic_status, "PYCNOCLINE_GRADIENT_CANDIDATE")
  expect_identical(result$input_mode, "CERTIFIED_THERMODYNAMIC_STATE")
  expect_false(result$threshold_applied)
  expect_equal(
    attr(result, "oceancube_qa")$transition_layer$
      netcdf_scientific_payload_reads,
    0L
  )
})

test_that("pycnocline retains C5 flat polarity and tie outcomes", {
  flat <- c10_replace(
    c10_state(), "sea_water_potential_density", rep(1025, 6)
  )
  decreasing <- c10_replace(
    c10_state(), "sea_water_potential_density", seq(1025, 1024.5, length.out = 6)
  )
  tied <- c10_replace(
    c10_state(), "sea_water_potential_density",
    1025 + 0.01 * (c(10, 50, 125, 250, 600, 1000) - 10)
  )
  expect_identical(
    transition_layer(flat, diagnostic = "pycnocline")$feature_status,
    "NO_MATCHING_POLARITY"
  )
  expect_identical(
    transition_layer(decreasing, diagnostic = "pycnocline")$feature_status,
    "NO_MATCHING_POLARITY"
  )
  expect_identical(
    transition_layer(tied, diagnostic = "pycnocline")$feature_status,
    "AMBIGUOUS_TIE"
  )
})

test_that("C10 N2 matches the official GSW-R example and preserves p_mid", {
  state <- c10_state()
  result <- stratification(state)
  expected_n2 <- c(
    0.060843209693499, 0.235723066151305, 0.216599928330380,
    0.012941204313372, 0.008434782795209
  ) * 1e-3
  expect_equal(
    as.numeric(result$data[1, 1, , 1, 1]), expected_n2,
    tolerance = 1.5e-8
  )
  expect_equal(
    as.numeric(result$data[1, 1, , 1, 2]),
    c(30, 87.5, 187.5, 425, 800),
    tolerance = 1.5e-8
  )
  expect_identical(
    result$vars,
    c("buoyancy_frequency_squared", "sea_water_pressure_midpoint")
  )
  expect_equal(as.numeric(result$depth), c(30, 87.5, 187.5, 425, 800))
  descriptor <- result$metadata$cf$current$stratification
  expect_identical(descriptor$certification_status, "CERTIFIED_C10_TEOS10_N2")
  expect_identical(descriptor$output_geometry, "GEOMETRY_NO_BOUNDS")
  expect_identical(descriptor$output_bounds_status, "BOUNDS_MISSING")
  expect_null(result$metadata$cf$current$thermodynamic_state)
  expect_equal(result$qa$stratification$netcdf_scientific_payload_reads, 0L)
})

test_that("stratification is storage-order and metric-unit invariant", {
  ascending <- stratification(c10_state())
  descending <- stratification(c10_state(
    sa = rev(c(34.7118, 34.8915, 35.0256, 34.8472, 34.7366, 34.7324)),
    ct = rev(c(28.8099, 28.4392, 22.7862, 10.2262, 6.8272, 4.3236)),
    pressure = rev(c(10, 50, 125, 250, 600, 1000)),
    depths = rev(c(10, 50, 125, 250, 600, 1000))
  ))
  kilometres <- stratification(c10_state(
    depths = c(0.01, 0.05, 0.125, 0.25, 0.6, 1), depth_unit = "km"
  ))
  expect_equal(
    as.numeric(ascending$data[1, 1, , 1, 1]),
    rev(as.numeric(descending$data[1, 1, , 1, 1])),
    tolerance = 1.5e-8
  )
  expect_equal(
    as.numeric(ascending$data[1, 1, , 1, 1]),
    as.numeric(kilometres$data[1, 1, , 1, 1]),
    tolerance = 1.5e-8
  )
})

test_that("stratification calculates every location independently", {
  result <- stratification(c10_state(latitude = c(4, 20)))
  expect_identical(unname(dim(result$data)), c(1L, 2L, 5L, 1L, 2L))
  expect_true(all(is.finite(result$data[, , , , 1L])))
  expect_false(isTRUE(all.equal(
    as.numeric(result$data[1, 1, , 1, 1]),
    as.numeric(result$data[1, 2, , 1, 1])
  )))
  expect_equal(
    as.numeric(result$data[1, 1, , 1, 2]),
    as.numeric(result$data[1, 2, , 1, 2])
  )
})

test_that("stratification rejects invalid pressure geometry and never bridges NA", {
  repeated <- c10_state()
  repeated <- c10_replace(
    repeated, "sea_water_pressure", c(10, 50, 50, 250, 600, 1000)
  )
  expect_error(
    stratification(repeated),
    class = "oceancube_stratification_pressure_geometry"
  )
  missing <- c10_state()
  missing <- c10_replace(
    missing, "absolute_salinity",
    c(34.7118, 34.8915, NA, 34.8472, 34.7366, 34.7324)
  )
  result <- stratification(missing)
  expect_true(all(is.na(result$data[1, 1, 2:3, 1, 1])))
  expect_true(is.finite(result$data[1, 1, 1, 1, 1]))
  expect_true(is.finite(result$data[1, 1, 4, 1, 1]))
})

test_that("C10 gap policy never interpolates and keeps explicit support", {
  state <- c10_state()
  state <- c10_with_bounds(state, rbind(
    c(0, 30), c(30, 80), c(100, 180),
    c(180, 350), c(350, 750), c(750, 1250)
  ))
  local <- stratification(state, support = "local")
  all_support <- stratification(state, support = "all")
  expect_true(is.na(local$data[1, 1, 2, 1, 1]))
  expect_true(is.finite(all_support$data[1, 1, 2, 1, 1]))
  expect_identical(
    local$metadata$cf$current$stratification$support_relation[[2L]],
    "GAPPED_SUPPORT"
  )
  state <- c10_replace(
    state, "sea_water_potential_density",
    c(1025, 1025, 1025.05, 1025.06, 1025.07, 1025.08)
  )
  density <- mixed_layer_depth(
    state, method = "density_threshold", threshold = 0.01,
    reference_depth_m = 50, support = "all"
  )
  expect_identical(density$status, "GAPPED_MLD_THRESHOLD_BRACKET")
  expect_true(is.na(density$mld_depth_m))
})

test_that("C10 outputs preserve serialization", {
  outputs <- list(
    mixed_layer_depth(c10_state(), method = "density_threshold"),
    transition_layer(c10_state(), diagnostic = "pycnocline"),
    stratification(c10_state())
  )
  for (value in outputs) {
    expect_identical(unserialize(serialize(value, NULL)), value)
    path <- tempfile(fileext = ".rds")
    saveRDS(value, path)
    expect_identical(readRDS(path), value)
  }
})
