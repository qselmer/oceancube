cexit_variable <- function(values, standard_name, unit,
                           cell_methods = "z: point") {
  list(values = values, standard_name = standard_name, unit = unit,
       cell_methods = cell_methods)
}

cexit_profile_path <- function(
    variables,
    depths = c(10, 50, 125, 250, 600, 1000),
    depth_unit = "m",
    positive = "down",
    bounds = NULL,
    longitude = 188,
    latitude = 4) {
  path <- tempfile("oceancube-c-exit-", fileext = ".nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", longitude)
  lat <- ncdf4::ncdim_def("lat", "degrees_north", latitude)
  z <- ncdf4::ncdim_def("z", depth_unit, depths)
  time <- ncdf4::ncdim_def("time", "days since 2000-01-01", 0)
  definitions <- lapply(names(variables), function(name) {
    ncdf4::ncvar_def(
      name, variables[[name]]$unit, list(lon, lat, z, time),
      missval = -9999, prec = "double"
    )
  })
  if (!is.null(bounds)) {
    nv <- ncdf4::ncdim_def("nv", "", 1:2, create_dimvar = FALSE)
    definitions <- c(definitions, list(
      ncdf4::ncvar_def("z_bnds", depth_unit, list(nv, z), prec = "double")
    ))
  }
  nc <- ncdf4::nc_create(path, definitions)
  for (name in names(variables)) {
    item <- variables[[name]]
    ncdf4::ncvar_put(
      nc, name,
      array(item$values, dim = c(1, 1, length(depths), 1))
    )
    ncdf4::ncatt_put(nc, name, "standard_name", item$standard_name)
    ncdf4::ncatt_put(nc, name, "cell_methods", item$cell_methods)
  }
  if (!is.null(bounds)) {
    ncdf4::ncvar_put(nc, "z_bnds", t(bounds))
    ncdf4::ncatt_put(nc, "z", "bounds", "z_bnds")
  }
  ncdf4::ncatt_put(nc, "z", "standard_name", "depth")
  ncdf4::ncatt_put(nc, "z", "positive", positive)
  ncdf4::ncatt_put(nc, "z", "axis", "Z")
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.13")
  ncdf4::nc_close(nc)
  path
}

cexit_read <- function(variables, ...) {
  read_nc(cexit_profile_path(variables, ...), vars = names(variables))
}

cexit_temperature <- function(
    values = c(25.1, 25.0, 24.95, 24.6, 23.5, 22),
    depths = c(0, 10, 20, 30, 40, 50), ...) {
  cexit_read(list(temperature = cexit_variable(
    values, "sea_water_temperature", "degree_Celsius"
  )), depths = depths, ...)
}

cexit_oxygen <- function(...) {
  cexit_read(list(oxygen = cexit_variable(
    c(220, 180, 80, 10, 5, 20, 80),
    "moles_of_oxygen_per_unit_mass_in_sea_water", "umol kg-1"
  )), depths = c(0, 20, 40, 60, 80, 100, 150), ...)
}

cexit_teos_source <- function(...) {
  cexit_read(list(
    salinity = cexit_variable(
      c(34.5487, 34.7275, 34.8605, 34.6810, 34.5680, 34.5600),
      "sea_water_practical_salinity", "1"
    ),
    temperature = cexit_variable(
      c(28.7856, 28.4329, 22.8103, 10.2600, 6.8863, 4.4036),
      "sea_water_temperature", "degree_Celsius"
    )
  ), ...)
}

cexit_direct_teos_state <- function(
    sa, ct, pressure,
    depths = pressure,
    latitude = 4) {
  source <- cexit_read(list(
    sa = cexit_variable(
      sa, "sea_water_absolute_salinity", "g kg-1"
    ),
    ct = cexit_variable(
      ct, "sea_water_conservative_temperature", "degree_Celsius"
    ),
    pressure = cexit_variable(
      pressure, "sea_water_pressure", "dbar"
    )
  ), depths = depths, latitude = latitude)
  thermodynamic_state(
    source, salinity = "sa", temperature = "ct", pressure = "pressure",
    reference_pressure_dbar = 0
  )
}

test_that("C-EXIT keeps the complete vertical public surface exact", {
  expected <- list(
    cube_layer_thickness = c("x", "depth_bounds", "unit"),
    cube_cell_volume = c("x", "depth_bounds", "unit"),
    layer_mean = c("x", "depth"),
    layer_integral = c("x", "depth"),
    depth_sample = c("x", "depth", "method"),
    depth_gradient = c("x", "method"),
    depth_feature = c("x", "polarity", "support"),
    transition_layer = c("x", "diagnostic", "variable", "support"),
    oxygen_boundary = c(
      "x", "threshold", "threshold_unit", "variable", "support"
    ),
    mixed_layer_depth = c(
      "x", "method", "variable", "reference_depth_m", "threshold", "support"
    ),
    thermodynamic_state = c(
      "x", "salinity", "temperature", "pressure",
      "reference_pressure_dbar"
    ),
    stratification = c("x", "metric", "support")
  )
  expect_identical(length(getNamespaceExports("oceancube")), 48L)
  for (name in names(expected)) {
    expect_identical(names(formals(get(name, asNamespace("oceancube")))),
                     expected[[name]])
  }
})

test_that("C-EXIT composes bounded cell support without semantic collapse", {
  bounds <- rbind(c(0, 10), c(10, 20), c(20, 30))
  values <- list(temperature = cexit_variable(
    c(1, 3, 5), "sea_water_temperature", "K", "z: mean"
  ))
  metres <- cexit_read(values, depths = c(5, 15, 25), bounds = bounds)
  kilometres <- cexit_read(
    values, depths = c(0.005, 0.015, 0.025), depth_unit = "km",
    bounds = bounds / 1000
  )
  descending <- cexit_read(
    list(temperature = cexit_variable(
      c(5, 3, 1), "sea_water_temperature", "K", "z: mean"
    )), depths = c(25, 15, 5), bounds = bounds[3:1, , drop = FALSE]
  )

  expect_equal(as.numeric(cube_layer_thickness(metres, unit = "m")),
               c(10, 10, 10))
  expect_equal(as.numeric(layer_mean(metres, c(0, 30))$data), 3)
  expect_equal(as.numeric(layer_integral(metres, c(0, 30))$data), 90)
  expect_equal(as.numeric(layer_integral(kilometres, c(0, 0.03))$data), 90)
  expect_equal(as.numeric(layer_integral(descending, c(0, 30))$data), 90)
  expect_false(identical(
    layer_mean(metres, c(0, 30))$metadata$cf$current$vertical_reduction,
    layer_integral(metres, c(0, 30))$metadata$cf$current$vertical_reduction
  ))

  gapped <- cexit_read(
    values, depths = c(5, 20, 35),
    bounds = rbind(c(0, 10), c(15, 25), c(30, 40))
  )
  expect_error(layer_integral(gapped, c(0, 40)),
               class = "oceancube_vertical_partial_coverage")
})

test_that("C-EXIT temperature gradient feature interpretation and MLD agree", {
  source <- cexit_temperature()
  source_cf <- source$metadata$cf$source
  gradient <- depth_gradient(source)
  feature <- depth_feature(gradient, polarity = "negative")
  thermocline <- transition_layer(source, diagnostic = "thermocline")
  mld <- mixed_layer_depth(source)

  expect_equal(feature$feature_depth_m, thermocline$feature_depth_m)
  expect_equal(feature$gradient, thermocline$gradient)
  expect_identical(thermocline$diagnostic_status,
                   "THERMOCLINE_GRADIENT_CANDIDATE")
  expect_equal(mld$mld_depth_m, 170 / 7)
  expect_false(isTRUE(all.equal(mld$mld_depth_m,
                                thermocline$feature_depth_m)))
  expect_identical(source$metadata$cf$source, source_cf)

  positive_up <- cexit_temperature(
    values = rev(c(25.1, 25.0, 24.95, 24.6, 23.5, 22)),
    depths = -rev(c(0, 10, 20, 30, 40, 50)), positive = "up"
  )
  expect_error(
    transition_layer(positive_up, diagnostic = "thermocline"),
    class = "oceancube_transition_input"
  )
})

test_that("C-EXIT oxygen branches and explicit threshold boundaries agree", {
  oxygen <- cexit_oxygen()
  upper <- transition_layer(oxygen, "upper_oxycline")
  lower <- transition_layer(oxygen, "lower_oxycline")
  zone <- oxygen_boundary(oxygen, threshold = 20)

  expect_equal(upper$core_depth_m, zone$core_depth_m)
  expect_equal(lower$core_depth_m, zone$core_depth_m)
  expect_true(upper$feature_depth_m < zone$core_depth_m)
  expect_true(lower$feature_depth_m > zone$core_depth_m)
  expect_identical(zone$upper_boundary_status,
                   "INTERPOLATED_THRESHOLD_CROSSING")
  expect_identical(zone$lower_boundary_status, "EXACT_THRESHOLD_POINT")
  expect_equal(zone$upper_boundary_depth_m, 400 / 7)
  expect_equal(zone$lower_boundary_depth_m, 100)

  incomplete <- cexit_read(list(oxygen = cexit_variable(
    c(220, 180, NA, 10, 5, 20, 80),
    "moles_of_oxygen_per_unit_mass_in_sea_water", "umol kg-1"
  )), depths = c(0, 20, 40, 60, 80, 100, 150))
  expect_identical(oxygen_boundary(incomplete, 20)$zone_status,
                   "INCOMPLETE_OXYGEN_PROFILE")
})

test_that("C-EXIT TEOS state is the sole input to density diagnostics", {
  source <- cexit_teos_source()
  source_cf <- source$metadata$cf$source
  temperature_mld <- mixed_layer_depth(source, method = "temperature_threshold")
  state <- thermodynamic_state(source)
  density_mld <- mixed_layer_depth(state, method = "density_threshold")
  explicit_mld <- mixed_layer_depth(
    state, method = "density_threshold", threshold = 0.05
  )
  explicit_point_two <- mixed_layer_depth(
    state, method = "density_threshold", threshold = 0.2
  )
  pycnocline <- transition_layer(state, diagnostic = "pycnocline")
  n2 <- stratification(state)

  expect_identical(state$metadata$cf$source, source_cf)
  expect_identical(
    state$metadata$cf$current$thermodynamic_state$method, "TEOS-10"
  )
  expect_identical(density_mld$threshold_source, "METHOD_DEFAULT")
  expect_equal(density_mld$threshold_effective, 0.03)
  expect_identical(explicit_mld$threshold_source, "EXPLICIT")
  expect_equal(explicit_mld$threshold_effective, 0.05)
  expect_identical(explicit_point_two$threshold_source, "EXPLICIT")
  expect_equal(explicit_point_two$threshold_effective, 0.2)
  expect_true(is.finite(temperature_mld$mld_depth_m))
  expect_true(is.finite(density_mld$mld_depth_m))
  expect_false(isTRUE(all.equal(
    temperature_mld$mld_depth_m, density_mld$mld_depth_m
  )))
  expect_identical(pycnocline$input_mode,
                   "CERTIFIED_THERMODYNAMIC_STATE")
  expect_identical(n2$vars,
                   c("buoyancy_frequency_squared",
                     "sea_water_pressure_midpoint"))
  expect_false(isTRUE(all.equal(
    as.numeric(n2$depth), as.numeric(n2$data[1, 1, , 1, 2])
  )))
  expect_false(isTRUE(all.equal(pycnocline$feature_depth_m,
                                as.numeric(n2$depth))))
  expect_true(all(vapply(list(state, n2), function(x) is.array(x$data),
                         logical(1))))

  selected <- cube_slice(state, variable = "sea_water_potential_density")
  expect_identical(
    selected$metadata$cf$current$thermodynamic_state$certification_status,
    state$metadata$cf$current$thermodynamic_state$certification_status
  )
  for (value in list(state, density_mld, pycnocline, n2, selected)) {
    expect_identical(unserialize(serialize(value, NULL)), value)
  }
})

test_that("C-EXIT keeps pycnocline and maximum N2 scientifically distinct", {
  state <- cexit_direct_teos_state(
    sa = c(
      34.08887452, 34.11333921, 34.17187431,
      34.14908716, 34.09525982, 34.08470499
    ),
    ct = c(
      23.928065590, 23.029194494, 20.167105222,
      14.297609209, 7.231717260, 4.156990057
    ),
    pressure = c(10, 50, 125, 250, 600, 1000)
  )
  pycnocline <- transition_layer(state, diagnostic = "pycnocline")
  n2 <- stratification(state)
  n2_values <- as.numeric(n2$data[1, 1, , 1, 1])
  n2_max_depth_m <- n2$depth[[which.max(n2_values)]]

  expect_equal(pycnocline$feature_depth_m, 87.5)
  expect_equal(n2_max_depth_m, 187.5)
  expect_false(isTRUE(all.equal(pycnocline$feature_depth_m, n2_max_depth_m)))
})

test_that("C-EXIT preserves negative N2 through selection and serialization", {
  state <- cexit_direct_teos_state(
    sa = rep(35, 4), ct = c(10, 11, 12, 13),
    pressure = c(10, 20, 30, 40)
  )
  n2 <- stratification(state)
  selected <- cube_slice(n2, variable = "buoyancy_frequency_squared")

  expect_true(any(n2$data[1, 1, , 1, 1] < 0, na.rm = TRUE))
  expect_true(any(selected$data < 0, na.rm = TRUE))
  expect_identical(
    selected$metadata$cf$current$stratification$certification_status,
    n2$metadata$cf$current$stratification$certification_status
  )
  expect_identical(unserialize(serialize(n2, NULL)), n2)
  expect_identical(unserialize(serialize(selected, NULL)), selected)
  path <- tempfile(fileext = ".rds")
  saveRDS(n2, path)
  expect_identical(readRDS(path), n2)
})
