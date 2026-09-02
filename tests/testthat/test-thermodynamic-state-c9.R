c9_variable <- function(values, standard_name, unit, cell_methods = "z: point",
                        long_name = NULL) {
  list(
    values = values, standard_name = standard_name, unit = unit,
    cell_methods = cell_methods, long_name = long_name
  )
}

c9_make_fixture <- function(
    variables,
    depths = c(10, 50, 125, 250, 600, 1000),
    depth_unit = "m",
    lon_values = c(188, 189),
    lat_values = c(4, 20),
    times = 0,
    pressure_standard_name = "sea_water_pressure") {
  path <- tempfile("oceancube-c9-", fileext = ".nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", lon_values)
  lat <- ncdf4::ncdim_def("lat", "degrees_north", lat_values)
  z <- ncdf4::ncdim_def("z", depth_unit, depths)
  time <- ncdf4::ncdim_def("time", "days since 2000-01-01", times)
  definitions <- lapply(names(variables), function(name) {
    ncdf4::ncvar_def(
      name, variables[[name]]$unit, list(lon, lat, z, time),
      missval = -9999, prec = "double"
    )
  })
  nc <- ncdf4::nc_create(path, definitions)
  ncell <- length(lon_values) * length(lat_values) * length(depths) *
    length(times)
  for (name in names(variables)) {
    item <- variables[[name]]
    payload <- if (length(item$values) == length(depths)) {
      rep(item$values, each = length(lon_values) * length(lat_values),
          times = length(times))
    } else {
      item$values
    }
    stopifnot(length(payload) == ncell)
    ncdf4::ncvar_put(nc, name, array(payload, dim = c(
      length(lon_values), length(lat_values), length(depths), length(times)
    )))
    standard_name <- item$standard_name
    if (identical(name, "pressure") && !is.null(pressure_standard_name)) {
      standard_name <- pressure_standard_name
    }
    if (!is.null(standard_name)) {
      ncdf4::ncatt_put(nc, name, "standard_name", standard_name)
    }
    if (!is.null(item$cell_methods)) {
      ncdf4::ncatt_put(nc, name, "cell_methods", item$cell_methods)
    }
    if (!is.null(item$long_name)) {
      ncdf4::ncatt_put(nc, name, "long_name", item$long_name)
    }
  }
  ncdf4::ncatt_put(nc, "z", "standard_name", "depth")
  ncdf4::ncatt_put(nc, "z", "positive", "down")
  ncdf4::ncatt_put(nc, "z", "axis", "Z")
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.13")
  ncdf4::nc_close(nc)
  path
}

c9_profile <- function(
    salinity_standard_name = "sea_water_practical_salinity",
    salinity_unit = "1",
    salinity_values = c(34.5487, 34.7275, 34.8605, 34.6810, 34.5680, 34.5600),
    temperature_standard_name = "sea_water_temperature",
    temperature_unit = "degree_Celsius",
    temperature_values = c(28.7856, 28.4329, 22.8103, 10.2600, 6.8863, 4.4036),
    pressure_values = NULL,
    pressure_unit = "dbar",
    pressure_standard_name = "sea_water_pressure",
    salinity_name = "salinity",
    temperature_name = "temperature",
    cell_methods = "z: point",
    ...) {
  variables <- list(
    c9_variable(salinity_values, salinity_standard_name, salinity_unit,
                cell_methods),
    c9_variable(temperature_values, temperature_standard_name, temperature_unit,
                cell_methods)
  )
  names(variables) <- c(salinity_name, temperature_name)
  if (!is.null(pressure_values)) {
    variables$pressure <- c9_variable(
      pressure_values, pressure_standard_name, pressure_unit, cell_methods
    )
  }
  read_nc(c9_make_fixture(
    variables, pressure_standard_name = pressure_standard_name, ...
  ), vars = names(variables))
}

c9_values <- function(x, variable) {
  as.numeric(x$data[, , , , match(variable, x$vars), drop = FALSE])
}

test_that("C9 exposes one bounded API and deterministic dependency behavior", {
  expect_identical(
    names(formals(thermodynamic_state)),
    c("x", "salinity", "temperature", "pressure", "reference_pressure_dbar")
  )
  expect_identical(length(getNamespaceExports("oceancube")), 47L)
  expect_error(
    thermodynamic_state(c9_profile(), reference_pressure_dbar = -1),
    class = "oceancube_teos10_reference_pressure"
  )
  testthat::local_mocked_bindings(
    .teos_gsw_available = function() FALSE,
    .package = "oceancube"
  )
  expect_error(
    thermodynamic_state(c9_profile()),
    class = "oceancube_teos10_dependency_missing"
  )
})

test_that("C9 rejects gsw below the certified minimum", {
  testthat::local_mocked_bindings(
    .teos_gsw_available = function() TRUE,
    .teos_gsw_version = function() "1.1-0",
    .package = "oceancube"
  )
  expect_error(
    thermodynamic_state(c9_profile()),
    class = "oceancube_teos10_dependency_version"
  )
})

test_that("official GSW vectors pass pressure SA CT rho and sigma0 parity", {
  z <- -c(10, 50, 125, 250, 600, 1000)
  p_expected <- 1e3 * c(
    0.010055726724518, 0.050283543374874, 0.125731858435610,
    0.251540299593468, 0.604210012340727, 1.007990337692001
  )
  SA <- c(34.7118, 34.8915, 35.0256, 34.8472, 34.7366, 34.7324)
  CT <- c(28.8099, 28.4392, 22.7862, 10.2262, 6.8272, 4.3236)
  p <- c(10, 50, 125, 250, 600, 1000)
  expect_equal(gsw::gsw_p_from_z(z, 4), p_expected, tolerance = 1.5e-8)
  expect_equal(
    gsw::gsw_SA_from_SP(
      c(34.5487, 34.7275, 34.8605, 34.6810, 34.5680, 34.5600),
      p, 188, 4
    ),
    c(34.711778344814114, 34.891522618230098, 35.025544862476920,
      34.847229026189588, 34.736628474576051, 34.732363065590846),
    tolerance = 1.5e-8
  )
  expect_equal(
    gsw::gsw_CT_from_t(
      SA, c(28.7856, 28.4329, 22.8103, 10.2600, 6.8863, 4.4036), p
    ),
    c(28.809919826700281, 28.439227816091140, 22.786176893078498,
      10.226189266620782, 6.827213633479988, 4.323575748610455),
    tolerance = 1.5e-8
  )
  expect_equal(
    gsw::gsw_rho(SA, CT, p),
    1e3 * c(1.021839935738108, 1.022262457966867, 1.024427195413316,
            1.027790152759127, 1.029837779000189, 1.032002453224572),
    tolerance = 1.5e-8
  )
  expect_equal(
    gsw::gsw_sigma0(SA, CT),
    c(21.797900819337656, 22.052215404397316, 23.892985307893923,
      26.667608665972011, 27.107380455119710, 27.409748977090885),
    tolerance = 1.5e-8
  )
})

test_that("SP plus in-situ temperature executes the complete state pipeline", {
  source <- c9_profile()
  before <- serialize(source$metadata$cf$source, NULL)
  state <- thermodynamic_state(source)
  expect_s3_class(state, "ocean_cube")
  expect_identical(
    state$vars,
    c("absolute_salinity", "conservative_temperature", "sea_water_pressure",
      "sea_water_density", "sea_water_potential_density")
  )
  expect_identical(unname(unlist(state$units)),
                   c("g kg-1", "degree_Celsius", "dbar", "kg m-3", "kg m-3"))
  expect_equal(c9_values(state, "sea_water_pressure")[seq(1, 24, by = 4)],
               gsw::gsw_p_from_z(-source$depth, 4), tolerance = 1.5e-8)
  expect_true(all(is.finite(state$data)))
  expect_identical(serialize(source$metadata$cf$source, NULL), before)
})

test_that("all six SA or SP and t pt0 or CT paths remain distinct", {
  depth <- c(10, 50, 125, 250, 600, 1000)
  p <- gsw::gsw_p_from_z(-depth, 4)
  SP <- c(34.5487, 34.7275, 34.8605, 34.6810, 34.5680, 34.5600)
  SA <- gsw::gsw_SA_from_SP(SP, p, 188, 4)
  t <- c(28.7856, 28.4329, 22.8103, 10.2600, 6.8863, 4.4036)
  CT <- gsw::gsw_CT_from_t(SA, t, p)
  pt0 <- gsw::gsw_pt_from_CT(SA, CT)
  sa_ct <- thermodynamic_state(c9_profile(
    salinity_standard_name = "sea_water_absolute_salinity",
    salinity_unit = "g kg-1", salinity_values = SA,
    temperature_standard_name = "sea_water_conservative_temperature",
    temperature_values = CT
  ))
  sp_ct <- thermodynamic_state(c9_profile(
    temperature_standard_name = "sea_water_conservative_temperature",
    temperature_values = CT
  ))
  sa_t <- thermodynamic_state(c9_profile(
    salinity_standard_name = "sea_water_absolute_salinity",
    salinity_unit = "g/kg", salinity_values = SA
  ))
  sa_pt <- thermodynamic_state(c9_profile(
    salinity_standard_name = "sea_water_absolute_salinity",
    salinity_unit = "g kg-1", salinity_values = SA,
    temperature_standard_name = "sea_water_potential_temperature",
    temperature_values = pt0
  ))
  sp_pt <- thermodynamic_state(c9_profile(
    temperature_standard_name = "sea_water_potential_temperature",
    temperature_values = pt0
  ))
  for (state in list(sa_ct, sp_ct, sa_t, sa_pt, sp_pt)) {
    expect_equal(c9_values(state, "absolute_salinity")[seq(1, 24, by = 4)],
                 SA, tolerance = 1.5e-8)
    expect_equal(c9_values(state, "conservative_temperature")[seq(1, 24, by = 4)],
                 CT, tolerance = 1.5e-8)
  }
  expect_identical(
    sa_ct$metadata$cf$current$thermodynamic_state$input_path,
    "SA_DIRECT__CT_DIRECT"
  )
})

test_that("explicit dbar and Pa pressure match the derived-pressure path", {
  base <- c9_profile()
  derived <- thermodynamic_state(base)
  pressure <- gsw::gsw_p_from_z(-base$depth, 4)
  explicit_dbar <- thermodynamic_state(c9_profile(pressure_values = pressure),
                                       pressure = "pressure")
  explicit_pa <- thermodynamic_state(c9_profile(
    pressure_values = pressure * 1e4, pressure_unit = "Pa"
  ), pressure = "pressure")
  index <- seq(1, 24, by = 4)
  expect_equal(explicit_dbar$data[1, 1, , , ], derived$data[1, 1, , , ],
               tolerance = 1.5e-8)
  expect_equal(explicit_pa$data[1, 1, , , ], explicit_dbar$data[1, 1, , , ],
               tolerance = 1.5e-8)
  expect_equal(c9_values(explicit_pa, "sea_water_pressure")[index], pressure,
               tolerance = 1.5e-8)
})

test_that("Kelvin Celsius metre kilometre and storage order are invariant", {
  values <- c(28.7856, 28.4329, 22.8103, 10.2600, 6.8863, 4.4036)
  celsius <- thermodynamic_state(c9_profile(temperature_values = values))
  kelvin <- thermodynamic_state(c9_profile(
    temperature_values = values + 273.15, temperature_unit = "K"
  ))
  kilometres <- thermodynamic_state(c9_profile(
    depths = c(10, 50, 125, 250, 600, 1000) / 1000, depth_unit = "km",
    temperature_values = values
  ))
  descending <- thermodynamic_state(c9_profile(
    depths = rev(c(10, 50, 125, 250, 600, 1000)),
    salinity_values = rev(c(34.5487, 34.7275, 34.8605, 34.6810, 34.5680, 34.5600)),
    temperature_values = rev(values)
  ))
  expect_equal(kelvin$data, celsius$data, tolerance = 1.5e-8)
  expect_equal(as.numeric(kilometres$data), as.numeric(celsius$data),
               tolerance = 1.5e-8)
  expect_equal(descending$data[, , 6:1, , , drop = FALSE], celsius$data,
               tolerance = 1.5e-8)
})

test_that("multi-location geography and longitude normalization are preserved", {
  multi <- thermodynamic_state(c9_profile())
  p4 <- c9_values(multi, "sea_water_pressure")[1:4]
  expect_false(isTRUE(all.equal(p4[[1L]], p4[[3L]])))
  west <- thermodynamic_state(c9_profile(lon_values = c(-172, -171)))
  east <- thermodynamic_state(c9_profile(lon_values = c(188, 189)))
  expect_equal(as.numeric(west$data), as.numeric(east$data), tolerance = 1.5e-8)
  expect_identical(west$lon, c(-172, -171))
})

test_that("reference pressure changes potential density only and sigma0 is exact", {
  zero <- thermodynamic_state(c9_profile(), reference_pressure_dbar = 0)
  thousand <- thermodynamic_state(c9_profile(), reference_pressure_dbar = 1000)
  expect_equal(c9_values(zero, "sea_water_density"),
               c9_values(thousand, "sea_water_density"), tolerance = 1.5e-8)
  expect_false(isTRUE(all.equal(
    c9_values(zero, "sea_water_potential_density"),
    c9_values(thousand, "sea_water_potential_density")
  )))
  SA <- c9_values(zero, "absolute_salinity")
  CT <- c9_values(zero, "conservative_temperature")
  expect_equal(c9_values(zero, "sea_water_potential_density") - 1000,
               gsw::gsw_sigma0(SA, CT), tolerance = 1.5e-8)
})

test_that("missingness follows the exact dependency graph", {
  temperature <- c(28.7856, NA, 22.8103, 10.2600, 6.8863, 4.4036)
  state <- thermodynamic_state(c9_profile(temperature_values = temperature))
  index_missing <- 5:8
  expect_true(all(is.finite(c9_values(state, "sea_water_pressure"))))
  expect_true(all(is.na(c9_values(state, "conservative_temperature")[index_missing])))
  expect_true(all(is.na(c9_values(state, "sea_water_density")[index_missing])))
  expect_true(all(is.finite(c9_values(state, "sea_water_density")[-index_missing])))

  SA <- rep(35, 6)
  CT <- rep(10, 6)
  p <- gsw::gsw_p_from_z(-c(10, 50, 125, 250, 600, 1000), 4)
  p[[2L]] <- NA
  explicit <- thermodynamic_state(c9_profile(
    salinity_standard_name = "sea_water_absolute_salinity",
    salinity_unit = "g kg-1", salinity_values = SA,
    temperature_standard_name = "sea_water_conservative_temperature",
    temperature_values = CT, pressure_values = p
  ), pressure = "pressure")
  expect_true(all(is.finite(c9_values(explicit, "absolute_salinity"))))
  expect_true(all(is.finite(c9_values(explicit, "conservative_temperature"))))
  expect_true(all(is.na(c9_values(explicit, "sea_water_density")[index_missing])))
})

test_that("semantic identity never comes from variable names or long_name", {
  wrong_salinity <- c9_profile(
    salinity_standard_name = "sea_water_temperature",
    salinity_name = "salinity"
  )
  expect_error(thermodynamic_state(wrong_salinity),
               class = "oceancube_teos10_salinity_variable")
  wrong_temperature <- c9_profile(
    temperature_standard_name = "moles_of_oxygen_per_unit_mass_in_sea_water",
    temperature_name = "temperature"
  )
  expect_error(thermodynamic_state(wrong_temperature),
               class = "oceancube_teos10_temperature_variable")
  generic <- c9_profile(salinity_standard_name = "sea_water_salinity")
  reference <- c9_profile(salinity_standard_name = "sea_water_reference_salinity")
  expect_error(thermodynamic_state(generic),
               class = "oceancube_teos10_salinity_variable")
  expect_error(thermodynamic_state(reference),
               class = "oceancube_teos10_salinity_variable")
})

test_that("source domains units ambiguity and pressure semantics are strict", {
  expect_error(thermodynamic_state(c9_profile(salinity_values = c(-1, rep(35, 5)))),
               class = "oceancube_teos10_negative_salinity")
  expect_error(thermodynamic_state(c9_profile(salinity_unit = "1e-3")),
               class = "oceancube_teos10_salinity_unit")
  wrong_pressure <- c9_profile(
    pressure_values = rep(10, 6), pressure_standard_name = "air_pressure"
  )
  expect_error(thermodynamic_state(wrong_pressure, pressure = "pressure"),
               class = "oceancube_teos10_pressure_variable")

  variables <- list(
    sp1 = c9_variable(rep(35, 6), "sea_water_practical_salinity", "1"),
    sp2 = c9_variable(rep(35, 6), "sea_water_practical_salinity", "1"),
    t = c9_variable(rep(10, 6), "sea_water_temperature", "degree_Celsius")
  )
  ambiguous <- read_nc(c9_make_fixture(variables), vars = names(variables))
  expect_error(thermodynamic_state(ambiguous),
               class = "oceancube_teos10_salinity_ambiguous")
  expect_s3_class(thermodynamic_state(ambiguous, salinity = "sp1"), "ocean_cube")
})

test_that("inside-funnel profiles pass and outside-funnel states abort", {
  expect_s3_class(thermodynamic_state(c9_profile()), "ocean_cube")
  outside <- c9_profile(
    salinity_standard_name = "sea_water_absolute_salinity",
    salinity_unit = "g kg-1", salinity_values = rep(35, 6),
    temperature_standard_name = "sea_water_conservative_temperature",
    temperature_values = rep(-4, 6)
  )
  expect_error(thermodynamic_state(outside),
               class = "oceancube_teos10_outside_funnel")
})

test_that("cell means and governed incompatible real fixtures reject", {
  cell_mean <- c9_profile(cell_methods = "z: mean")
  expect_error(thermodynamic_state(cell_mean),
               class = "oceancube_teos10_value_semantics_unsupported")
  root <- file.path("fixtures", "real-data")
  woa <- read_nc(file.path(root, "noaa-woa23-monthly-vertical-fv1.nc"),
                 vars = c("t_an", "s_an"))
  expect_error(thermodynamic_state(woa),
               class = "oceancube_teos10_value_semantics_unsupported")
  oisst <- read_nc(file.path(root, "noaa-oisst21-surface-time-fv1.nc"),
                  vars = "sst")
  expect_error(thermodynamic_state(oisst),
               class = "oceancube_teos10_salinity_variable")
  oxygen <- read_nc(
    file.path(root, "noaa-woa23-monthly-oxygen-fv1.nc"), vars = "o_an"
  )
  expect_error(thermodynamic_state(oxygen))
})

test_that("metadata provenance QA and serialization are bounded and truthful", {
  source <- c9_profile()
  state <- thermodynamic_state(source)
  descriptor <- state$metadata$cf$current$thermodynamic_state
  expect_identical(descriptor$schema_name, "oceancube_thermodynamic_state")
  expect_identical(descriptor$schema_version, "1.0.0")
  expect_identical(descriptor$method, "TEOS-10")
  expect_identical(descriptor$reference_pressure_dbar, 0)
  expect_identical(
    vapply(descriptor$output_variables, `[[`, character(1L), "standard_name"),
    c("sea_water_absolute_salinity", "sea_water_conservative_temperature",
      "sea_water_pressure", "sea_water_density", "sea_water_potential_density")
  )
  record <- tail(state$provenance$history, 1L)[[1L]]
  expect_identical(record$operation, "thermodynamic_state")
  expect_identical(record$scientific_method$id,
                   "oceancube:teos10_thermodynamic_state")
  expect_silent(oceancube:::.provenance_validate(state$provenance, strict = TRUE))
  qa <- state$qa$thermodynamic_state
  expect_identical(qa$payload_reads, 1L)
  expect_identical(qa$netcdf_payload_reads, 0L)
  expect_identical(qa$source_variables_read, c("salinity", "temperature"))
  expect_false(any(grepl("D:|oquispe|Temp", capture.output(str(qa)), fixed = FALSE)))
  expect_identical(unserialize(serialize(state, NULL)), state)
  path <- tempfile(fileext = ".rds")
  saveRDS(state, path)
  expect_identical(readRDS(path), state)
})

test_that("deferred NetCDF input performs one bounded multi-variable payload read", {
  variables <- list(
    salinity = c9_variable(
      c(34.5487, 34.7275, 34.8605, 34.6810, 34.5680, 34.5600),
      "sea_water_practical_salinity", "1"
    ),
    temperature = c9_variable(
      c(28.7856, 28.4329, 22.8103, 10.2600, 6.8863, 4.4036),
      "sea_water_temperature", "degree_Celsius"
    )
  )
  deferred <- cube_open(c9_make_fixture(variables), vars = names(variables))
  state <- thermodynamic_state(deferred)
  qa <- state$qa$thermodynamic_state
  expect_identical(qa$payload_reads, 1L)
  expect_identical(qa$netcdf_payload_reads, 1L)
  expect_identical(qa$source_variables_read, c("salinity", "temperature"))
  expect_identical(.cube_backend(state), "memory")
})

test_that("C1 through C8 public contracts and numerical behavior do not change", {
  expect_identical(names(formals(mixed_layer_depth)),
                   c("x", "method", "variable", "reference_depth_m", "threshold", "support"))
  expect_identical(names(formals(oxygen_boundary)),
                   c("x", "threshold", "threshold_unit", "variable", "support"))
  expect_identical(names(formals(transition_layer)),
                   c("x", "diagnostic", "variable", "support"))
  expect_error(
    mixed_layer_depth(c9_profile(), method = "density_threshold"),
    class = "oceancube_mld_method_unsupported"
  )
  temperature <- cube_slice(c9_profile(
    salinity_values = rep(35, 6),
    temperature_values = c(25.1, 25, 24.95, 24.6, 23.5, 22),
    depths = c(0, 10, 20, 30, 40, 50)
  ), variable = "temperature")
  expect_equal(unique(mixed_layer_depth(temperature)$mld_depth_m), 170 / 7)
  expect_identical(unique(transition_layer(
    cube_slice(c9_profile(
      salinity_values = rep(35, 4),
      temperature_values = c(25, 24, 19, 18),
      depths = c(0, 10, 20, 30)
    ), variable = "temperature"),
    "thermocline"
  )$diagnostic_status), "THERMOCLINE_GRADIENT_CANDIDATE")
})
