stopifnot(requireNamespace("oceancube", quietly = TRUE))
stopifnot(requireNamespace("gsw", quietly = TRUE))
stopifnot(requireNamespace("ncdf4", quietly = TRUE))

c10_smoke_variable <- function(values, standard_name, unit) {
  list(values = values, standard_name = standard_name, unit = unit)
}

c10_smoke_file <- function(
    variables, depths = c(10, 50, 125, 250, 600, 1000),
    depth_unit = "m", latitude = 4, bounds = NULL) {
  path <- tempfile("oceancube-c10-installed-", fileext = ".nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", 188)
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
    payload <- item$values
    if (length(payload) == length(depths) && length(latitude) > 1L) {
      payload <- rep(payload, each = length(latitude))
    }
    ncdf4::ncvar_put(nc, name, payload)
    ncdf4::ncatt_put(nc, name, "standard_name", item$standard_name)
    ncdf4::ncatt_put(nc, name, "cell_methods", "z: point")
  }
  if (!is.null(bounds)) {
    ncdf4::ncvar_put(nc, "z_bnds", t(bounds))
    ncdf4::ncatt_put(nc, "z", "bounds", "z_bnds")
  }
  ncdf4::ncatt_put(nc, "z", "standard_name", "depth")
  ncdf4::ncatt_put(nc, "z", "positive", "down")
  ncdf4::ncatt_put(nc, "z", "axis", "Z")
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.13")
  ncdf4::nc_close(nc)
  path
}

c10_smoke_source <- function(
    sa = c(34.7118, 34.8915, 35.0256, 34.8472, 34.7366, 34.7324),
    ct = c(28.8099, 28.4392, 22.7862, 10.2262, 6.8272, 4.3236),
    pressure = c(10, 50, 125, 250, 600, 1000), ...) {
  file <- c10_smoke_file(list(
    sa = c10_smoke_variable(sa, "sea_water_absolute_salinity", "g kg-1"),
    ct = c10_smoke_variable(
      ct, "sea_water_conservative_temperature", "degree_Celsius"
    ),
    pressure = c10_smoke_variable(
      pressure, "sea_water_pressure", "dbar"
    )
  ), ...)
  oceancube::read_nc(file, vars = c("sa", "ct", "pressure"))
}

c10_smoke_state <- function(...) {
  oceancube::thermodynamic_state(
    c10_smoke_source(...), salinity = "sa", temperature = "ct",
    pressure = "pressure", reference_pressure_dbar = 0
  )
}

c10_smoke_rejects <- function(expr, class = NULL) {
  condition <- tryCatch({force(expr); NULL}, error = identity)
  !is.null(condition) && (is.null(class) || inherits(condition, class))
}

exports <- getNamespaceExports("oceancube")
stopifnot(length(exports) == 48L, "stratification" %in% exports)
stopifnot(identical(
  names(formals(oceancube::stratification)), c("x", "metric", "support")
))
stopifnot(identical(
  names(formals(oceancube::mixed_layer_depth)),
  c("x", "method", "variable", "reference_depth_m", "threshold", "support")
))
stopifnot(identical(
  names(formals(oceancube::transition_layer)),
  c("x", "diagnostic", "variable", "support")
))

official <- c10_smoke_state()
density_default <- oceancube::mixed_layer_depth(
  official, method = "density_threshold"
)
density_explicit <- oceancube::mixed_layer_depth(
  official, method = "density_threshold", threshold = 0.2
)
stopifnot(
  density_default$threshold_effective == 0.03,
  identical(density_default$threshold_source, "METHOD_DEFAULT"),
  density_explicit$threshold_effective == 0.2,
  identical(density_explicit$threshold_source, "EXPLICIT")
)

inversion <- c10_smoke_state(
  sa = rep(35, 4), ct = c(20, 21, 19, 15),
  pressure = c(10, 20, 30, 40), depths = c(10, 20, 30, 40)
)
inversion_mld <- oceancube::mixed_layer_depth(
  inversion, method = "density_threshold"
)
stopifnot(
  inversion_mld$density_inversion_encountered,
  is.finite(inversion_mld$mld_depth_m)
)

wrong_reference <- oceancube::thermodynamic_state(
  c10_smoke_source(), salinity = "sa", temperature = "ct",
  pressure = "pressure", reference_pressure_dbar = 1000
)
stopifnot(c10_smoke_rejects(
  oceancube::mixed_layer_depth(
    wrong_reference, method = "density_threshold"
  ),
  "oceancube_c10_state_input"
))

pycnocline <- oceancube::transition_layer(
  official, diagnostic = "pycnocline"
)
flat <- c10_smoke_state(
  sa = rep(35, 4), ct = rep(10, 4),
  pressure = c(10, 20, 30, 40), depths = c(10, 20, 30, 40)
)
flat_pycnocline <- oceancube::transition_layer(flat, diagnostic = "pycnocline")
decreasing_density <- c10_smoke_state(
  sa = rep(35, 4), ct = c(10, 11, 12, 13),
  pressure = c(10, 20, 30, 40), depths = c(10, 20, 30, 40)
)
inverted_pycnocline <- oceancube::transition_layer(
  decreasing_density, diagnostic = "pycnocline"
)
target_density <- 1025:1028
tie_sa <- vapply(target_density, function(target) {
  stats::uniroot(
    function(value) gsw::gsw_rho(value, 10, 0) - target,
    interval = c(0, 50), tol = 1e-12
  )$root
}, numeric(1L))
tie <- c10_smoke_state(
  sa = tie_sa, ct = rep(10, 4), pressure = c(10, 20, 30, 40),
  depths = c(10, 20, 30, 40)
)
tie_pycnocline <- oceancube::transition_layer(tie, diagnostic = "pycnocline")
stopifnot(
  identical(pycnocline$diagnostic_status, "PYCNOCLINE_GRADIENT_CANDIDATE"),
  identical(flat_pycnocline$feature_status, "NO_MATCHING_POLARITY"),
  identical(inverted_pycnocline$feature_status, "NO_MATCHING_POLARITY"),
  identical(tie_pycnocline$feature_status, "AMBIGUOUS_TIE")
)

gap_bounds <- rbind(
  c(0, 30), c(30, 80), c(100, 180),
  c(180, 350), c(350, 750), c(750, 1250)
)
gap_state <- c10_smoke_state(bounds = gap_bounds)
gap_pycnocline <- oceancube::transition_layer(
  gap_state, diagnostic = "pycnocline", support = "all"
)
stopifnot(any(grepl("GAPPED", gap_pycnocline$diagnostic_status)))

n2 <- oceancube::stratification(official)
official_n2 <- c(
  0.060843209693499, 0.235723066151305, 0.216599928330380,
  0.012941204313372, 0.008434782795209
) * 1e-3
official_p_mid <- c(30, 87.5, 187.5, 425, 800)
stopifnot(
  max(abs(as.numeric(n2$data[1, 1, , 1, 1]) - official_n2)) < 1.5e-8,
  max(abs(as.numeric(n2$data[1, 1, , 1, 2]) - official_p_mid)) < 1.5e-8,
  identical(n2$vars, c(
    "buoyancy_frequency_squared", "sea_water_pressure_midpoint"
  ))
)

unstable <- oceancube::stratification(decreasing_density)
stopifnot(any(unstable$data[, , , , 1] < 0, na.rm = TRUE))

missing <- c10_smoke_state(
  sa = c(34.7118, 34.8915, NA, 34.8472, 34.7366, 34.7324)
)
missing_n2 <- oceancube::stratification(missing)
stopifnot(
  is.finite(missing_n2$data[1, 1, 1, 1, 1]),
  all(is.na(missing_n2$data[1, 1, 2:3, 1, 1])),
  is.finite(missing_n2$data[1, 1, 4, 1, 1])
)

repeated <- c10_smoke_state(pressure = c(10, 50, 50, 250, 600, 1000))
decreasing_p <- c10_smoke_state(
  pressure = c(10, 50, 40, 250, 600, 1000)
)
stopifnot(
  c10_smoke_rejects(
    oceancube::stratification(repeated),
    "oceancube_stratification_pressure_geometry"
  ),
  c10_smoke_rejects(
    oceancube::stratification(decreasing_p),
    "oceancube_stratification_pressure_geometry"
  )
)

gap_local <- oceancube::stratification(gap_state, support = "local")
gap_all <- oceancube::stratification(gap_state, support = "all")
stopifnot(
  is.na(gap_local$data[1, 1, 2, 1, 1]),
  is.finite(gap_all$data[1, 1, 2, 1, 1])
)

descending <- oceancube::stratification(c10_smoke_state(
  sa = rev(c(34.7118, 34.8915, 35.0256, 34.8472, 34.7366, 34.7324)),
  ct = rev(c(28.8099, 28.4392, 22.7862, 10.2262, 6.8272, 4.3236)),
  pressure = rev(c(10, 50, 125, 250, 600, 1000)),
  depths = rev(c(10, 50, 125, 250, 600, 1000))
))
kilometres <- oceancube::stratification(c10_smoke_state(
  depths = c(0.01, 0.05, 0.125, 0.25, 0.6, 1), depth_unit = "km"
))
multiple <- oceancube::stratification(c10_smoke_state(latitude = c(4, 20)))
stopifnot(
  max(abs(
    as.numeric(n2$data[1, 1, , 1, 1]) -
      rev(as.numeric(descending$data[1, 1, , 1, 1]))
  )) < 1.5e-8,
  max(abs(
    as.numeric(n2$data[1, 1, , 1, 1]) -
      as.numeric(kilometres$data[1, 1, , 1, 1])
  )) < 1.5e-8,
  identical(unname(dim(multiple$data)), c(1L, 2L, 5L, 1L, 2L))
)

temperature_file <- c10_smoke_file(list(
  temperature = c10_smoke_variable(
    c(25.1, 25, 24.95, 24.6, 23.5),
    "sea_water_temperature", "degree_Celsius"
  )
), depths = c(0, 10, 20, 30, 40))
temperature <- oceancube::read_nc(temperature_file, vars = "temperature")
temperature_mld <- oceancube::mixed_layer_depth(temperature)
expected_temperature_mld <- 20 + 10 * (0.2 - 0.05) / (0.4 - 0.05)
stopifnot(isTRUE(all.equal(
  unique(temperature_mld$mld_depth_m), expected_temperature_mld
)))

thermocline <- oceancube::transition_layer(
  temperature, diagnostic = "thermocline"
)
oxygen_file <- c10_smoke_file(list(
  oxygen = c10_smoke_variable(
    c(220, 180, 80, 10, 5, 20),
    "moles_of_oxygen_per_unit_mass_in_sea_water", "umol kg-1"
  )
))
oxygen <- oceancube::read_nc(oxygen_file, vars = "oxygen")
oxygen_transition <- oceancube::transition_layer(
  oxygen, diagnostic = "upper_oxycline"
)
oxygen_boundary <- oceancube::oxygen_boundary(oxygen, threshold = 20)
stopifnot(
  identical(thermocline$diagnostic_status,
            "THERMOCLINE_GRADIENT_CANDIDATE"),
  identical(oxygen_transition$diagnostic_status,
            "UPPER_OXYCLINE_GRADIENT_CANDIDATE"),
  identical(oxygen_boundary$zone_status, "THRESHOLD_ZONE_PRESENT"),
  identical(unserialize(serialize(official, NULL)), official)
)

cat("C10_INSTALLED_PUBLIC_SMOKE: PASS\n")
cat("C10_INSTALLED_API_EXPORTS: 48\n")
cat("C10_INSTALLED_GSW_VERSION: ",
    as.character(utils::packageVersion("gsw")), "\n", sep = "")
cat("C10_INSTALLED_INTERNAL_CALLS: 0\n")
