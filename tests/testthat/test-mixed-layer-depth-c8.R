c8_variable <- function(values, standard_name, unit, long_name = NULL) {
  list(
    values = values, standard_name = standard_name,
    unit = unit, long_name = long_name
  )
}

c8_make_profile <- function(
    variables,
    depths = c(0, 10, 20, 30, 40),
    depth_unit = "m",
    cell_methods = "z: point",
    bounds = NULL,
    times = 0) {
  path <- tempfile("oceancube-c8-", fileext = ".nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", c(-80, -79))
  lat <- ncdf4::ncdim_def("lat", "degrees_north", c(-12, -11))
  z <- ncdf4::ncdim_def("z", depth_unit, depths)
  time <- ncdf4::ncdim_def("time", "days since 2000-01-01", times)
  definitions <- lapply(names(variables), function(name) {
    ncdf4::ncvar_def(
      name, variables[[name]]$unit, list(lon, lat, z, time), prec = "double"
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
    payload <- array(
      rep(item$values, each = 4L),
      c(2, 2, length(depths), length(times))
    )
    ncdf4::ncvar_put(nc, name, payload)
    if (!is.null(item$standard_name)) {
      ncdf4::ncatt_put(nc, name, "standard_name", item$standard_name)
    }
    if (!is.null(item$long_name)) {
      ncdf4::ncatt_put(nc, name, "long_name", item$long_name)
    }
    ncdf4::ncatt_put(nc, name, "cell_methods", cell_methods)
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

c8_temperature <- function(
    values = c(25.1, 25.0, 24.95, 24.6, 23.5),
    depths = c(0, 10, 20, 30, 40),
    standard_name = "sea_water_temperature",
    unit = "degrees_celsius",
    variable = "temperature",
    long_name = NULL,
    ...) {
  variables <- list(c8_variable(values, standard_name, unit, long_name))
  names(variables) <- variable
  read_nc(c8_make_profile(variables, depths = depths, ...), vars = variable)
}

test_that("C8 exposes exactly one new public function and bounded method", {
  expect_identical(
    names(formals(mixed_layer_depth)),
    c("x", "method", "variable", "reference_depth_m", "threshold", "support")
  )
  expect_identical(length(getNamespaceExports("oceancube")), 47L)
  source <- c8_temperature()
  expect_error(
    mixed_layer_depth(source, method = "density_threshold"),
    class = "oceancube_mld_method_unsupported"
  )
  expect_error(mixed_layer_depth(source, threshold = 0),
               class = "oceancube_mld_threshold")
  expect_error(mixed_layer_depth(source, reference_depth_m = -1),
               class = "oceancube_mld_reference_depth")
})

test_that("analytic temperature profile resolves first interpolated MLD", {
  result <- mixed_layer_depth(c8_temperature())
  expected <- 20 + 10 * (0.2 - 0.05) / (0.4 - 0.05)
  expect_equal(nrow(result), 4L)
  expect_equal(unique(result$reference_temperature), 25)
  expect_identical(unique(result$reference_status), "REFERENCE_EXACT_POINT")
  expect_identical(
    unique(result$status), "MLD_INTERPOLATED_THRESHOLD_CROSSING"
  )
  expect_equal(unique(result$mld_depth_m), expected)
  expect_equal(unique(result$mld_depth), expected)
  expect_identical(unique(result$crossing_direction), "COOLER_WITH_DEPTH")
  expect_equal(unique(result$bracket_shallow_depth_m), 20)
  expect_equal(unique(result$bracket_deep_depth_m), 30)
})

test_that("exact threshold and inversions retain distinct evidence", {
  exact <- mixed_layer_depth(c8_temperature(
    c(25.1, 25, 24.9, 24.8, 24.5)
  ))
  inversion <- mixed_layer_depth(c8_temperature(
    c(24.9, 25, 25.1, 25.4, 25.7)
  ))
  expect_identical(unique(exact$status), "MLD_EXACT_THRESHOLD_POINT")
  expect_equal(unique(exact$mld_depth_m), 30)
  expect_identical(unique(exact$crossing_direction), "COOLER_WITH_DEPTH")
  expect_identical(
    unique(inversion$status), "MLD_INTERPOLATED_THRESHOLD_CROSSING"
  )
  expect_identical(unique(inversion$crossing_direction), "WARMER_WITH_DEPTH")
  expect_equal(unique(inversion$mld_depth_m), 70 / 3)
})

test_that("first physical crossing wins across later recrossings", {
  result <- mixed_layer_depth(c8_temperature(
    c(25.1, 25, 24.7, 24.9, 24.5)
  ))
  expect_equal(unique(result$mld_depth_m), 10 + 10 * 2 / 3)
  expect_equal(unique(result$bracket_shallow_depth_m), 10)
  expect_equal(unique(result$bracket_deep_depth_m), 20)
})

test_that("reference interpolation is local and numerically exact", {
  source <- c8_temperature(
    c(25.2, 25.1, 24.9, 24.5), depths = c(0, 5, 15, 25)
  )
  result <- mixed_layer_depth(source)
  expect_identical(
    unique(result$reference_status), "REFERENCE_INTERPOLATED_POINT"
  )
  expect_equal(unique(result$reference_temperature), 25)
  expect_equal(unique(result$mld_depth_m), 17.5)

  gapped_bounds <- rbind(c(0, 4), c(4, 7), c(13, 20), c(20, 30))
  gapped <- c8_temperature(
    c(25.2, 25.1, 24.9, 24.5), depths = c(0, 5, 15, 25),
    bounds = gapped_bounds
  )
  rejected <- mixed_layer_depth(gapped, support = "all")
  expect_identical(unique(rejected$status), "REFERENCE_GAPPED_BRACKET")
  expect_true(all(is.na(rejected$mld_depth_m)))
})

test_that("reference outside and unresolved temperatures never extrapolate", {
  source <- c8_temperature()
  outside <- mixed_layer_depth(source, reference_depth_m = 50)
  expect_identical(unique(outside$status), "REFERENCE_DEPTH_OUTSIDE_PROFILE")
  expect_true(all(is.na(outside$mld_depth_m)))
  missing <- mixed_layer_depth(c8_temperature(
    c(25.1, NA, 24.9, 24.5), depths = c(0, 5, 15, 25)
  ))
  expect_identical(unique(missing$status), "REFERENCE_TEMPERATURE_UNRESOLVED")
})

test_that("descending storage and metre-kilometre encodings are invariant", {
  depths <- c(0, 10, 20, 30, 40)
  values <- c(25.1, 25, 24.95, 24.6, 23.5)
  ascending <- mixed_layer_depth(c8_temperature(values, depths))
  descending <- mixed_layer_depth(c8_temperature(rev(values), rev(depths)))
  kilometres <- mixed_layer_depth(c8_temperature(
    values, depths / 1000, depth_unit = "km"
  ))
  fields <- c(
    "reference_temperature", "mld_depth_m", "crossing_direction", "status"
  )
  expect_equal(ascending[fields], descending[fields])
  expect_equal(ascending[c("mld_depth_m")], kilometres[c("mld_depth_m")])
  expect_equal(unique(kilometres$mld_depth), unique(ascending$mld_depth) / 1000)
})

test_that("explicit support gaps never produce an exact MLD", {
  bounds <- rbind(
    c(-5, 5), c(5, 15), c(15, 22), c(28, 35), c(35, 45)
  )
  source <- c8_temperature(bounds = bounds)
  local <- mixed_layer_depth(source, support = "local")
  all_support <- mixed_layer_depth(source, support = "all")
  expect_identical(
    unique(local$status), "MLD_UNRESOLVED_BEFORE_SUPPORT_GAP"
  )
  expect_identical(
    unique(all_support$status), "GAPPED_MLD_THRESHOLD_BRACKET"
  )
  expect_true(all(is.na(local$mld_depth_m)))
  expect_true(all(is.na(all_support$mld_depth_m)))
  expect_equal(unique(all_support$support_gap_m), 6)
})

test_that("missingness is strict only through the selected crossing", {
  before <- mixed_layer_depth(c8_temperature(
    c(25.1, 25, NA, 24.6, 23.5)
  ))
  after <- mixed_layer_depth(c8_temperature(
    c(25.1, 25, 24.95, 24.6, NA)
  ))
  complete <- mixed_layer_depth(c8_temperature())
  expect_identical(
    unique(before$status), "MLD_UNRESOLVED_INCOMPLETE_PATH"
  )
  expect_true(all(is.na(before$mld_depth_m)))
  expect_equal(after[c("mld_depth_m", "status")],
               complete[c("mld_depth_m", "status")])
})

test_that("no crossing is open at bottom rather than deepest substitution", {
  result <- mixed_layer_depth(c8_temperature(
    c(25.1, 25, 24.95, 24.9, 24.85)
  ))
  expect_identical(unique(result$status), "MLD_OPEN_AT_PROFILE_BOTTOM")
  expect_true(all(is.na(result$mld_depth_m)))
  expect_identical(unique(result$path_completeness),
                   "COMPLETE_TO_PROFILE_BOTTOM")
})

test_that("temperature semantics use source standard_name and interval units", {
  potential <- mixed_layer_depth(c8_temperature(
    standard_name = "sea_water_potential_temperature", unit = "kelvin"
  ))
  conservative <- mixed_layer_depth(c8_temperature(
    standard_name = "sea_water_conservative_temperature", unit = "degree_C"
  ))
  kelvin <- mixed_layer_depth(c8_temperature(
    c(298.25, 298.15, 298.10, 297.75, 296.65), unit = "K"
  ))
  expect_identical(unique(potential$temperature_basis), "POTENTIAL_TEMPERATURE")
  expect_identical(unique(conservative$temperature_basis),
                   "CONSERVATIVE_TEMPERATURE")
  expect_equal(unique(kelvin$mld_depth_m),
               unique(mixed_layer_depth(c8_temperature())$mld_depth_m))
})

test_that("variable names and long_name never establish temperature identity", {
  fake <- c8_temperature(
    standard_name = NULL, variable = "temperature",
    long_name = "sea water temperature"
  )
  expect_error(mixed_layer_depth(fake),
               class = "oceancube_transition_variable")

  variables <- list(
    a = c8_variable(c(25.1, 25, 24.95, 24.6, 23.5),
                    "sea_water_temperature", "K"),
    b = c8_variable(c(24.1, 24, 23.95, 23.6, 22.5),
                    "sea_water_potential_temperature", "K")
  )
  multiple <- read_nc(c8_make_profile(variables), vars = names(variables))
  expect_error(mixed_layer_depth(multiple),
               class = "oceancube_transition_variable_ambiguous")
  expect_identical(unique(mixed_layer_depth(multiple, variable = "b")$variable),
                   "b")
})

test_that("cell means WOA and surface-only OISST are outside C8 subset", {
  cells <- c8_temperature(cell_methods = "z: mean", bounds = rbind(
    c(-5, 5), c(5, 15), c(15, 25), c(25, 35), c(35, 45)
  ))
  expect_error(mixed_layer_depth(cells),
               class = "oceancube_mld_value_semantics_unsupported")

  root <- file.path("fixtures", "real-data")
  woa <- read_nc(file.path(root, "noaa-woa23-monthly-vertical-fv1.nc"),
                 vars = "t_an")
  expect_error(mixed_layer_depth(woa),
               class = "oceancube_mld_value_semantics_unsupported")
  oisst <- read_nc(file.path(root, "noaa-oisst21-surface-time-fv1.nc"),
                  vars = "sst")
  expect_error(mixed_layer_depth(oisst))
})

test_that("C8 output provenance QA and source metadata are bounded", {
  source <- c8_temperature()
  before <- serialize(source$metadata$cf$source, NULL)
  result <- mixed_layer_depth(source)
  required <- c(
    "longitude", "latitude", "time", "variable",
    "temperature_standard_name", "temperature_basis", "temperature_unit",
    "method", "reference_depth_requested_m", "reference_depth_resolved_m",
    "reference_temperature", "reference_status", "threshold", "threshold_unit",
    "mld_depth", "depth_unit", "mld_depth_m", "crossing_direction",
    "bracket_shallow_depth", "bracket_deep_depth",
    "bracket_shallow_depth_m", "bracket_deep_depth_m",
    "bracket_shallow_temperature", "bracket_deep_temperature",
    "support_relation", "support_gap_m", "n_path_points", "n_path_finite",
    "path_completeness", "status", "certification_status"
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(required %in% names(result)))
  expect_identical(serialize(source$metadata$cf$source, NULL), before)
  provenance <- attr(result, "oceancube_provenance")
  expect_identical(tail(provenance$history, 1)[[1L]]$operation,
                   "mixed_layer_depth")
  expect_identical(
    tail(provenance$history, 1)[[1L]]$scientific_method$id,
    "oceancube:first_temperature_threshold_departure"
  )
  expect_silent(oceancube:::.provenance_validate(provenance, strict = TRUE))
  qa <- attr(result, "oceancube_qa")$mixed_layer_depth
  expect_identical(qa$profiles_total, 4L)
  expect_identical(qa$resolved_interpolated, 4L)
  expect_identical(qa$resolved_exact, 0L)
  expect_identical(qa$temperature_inversions, 0L)
})

test_that("C6 and C7 signatures and numerical diagnostics remain unchanged", {
  expect_identical(names(formals(transition_layer)),
                   c("x", "diagnostic", "variable", "support"))
  expect_identical(names(formals(oxygen_boundary)),
                   c("x", "threshold", "threshold_unit", "variable", "support"))
  temperature <- c8_temperature(c(25, 24, 19, 18), depths = c(0, 10, 20, 30))
  thermocline <- transition_layer(temperature, "thermocline")
  expect_identical(unique(thermocline$diagnostic_status),
                   "THERMOCLINE_GRADIENT_CANDIDATE")
  oxygen_variables <- list(oxygen = c8_variable(
    c(220, 180, 80, 10, 5, 20, 80),
    "moles_of_oxygen_per_unit_mass_in_sea_water", "umol kg-1"
  ))
  oxygen <- read_nc(c8_make_profile(
    oxygen_variables, depths = c(0, 20, 40, 60, 80, 100, 150)
  ), vars = "oxygen")
  expect_identical(unique(transition_layer(
    oxygen, "upper_oxycline"
  )$diagnostic_status), "UPPER_OXYCLINE_GRADIENT_CANDIDATE")
  expect_identical(unique(transition_layer(
    oxygen, "lower_oxycline"
  )$diagnostic_status), "LOWER_OXYCLINE_GRADIENT_CANDIDATE")
  expect_identical(unique(oxygen_boundary(oxygen, 20)$zone_status),
                   "THRESHOLD_ZONE_PRESENT")
})
