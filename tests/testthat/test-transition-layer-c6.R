c6_make_profile <- function(
    variables,
    depths = c(0, 10, 20, 30),
    depth_unit = "m",
    cell_methods = "z: point",
    bounds = NULL,
    times = 0) {
  path <- tempfile("oceancube-c6-", fileext = ".nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", c(-80, -79))
  lat <- ncdf4::ncdim_def("lat", "degrees_north", c(-12, -11))
  z <- ncdf4::ncdim_def("z", depth_unit, depths)
  time <- ncdf4::ncdim_def("time", "days since 2000-01-01", times)
  definitions <- lapply(names(variables), function(name) {
    ncdf4::ncvar_def(name, variables[[name]]$unit, list(lon, lat, z, time))
  })
  if (!is.null(bounds)) {
    nv <- ncdf4::ncdim_def("nv", "", 1:2, create_dimvar = FALSE)
    definitions <- c(
      definitions,
      list(ncdf4::ncvar_def("z_bnds", depth_unit, list(nv, z)))
    )
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

c6_variable <- function(values, standard_name, unit, long_name = NULL) {
  list(
    values = values,
    standard_name = standard_name,
    unit = unit,
    long_name = long_name
  )
}

c6_temperature <- function(
    values = c(25, 24, 19, 18),
    standard_name = "sea_water_temperature",
    unit = "degree_Celsius",
    variable = "temperature",
    long_name = NULL,
    ...) {
  variables <- list(c6_variable(values, standard_name, unit, long_name))
  names(variables) <- variable
  read_nc(c6_make_profile(variables, ...), vars = variable)
}

c6_salinity <- function(
    values = c(34, 34.1, 35.1, 35.2),
    standard_name = "sea_water_practical_salinity",
    unit = "1",
    variable = "salinity",
    ...) {
  variables <- list(c6_variable(values, standard_name, unit))
  names(variables) <- variable
  read_nc(c6_make_profile(variables, ...), vars = variable)
}

test_that("C6 exposes one exact public signature", {
  expect_identical(
    names(formals(transition_layer)),
    c("x", "diagnostic", "variable", "support")
  )
  expect_identical(length(getNamespaceExports("oceancube")), 46L)
  expect_error(transition_layer(c6_temperature()),
               class = "oceancube_transition_diagnostic")
  expect_error(transition_layer(c6_temperature(), "oxycline"),
               class = "oceancube_transition_diagnostic")
})

test_that("thermocline composes the certified negative-gradient candidate", {
  result <- transition_layer(c6_temperature(), "thermocline")
  expect_s3_class(result, "data.frame", exact = TRUE)
  expect_equal(nrow(result), 4L)
  expect_equal(result$feature_depth_m, rep(15, 4L))
  expect_equal(result$gradient, rep(-0.5, 4L))
  expect_equal(result$diagnostic_strength, rep(0.5, 4L))
  expect_identical(unique(result$gradient_direction), "DECREASING_WITH_DEPTH")
  expect_identical(
    unique(result$diagnostic_definition),
    "THERMOCLINE_MAX_NEGATIVE_TEMPERATURE_GRADIENT"
  )
  expect_identical(unique(result$gradient_value_semantics),
                   "POINT_SECANT_GRADIENT")
  expect_identical(unique(result$gradient_method), "point")
  expect_identical(unique(result$input_mode), "SOURCE_PROFILE")
  expect_identical(unique(result$diagnostic_status),
                   "THERMOCLINE_GRADIENT_CANDIDATE")
  expect_true(all(!result$threshold_applied))
})

test_that("output columns and longitude-fastest row order are stable", {
  source <- c6_temperature(times = c(0, 1))
  result <- transition_layer(source, "thermocline")
  required <- c(
    "diagnostic", "diagnostic_definition", "variable_standard_name",
    "variable_semantic_family", "temperature_or_salinity_basis",
    "input_mode", "gradient_value_semantics", "gradient_method",
    "gradient_direction", "diagnostic_strength",
    "diagnostic_strength_unit", "threshold_applied", "feature_status",
    "feature_certification_status", "diagnostic_status",
    "diagnostic_certification_status", "feature_depth", "feature_depth_m",
    "source_depth_1", "source_depth_2", "source_depth_1_m",
    "source_depth_2_m", "spacing_m", "support_relation", "support_gap_m",
    "localization_half_span_m", "gradient_completeness",
    "n_support_eligible", "n_finite_gradient", "n_matching_candidates",
    "n_tied", "feature_tolerance"
  )
  expect_true(all(required %in% names(result)))
  expect_equal(nrow(result), 8L)
  expect_identical(result$longitude, c(-80, -79, -80, -79, -80, -79, -80, -79))
  expect_identical(result$latitude, c(-12, -12, -11, -11, -12, -12, -11, -11))
  expect_identical(as.numeric(result$time), rep(as.numeric(source$time), each = 4L))
})

test_that("Kelvin Celsius and temperature bases remain explicit", {
  celsius <- transition_layer(c6_temperature(), "thermocline")
  kelvin <- transition_layer(c6_temperature(
    values = c(298.15, 297.15, 292.15, 291.15), unit = "K"
  ), "thermocline")
  potential <- transition_layer(c6_temperature(
    standard_name = "sea_water_potential_temperature", unit = "kelvin"
  ), "thermocline")
  conservative <- transition_layer(c6_temperature(
    standard_name = "sea_water_conservative_temperature",
    unit = "degrees_C"
  ), "thermocline")

  expect_equal(kelvin$feature_depth_m, celsius$feature_depth_m)
  expect_equal(kelvin$gradient, celsius$gradient)
  expect_identical(unique(celsius$temperature_or_salinity_basis),
                   "IN_SITU_SEA_WATER_TEMPERATURE")
  expect_identical(unique(potential$temperature_or_salinity_basis),
                   "POTENTIAL_TEMPERATURE")
  expect_identical(unique(conservative$temperature_or_salinity_basis),
                   "CONSERVATIVE_TEMPERATURE")
})

test_that("temperature inversion has no absolute fallback", {
  result <- transition_layer(c6_temperature(
    values = c(10, 11, 15, 20)
  ), "thermocline")
  expect_identical(unique(result$feature_status), "NO_MATCHING_POLARITY")
  expect_identical(unique(result$diagnostic_status), "NO_MATCHING_POLARITY")
  expect_true(all(is.na(result$feature_depth)))
  expect_true(all(is.na(result$diagnostic_strength)))
})

test_that("cell-mean thermocline preserves C4 support semantics", {
  bounds <- rbind(c(0, 10), c(10, 20), c(20, 30), c(30, 40))
  source <- c6_temperature(
    values = c(25, 24, 19, 18),
    cell_methods = "z: mean",
    bounds = bounds,
    depths = rowMeans(bounds)
  )
  result <- transition_layer(source, "thermocline")
  expect_identical(unique(result$gradient_value_semantics),
                   "CELL_MEAN_SECANT_GRADIENT")
  expect_identical(unique(result$gradient_method), "cell")
  expect_identical(unique(result$support_relation), "CONTIGUOUS_SUPPORT")
  expect_identical(unique(result$diagnostic_status),
                   "THERMOCLINE_GRADIENT_CANDIDATE")
})

test_that("halocline uses absolute gradient and preserves its sign", {
  positive <- transition_layer(c6_salinity(), "halocline")
  negative <- transition_layer(c6_salinity(
    values = c(35.2, 35.1, 34.1, 34)
  ), "halocline")
  absolute <- transition_layer(c6_salinity(
    standard_name = "sea_water_absolute_salinity", unit = "g kg-1"
  ), "halocline")
  reference <- transition_layer(c6_salinity(
    standard_name = "sea_water_reference_salinity", unit = "g/kg"
  ), "halocline")

  expect_equal(unique(positive$feature_depth_m), 15)
  expect_equal(unique(positive$gradient), 0.1)
  expect_equal(unique(positive$diagnostic_strength), 0.1)
  expect_identical(unique(positive$gradient_direction), "INCREASING_WITH_DEPTH")
  expect_equal(unique(negative$gradient), -0.1)
  expect_identical(unique(negative$gradient_direction), "DECREASING_WITH_DEPTH")
  expect_identical(unique(positive$temperature_or_salinity_basis),
                   "PRACTICAL_SALINITY")
  expect_identical(unique(absolute$temperature_or_salinity_basis),
                   "ABSOLUTE_SALINITY")
  expect_identical(unique(reference$temperature_or_salinity_basis),
                   "REFERENCE_SALINITY")
})

test_that("bounded temperature and salinity unit contracts reject mismatches", {
  expect_error(
    transition_layer(c6_temperature(unit = "mol m-3"), "thermocline"),
    class = "oceancube_transition_unit_unsupported"
  )
  expect_error(
    transition_layer(c6_salinity(
      standard_name = "sea_water_absolute_salinity", unit = "1"
    ), "halocline"),
    class = "oceancube_transition_unit_unsupported"
  )
  generic <- transition_layer(c6_salinity(
    standard_name = "sea_water_salinity", unit = "1e-3"
  ), "halocline")
  expect_identical(unique(generic$temperature_or_salinity_basis),
                   "GENERIC_SALINITY")
})

test_that("standard_name is mandatory and names never establish eligibility", {
  misleading <- c6_temperature(
    standard_name =
      "mole_concentration_of_dissolved_molecular_oxygen_in_sea_water",
    variable = "temperature"
  )
  missing <- c6_temperature(
    standard_name = NULL, variable = "temperature", long_name = "temperature"
  )
  expect_error(transition_layer(misleading, "thermocline"),
               class = "oceancube_transition_variable")
  expect_error(transition_layer(missing, "thermocline"),
               class = "oceancube_transition_variable")
  expect_error(transition_layer(c6_salinity(), "thermocline", "salinity"),
               class = "oceancube_transition_variable")
})

test_that("variable resolution rejects ambiguity and supports exact selection", {
  variables <- list(
    in_situ = c6_variable(
      c(25, 24, 19, 18), "sea_water_temperature", "degree_Celsius"
    ),
    potential = c6_variable(
      c(24, 23, 17, 16), "sea_water_potential_temperature", "K"
    )
  )
  x <- read_nc(c6_make_profile(variables), vars = names(variables))
  expect_error(transition_layer(x, "thermocline"),
               class = "oceancube_transition_variable_ambiguous")
  selected <- transition_layer(x, "thermocline", variable = "potential")
  expect_identical(unique(selected$variable), "potential")
  expect_identical(unique(selected$temperature_or_salinity_basis),
                   "POTENTIAL_TEMPERATURE")

  mixed_variables <- list(
    heat = variables$in_situ,
    salt = c6_variable(
      c(34, 34.1, 35.1, 35.2), "sea_water_practical_salinity", "1"
    )
  )
  mixed <- read_nc(c6_make_profile(mixed_variables), vars = names(mixed_variables))
  expect_identical(unique(transition_layer(mixed, "thermocline")$variable),
                   "heat")
  expect_identical(unique(transition_layer(mixed, "halocline")$variable),
                   "salt")
})

test_that("source and certified-gradient modes are scientifically equivalent", {
  source <- c6_temperature()
  direct <- transition_layer(source, "thermocline")
  supplied <- transition_layer(depth_gradient(source), "thermocline")
  fields <- c(
    "feature_depth", "feature_depth_m", "gradient", "gradient_magnitude",
    "support_relation", "spacing_m", "support_gap_m", "feature_status",
    "diagnostic_status"
  )
  expect_equal(direct[fields], supplied[fields])
  expect_identical(unique(direct$input_mode), "SOURCE_PROFILE")
  expect_identical(unique(supplied$input_mode), "CERTIFIED_GRADIENT")
  operations <- vapply(
    attr(supplied, "oceancube_provenance")$history,
    `[[`, character(1L), "operation"
  )
  expect_identical(sum(operations == "depth_gradient"), 1L)
  expect_identical(tail(operations, 2L), c("depth_feature", "transition_layer"))
})

test_that("derived C1 C2 and C3 inputs cannot receive C6 interpretation", {
  point <- c6_temperature()
  sampled <- depth_sample(point, c(2, 12, 22), method = "linear")
  expect_error(transition_layer(sampled, "thermocline"),
               class = "oceancube_transition_derived_input")
  expect_error(transition_layer(depth_gradient(sampled), "thermocline"),
               class = "oceancube_transition_derived_gradient")

  bounds <- rbind(c(0, 10), c(10, 20), c(20, 30), c(30, 40))
  cells <- c6_temperature(
    cell_methods = "z: mean", bounds = bounds, depths = rowMeans(bounds)
  )
  averaged <- layer_mean(cells, c(0, 20, 40))
  expect_error(transition_layer(averaged, "thermocline"),
               class = "oceancube_transition_derived_input")
  expect_error(transition_layer(depth_gradient(averaged), "thermocline"),
               class = "oceancube_transition_derived_gradient")
  expect_error(
    transition_layer(layer_integral(cells, c(0, 20, 40)), "thermocline"),
    class = "oceancube_transition_derived_input"
  )
  expect_error(
    transition_layer(depth_feature(depth_gradient(point)), "thermocline")
  )
})

test_that("local and all policies retain gapped C5 authority", {
  bounds <- rbind(c(0, 10), c(10, 20), c(30, 40), c(40, 50))
  centres <- rowMeans(bounds)
  values <- c(25, 24, 14, 13)
  cells <- c6_temperature(
    values = values, cell_methods = "z: mean",
    bounds = bounds, depths = centres
  )
  local <- transition_layer(cells, "thermocline", support = "local")
  all_support <- transition_layer(cells, "thermocline", support = "all")
  expect_false(any(local$support_relation == "GAPPED_SUPPORT", na.rm = TRUE))
  expect_identical(unique(all_support$support_relation), "GAPPED_SUPPORT")
  expect_identical(unique(all_support$diagnostic_status),
                   "GAPPED_THERMOCLINE_GRADIENT_CANDIDATE")
  expect_identical(
    unique(all_support$diagnostic_certification_status),
    "CERTIFIED_UNTHRESHOLDED_GAPPED_CANDIDATE"
  )
})

test_that("ties and incomplete profiles cannot gain stronger interpretation", {
  tied <- transition_layer(c6_salinity(
    values = c(34, 35, 34, 34.2)
  ), "halocline")
  incomplete <- transition_layer(c6_temperature(
    values = c(25, NA, 19, 18)
  ), "thermocline")
  expect_identical(unique(tied$feature_status), "AMBIGUOUS_TIE")
  expect_identical(unique(tied$diagnostic_status), "AMBIGUOUS_TIE")
  expect_true(all(is.na(tied$feature_depth)))
  expect_true(all(grepl(
    "INCOMPLETE_PROFILE$", incomplete$diagnostic_certification_status
  )))
})

test_that("WOA metadata and both diagnostics agree with C4-C5 references", {
  fixture <- file.path(
    "fixtures", "real-data", "noaa-woa23-monthly-vertical-fv1.nc"
  )
  woa <- read_nc(fixture, vars = c("t_an", "s_an"))
  therm_local <- transition_layer(woa, "thermocline", support = "local")
  therm_all <- transition_layer(woa, "thermocline", support = "all")
  halo_local <- transition_layer(woa, "halocline", support = "local")
  halo_all <- transition_layer(woa, "halocline", support = "all")
  reference_t <- depth_feature(
    depth_gradient(cube_slice(woa, variable = "t_an")),
    polarity = "negative", support = "all"
  )
  reference_s <- depth_feature(
    depth_gradient(cube_slice(woa, variable = "s_an")),
    polarity = "absolute", support = "all"
  )
  fields <- c(
    "gradient_index", "feature_depth", "feature_depth_m", "gradient",
    "gradient_magnitude", "spacing_m", "support_relation", "support_gap_m"
  )

  expect_identical(unique(therm_all$variable_standard_name),
                   "sea_water_temperature")
  expect_identical(unique(therm_all$variable_unit), "degrees_celsius")
  expect_identical(unique(halo_all$variable_standard_name),
                   "sea_water_practical_salinity")
  expect_identical(unique(halo_all$variable_unit), "1")
  expect_true(all(therm_local$feature_status == "NO_LOCAL_SUPPORT"))
  expect_true(all(halo_local$feature_status == "NO_LOCAL_SUPPORT"))
  expect_equal(therm_all[fields], reference_t[fields])
  expect_equal(halo_all[fields], reference_s[fields])
  therm_gapped <- which(therm_all$support_relation == "GAPPED_SUPPORT")
  halo_gapped <- which(halo_all$support_relation == "GAPPED_SUPPORT")
  expect_true(all(
    grepl("GAPPED.*THERMOCLINE", therm_all$diagnostic_status[therm_gapped])
  ))
  expect_true(all(
    grepl("GAPPED.*HALOCLINE", halo_all$diagnostic_status[halo_gapped])
  ))
})

test_that("deferred mixed WOA reads only the selected variable once", {
  fixture <- file.path(
    "fixtures", "real-data", "noaa-woa23-monthly-vertical-fv1.nc"
  )
  source <- cube_open(fixture, vars = c("t_an", "s_an"))
  therm <- transition_layer(source, "thermocline", support = "all")
  halo <- transition_layer(source, "halocline", support = "all")
  therm_qa <- attr(therm, "oceancube_qa")$transition_layer
  halo_qa <- attr(halo, "oceancube_qa")$transition_layer
  expect_identical(therm_qa$netcdf_scientific_payload_reads, 1L)
  expect_identical(therm_qa$variables_read, "t_an")
  expect_identical(therm_qa$levels_read, 6L)
  expect_identical(halo_qa$netcdf_scientific_payload_reads, 1L)
  expect_identical(halo_qa$variables_read, "s_an")
  expect_identical(halo_qa$levels_read, 6L)
})

test_that("unsupported real-data domains fail without guard weakening", {
  root <- file.path("fixtures", "real-data")
  oisst <- read_nc(
    file.path(root, "noaa-oisst21-surface-time-fv1.nc"), vars = "sst"
  )
  expect_error(transition_layer(oisst, "thermocline"),
               class = "oceancube_transition_variable")
  expect_error(
    read_nc(
      file.path(root, "noaa-woa23-vertical-fv1.nc"),
      vars = c("t_an", "s_an")
    ),
    "climatology support|provider-specific offset"
  )
  expect_error(read_nc(file.path(root, "noaa-etopo2022-bathymetry-fv1.nc")),
               "Could not identify time")
})

test_that("Provenance QA serialization privacy and determinism are bounded", {
  source <- c6_temperature()
  first <- transition_layer(source, "thermocline")
  second <- transition_layer(source, "thermocline")
  provenance <- attr(first, "oceancube_provenance")
  qa <- attr(first, "oceancube_qa")
  record <- tail(provenance$history, 1L)[[1L]]
  serialized <- unserialize(serialize(first, NULL))
  path <- tempfile(fileext = ".rds")
  saveRDS(first, path)
  restored <- readRDS(path)

  expect_identical(record$operation, "transition_layer")
  expect_identical(
    record$scientific_method$id,
    "oceancube:variable_aware_thermocline_gradient_candidate"
  )
  expect_identical(record$parameters$resolved$threshold_applied, FALSE)
  expect_identical(qa$transition_layer$diagnostic, "thermocline")
  expect_true("vertical_feature" %in% names(qa))
  expect_identical(serialized, first)
  expect_identical(restored, first)
  expect_identical(unclass(first), unclass(second))
  transition_record_text <- paste(capture.output(str(record)), collapse = " ")
  expect_false(grepl("[A-Za-z]:[/\\\\]|oquispe|tempfile", transition_record_text,
                     ignore.case = TRUE))
})

test_that("source CF metadata remains immutable", {
  source <- c6_temperature()
  before <- serialize(source$metadata$cf$source, NULL)
  transition_layer(source, "thermocline")
  expect_identical(serialize(source$metadata$cf$source, NULL), before)
})
