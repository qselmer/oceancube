c7_variable <- function(values, standard_name, unit, long_name = NULL) {
  list(values = values, standard_name = standard_name,
       unit = unit, long_name = long_name)
}

c7_make_profile <- function(
    variables,
    depths = c(0, 20, 40, 60, 80, 100, 150),
    cell_methods = "z: point",
    bounds = NULL,
    times = 0) {
  path <- tempfile("oceancube-c7-", fileext = ".nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", c(-80, -79))
  lat <- ncdf4::ncdim_def("lat", "degrees_north", c(-12, -11))
  z <- ncdf4::ncdim_def("z", "m", depths)
  time <- ncdf4::ncdim_def("time", "days since 2000-01-01", times)
  definitions <- lapply(names(variables), function(name) {
    ncdf4::ncvar_def(name, variables[[name]]$unit, list(lon, lat, z, time))
  })
  if (!is.null(bounds)) {
    nv <- ncdf4::ncdim_def("nv", "", 1:2, create_dimvar = FALSE)
    definitions <- c(definitions, list(
      ncdf4::ncvar_def("z_bnds", "m", list(nv, z))
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

c7_oxygen <- function(
    values = c(220, 180, 80, 10, 5, 20, 80),
    depths = c(0, 20, 40, 60, 80, 100, 150),
    standard_name = "moles_of_oxygen_per_unit_mass_in_sea_water",
    unit = "umol kg-1",
    variable = "oxygen",
    long_name = NULL,
    ...) {
  variables <- list(c7_variable(values, standard_name, unit, long_name))
  names(variables) <- variable
  read_nc(c7_make_profile(variables, depths = depths, ...), vars = variable)
}

test_that("C7 exposes one exact new API and preserves transition signature", {
  expect_identical(
    names(formals(transition_layer)),
    c("x", "diagnostic", "variable", "support")
  )
  expect_identical(
    names(formals(oxygen_boundary)),
    c("x", "threshold", "threshold_unit", "variable", "support")
  )
  expect_identical(length(getNamespaceExports("oceancube")), 47L)
  expect_error(oxygen_boundary(c7_oxygen()),
               class = "oceancube_oxygen_threshold")
})

test_that("upper and lower oxycline candidates are branch aware", {
  source <- c7_oxygen()
  upper <- transition_layer(source, "upper_oxycline")
  lower <- transition_layer(source, "lower_oxycline")
  expect_equal(nrow(upper), 4L)
  expect_equal(unique(upper$core_depth_m), 80)
  expect_identical(unique(upper$core_type), "UNIQUE_MINIMUM")
  expect_equal(unique(upper$feature_depth_m), 30)
  expect_equal(unique(upper$gradient), -5)
  expect_equal(unique(upper$spacing_m), 20)
  expect_identical(unique(upper$branch), "UPPER")
  expect_identical(unique(upper$diagnostic_status),
                   "UPPER_OXYCLINE_GRADIENT_CANDIDATE")
  expect_equal(unique(lower$feature_depth_m), 125)
  expect_equal(unique(lower$gradient), 1.2)
  expect_equal(unique(lower$spacing_m), 50)
  expect_identical(unique(lower$branch), "LOWER")
  expect_identical(unique(lower$diagnostic_status),
                   "LOWER_OXYCLINE_GRADIENT_CANDIDATE")
})

test_that("plateau excludes internal gradients and disjoint minima are ambiguous", {
  plateau <- c7_oxygen(c(220, 100, 5, 5, 5, 30, 100))
  upper <- transition_layer(plateau, "upper_oxycline")
  lower <- transition_layer(plateau, "lower_oxycline")
  expect_identical(unique(upper$core_type), "CONTIGUOUS_MINIMUM_PLATEAU")
  expect_equal(unique(upper$core_shallow_depth_m), 40)
  expect_equal(unique(upper$core_deep_depth_m), 80)
  expect_equal(unique(upper$feature_depth_m), 10)
  expect_equal(unique(lower$feature_depth_m), 125)

  disjoint <- c7_oxygen(
    c(220, 5, 60, 5, 100), depths = c(0, 20, 40, 60, 80)
  )
  ambiguous <- transition_layer(disjoint, "upper_oxycline")
  expect_identical(unique(ambiguous$core_type),
                   "AMBIGUOUS_DISJOINT_MINIMA")
  expect_identical(unique(ambiguous$diagnostic_status),
                   "AMBIGUOUS_DISJOINT_MINIMA")
  expect_true(all(is.na(ambiguous$gradient)))
})

test_that("flat incomplete and edge cores remain conservative", {
  flat <- transition_layer(c7_oxygen(
    rep(50, 4), depths = c(0, 20, 40, 60)
  ), "upper_oxycline")
  incomplete <- transition_layer(c7_oxygen(
    c(220, 180, NA, 10, 5, 20, 80)
  ), "upper_oxycline")
  shallow <- transition_layer(c7_oxygen(
    c(5, 20, 40, 60), depths = c(0, 20, 40, 60)
  ), "upper_oxycline")
  deep <- transition_layer(c7_oxygen(
    c(80, 60, 20, 5), depths = c(0, 20, 40, 60)
  ), "lower_oxycline")
  expect_identical(unique(flat$diagnostic_status), "FLAT_OXYGEN_PROFILE")
  expect_identical(unique(incomplete$diagnostic_status),
                   "INCOMPLETE_OXYGEN_PROFILE")
  expect_true(all(incomplete$oxygen_profile_completeness < 1))
  expect_identical(unique(shallow$diagnostic_status), "NO_UPPER_BRANCH")
  expect_identical(unique(deep$diagnostic_status), "NO_LOWER_BRANCH")
})

test_that("physical results are invariant to descending storage order", {
  depths <- c(0, 20, 40, 60, 80, 100, 150)
  values <- c(220, 180, 80, 10, 5, 20, 80)
  ascending <- c7_oxygen(values, depths)
  descending <- c7_oxygen(rev(values), rev(depths))
  fields <- c("core_depth_m", "feature_depth_m", "gradient", "spacing_m")
  expect_equal(
    transition_layer(ascending, "upper_oxycline")[fields],
    transition_layer(descending, "upper_oxycline")[fields]
  )
  expect_equal(
    transition_layer(ascending, "lower_oxycline")[fields],
    transition_layer(descending, "lower_oxycline")[fields]
  )
})

test_that("oxygen semantics use standard_name and bounded quantity families", {
  amount_volume <- transition_layer(c7_oxygen(
    standard_name =
      "mole_concentration_of_dissolved_molecular_oxygen_in_sea_water",
    unit = "mmol m-3", variable = "x123"
  ), "upper_oxycline")
  mass_volume <- transition_layer(c7_oxygen(
    standard_name = "mass_concentration_of_oxygen_in_sea_water",
    unit = "mg L-1"
  ), "upper_oxycline")
  expect_identical(unique(amount_volume$oxygen_quantity_family),
                   "AMOUNT_PER_VOLUME")
  expect_identical(unique(mass_volume$oxygen_quantity_family),
                   "MASS_PER_VOLUME")
  expect_error(
    transition_layer(c7_oxygen(
      standard_name = "sea_water_temperature", variable = "oxygen"
    ), "upper_oxycline"),
    class = "oceancube_oxygen_variable"
  )
  expect_error(
    transition_layer(c7_oxygen(
      standard_name = "fractional_saturation_of_oxygen_in_sea_water"
    ), "upper_oxycline"),
    class = "oceancube_oxygen_variable"
  )
})

test_that("oxygen variable resolution rejects zero multiple and false heuristics", {
  two <- list(
    first = c7_variable(
      c(220, 180, 80, 10, 5, 20, 80),
      "moles_of_oxygen_per_unit_mass_in_sea_water", "umol kg-1"
    ),
    second = c7_variable(
      c(210, 170, 70, 9, 4, 19, 70),
      "moles_of_oxygen_per_unit_mass_in_sea_water", "umol kg-1"
    )
  )
  cube <- read_nc(c7_make_profile(two), vars = names(two))
  expect_error(transition_layer(cube, "upper_oxycline"),
               class = "oceancube_oxygen_variable_ambiguous")
  expect_identical(unique(transition_layer(
    cube, "upper_oxycline", variable = "second"
  )$variable), "second")
  missing <- c7_oxygen(standard_name = NULL, variable = "oxygen")
  expect_error(oxygen_boundary(missing, 20),
               class = "oceancube_oxygen_variable")
  long_name_only <- c7_oxygen(
    standard_name = NULL, variable = "not_oxygen",
    long_name = "dissolved molecular oxygen concentration"
  )
  expect_error(transition_layer(long_name_only, "upper_oxycline"),
               class = "oceancube_oxygen_variable")
})

test_that("point threshold zones resolve exact and interpolated boundaries", {
  result <- oxygen_boundary(c7_oxygen(), 20)
  expect_identical(unique(result$zone_status), "THRESHOLD_ZONE_PRESENT")
  expect_equal(unique(result$core_depth_m), 80)
  expect_identical(unique(result$upper_boundary_status),
                   "INTERPOLATED_THRESHOLD_CROSSING")
  expect_equal(unique(result$upper_boundary_depth_m), 400 / 7)
  expect_identical(unique(result$lower_boundary_status),
                   "EXACT_THRESHOLD_POINT")
  expect_equal(unique(result$lower_boundary_depth_m), 100)
  expect_equal(unique(result$zone_observed_thickness_m), 100 - 400 / 7)
  expect_identical(unique(result$threshold_unit_interpretation), "SOURCE_UNIT")
})

test_that("threshold absence span and core component selection are explicit", {
  absent <- oxygen_boundary(c7_oxygen(), 4)
  span <- oxygen_boundary(c7_oxygen(), 300)
  multi <- oxygen_boundary(c7_oxygen(
    c(10, 5, 80, 70, 4, 10, 100)
  ), 20)
  expect_identical(unique(absent$zone_status), "THRESHOLD_ZONE_ABSENT")
  expect_identical(unique(span$zone_status), "THRESHOLD_ZONE_SPANS_PROFILE")
  expect_identical(unique(span$upper_boundary_status),
                   "UPPER_BOUNDARY_OPEN_AT_PROFILE_EDGE")
  expect_identical(unique(span$lower_boundary_status),
                   "LOWER_BOUNDARY_OPEN_AT_PROFILE_EDGE")
  expect_equal(unique(multi$zone_observed_top_m), 80)
  expect_equal(unique(multi$zone_observed_bottom_m), 100)
})

test_that("same-family threshold scaling works and cross-basis conversion fails", {
  source <- c7_oxygen()
  micromol <- oxygen_boundary(source, 20, "umol kg-1")
  millimol <- oxygen_boundary(source, 0.020, "mmol kg-1")
  mol <- oxygen_boundary(source, 0.000020, "mol kg-1")
  fields <- c("zone_status", "upper_boundary_depth_m", "lower_boundary_depth_m")
  expect_equal(micromol[fields], millimol[fields])
  expect_equal(micromol[fields], mol[fields])
  expect_error(oxygen_boundary(source, 1, "mg L-1"),
               class = "oceancube_oxygen_unit_cross_basis")
  expect_error(oxygen_boundary(source, -1),
               class = "oceancube_oxygen_threshold")
  expect_error(oxygen_boundary(source, NA_real_),
               class = "oceancube_oxygen_threshold")
  expect_error(oxygen_boundary(source, Inf),
               class = "oceancube_oxygen_threshold")
  expect_error(oxygen_boundary(source, c(10, 20)),
               class = "oceancube_oxygen_threshold")
})

test_that("cell means and gapped point brackets never fabricate crossings", {
  depths <- c(0, 20, 40, 60, 80, 100, 150)
  contiguous <- cbind(
    c(-10, 10, 30, 50, 70, 90, 125),
    c(10, 30, 50, 70, 90, 110, 175)
  )
  cells <- c7_oxygen(
    cell_methods = "z: mean", bounds = contiguous
  )
  cell_result <- oxygen_boundary(cells, 20)
  expect_identical(unique(cell_result$core_value_semantics),
                   "CELL_MEAN_MINIMUM")
  expect_identical(unique(cell_result$upper_boundary_status),
                   "CELL_MEAN_THRESHOLD_BRACKET_ONLY")
  expect_true(all(is.na(cell_result$upper_boundary_depth_m)))
  expect_true(all(is.na(cell_result$zone_observed_thickness_m)))

  gapped_bounds <- contiguous
  gapped_bounds[7, ] <- c(140, 160)
  gapped <- c7_oxygen(bounds = gapped_bounds)
  all_support <- oxygen_boundary(gapped, 20, support = "all")
  local <- oxygen_boundary(gapped, 20, support = "local")
  expect_identical(unique(all_support$lower_boundary_status),
                   "GAPPED_THRESHOLD_BRACKET_ONLY")
  expect_true(all(is.na(all_support$lower_boundary_depth_m)))
  expect_identical(unique(local$lower_boundary_status),
                   "LOCAL_SUPPORT_GAP_COMPONENT_EDGE")
  all_feature <- transition_layer(gapped, "lower_oxycline", support = "all")
  local_feature <- transition_layer(gapped, "lower_oxycline", support = "local")
  expect_identical(unique(all_feature$diagnostic_status),
                   "GAPPED_LOWER_OXYCLINE_GRADIENT_CANDIDATE")
  expect_identical(unique(local_feature$diagnostic_status),
                   "LOWER_OXYCLINE_GRADIENT_CANDIDATE")
})

test_that("oxygen boundary is storage-order invariant and incomplete-safe", {
  depths <- c(0, 20, 40, 60, 80, 100, 150)
  values <- c(220, 180, 80, 10, 5, 20, 80)
  fields <- c(
    "core_depth_m", "zone_observed_top_m", "zone_observed_bottom_m",
    "upper_boundary_depth_m", "lower_boundary_depth_m"
  )
  expect_equal(
    oxygen_boundary(c7_oxygen(values, depths), 20)[fields],
    oxygen_boundary(c7_oxygen(rev(values), rev(depths)), 20)[fields]
  )
  incomplete <- oxygen_boundary(c7_oxygen(
    c(220, 180, NA, 10, 5, 20, 80)
  ), 20)
  expect_identical(unique(incomplete$zone_status),
                   "INCOMPLETE_OXYGEN_PROFILE")
  expect_true(all(is.na(incomplete$core_depth_m)))
})

test_that("WOA23 oxygen fixture preserves metadata and manual C7 results", {
  fixture <- testthat::test_path(
    "fixtures", "real-data", "noaa-woa23-monthly-oxygen-fv1.nc"
  )
  nc <- ncdf4::nc_open(fixture)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  expect_identical(
    vapply(nc$dim, function(x) x$len, integer(1L)),
    c(nbounds = 2L, lon = 3L, lat = 3L, depth = 47L, time = 1L)
  )
  expect_identical(ncdf4::ncatt_get(nc, "o_an", "standard_name")$value,
                   "moles_of_oxygen_per_unit_mass_in_sea_water")
  expect_identical(ncdf4::ncatt_get(nc, "o_an", "units")$value,
                   "micromoles_per_kilogram")
  expect_match(ncdf4::ncatt_get(nc, "o_an", "cell_methods")$value,
               "depth: mean", fixed = TRUE)
  expect_identical(range(as.numeric(nc$dim$depth$vals)), c(0, 1000))
  expect_identical(ncdf4::ncatt_get(nc, "time", "units")$value,
                   "months since 1965-01-01 00:00:00")

  profile <- cube_slice(
    read_nc(fixture, vars = "o_an"),
    longitude = 1L, latitude = 1L, by = "index"
  )
  upper_local <- transition_layer(profile, "upper_oxycline", support = "local")
  upper_all <- transition_layer(profile, "upper_oxycline", support = "all")
  lower_local <- transition_layer(profile, "lower_oxycline", support = "local")
  lower_all <- transition_layer(profile, "lower_oxycline", support = "all")
  boundary <- oxygen_boundary(
    profile, 20, "micromoles_per_kilogram", support = "all"
  )
  expect_equal(unique(upper_local$core_depth_m), 325)
  expect_equal(unique(upper_local$feature_depth_m), 162.5)
  expect_equal(unique(upper_local$gradient), -2.070361, tolerance = 1e-6)
  expect_equal(upper_local[c("feature_depth_m", "gradient")],
               upper_all[c("feature_depth_m", "gradient")])
  expect_equal(unique(lower_local$feature_depth_m), 487.5)
  expect_equal(unique(lower_local$gradient), 0.190322, tolerance = 1e-6)
  expect_equal(lower_local[c("feature_depth_m", "gradient")],
               lower_all[c("feature_depth_m", "gradient")])
  expect_identical(unique(boundary$zone_status), "THRESHOLD_ZONE_PRESENT")
  expect_equal(unique(boundary$zone_observed_top_m), 275)
  expect_equal(unique(boundary$zone_observed_bottom_m), 425)
  expect_identical(unique(boundary$upper_boundary_status),
                   "CELL_MEAN_THRESHOLD_BRACKET_ONLY")
  expect_identical(unique(boundary$lower_boundary_status),
                   "CELL_MEAN_THRESHOLD_BRACKET_ONLY")
  expect_true(all(is.na(boundary$zone_observed_thickness_m)))
})

test_that("deferred WOA23 oxygen diagnostics read one scientific payload", {
  fixture <- testthat::test_path(
    "fixtures", "real-data", "noaa-woa23-monthly-oxygen-fv1.nc"
  )
  source <- cube_open(fixture, vars = "o_an")
  feature <- transition_layer(source, "upper_oxycline")
  boundary <- oxygen_boundary(source, 20)
  feature_qa <- attr(feature, "oceancube_qa")$oxygen_profile
  boundary_qa <- attr(boundary, "oceancube_qa")$oxygen_boundary
  expect_identical(feature_qa$netcdf_scientific_payload_reads, 1L)
  expect_identical(feature_qa$variables_read, "o_an")
  expect_identical(feature_qa$levels_read, 47L)
  expect_identical(boundary_qa$netcdf_scientific_payload_reads, 1L)
  expect_identical(boundary_qa$variables_read, "o_an")
  expect_identical(boundary_qa$levels_read, 47L)
})

test_that("derived and gradient-only oxygen inputs remain outside C7", {
  point <- c7_oxygen()
  sampled <- depth_sample(point, c(10, 30, 50, 70, 90, 120), method = "linear")
  expect_error(transition_layer(sampled, "upper_oxycline"),
               class = "oceancube_transition_derived_input")
  expect_error(transition_layer(depth_gradient(point), "upper_oxycline"),
               class = "oceancube_transition_derived_input")
  expect_error(oxygen_boundary(sampled, 20),
               class = "oceancube_transition_derived_input")
})

test_that("C6 thermocline and halocline routes remain unchanged", {
  temperature <- read_nc(c7_make_profile(list(
    temperature = c7_variable(
      c(25, 24, 19, 18, 17, 16, 15),
      "sea_water_temperature", "degree_Celsius"
    )
  )), vars = "temperature")
  salinity <- read_nc(c7_make_profile(list(
    salinity = c7_variable(
      c(34, 34.1, 35.1, 35.2, 35.3, 35.4, 35.5),
      "sea_water_practical_salinity", "1"
    )
  )), vars = "salinity")
  expect_identical(unique(transition_layer(
    temperature, "thermocline"
  )$diagnostic_status), "THERMOCLINE_GRADIENT_CANDIDATE")
  expect_identical(unique(transition_layer(
    salinity, "halocline"
  )$diagnostic_status), "HALOCLINE_GRADIENT_CANDIDATE")
})

test_that("C7 provenance QA serialization and privacy are bounded", {
  source <- c7_oxygen()
  upper <- transition_layer(source, "upper_oxycline")
  boundary <- oxygen_boundary(source, 20)
  expect_identical(tail(
    attr(upper, "oceancube_provenance")$history, 1L
  )[[1L]]$operation, "transition_layer")
  expect_identical(tail(
    attr(boundary, "oceancube_provenance")$history, 1L
  )[[1L]]$operation, "oxygen_boundary")
  upper_record <- tail(
    attr(upper, "oceancube_provenance")$history, 1L
  )[[1L]]
  boundary_record <- tail(
    attr(boundary, "oceancube_provenance")$history, 1L
  )[[1L]]
  expect_true(all(c(
    "core_member_indices", "core_depths_m", "branch_pair_indices",
    "candidate_status"
  ) %in% names(upper_record$parameters$resolved)))
  expect_true(all(c(
    "oxygen_core", "component_indices", "upper_boundary_evidence",
    "lower_boundary_evidence", "interpolation_profiles", "gapped_profiles",
    "exact_thickness_profiles"
  ) %in% names(boundary_record$parameters$resolved)))
  expect_true("oxygen_profile" %in% names(attr(upper, "oceancube_qa")))
  expect_true("oxygen_boundary" %in% names(attr(boundary, "oceancube_qa")))
  expect_identical(unserialize(serialize(upper, NULL)), upper)
  expect_identical(unserialize(serialize(boundary, NULL)), boundary)
  text <- paste(capture.output(str(list(
    attr(upper, "oceancube_provenance"),
    attr(boundary, "oceancube_provenance")
  ))), collapse = " ")
  expect_false(grepl("[A-Za-z]:[/\\\\]|oquispe|tempfile", text,
                     ignore.case = TRUE))
})
