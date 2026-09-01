stopifnot("oceancube" %in% loadedNamespaces())

smoke_variable <- function(values, standard_name, unit) {
  list(values = values, standard_name = standard_name, unit = unit)
}

smoke_profile <- function(
    variables, depths, depth_unit = "m", cell_methods = "z: point",
    bounds = NULL) {
  path <- tempfile("oceancube-c8-installed-", fileext = ".nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", -80)
  lat <- ncdf4::ncdim_def("lat", "degrees_north", -12)
  z <- ncdf4::ncdim_def("z", depth_unit, depths)
  time <- ncdf4::ncdim_def("time", "days since 2000-01-01", 0)
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
    ncdf4::ncvar_put(nc, name, array(
      item$values, c(1, 1, length(depths), 1)
    ))
    ncdf4::ncatt_put(nc, name, "standard_name", item$standard_name)
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

smoke_temperature <- function(
    values = c(25.1, 25, 24.95, 24.6, 23.5),
    depths = c(0, 10, 20, 30, 40), ...) {
  oceancube::read_nc(smoke_profile(
    list(temperature = smoke_variable(
      values, "sea_water_temperature", "degrees_celsius"
    )), depths, ...
  ), vars = "temperature")
}

smoke_error <- function(expr) inherits(try(force(expr), silent = TRUE), "try-error")
smoke_mld <- function(x, ...) oceancube::mixed_layer_depth(x, ...)

stopifnot(length(getNamespaceExports("oceancube")) == 46L)
stopifnot("mixed_layer_depth" %in% getNamespaceExports("oceancube"))
stopifnot(identical(
  names(formals(oceancube::mixed_layer_depth)),
  c("x", "method", "variable", "reference_depth_m", "threshold", "support")
))

base <- smoke_mld(smoke_temperature())
expected <- 20 + 10 * (0.2 - 0.05) / (0.4 - 0.05)
stopifnot(isTRUE(all.equal(unique(base$mld_depth_m), expected)))
exact <- smoke_mld(smoke_temperature(c(25.1, 25, 24.9, 24.8, 24.5)))
stopifnot(identical(unique(exact$status), "MLD_EXACT_THRESHOLD_POINT"))
inversion <- smoke_mld(smoke_temperature(c(24.9, 25, 25.1, 25.4, 25.7)))
stopifnot(identical(unique(inversion$crossing_direction), "WARMER_WITH_DEPTH"))
multiple <- smoke_mld(smoke_temperature(c(25.1, 25, 24.7, 24.9, 24.5)))
stopifnot(isTRUE(all.equal(unique(multiple$mld_depth_m), 10 + 20 / 3)))
descending <- smoke_mld(smoke_temperature(
  rev(c(25.1, 25, 24.95, 24.6, 23.5)), rev(c(0, 10, 20, 30, 40))
))
stopifnot(isTRUE(all.equal(base$mld_depth_m, descending$mld_depth_m)))
kilometres <- smoke_mld(smoke_temperature(
  depths = c(0, 0.01, 0.02, 0.03, 0.04), depth_unit = "km"
))
stopifnot(isTRUE(all.equal(base$mld_depth_m, kilometres$mld_depth_m)))

reference <- smoke_mld(smoke_temperature(
  c(25.2, 25.1, 24.9, 24.5), c(0, 5, 15, 25)
))
stopifnot(identical(unique(reference$reference_status),
                    "REFERENCE_INTERPOLATED_POINT"))
reference_gap <- smoke_mld(smoke_temperature(
  c(25.2, 25.1, 24.9, 24.5), c(0, 5, 15, 25),
  bounds = rbind(c(0, 4), c(4, 7), c(13, 20), c(20, 30))
), support = "all")
stopifnot(identical(unique(reference_gap$status), "REFERENCE_GAPPED_BRACKET"))

missing_before <- smoke_mld(smoke_temperature(c(25.1, 25, NA, 24.6, 23.5)))
stopifnot(identical(unique(missing_before$status),
                    "MLD_UNRESOLVED_INCOMPLETE_PATH"))
missing_after <- smoke_mld(smoke_temperature(c(25.1, 25, 24.95, 24.6, NA)))
stopifnot(isTRUE(all.equal(base$mld_depth_m, missing_after$mld_depth_m)))
open <- smoke_mld(smoke_temperature(c(25.1, 25, 24.95, 24.9, 24.85)))
stopifnot(identical(unique(open$status), "MLD_OPEN_AT_PROFILE_BOTTOM"))

cell_mean <- smoke_temperature(
  cell_methods = "z: mean",
  bounds = rbind(c(-5, 5), c(5, 15), c(15, 25), c(25, 35), c(35, 45))
)
stopifnot(smoke_error(smoke_mld(cell_mean)))
fixture_root <- file.path("tests", "testthat", "fixtures", "real-data")
woa <- oceancube::read_nc(
  file.path(fixture_root, "noaa-woa23-monthly-vertical-fv1.nc"), vars = "t_an"
)
oisst <- oceancube::read_nc(
  file.path(fixture_root, "noaa-oisst21-surface-time-fv1.nc"), vars = "sst"
)
stopifnot(smoke_error(smoke_mld(woa)), smoke_error(smoke_mld(oisst)))

salinity <- oceancube::read_nc(smoke_profile(
  list(salinity = smoke_variable(
    c(34, 34.1, 34.8, 35), "sea_water_practical_salinity", "1"
  )), c(0, 10, 20, 30)
), vars = "salinity")
thermocline <- oceancube::transition_layer(
  smoke_temperature(c(25, 24, 19, 18), c(0, 10, 20, 30)), "thermocline"
)
halocline <- oceancube::transition_layer(salinity, "halocline")
stopifnot(identical(unique(thermocline$diagnostic_status),
                    "THERMOCLINE_GRADIENT_CANDIDATE"))
stopifnot(identical(unique(halocline$diagnostic_status),
                    "HALOCLINE_GRADIENT_CANDIDATE"))

oxygen <- oceancube::read_nc(smoke_profile(
  list(oxygen = smoke_variable(
    c(220, 180, 80, 10, 5, 20, 80),
    "moles_of_oxygen_per_unit_mass_in_sea_water", "umol kg-1"
  )), c(0, 20, 40, 60, 80, 100, 150)
), vars = "oxygen")
upper <- oceancube::transition_layer(oxygen, "upper_oxycline")
lower <- oceancube::transition_layer(oxygen, "lower_oxycline")
boundary <- oceancube::oxygen_boundary(oxygen, 20)
stopifnot(identical(unique(upper$diagnostic_status),
                    "UPPER_OXYCLINE_GRADIENT_CANDIDATE"))
stopifnot(identical(unique(lower$diagnostic_status),
                    "LOWER_OXYCLINE_GRADIENT_CANDIDATE"))
stopifnot(identical(unique(boundary$zone_status), "THRESHOLD_ZONE_PRESENT"))

cat("C8_INSTALLED_PUBLIC_SMOKE: PASS\n")
cat("C8_INSTALLED_API_EXPORTS: 46\n")
cat("C8_INSTALLED_INTERNAL_CALLS: 0\n")
