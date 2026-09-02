stopifnot(requireNamespace("oceancube", quietly = TRUE))
stopifnot(requireNamespace("gsw", quietly = TRUE))
stopifnot(requireNamespace("ncdf4", quietly = TRUE))

c9_smoke_variable <- function(values, standard_name, unit,
                              cell_methods = "z: point") {
  list(values = values, standard_name = standard_name, unit = unit,
       cell_methods = cell_methods)
}

c9_smoke_file <- function(variables, depths = c(10, 50, 125, 250, 600, 1000),
                          depth_unit = "m", lon = c(188, 189),
                          lat = c(4, 20), pressure_name = "sea_water_pressure") {
  file <- tempfile(fileext = ".nc")
  x <- ncdf4::ncdim_def("lon", "degrees_east", lon)
  y <- ncdf4::ncdim_def("lat", "degrees_north", lat)
  z <- ncdf4::ncdim_def("z", depth_unit, depths)
  time <- ncdf4::ncdim_def("time", "days since 2000-01-01", 0)
  defs <- lapply(names(variables), function(name) ncdf4::ncvar_def(
    name, variables[[name]]$unit, list(x, y, z, time), missval = -9999,
    prec = "double"
  ))
  nc <- ncdf4::nc_create(file, defs)
  for (name in names(variables)) {
    item <- variables[[name]]
    values <- if (length(item$values) == length(depths)) {
      rep(item$values, each = length(lon) * length(lat))
    } else item$values
    ncdf4::ncvar_put(nc, name, array(
      values, c(length(lon), length(lat), length(depths), 1)
    ))
    standard_name <- if (identical(name, "pressure")) pressure_name else {
      item$standard_name
    }
    if (!is.null(standard_name)) {
      ncdf4::ncatt_put(nc, name, "standard_name", standard_name)
    }
    ncdf4::ncatt_put(nc, name, "cell_methods", item$cell_methods)
  }
  ncdf4::ncatt_put(nc, "z", "standard_name", "depth")
  ncdf4::ncatt_put(nc, "z", "positive", "down")
  ncdf4::ncatt_put(nc, "z", "axis", "Z")
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.13")
  ncdf4::nc_close(nc)
  file
}

c9_smoke_source <- function(
    salinity_standard_name = "sea_water_practical_salinity",
    salinity_unit = "1",
    salinity_values = c(34.5487, 34.7275, 34.8605, 34.6810, 34.5680, 34.5600),
    temperature_standard_name = "sea_water_temperature",
    temperature_unit = "degree_Celsius",
    temperature_values = c(28.7856, 28.4329, 22.8103, 10.2600, 6.8863, 4.4036),
    pressure_values = NULL,
    pressure_unit = "dbar",
    cell_methods = "z: point",
    ...) {
  variables <- list(
    salinity = c9_smoke_variable(
      salinity_values, salinity_standard_name, salinity_unit, cell_methods
    ),
    temperature = c9_smoke_variable(
      temperature_values, temperature_standard_name, temperature_unit,
      cell_methods
    )
  )
  if (!is.null(pressure_values)) {
    variables$pressure <- c9_smoke_variable(
      pressure_values, "sea_water_pressure", pressure_unit, cell_methods
    )
  }
  file <- c9_smoke_file(variables, ...)
  oceancube::cube_open(file, vars = names(variables))
}

c9_rejects <- function(expr, class = NULL) {
  condition <- tryCatch({force(expr); NULL}, error = identity)
  if (is.null(condition)) return(FALSE)
  is.null(class) || inherits(condition, class)
}

stopifnot(length(getNamespaceExports("oceancube")) == 47L)
stopifnot("thermodynamic_state" %in% getNamespaceExports("oceancube"))
stopifnot(identical(
  names(formals(oceancube::thermodynamic_state)),
  c("x", "salinity", "temperature", "pressure", "reference_pressure_dbar")
))

base <- c9_smoke_source()
sp_t <- oceancube::thermodynamic_state(base)
stopifnot(identical(sp_t$vars, c(
  "absolute_salinity", "conservative_temperature", "sea_water_pressure",
  "sea_water_density", "sea_water_potential_density"
)))
stopifnot(all(is.finite(sp_t$data)))
stopifnot(sp_t$qa$thermodynamic_state$netcdf_payload_reads == 1L)

depth <- base$depth
p <- gsw::gsw_p_from_z(-depth, 4)
SP <- c(34.5487, 34.7275, 34.8605, 34.6810, 34.5680, 34.5600)
SA <- gsw::gsw_SA_from_SP(SP, p, 188, 4)
t <- c(28.7856, 28.4329, 22.8103, 10.2600, 6.8863, 4.4036)
CT <- gsw::gsw_CT_from_t(SA, t, p)
pt0 <- gsw::gsw_pt_from_CT(SA, CT)

sa_ct <- oceancube::thermodynamic_state(c9_smoke_source(
  salinity_standard_name = "sea_water_absolute_salinity",
  salinity_unit = "g kg-1", salinity_values = SA,
  temperature_standard_name = "sea_water_conservative_temperature",
  temperature_values = CT
))
sp_ct <- oceancube::thermodynamic_state(c9_smoke_source(
  temperature_standard_name = "sea_water_conservative_temperature",
  temperature_values = CT
))
sa_t <- oceancube::thermodynamic_state(c9_smoke_source(
  salinity_standard_name = "sea_water_absolute_salinity",
  salinity_unit = "g/kg", salinity_values = SA
))
pt_state <- oceancube::thermodynamic_state(c9_smoke_source(
  salinity_standard_name = "sea_water_absolute_salinity",
  salinity_unit = "g kg-1", salinity_values = SA,
  temperature_standard_name = "sea_water_potential_temperature",
  temperature_values = pt0
))
for (state in list(sa_ct, sp_ct, sa_t, pt_state)) {
  stopifnot(max(abs(as.numeric(state$data[, , , , 1])[
    seq(1, 24, by = 4)
  ] - SA)) < 1.5e-8)
  stopifnot(max(abs(as.numeric(state$data[, , , , 2])[
    seq(1, 24, by = 4)
  ] - CT)) < 1.5e-8)
}

explicit <- oceancube::thermodynamic_state(c9_smoke_source(
  pressure_values = p
), pressure = "pressure")
stopifnot(max(abs(
  as.numeric(explicit$data[1, 1, , , ]) -
    as.numeric(sp_t$data[1, 1, , , ])
)) < 1.5e-8)

kelvin <- oceancube::thermodynamic_state(c9_smoke_source(
  temperature_unit = "K", temperature_values = t + 273.15
))
kilometres <- oceancube::thermodynamic_state(c9_smoke_source(
  depths = depth / 1000, depth_unit = "km"
))
descending <- oceancube::thermodynamic_state(c9_smoke_source(
  depths = rev(depth), salinity_values = rev(SP), temperature_values = rev(t)
))
stopifnot(max(abs(as.numeric(kelvin$data) - as.numeric(sp_t$data))) < 1.5e-8)
stopifnot(max(abs(as.numeric(kilometres$data) - as.numeric(sp_t$data))) < 1.5e-8)
stopifnot(max(abs(
  as.numeric(descending$data[, , 6:1, , , drop = FALSE]) -
    as.numeric(sp_t$data)
)) < 1.5e-8)

west <- oceancube::thermodynamic_state(c9_smoke_source(lon = c(-172, -171)))
stopifnot(max(abs(as.numeric(west$data) - as.numeric(sp_t$data))) < 1.5e-8)
stopifnot(!isTRUE(all.equal(
  sp_t$data[1, 1, 1, 1, 3], sp_t$data[1, 2, 1, 1, 3]
)))

rho0 <- oceancube::thermodynamic_state(base, reference_pressure_dbar = 0)
rho1000 <- oceancube::thermodynamic_state(base, reference_pressure_dbar = 1000)
stopifnot(max(abs(
  rho0$data[, , , , 4] - rho1000$data[, , , , 4]
)) < 1.5e-8)
stopifnot(!isTRUE(all.equal(
  rho0$data[, , , , 5], rho1000$data[, , , , 5]
)))
stopifnot(max(abs(
  as.numeric(rho0$data[, , , , 5]) - 1000 -
    gsw::gsw_sigma0(as.numeric(rho0$data[, , , , 1]),
                    as.numeric(rho0$data[, , , , 2]))
)) < 1.5e-8)

missing <- oceancube::thermodynamic_state(c9_smoke_source(
  temperature_values = c(t[1], NA, t[-c(1, 2)])
))
stopifnot(all(is.finite(missing$data[, , , , 3])))
stopifnot(anyNA(missing$data[, , , , 4]))

stopifnot(c9_rejects(
  oceancube::thermodynamic_state(c9_smoke_source(
    salinity_standard_name = "sea_water_salinity"
  )), "oceancube_teos10_salinity_variable"
))
stopifnot(c9_rejects(
  oceancube::thermodynamic_state(c9_smoke_source(
    salinity_values = c(-1, rep(35, 5))
  )), "oceancube_teos10_negative_salinity"
))
stopifnot(c9_rejects(
  oceancube::thermodynamic_state(c9_smoke_source(
    salinity_standard_name = "sea_water_absolute_salinity",
    salinity_unit = "g kg-1", salinity_values = rep(35, 6),
    temperature_standard_name = "sea_water_conservative_temperature",
    temperature_values = rep(-4, 6)
  )), "oceancube_teos10_outside_funnel"
))
stopifnot(c9_rejects(
  oceancube::thermodynamic_state(c9_smoke_source(cell_methods = "z: mean")),
  "oceancube_teos10_value_semantics_unsupported"
))

wrong_pressure_file <- c9_smoke_file(list(
  salinity = c9_smoke_variable(SP, "sea_water_practical_salinity", "1"),
  temperature = c9_smoke_variable(t, "sea_water_temperature", "degree_Celsius"),
  pressure = c9_smoke_variable(p, "air_pressure", "dbar")
), pressure_name = "air_pressure")
wrong_pressure <- oceancube::cube_open(
  wrong_pressure_file, vars = c("salinity", "temperature", "pressure")
)
stopifnot(c9_rejects(
  oceancube::thermodynamic_state(wrong_pressure, pressure = "pressure"),
  "oceancube_teos10_pressure_variable"
))

source_root <- Sys.getenv("OCEANCUBE_SOURCE_ROOT", unset = "")
if (nzchar(source_root)) {
  fixtures <- file.path(source_root, "tests", "testthat", "fixtures", "real-data")
  woa <- oceancube::cube_open(
    file.path(fixtures, "noaa-woa23-monthly-vertical-fv1.nc"),
    vars = c("t_an", "s_an")
  )
  stopifnot(c9_rejects(
    oceancube::thermodynamic_state(woa),
    "oceancube_teos10_value_semantics_unsupported"
  ))
  oisst <- oceancube::cube_open(
    file.path(fixtures, "noaa-oisst21-surface-time-fv1.nc"), vars = "sst"
  )
  stopifnot(c9_rejects(oceancube::thermodynamic_state(oisst)))
}

temperature_only <- oceancube::cube_slice(base, variable = "temperature")
stopifnot(c9_rejects(
  oceancube::mixed_layer_depth(temperature_only, method = "density_threshold"),
  "oceancube_mld_method_unsupported"
))
stopifnot(nrow(oceancube::mixed_layer_depth(
  oceancube::cube_slice(c9_smoke_source(
    salinity_values = rep(35, 6),
    temperature_values = c(25.1, 25, 24.95, 24.6, 23.5, 22),
    depths = c(0, 10, 20, 30, 40, 50)
  ), variable = "temperature")
)) == 4L)
stopifnot(nrow(oceancube::transition_layer(
  oceancube::cube_slice(c9_smoke_source(
    salinity_values = rep(35, 6),
    temperature_values = c(25, 24, 19, 18, 17, 16),
    depths = c(0, 10, 20, 30, 40, 50)
  ), variable = "temperature"), "thermocline"
)) == 4L)

oxygen_file <- c9_smoke_file(list(
  oxygen = c9_smoke_variable(
    c(220, 180, 80, 10, 5, 20),
    "moles_of_oxygen_per_unit_mass_in_sea_water", "umol kg-1"
  )
))
oxygen <- oceancube::cube_open(oxygen_file, vars = "oxygen")
stopifnot(nrow(oceancube::transition_layer(oxygen, "upper_oxycline")) == 4L)
stopifnot(nrow(oceancube::oxygen_boundary(oxygen, 20)) == 4L)

serialized <- unserialize(serialize(sp_t, NULL))
stopifnot(identical(serialized, sp_t))

cat("C9_INSTALLED_PUBLIC_SMOKE: PASS\n")
cat("C9_INSTALLED_API_EXPORTS: 47\n")
cat("C9_INSTALLED_GSW_VERSION: ", as.character(utils::packageVersion("gsw")), "\n", sep = "")
cat("C9_INSTALLED_INTERNAL_CALLS: 0\n")
