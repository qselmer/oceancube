isolated <- Sys.getenv("OCEANCUBE_ISOLATED_LIB", unset = NA_character_)
stopifnot(!is.na(isolated), dir.exists(isolated))
.libPaths(c(isolated, .libPaths()))
source("dev/hardening/density-stratification/c10-installed-smoke.R", chdir = FALSE)

# Exercise every C1-C10 public family explicitly from the installed package.
cell_path <- c10_smoke_file(list(
  temperature = c10_smoke_variable(
    c(1, 3, 5), "sea_water_temperature", "K"
  )
), depths = c(5, 15, 25), bounds = rbind(c(0, 10), c(10, 20), c(20, 30)))
nc <- ncdf4::nc_open(cell_path, write = TRUE)
ncdf4::ncatt_put(nc, "temperature", "cell_methods", "z: mean")
ncdf4::nc_close(nc)
cell <- oceancube::read_nc(cell_path, vars = "temperature")
stopifnot(
  identical(as.numeric(oceancube::cube_layer_thickness(cell, unit = "m")),
            c(10, 10, 10)),
  as.numeric(oceancube::layer_mean(cell, c(0, 30))$data) == 3,
  as.numeric(oceancube::layer_integral(cell, c(0, 30))$data) == 90
)

sampled <- oceancube::depth_sample(temperature, depth = 15)
gradient <- oceancube::depth_gradient(temperature)
feature <- oceancube::depth_feature(gradient, polarity = "negative")
stopifnot(inherits(sampled, "ocean_cube"), nrow(feature) == 1L)

salinity_path <- c10_smoke_file(list(
  salinity = c10_smoke_variable(
    c(34, 34.1, 34.5, 35, 35.1, 35.2),
    "sea_water_practical_salinity", "1"
  )
))
salinity <- oceancube::read_nc(salinity_path, vars = "salinity")
halocline <- oceancube::transition_layer(salinity, diagnostic = "halocline")
lower_oxycline <- oceancube::transition_layer(
  oxygen, diagnostic = "lower_oxycline"
)
stopifnot(
  identical(halocline$diagnostic_status, "HALOCLINE_GRADIENT_CANDIDATE"),
  identical(lower_oxycline$diagnostic_status,
            "LOWER_OXYCLINE_GRADIENT_CANDIDATE")
)

cat("C_EXIT_INSTALLED_WITH_GSW: PASS\n")
cat("C_EXIT_INSTALLED_PUBLIC_CHAIN: C1-C10 ALL REQUIRED FAMILIES PASS\n")
