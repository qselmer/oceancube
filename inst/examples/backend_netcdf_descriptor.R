# Descriptor NetCDF sin lectura de datos oceanograficos -------------------

make_netcdf_backend_fixture <- function() {
  file <- tempfile(
    pattern = "oceancube-descriptor-ejemplo-",
    tmpdir = tempdir(),
    fileext = ".nc"
  )
  lon <- ncdf4::ncdim_def(
    "longitude",
    "degrees_east",
    c(-80, -79, -78)
  )
  lat <- ncdf4::ncdim_def(
    "latitude",
    "degrees_north",
    c(-12, -11)
  )
  depth <- ncdf4::ncdim_def("depth", "m", c(0, 50))
  time <- ncdf4::ncdim_def(
    "time",
    "days since 2000-01-01 00:00:00",
    0:3
  )
  temperature <- ncdf4::ncvar_def(
    "temperature",
    "degree_Celsius",
    list(lon, lat, depth, time),
    missval = -32767L,
    longname = "Sea water potential temperature",
    prec = "short"
  )
  oxygen <- ncdf4::ncvar_def(
    "oxygen",
    "mmol m-3",
    list(time, depth, lat, lon),
    missval = -9999,
    longname = "Dissolved molecular oxygen",
    prec = "double"
  )

  nc <- ncdf4::nc_create(
    file,
    vars = list(temperature, oxygen),
    force_v4 = TRUE
  )
  tryCatch(
    {
      # Estos valores crean el fixture; el descriptor nunca los lee.
      ncdf4::ncvar_put(
        nc,
        "temperature",
        array(0:47, dim = c(3, 2, 2, 4))
      )
      ncdf4::ncvar_put(
        nc,
        "oxygen",
        array(as.double(101:148), dim = c(4, 2, 2, 3))
      )
      ncdf4::ncatt_put(nc, "longitude", "standard_name", "longitude")
      ncdf4::ncatt_put(nc, "longitude", "axis", "X")
      ncdf4::ncatt_put(nc, "latitude", "standard_name", "latitude")
      ncdf4::ncatt_put(nc, "latitude", "axis", "Y")
      ncdf4::ncatt_put(nc, "depth", "standard_name", "depth")
      ncdf4::ncatt_put(nc, "depth", "axis", "Z")
      ncdf4::ncatt_put(nc, "depth", "positive", "down")
      ncdf4::ncatt_put(nc, "time", "standard_name", "time")
      ncdf4::ncatt_put(nc, "time", "axis", "T")
      ncdf4::ncatt_put(nc, "time", "calendar", "gregorian")
      ncdf4::ncatt_put(
        nc,
        "temperature",
        "standard_name",
        "sea_water_potential_temperature"
      )
      ncdf4::ncatt_put(
        nc,
        "temperature",
        "missing_value",
        -32766L,
        prec = "short"
      )
      ncdf4::ncatt_put(
        nc,
        "temperature",
        "scale_factor",
        0.1,
        prec = "double"
      )
      ncdf4::ncatt_put(
        nc,
        "temperature",
        "add_offset",
        10,
        prec = "double"
      )
    },
    finally = ncdf4::nc_close(nc)
  )
  file
}

local({
  file <- make_netcdf_backend_fixture()
  rds <- tempfile(
    pattern = "oceancube-descriptor-ejemplo-",
    tmpdir = tempdir(),
    fileext = ".rds"
  )
  on.exit(unlink(c(file, paste0(file, ".moved"), rds)), add = TRUE)

cat("Fixture temporal:", file, "\n")
storage <- oceancube:::.new_netcdf_storage(
  file = file,
  variables = c("temperature", "oxygen"),
  source = "fixture-educativo",
  dataset_id = "descriptor-ejemplo"
)

cat("Ruta normalizada:", storage$file$normalized_path, "\n")
cat(
  "Dimensiones logicas:",
  paste(storage$dimensions$shape, collapse = " x "),
  "\n"
)
cat(
  "Variables:",
  paste(storage$variables$order, collapse = ", "),
  "\n"
)
cat(
  "Permutacion temperature:",
  paste(
    storage$variables$map$temperature$
      source_to_canonical_permutation,
    collapse = ", "
  ),
  "\n"
)
cat(
  "Permutacion oxygen:",
  paste(
    storage$variables$map$oxygen$
      source_to_canonical_permutation,
    collapse = ", "
  ),
  "\n"
)

x_nc <- oceancube:::.new_netcdf_cube(storage)
stopifnot(
  identical(class(x_nc), c("ocean_cube", "list")),
  identical(oceancube:::.cube_backend(x_nc), "netcdf"),
  identical(
    oceancube:::.cube_shape(x_nc),
    c(
      longitude = 3L,
      latitude = 2L,
      depth = 2L,
      time = 4L,
      variable = 2L
    )
  ),
  !"data" %in% names(x_nc),
  "storage" %in% names(x_nc)
)

print(x_nc)
print(summary(x_nc))

saveRDS(x_nc, rds)
restored <- readRDS(rds)
stopifnot(
  identical(oceancube:::.cube_backend(restored), "netcdf"),
  identical(
    oceancube:::.cube_shape(restored),
    oceancube:::.cube_shape(x_nc)
  ),
  !"data" %in% names(restored)
)
cat("Serializacion RDS: descriptor restaurado sin conexion abierta.\n")

read_values <- oceancube:::.cube_read(x_nc)
stopifnot(
  identical(unname(dim(read_values)), c(3L, 2L, 2L, 4L, 2L)),
  isTRUE(all.equal(read_values[1, 1, 1, 1, 1], 10))
)
cat("Lectura controlada: descriptor materializado en orden canónico.\n")

write_error <- tryCatch(
  {
    oceancube:::.cube_write_block(
      x_nc,
      values = array(0, dim = rep(1L, 5L)),
      start = rep(1L, 5L)
    )
    NA_character_
  },
  error = conditionMessage
)
stopifnot(grepl("NetCDF backend is read-only", write_error, fixed = TRUE))
cat("Escritura controlada:", write_error, "\n")

moved <- paste0(file, ".moved")
stopifnot(file.rename(file, moved))
stopifnot(file.rename(moved, file))
stopifnot(identical(unlink(file), 0L), !file.exists(file))
stopifnot(identical(unlink(rds), 0L), !file.exists(rds))

  message(
    "DESCRIPTOR NETCDF COMPLETADO: todas las comprobaciones fueron satisfactorias."
  )
})
