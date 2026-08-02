# cube_slice(): índices y coordenadas sin interpolación -------------------

cat("1. Construimos un cubo basal determinista de cinco dimensiones.\n")
longitude <- c(-80, -79, -78)
latitude <- c(-12, -11)
depth <- c(0, 50)
time <- as.Date(c(
  "2020-01-01", "2020-02-01",
  "2021-01-01", "2021-02-01"
))
variable <- c("temperature", "oxygen")
values <- array(
  NA_real_,
  dim = c(3L, 2L, 2L, 4L, 2L)
)
for (m in seq_along(variable)) {
  for (l in seq_along(time)) {
    for (k in seq_along(depth)) {
      for (j in seq_along(latitude)) {
        for (i in seq_along(longitude)) {
          values[i, j, k, l, m] <-
            10000 * m + 1000 * l + 100 * k + 10 * j + i
        }
      }
    }
  }
}
x <- ocean_cube(
  lon = longitude,
  lat = latitude,
  depth = depth,
  time = time,
  vars = variable,
  units = c(temperature = "degC", oxygen = "mmol m-3"),
  source = "ejemplo-sintetico",
  dataset_id = "cube-slice",
  data = values
)

cat("2. by='index': 3 significa la tercera posición, no longitud 3 grados.\n")
slice_index <- cube_slice(
  x,
  longitude = c(3L, 1L),
  time = c(4L, 2L),
  variable = c(2L, 1L),
  by = "index"
)
stopifnot(
  identical(slice_index$lon, c(-78, -80)),
  identical(slice_index$time, time[c(4L, 2L)]),
  identical(slice_index$vars, c("oxygen", "temperature"))
)

cat("3. by='value': -78 y -80 son coordenadas almacenadas.\n")
slice_value <- cube_slice(
  x,
  longitude = c(-78, -80),
  latitude = -11,
  depth = 50,
  time = as.Date(c("2021-02-01", "2020-01-01")),
  variable = c("oxygen", "temperature"),
  by = "value",
  match = "exact"
)
stopifnot(
  identical(unname(dim(slice_value$data)), c(2L, 1L, 1L, 2L, 2L)),
  identical(
    c(
      slice_value$data[1, 1, 1, 1, 1],
      slice_value$data[2, 1, 1, 1, 1],
      slice_value$data[1, 1, 1, 2, 2],
      slice_value$data[2, 1, 1, 2, 2]
    ),
    c(24223, 24221, 11223, 11221)
  )
)

cat("4. Nearest elige una celda existente; no interpola sus valores.\n")
nearest <- cube_slice(
  x,
  longitude = -79.4,
  latitude = -11.2,
  depth = 40,
  time = as.Date("2021-01-10"),
  variable = "temperature",
  by = "value",
  match = "nearest",
  tolerance = list(
    longitude = 0.5,
    latitude = 0.5,
    depth = 15,
    time = as.difftime(15, units = "days")
  )
)
stopifnot(
  identical(nearest$lon, -79),
  identical(nearest$lat, -11),
  identical(nearest$depth, 50),
  identical(nearest$time, as.Date("2021-01-01")),
  identical(nearest$data[1, 1, 1, 1, 1], 13222)
)

cat("5. La procedencia explica índices, coincidencias y distancias.\n")
nearest_record <- nearest$provenance$cube_slice
stopifnot(
  identical(nearest_record$resolved_indices$longitude, 2L),
  identical(nearest_record$resolved_indices$latitude, 2L),
  identical(nearest_record$resolved_indices$depth, 2L),
  identical(nearest_record$resolved_indices$time, 3L)
)
print(nearest_record$distances)

cat("6. Se conservan orden descendente, índices no contiguos y ejes singleton.\n")
descending <- cube_slice(
  x,
  longitude = c(-78, -80),
  time = as.Date(c("2021-02-01", "2020-01-01")),
  variable = "oxygen",
  by = "value"
)
singleton <- cube_slice(
  x,
  longitude = -80,
  latitude = -12,
  depth = 0,
  time = as.Date("2020-01-01"),
  variable = "temperature",
  by = "value"
)
stopifnot(
  identical(descending$lon, c(-78, -80)),
  identical(unname(dim(singleton$data)), rep(1L, 5L))
)

cat("7. Tanto una entrada memory como su selección terminan en memory.\n")
stopifnot(
  identical(oceancube:::.cube_backend(x), "memory"),
  identical(oceancube:::.cube_backend(slice_value), "memory")
)

cat("8. Los errores distinguen dominio y tolerancia.\n")
outside_domain <- try(
  cube_slice(
    x,
    longitude = -70,
    by = "value",
    match = "nearest"
  ),
  silent = TRUE
)
outside_tolerance <- try(
  cube_slice(
    x,
    depth = 40,
    by = "value",
    match = "nearest",
    tolerance = list(depth = 9)
  ),
  silent = TRUE
)
stopifnot(
  inherits(outside_domain, "try-error"),
  inherits(outside_tolerance, "try-error")
)

cat("9. Demostración NetCDF temporal: se lee solo la selección solicitada.\n")
stopifnot(requireNamespace("ncdf4", quietly = TRUE))
netcdf_file <- tempfile(fileext = ".nc")
on.exit(unlink(netcdf_file), add = TRUE)
lon_dim <- ncdf4::ncdim_def("longitude", "degrees_east", longitude)
lat_dim <- ncdf4::ncdim_def("latitude", "degrees_north", latitude)
depth_dim <- ncdf4::ncdim_def("depth", "m", depth)
time_dim <- ncdf4::ncdim_def(
  "time",
  "days since 2020-01-01 00:00:00",
  c(0, 31, 366, 397)
)
temperature_def <- ncdf4::ncvar_def(
  "temperature",
  "degC",
  list(lon_dim, lat_dim, depth_dim, time_dim),
  missval = -9999,
  prec = "double"
)
nc <- ncdf4::nc_create(netcdf_file, temperature_def)
tryCatch(
  {
    ncdf4::ncvar_put(nc, "temperature", values[, , , , 1])
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
  },
  finally = ncdf4::nc_close(nc)
)
storage <- oceancube:::.new_netcdf_storage(
  netcdf_file,
  "temperature",
  source = "ejemplo-netcdf"
)
x_netcdf <- oceancube:::.new_netcdf_cube(storage)
slice_netcdf <- cube_slice(
  x_netcdf,
  longitude = c(-80, -78),
  time = as.POSIXct(
    c("2020-01-01", "2021-02-01"),
    tz = "UTC"
  ),
  variable = "temperature",
  by = "value"
)
stopifnot(
  identical(oceancube:::.cube_backend(x_netcdf), "netcdf"),
  identical(oceancube:::.cube_backend(slice_netcdf), "memory"),
  identical(
    slice_netcdf$provenance$cube_slice$netcdf_read$variables,
    "temperature"
  )
)

cat("10. La selección materializada sobrevive al archivo NetCDF.\n")
netcdf_values <- oceancube:::.cube_read(slice_netcdf)
stopifnot(unlink(netcdf_file) == 0L)
stopifnot(identical(oceancube:::.cube_read(slice_netcdf), netcdf_values))

cat("CUBE_SLICE COMPLETADO: todas las comprobaciones fueron satisfactorias.\n")
