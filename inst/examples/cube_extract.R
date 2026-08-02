# cube_extract(): puntos, perfiles, series y tablas -----------------------

cat("1. Construimos el cubo basal determinista.\n")
longitude <- c(-80, -79, -78)
latitude <- c(-12, -11)
depth <- c(0, 50)
time <- as.Date(c(
  "2020-01-01", "2020-02-01",
  "2021-01-01", "2021-02-01"
))
variable <- c("temperature", "oxygen")
values <- array(NA_real_, dim = c(3L, 2L, 2L, 4L, 2L))
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
  dataset_id = "cube-extract",
  data = values
)

cat("2. Seleccionar devuelve un cubo; extraer devuelve una tabla.\n")
selected <- cube_slice(x, longitude = -79, latitude = -11)
extracted <- cube_extract(x, longitude = -79, latitude = -11)
stopifnot(
  inherits(selected, "ocean_cube"),
  is.data.frame(extracted),
  !inherits(extracted, "ocean_cube")
)

cat("3. Un punto fija los cinco ejes conceptuales.\n")
point <- cube_extract(
  x,
  longitude = -79,
  latitude = -11,
  depth = 50,
  time = as.Date("2021-02-01"),
  variable = "oxygen",
  mode = "point"
)
stopifnot(nrow(point) == 1L, identical(point$value, 24222))

cat("4. Un perfil vertical varía depth y puede incluir varias variables.\n")
profile <- cube_extract(
  x,
  longitude = -79,
  latitude = -11,
  time = as.Date("2021-02-01"),
  variable = c("temperature", "oxygen"),
  mode = "profile"
)
stopifnot(
  nrow(profile) == 4L,
  identical(profile$depth, c(0, 50, 0, 50))
)

cat("5. Una serie temporal varía time sin agregar ni ordenar.\n")
series <- cube_extract(
  x,
  longitude = -79,
  latitude = -11,
  depth = 0,
  variable = "temperature",
  mode = "series"
)
stopifnot(
  nrow(series) == 4L,
  identical(series$time, time),
  identical(series$value, c(11122, 12122, 13122, 14122))
)

cat("6. El modo table permite cualquier producto cartesiano.\n")
general <- cube_extract(
  x,
  longitude = c(-78, -80),
  latitude = c(-11, -12),
  depth = 50,
  time = as.Date(c("2021-02-01", "2020-01-01")),
  variable = c("oxygen", "temperature"),
  mode = "table"
)
stopifnot(nrow(general) == 16L)

cat("7. El formato largo contiene una fila por valor del array.\n")
stopifnot(identical(
  names(point),
  c("longitude", "latitude", "depth", "time", "variable", "unit", "value")
))

cat("8. El formato ancho contiene una columna por variable.\n")
wide <- cube_extract(
  x,
  longitude = -79,
  latitude = -11,
  depth = 0,
  mode = "series",
  format = "wide"
)
stopifnot(
  nrow(wide) == 4L,
  all(c("temperature", "oxygen") %in% names(wide))
)

cat("9-10. Los índices son globales y las columnas muestran coordenadas.\n")
indexed <- cube_extract(
  x,
  longitude = c(3L, 1L),
  latitude = 2L,
  depth = 2L,
  time = 4L,
  variable = 2L,
  by = "index",
  keep_index = TRUE
)
stopifnot(
  identical(indexed$longitude_index, c(3L, 1L)),
  identical(indexed$longitude, c(-78, -80))
)

cat("11. exact exige coordenadas almacenadas.\n")
exact_error <- try(cube_extract(x, longitude = -79.5), silent = TRUE)
stopifnot(inherits(exact_error, "try-error"))

cat("12-13. nearest elige una celda y tolerance limita la distancia.\n")
nearest <- cube_extract(
  x,
  longitude = -79.4,
  latitude = -11.2,
  depth = 40,
  time = as.Date("2021-01-10"),
  variable = "temperature",
  match = "nearest",
  tolerance = list(
    longitude = 0.5,
    latitude = 0.5,
    depth = 15,
    time = as.difftime(15, units = "days")
  ),
  keep_index = TRUE,
  keep_distance = TRUE
)
stopifnot(
  identical(nearest$longitude, -79),
  identical(nearest$depth, 50),
  identical(nearest$value, 13222)
)

cat("14. keep_index identifica cada posición del cubo original.\n")
stopifnot(
  identical(
    unname(unlist(nearest[c(
      "longitude_index", "latitude_index", "depth_index",
      "time_index", "variable_index"
    )])),
    c(2L, 2L, 2L, 3L, 1L)
  )
)

cat("15. keep_distance conserva solicitud y distancia por eje.\n")
stopifnot(
  isTRUE(all.equal(nearest$longitude_requested, -79.4)),
  isTRUE(all.equal(nearest$longitude_distance, 0.4)),
  identical(nearest$depth_distance, 10),
  as.numeric(nearest$time_distance, units = "days") == 9
)

cat("16. La unidad permanece alineada con cada variable.\n")
stopifnot(
  identical(profile$unit, c("degC", "degC", "mmol m-3", "mmol m-3"))
)

cat("17. Un dato ausente conserva su fila.\n")
x_missing <- x
x_missing$data[2, 2, 2, 4, 2] <- NA_real_
missing_point <- cube_extract(
  x_missing,
  longitude = -79,
  latitude = -11,
  depth = 50,
  time = as.Date("2021-02-01"),
  variable = "oxygen"
)
stopifnot(nrow(missing_point) == 1L, is.na(missing_point$value))

cat("18. NetCDF lee únicamente la selección solicitada.\n")
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
  "temperature", "degC",
  list(lon_dim, lat_dim, depth_dim, time_dim),
  missval = -9999, prec = "double"
)
oxygen_def <- ncdf4::ncvar_def(
  "oxygen", "mmol m-3",
  list(lon_dim, lat_dim, depth_dim, time_dim),
  missval = -9999, prec = "double"
)
nc <- ncdf4::nc_create(netcdf_file, list(temperature_def, oxygen_def))
tryCatch(
  {
    ncdf4::ncvar_put(nc, "temperature", values[, , , , 1])
    ncdf4::ncvar_put(nc, "oxygen", values[, , , , 2])
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
  c("temperature", "oxygen"),
  source = "ejemplo-netcdf"
)
x_netcdf <- oceancube:::.new_netcdf_cube(storage)
netcdf_point <- cube_extract(
  x_netcdf,
  longitude = -79,
  latitude = -11,
  depth = 50,
  time = x_netcdf$time[[4L]],
  variable = "oxygen"
)
read_record <- attr(netcdf_point, "oceancube_provenance")$netcdf_read
stopifnot(
  nrow(netcdf_point) == 1L,
  identical(read_record$variables, "oxygen"),
  identical(read_record$values_requested, 1)
)

cat("19. Memory y NetCDF producen la misma observación.\n")
stopifnot(identical(point$value, netcdf_point$value))

cat("20. link_events trabaja fila a fila; cube_extract crea una grilla.\n")
events <- data.frame(
  lon = c(-79, -80),
  lat = c(-11, -12),
  date = as.Date(c("2021-02-01", "2020-01-01"))
)
linked <- link_events(x, events, vars = "oxygen")
grid <- cube_extract(
  x,
  longitude = c(-79, -80),
  latitude = c(-11, -12),
  depth = 50,
  time = as.Date(c("2021-02-01", "2020-01-01")),
  variable = "oxygen"
)
stopifnot(nrow(linked) == 2L, nrow(grid) == 8L)

cat("21. Extraer sin selectores materializa todo el cubo como tabla.\n")
all_cells <- cube_extract(x)
stopifnot(nrow(all_cells) == prod(oceancube:::.cube_shape(x)))

cat("CUBE_EXTRACT COMPLETADO: todas las comprobaciones fueron satisfactorias.\n")
