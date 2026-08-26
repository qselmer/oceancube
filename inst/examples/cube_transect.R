# Ejemplo educativo de cube_transect()

# 1. Construcción del cubo basal.
make_baseline_cube <- function() {
  longitude <- c(-80, -79, -78)
  latitude <- c(-12, -11)
  depth <- c(0, 50)
  time <- as.Date(c(
    "2020-01-01", "2020-02-01", "2021-01-01", "2021-02-01"
  ))
  variable <- c("temperature", "oxygen")
  values <- array(
    NA_real_,
    dim = c(3, 2, 2, 4, 2),
    dimnames = list(
      longitude = longitude, latitude = latitude, depth = depth,
      time = time, variable = variable
    )
  )
  for (i in seq_along(longitude)) {
    for (j in seq_along(latitude)) {
      for (k in seq_along(depth)) {
        for (l in seq_along(time)) {
          for (m in seq_along(variable)) {
            values[i, j, k, l, m] <-
              10000 * m + 1000 * l + 100 * k + 10 * j + i
          }
        }
      }
    }
  }
  ocean_cube(
    lon = longitude, lat = latitude, depth = depth, time = time,
    vars = variable,
    units = c(temperature = "degC", oxygen = "mmol m-3"),
    data = values
  )
}

x <- make_baseline_cube()

# 2. Creación de una trayectoria.
path <- data.frame(
  station = c("A", "B", "C"),
  longitude = c(-80, -79, -78),
  latitude = c(-12, -11, -12)
)

# 3. El orden de los puntos es el orden de las filas de path.
stopifnot(identical(path$station, c("A", "B", "C")))

# 4. Son tres pares, no las 3 × 2 combinaciones de longitude × latitude.
stopifnot(nrow(path) == 3L, nrow(unique(path[c("longitude", "latitude")])) == 3L)

# 5. Perfil vertical de un único punto.
profile <- cube_transect(
  x, path[1, ], id_col = "station", time = as.Date("2021-02-01"),
  variable = c("temperature", "oxygen"), match = "exact", mode = "profile"
)
stopifnot(nrow(profile) == 4L)

# 6. Transecto horizontal en una profundidad.
horizontal <- cube_transect(
  x, path, id_col = "station", depth = 0,
  time = as.Date("2021-02-01"), variable = "temperature",
  match = "exact", mode = "horizontal"
)
stopifnot(nrow(horizontal) == 3L)

# 7. Sección vertical: punto × profundidad × variable.
section <- cube_transect(
  x, path, id_col = "station", depth = c(0, 50),
  time = as.Date("2021-02-01"),
  variable = c("temperature", "oxygen"),
  match = "exact", mode = "section", keep_index = TRUE
)
stopifnot(nrow(section) == 12L)

# 8. Selección exacta: cada coordenada debe existir en el cubo.
stopifnot(identical(unique(section$longitude), path$longitude))

# 9. Nearest selecciona celdas, no interpola valores.
path_nearest <- data.frame(
  station = c("N1", "N2", "N3"),
  longitude = c(-79.8, -79.2, -78.2),
  latitude = c(-11.8, -11.1, -11.8)
)

# 10. Tolerancias máximas por eje.
nearest <- cube_transect(
  x, path_nearest, id_col = "station", depth = 40,
  time = as.Date("2021-01-10"), variable = "temperature",
  tolerance = list(
    longitude = 0.5, latitude = 0.5, depth = 15,
    time = as.difftime(15, units = "days")
  ),
  mode = "horizontal", keep_index = TRUE
)
stopifnot(nrow(nearest) == 3L)

# 11. Distancia acumulada sobre las coordenadas solicitadas.
stopifnot(nearest$requested_distance_km[[1L]] == 0)

# 12. Distancia acumulada sobre las celdas coincidentes.
stopifnot(nearest$matched_distance_km[[1L]] == 0)

# 13. Puntos diferentes pueden caer en una misma celda sin colapsarse.
same_cell_path <- data.frame(
  longitude = c(-79.4, -79.2), latitude = c(-11.2, -11.1)
)
same_cell <- cube_transect(
  x, same_cell_path, depth = 0, time = as.Date("2021-01-01"),
  variable = "temperature", mode = "horizontal"
)
stopifnot(nrow(same_cell) == 2L, same_cell$value[[1L]] == same_cell$value[[2L]])

# 14. Formato largo: una fila por punto × profundidad × variable.
stopifnot(all(c("variable", "unit", "value") %in% names(section)))

# 15. Formato ancho: una fila por punto × profundidad.
section_wide <- cube_transect(
  x, path, id_col = "station", depth = c(0, 50),
  time = as.Date("2021-02-01"),
  variable = c("temperature", "oxygen"), match = "exact",
  mode = "section", format = "wide"
)
stopifnot(nrow(section_wide) == 6L)

# 16. Los índices son globales y opcionales.
stopifnot(all(c(
  "longitude_index", "latitude_index", "depth_index",
  "time_index", "variable_index"
) %in% names(section)))

# 17. El orden de variables solicitado se conserva.
stopifnot(identical(unique(section$variable), c("temperature", "oxygen")))

# 18. Las unidades permanecen alineadas con las variables.
stopifnot(identical(
  attr(section, "units"),
  c(temperature = "degC", oxygen = "mmol m-3")
))

# 19. Un dato ausente válido conserva su fila.
x_na <- x
x_na$data[1, 1, 1, 4, 1] <- NA_real_
section_na <- cube_transect(
  x_na, path[1, ], depth = 0, time = as.Date("2021-02-01"),
  variable = "temperature", match = "exact", mode = "profile"
)
stopifnot(nrow(section_na) == 1L, is.na(section_na$value))

# 20. Un NetCDF temporal se lee por pares con una sola conexión.
make_example_netcdf <- function(values) {
  file <- tempfile("oceancube-transect-", fileext = ".nc")
  lon_dim <- ncdf4::ncdim_def("longitude", "degrees_east", x$lon)
  lat_dim <- ncdf4::ncdim_def("latitude", "degrees_north", x$lat)
  depth_dim <- ncdf4::ncdim_def("depth", "m", x$depth)
  time_dim <- ncdf4::ncdim_def(
    "time", "days since 2020-01-01 00:00:00", c(0, 31, 366, 397)
  )
  definitions <- lapply(seq_along(x$vars), function(i) {
    ncdf4::ncvar_def(
      x$vars[[i]], x$units[[i]],
      list(lon_dim, lat_dim, depth_dim, time_dim),
      missval = -9999, prec = "double"
    )
  })
  nc <- ncdf4::nc_create(file, definitions)
  tryCatch({
    ncdf4::ncvar_put(nc, "temperature", values[, , , , 1])
    ncdf4::ncvar_put(nc, "oxygen", values[, , , , 2])
    ncdf4::ncatt_put(nc, "longitude", "standard_name", "longitude")
    ncdf4::ncatt_put(nc, "longitude", "axis", "X")
    ncdf4::ncatt_put(nc, "latitude", "standard_name", "latitude")
    ncdf4::ncatt_put(nc, "latitude", "axis", "Y")
    ncdf4::ncatt_put(nc, "depth", "standard_name", "depth")
    ncdf4::ncatt_put(nc, "depth", "axis", "Z")
    ncdf4::ncatt_put(nc, "time", "standard_name", "time")
    ncdf4::ncatt_put(nc, "time", "axis", "T")
    ncdf4::ncatt_put(nc, "time", "calendar", "gregorian")
  }, finally = ncdf4::nc_close(nc))
  file
}

netcdf_file <- make_example_netcdf(x$data)
x_netcdf <- oceancube:::.new_netcdf_cube(
  oceancube:::.new_netcdf_storage(
    netcdf_file, c("temperature", "oxygen")
  )
)
x_memory_equivalent <- cube_collect(x_netcdf)
equivalent_time <- x_netcdf$time[[4L]]
section_netcdf <- cube_transect(
  x_netcdf, path, id_col = "station", depth = c(0, 50),
  time = equivalent_time, variable = c("temperature", "oxygen"),
  match = "exact", mode = "section", keep_index = TRUE
)
metrics <- attr(section_netcdf, "oceancube_qa")$transect$physical_reads
stopifnot(metrics$n_open == 1L, metrics$n_ncvar_get == 6L)

# 21. Los datos memory y NetCDF son equivalentes.
section_memory_equivalent <- cube_transect(
  x_memory_equivalent, path, id_col = "station", depth = c(0, 50),
  time = equivalent_time, variable = c("temperature", "oxygen"),
  match = "exact", mode = "section", keep_index = TRUE
)
strip_light_attributes <- function(z) {
  attributes(z) <- attributes(z)[c("names", "row.names", "class")]
  z
}
stopifnot(isTRUE(all.equal(
  strip_light_attributes(section_memory_equivalent),
  strip_light_attributes(section_netcdf)
)))

# 22. Nearest no interpola: cada valor procede de una celda almacenada.
stopifnot(nearest$value %in% as.vector(x$data))

# 23. La trayectoria no se densifica: conserva exactamente sus puntos.
stopifnot(length(unique(nearest$point_order)) == nrow(path_nearest))

# 24. La tabla materializada no depende posteriormente del archivo.
removed <- file.remove(netcdf_file)
stopifnot(removed, nrow(section_netcdf) == 12L)
invisible(summary(section_netcdf))

cat("CUBE_TRANSECT COMPLETADO: todas las comprobaciones fueron satisfactorias.\n")
