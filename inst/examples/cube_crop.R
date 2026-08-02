# cube_crop(): intervalos cerrados sobre centros de celda -----------------

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
  dataset_id = "cube-crop",
  data = values
)

cat("2. slice pide puntos; crop conserva todos los centros del intervalo.\n")
slice_points <- cube_slice(
  x,
  longitude = c(-80, -78),
  by = "value"
)
crop_interval <- cube_crop(
  x,
  longitude = c(-80, -78)
)
stopifnot(
  identical(slice_points$lon, c(-80, -78)),
  identical(crop_interval$lon, c(-80, -79, -78))
)

cat("3-7. Combinamos rangos de lon, lat, depth, time y variable exacta.\n")
cropped <- cube_crop(
  x,
  longitude = c(-79.5, -78),
  latitude = c(-12, -11),
  depth = c(0, 50),
  time = as.Date(c("2020-02-01", "2021-01-01")),
  variable = "oxygen"
)
stopifnot(
  identical(cropped$lon, c(-79, -78)),
  identical(cropped$lat, c(-12, -11)),
  identical(cropped$depth, c(0, 50)),
  identical(
    cropped$time,
    as.Date(c("2020-02-01", "2021-01-01"))
  ),
  identical(cropped$vars, "oxygen"),
  identical(unname(dim(cropped$data)), c(2L, 2L, 2L, 2L, 1L)),
  identical(
    c(
      cropped$data[1, 1, 1, 1, 1],
      cropped$data[2, 1, 1, 1, 1],
      cropped$data[1, 2, 2, 2, 1],
      cropped$data[2, 2, 2, 2, 1]
    ),
    c(22112, 22113, 23222, 23223)
  )
)

cat("8. bbox abrevia conjuntamente longitude y latitude.\n")
bbox_crop <- cube_crop(
  x,
  bbox = c(
    xmin = -80,
    ymin = -12,
    xmax = -79,
    ymax = -11
  )
)
explicit_crop <- cube_crop(
  x,
  longitude = c(-80, -79),
  latitude = c(-12, -11)
)
stopifnot(
  identical(
    oceancube:::.cube_read(bbox_crop),
    oceancube:::.cube_read(explicit_crop)
  )
)

cat("9. Los límites son inclusivos.\n")
inclusive <- cube_crop(x, longitude = c(-80, -79))
stopifnot(identical(inclusive$lon, c(-80, -79)))

cat("10. Se evalúan centros; no se inventan límites de celda.\n")
centres <- cube_crop(x, longitude = c(-79.8, -78.8))
stopifnot(identical(centres$lon, -79))

cat("11. Un eje descendente conserva su orden original.\n")
descending_x <- ocean_cube(
  lon = rev(longitude),
  lat = rev(latitude),
  depth = rev(depth),
  time = rev(time),
  vars = "temperature",
  data = values[3:1, 2:1, 2:1, 4:1, 1, drop = FALSE]
)
descending_crop <- cube_crop(
  descending_x,
  longitude = c(-80, -79),
  latitude = c(-12, -11),
  depth = c(0, 50),
  time = as.Date(c("2020-02-01", "2021-02-01"))
)
stopifnot(
  identical(descending_crop$lon, c(-79, -80)),
  identical(descending_crop$lat, c(-11, -12)),
  identical(descending_crop$depth, c(50, 0)),
  identical(
    descending_crop$time,
    as.Date(c("2021-02-01", "2021-01-01", "2020-02-01"))
  )
)

cat("12. outside='error' rechaza un rango que excede el dominio.\n")
outside_error <- try(
  cube_crop(x, longitude = c(-81, -79)),
  silent = TRUE
)
stopifnot(inherits(outside_error, "try-error"))

cat("13. outside='clip' registra rango solicitado y aplicado.\n")
clipped <- cube_crop(
  x,
  longitude = c(-81, -79),
  latitude = c(-12.5, -11),
  outside = "clip"
)
clip_record <- clipped$provenance$cube_crop
stopifnot(
  identical(clip_record$ranges_requested$longitude, c(-81, -79)),
  identical(clip_record$ranges_applied$longitude, c(-80, -79)),
  isTRUE(clip_record$clipped[["longitude"]]),
  isTRUE(clip_record$clipped[["latitude"]])
)

cat("14. Un intervalo sin centros no usa nearest y produce error.\n")
empty_centres <- try(
  cube_crop(x, longitude = c(-79.8, -79.6)),
  silent = TRUE
)
stopifnot(inherits(empty_centres, "try-error"))

cat("15. Un cubo superficial se conserva solo con depth=NULL.\n")
surface <- ocean_cube(
  lon = c(-80, -79),
  lat = -12,
  depth = NA_real_,
  time = as.Date(c("2020-01-01", "2020-02-01")),
  vars = "sst",
  data = array(1:4, dim = c(2, 1, 1, 2, 1))
)
surface_crop <- cube_crop(surface, longitude = c(-80, -80))
surface_error <- try(cube_crop(surface, depth = c(0, 0)), silent = TRUE)
stopifnot(
  identical(surface_crop$depth, NA_real_),
  inherits(surface_error, "try-error")
)

cat("16. La entrada memory y la salida recortada usan backend memory.\n")
stopifnot(
  identical(oceancube:::.cube_backend(x), "memory"),
  identical(oceancube:::.cube_backend(cropped), "memory")
)

cat("17. Creamos un NetCDF temporal para demostrar lectura parcial.\n")
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
oxygen_def <- ncdf4::ncvar_def(
  "oxygen",
  "mmol m-3",
  list(lon_dim, lat_dim, depth_dim, time_dim),
  missval = -9999,
  prec = "double"
)
nc <- ncdf4::nc_create(
  netcdf_file,
  list(temperature_def, oxygen_def)
)
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
netcdf_crop <- cube_crop(
  x_netcdf,
  longitude = c(-80, -79),
  latitude = c(-12, -12),
  time = as.POSIXct(c("2020-01-01", "2020-02-01"), tz = "UTC"),
  variable = "temperature"
)
netcdf_record <- netcdf_crop$provenance$cube_crop$netcdf_read
stopifnot(
  identical(oceancube:::.cube_backend(x_netcdf), "netcdf"),
  identical(oceancube:::.cube_backend(netcdf_crop), "memory"),
  identical(netcdf_record$variables, "temperature"),
  identical(
    netcdf_record$physical_count,
    c(longitude = 2L, latitude = 1L, depth = 2L, time = 2L)
  )
)

cat("18. La salida materializada sobrevive a la eliminación del archivo.\n")
materialized_values <- oceancube:::.cube_read(netcdf_crop)
stopifnot(unlink(netcdf_file) == 0L)
stopifnot(
  identical(
    oceancube:::.cube_read(netcdf_crop),
    materialized_values
  )
)

cat("19. La procedencia conserva rangos, índices y backend de origen.\n")
stopifnot(
  identical(
    netcdf_crop$provenance$cube_crop$backend_from,
    "netcdf"
  ),
  identical(
    netcdf_crop$provenance$cube_crop$backend_to,
    "memory"
  )
)

cat("20. dc y ocean_mask se subsetean cuando su forma es inequívoca.\n")
x$dc <- matrix(seq_len(6L), nrow = 3L, ncol = 2L)
x$mask <- stock_mask(x, stock = "ejemplo")
auxiliary_crop <- cube_crop(
  x,
  longitude = c(-79, -78),
  latitude = c(-11, -11),
  depth = c(50, 50)
)
stopifnot(
  identical(auxiliary_crop$dc, x$dc[2:3, 2L, drop = FALSE]),
  identical(
    auxiliary_crop$mask$mask,
    x$mask$mask[2:3, 2L, 2L, drop = FALSE]
  )
)

cat("21. Un recorte puntual conserva cinco dimensiones singleton.\n")
singleton <- cube_crop(
  x,
  longitude = c(-79, -79),
  latitude = c(-11, -11),
  depth = c(50, 50),
  time = as.Date(c("2021-02-01", "2021-02-01")),
  variable = "oxygen"
)
stopifnot(identical(unname(dim(singleton$data)), rep(1L, 5L)))

cat("CUBE_CROP COMPLETADO: todas las comprobaciones fueron satisfactorias.\n")
