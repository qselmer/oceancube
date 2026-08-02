# Ejemplo educativo de cube_mask()
stopifnot(requireNamespace("sf", quietly = TRUE))

# 1. Construcción del cubo basal.
make_baseline_cube <- function() {
  lon <- c(-80, -79, -78)
  lat <- c(-12, -11)
  depth <- c(0, 50)
  time <- as.Date(c(
    "2020-01-01", "2020-02-01", "2021-01-01", "2021-02-01"
  ))
  vars <- c("temperature", "oxygen")
  values <- array(NA_real_, dim = c(3, 2, 2, 4, 2))
  for (i in seq_along(lon)) {
    for (j in seq_along(lat)) {
      for (k in seq_along(depth)) {
        for (l in seq_along(time)) {
          for (m in seq_along(vars)) {
            values[i, j, k, l, m] <-
              10000 * m + 1000 * l + 100 * k + 10 * j + i
          }
        }
      }
    }
  }
  ocean_cube(
    lon, lat, time, values, depth, vars,
    units = c(temperature = "degC", oxygen = "mmol m-3")
  )
}

x <- make_baseline_cube()
values_original <- oceancube:::.cube_read(x)

# 2. Creación de un polígono.
rectangle <- function(xmin, ymin, xmax, ymax) {
  sf::st_sfc(sf::st_polygon(list(matrix(
    c(
      xmin, ymin, xmax, ymin, xmax, ymax,
      xmin, ymax, xmin, ymin
    ),
    ncol = 2, byrow = TRUE
  ))), crs = 4326)
}
polygon <- rectangle(-80.5, -12.5, -78.5, -10.5)

# 3. Los centros son longitude × latitude, con longitude más rápida.
centers <- expand.grid(longitude = x$lon, latitude = x$lat)
stopifnot(nrow(centers) == 6L)

# 4. La clasificación inicial es una máscara espacial 2D.
# 5. Esa máscara se aplica explícitamente sobre el cubo 5D.
# 6. keep = "inside" conserva centros cubiertos.
inside <- cube_mask(x, polygon, keep = "inside", boundary = "include")
stopifnot(identical(unname(oceancube:::.cube_shape(inside)),
                    unname(oceancube:::.cube_shape(x))))

# 7. keep = "outside" conserva el complemento geométrico.
outside <- cube_mask(x, polygon, keep = "outside")
stopifnot(identical(
  outside$mask$polygon_keep[, , 1L],
  !inside$mask$polygon_keep[, , 1L]
))

# 8. Borde incluido.
edge_polygon <- rectangle(-80.5, -12.5, -79, -10.5)
edge_include <- cube_mask(x, edge_polygon, boundary = "include")

# 9. Borde excluido.
edge_exclude <- cube_mask(x, edge_polygon, boundary = "exclude")
stopifnot(
  edge_include$mask$coverage$n_centers_inside_polygon >
    edge_exclude$mask$coverage$n_centers_inside_polygon
)

# 10. Varias features se combinan por unión.
feature_a <- rectangle(-80.2, -12.2, -79.8, -11.8)[[1L]]
feature_b <- rectangle(-78.2, -11.2, -77.8, -10.8)[[1L]]
multiple <- sf::st_sfc(feature_a, feature_b, crs = 4326)
multiple_result <- cube_mask(x, multiple)
stopifnot(multiple_result$mask$coverage$n_polygon_features == 2L)

# 11. Un hueco interior no se rellena.
outer <- matrix(
  c(-80.5, -12.5, -77.5, -12.5, -77.5, -10.5,
    -80.5, -10.5, -80.5, -12.5),
  ncol = 2, byrow = TRUE
)
hole <- matrix(
  c(-79.2, -11.2, -79.2, -10.8, -78.8, -10.8,
    -78.8, -11.2, -79.2, -11.2),
  ncol = 2, byrow = TRUE
)
with_hole <- sf::st_sfc(sf::st_polygon(list(outer, hole)), crs = 4326)
hole_result <- cube_mask(x, with_hole)
stopifnot(!hole_result$mask$polygon_keep[2, 2, 1])

# 12. Cobertura por conteo de centros.
coverage <- inside$mask$coverage
stopifnot(
  coverage$n_spatial_cells_total == 6L,
  coverage$n_centers_inside_polygon == 4L
)

# 13. Esta fracción no es cobertura por área o km².
stopifnot(
  coverage$semantics == "cell_center",
  coverage$fraction_cells_kept == 4 / 6
)

# 14. La forma es idéntica antes y después.
values_inside <- oceancube:::.cube_read(inside)
stopifnot(identical(dim(values_original), dim(values_inside)))

# 15. Los valores conservados no cambian.
stopifnot(values_inside[1, 1, 1, 1, 1] ==
            values_original[1, 1, 1, 1, 1])

# 16. Los valores excluidos se convierten en NA.
stopifnot(is.na(values_inside[3, 1, 1, 1, 1]))

# 17. Una máscara previa se combina mediante AND.
x_previous <- x
x_previous$mask <- stock_mask(x, depth = c(0, 0))
combined <- cube_mask(x_previous, polygon)
stopifnot(!any(
  combined$mask$mask & !x_previous$mask$mask
))

# 18. dc se conserva; derivados potencialmente obsoletos se descartan.
x_aux <- x
x_aux$dc <- matrix(seq_len(6), nrow = 3L)
x_aux$climatology <- list(old = TRUE)
aux_result <- cube_mask(x_aux, polygon)
stopifnot(identical(aux_result$dc, x_aux$dc))
stopifnot(is.null(aux_result$climatology))

# 19. NetCDF inside usa el bounding rectangle mínimo.
make_mask_netcdf <- function(values) {
  file <- tempfile("oceancube-mask-", fileext = ".nc")
  lon_dim <- ncdf4::ncdim_def("longitude", "degrees_east", x$lon)
  lat_dim <- ncdf4::ncdim_def("latitude", "degrees_north", x$lat)
  depth_dim <- ncdf4::ncdim_def("depth", "m", x$depth)
  time_dim <- ncdf4::ncdim_def(
    "time", "days since 2020-01-01", c(0, 31, 366, 397)
  )
  defs <- lapply(seq_along(x$vars), function(i) {
    ncdf4::ncvar_def(
      x$vars[[i]], x$units[[i]],
      list(lon_dim, lat_dim, depth_dim, time_dim),
      missval = -9999, prec = "double"
    )
  })
  nc <- ncdf4::nc_create(file, defs)
  tryCatch({
    ncdf4::ncvar_put(nc, "temperature", values[, , , , 1])
    ncdf4::ncvar_put(nc, "oxygen", values[, , , , 2])
    for (item in list(
      c("longitude", "longitude", "X"),
      c("latitude", "latitude", "Y"),
      c("depth", "depth", "Z"),
      c("time", "time", "T")
    )) {
      ncdf4::ncatt_put(nc, item[[1]], "standard_name", item[[2]])
      ncdf4::ncatt_put(nc, item[[1]], "axis", item[[3]])
    }
    ncdf4::ncatt_put(nc, "time", "calendar", "gregorian")
  }, finally = ncdf4::nc_close(nc))
  file
}

source_file <- make_mask_netcdf(values_original)
x_netcdf <- oceancube:::.new_netcdf_cube(
  oceancube:::.new_netcdf_storage(
    source_file, c("temperature", "oxygen")
  )
)
small_polygon <- rectangle(-80.2, -12.2, -79.8, -11.8)
masked_netcdf <- cube_mask(x_netcdf, small_polygon)
read_metrics <-
  masked_netcdf$provenance$cube_mask$bounding_rectangle_read
stopifnot(
  read_metrics$spatial_read == "bounding_rectangle",
  read_metrics$spatial_cells_in_bbox == 1L,
  read_metrics$n_open == 1L
)

# 20. La salida NetCDF siempre usa backend memory.
stopifnot(oceancube:::.cube_backend(masked_netcdf) == "memory")

# 21. La salida sigue funcionando después de eliminar el NetCDF.
stopifnot(file.remove(source_file))
stopifnot(nrow(summary(masked_netcdf)) == 5L)

# 22. No existe interpolación: solo valores originales o NA.
observed <- as.vector(oceancube:::.cube_read(masked_netcdf))
stopifnot(all(is.na(observed) | observed %in% as.vector(values_original)))

# 23. No se recorta ninguna dimensión.
stopifnot(identical(
  unname(oceancube:::.cube_shape(masked_netcdf)),
  unname(oceancube:::.cube_shape(x))
))

# 24. No se calcula ni almacena ponderación o fracción por área.
stopifnot(!any(c(
  "area_fraction", "area_coverage", "weighted_area"
) %in% names(masked_netcdf$mask$coverage)))

cat("CUBE_MASK COMPLETADO: todas las comprobaciones fueron satisfactorias.\n")
