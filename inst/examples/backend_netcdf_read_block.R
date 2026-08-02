local({
  stopifnot(requireNamespace("ncdf4", quietly = TRUE))

  formula_4d <- function(variable_index) {
    result <- array(NA_real_, dim = c(3L, 2L, 2L, 4L))
    for (time_index in seq_len(4L)) {
      for (depth_index in seq_len(2L)) {
        for (latitude_index in seq_len(2L)) {
          for (longitude_index in seq_len(3L)) {
            result[
              longitude_index,
              latitude_index,
              depth_index,
              time_index
            ] <- 10000 * variable_index +
              1000 * time_index +
              100 * depth_index +
              10 * latitude_index +
              longitude_index
          }
        }
      }
    }
    result
  }

  file <- tempfile(
    pattern = "oceancube-read-block-",
    tmpdir = tempdir(),
    fileext = ".nc"
  )
  on.exit(unlink(file), add = TRUE)

  longitude <- c(-80, -79, -78)
  latitude <- c(-12, -11)
  depth <- c(0, 50)
  time <- 0:3
  lon_dim <- ncdf4::ncdim_def(
    "longitude",
    "degrees_east",
    longitude
  )
  lat_dim <- ncdf4::ncdim_def(
    "latitude",
    "degrees_north",
    latitude
  )
  depth_dim <- ncdf4::ncdim_def("depth", "m", depth)
  time_dim <- ncdf4::ncdim_def(
    "time",
    "days since 2020-01-01 00:00:00",
    time
  )
  temperature_def <- ncdf4::ncvar_def(
    "temperature",
    "degree_Celsius",
    list(lon_dim, lat_dim, depth_dim, time_dim),
    missval = -32767L,
    prec = "short"
  )
  oxygen_def <- ncdf4::ncvar_def(
    "oxygen",
    "mmol m-3",
    list(time_dim, depth_dim, lat_dim, lon_dim),
    missval = -9999,
    prec = "double"
  )

  temperature <- formula_4d(1L)
  packed_temperature <- (temperature - 11000) / 0.5
  packed_temperature[1, 2, 2, 2] <- -32767
  packed_temperature[3, 1, 2, 3] <- -32766
  temperature[1, 2, 2, 2] <- NA_real_
  temperature[3, 1, 2, 3] <- NA_real_
  oxygen <- formula_4d(2L)
  oxygen[1, 2, 1, 3] <- NA_real_

  nc <- ncdf4::nc_create(
    file,
    list(temperature_def, oxygen_def),
    force_v4 = TRUE
  )
  tryCatch(
    {
      ncdf4::ncvar_put(nc, "temperature", packed_temperature)
      ncdf4::ncvar_put(
        nc,
        "oxygen",
        aperm(oxygen, c(4L, 3L, 2L, 1L))
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
        0.5,
        prec = "double"
      )
      ncdf4::ncatt_put(
        nc,
        "temperature",
        "add_offset",
        11000,
        prec = "double"
      )
      for (item in list(
        c("longitude", "longitude", "X"),
        c("latitude", "latitude", "Y"),
        c("depth", "depth", "Z"),
        c("time", "time", "T")
      )) {
        ncdf4::ncatt_put(nc, item[[1L]], "standard_name", item[[2L]])
        ncdf4::ncatt_put(nc, item[[1L]], "axis", item[[3L]])
      }
      ncdf4::ncatt_put(nc, "depth", "positive", "down")
      ncdf4::ncatt_put(nc, "time", "calendar", "gregorian")
    },
    finally = ncdf4::nc_close(nc)
  )

  cat("1. Fixture temporal creado en tempdir().\n")
  storage <- oceancube:::.new_netcdf_storage(
    file,
    variables = c("temperature", "oxygen"),
    source = "ejemplo-educativo"
  )
  x_nc <- oceancube:::.new_netcdf_cube(storage)
  cat("2. Backend:", oceancube:::.cube_backend(x_nc), "\n")
  cat(
    "3. Forma lógica:",
    paste(oceancube:::.cube_shape(x_nc), collapse = " x "),
    "\n"
  )

  cell <- oceancube:::.cube_read_block(
    x_nc,
    start = c(1L, 1L, 1L, 1L, 1L),
    count = c(1L, 1L, 1L, 1L, 1L)
  )
  stopifnot(identical(unname(cell), array(11111, dim = rep(1L, 5L))))
  cat("4. Una celda:", cell[1, 1, 1, 1, 1], "\n")

  start <- c(2L, 1L, 1L, 2L, 1L)
  count <- c(2L, 2L, 1L, 2L, 2L)
  block <- oceancube:::.cube_read_block(x_nc, start, count)
  stopifnot(identical(unname(dim(block)), count))
  cat(
    "5. start indica la primera posición de lon, lat, depth, time y variable:",
    paste(start, collapse = ", "),
    "\n"
  )
  cat(
    "6. count indica longitudes contiguas y también la forma del bloque:",
    paste(count, collapse = ", "),
    "\n"
  )
  cat(
    "7. Dos variables forman el quinto eje:",
    paste(dimnames(block)$variable, collapse = ", "),
    "\n"
  )

  oxygen_translation <- oceancube:::.translate_netcdf_block(
    storage$variables$map$oxygen,
    canonical_start = start[1:4],
    canonical_count = count[1:4]
  )
  cat(
    "8. oxygen se solicita físicamente como",
    paste(oxygen_translation$source_axes, collapse = " x "),
    "y se permuta a longitude x latitude x depth x time.\n"
  )

  full <- oceancube:::.cube_read(x_nc)
  stopifnot(
    identical(full[1, 1, 1, 1, 1], 11111),
    identical(full[3, 2, 2, 4, 2], 24223),
    is.na(full[1, 2, 2, 2, 1]),
    is.na(full[3, 1, 2, 3, 1])
  )
  cat("9. Lectura completa:", paste(dim(full), collapse = " x "), "\n")

  expected <- array(
    NA_real_,
    dim = c(3L, 2L, 2L, 4L, 2L)
  )
  expected[, , , , 1] <- temperature
  expected[, , , , 2] <- oxygen
  x_memory <- ocean_cube(
    lon = longitude,
    lat = latitude,
    depth = depth,
    time = as.Date("2020-01-01") + time,
    vars = c("temperature", "oxygen"),
    data = expected
  )
  dimnames(x_memory$data) <- dimnames(full)
  stopifnot(identical(oceancube:::.cube_read(x_memory), full))
  cat("10. Equivalencia con memory: comprobada.\n")
  cat("11. Fill value y missing_value: ambos se devolvieron como NA.\n")

  noncontiguous <- oceancube:::.cube_read(
    x_nc,
    index = list(longitude = c(1L, 3L))
  )
  stopifnot(
    identical(
      noncontiguous,
      full[c(1L, 3L), , , , , drop = FALSE]
    )
  )
  cat("12. Índice no contiguo: lectura ordenada comprobada.\n")

  read_only <- tryCatch(
    {
      oceancube:::.cube_write_block(
        x_nc,
        array(0, dim = rep(1L, 5L)),
        rep(1L, 5L)
      )
      NULL
    },
    error = identity
  )
  stopifnot(inherits(read_only, "oceancube_netcdf_read_only"))
  cat("13. Escritura: backend read-only comprobado.\n")

  rds <- tempfile(tmpdir = tempdir(), fileext = ".rds")
  on.exit(unlink(rds), add = TRUE)
  saveRDS(x_nc, rds)
  restored <- readRDS(rds)
  restored_cell <- oceancube:::.cube_read_block(
    restored,
    rep(1L, 5L),
    rep(1L, 5L)
  )
  stopifnot(
    identical(
      unname(restored_cell),
      array(11111, dim = rep(1L, 5L))
    )
  )
  cat("14. Serialización y restauración: lectura comprobada.\n")

  stopifnot(unlink(file) == 0L, !file.exists(file))
  cat("15. Conexión cerrada: el fixture pudo eliminarse.\n")
  cat(
    paste0(
      "LECTURA NETCDF POR BLOQUES COMPLETADA: ",
      "todas las comprobaciones fueron satisfactorias.\n"
    )
  )
})
