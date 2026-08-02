local({
  if (!exists("make_netcdf_backend_fixture", mode = "function")) {
    source("tests/testthat/helper-netcdf-backend.R")
  }

  file <- make_netcdf_backend_fixture()
  on.exit(unlink(file), add = TRUE)
  storage <- oceancube:::.new_netcdf_storage(
    file,
    variables = c("temperature", "oxygen"),
    source = "ejemplo-indices",
    dataset_id = "ejemplo-collect"
  )
  x_netcdf <- oceancube:::.new_netcdf_cube(storage)
  cat("1. Fixture y cubo NetCDF temporal creados.\n")

  contiguous <- oceancube:::.cube_read(
    x_netcdf,
    index = list(longitude = 1:2, time = 2:3)
  )
  stopifnot(identical(unname(dim(contiguous)), c(2L, 2L, 2L, 2L, 2L)))
  cat("2. Lectura contigua comprobada.\n")

  index <- list(
    longitude = c(3L, 1L),
    time = c(4L, 2L),
    variable = c(2L, 1L)
  )
  plan <- oceancube:::.plan_cube_index_read(x_netcdf, index)
  selected <- oceancube:::.cube_read(x_netcdf, index = index)
  stopifnot(
    identical(plan$physical_start, c(
      longitude = 1L,
      latitude = 1L,
      depth = 1L,
      time = 2L
    )),
    identical(plan$physical_count, c(
      longitude = 3L,
      latitude = 2L,
      depth = 2L,
      time = 3L
    )),
    identical(plan$local_index$longitude, c(3L, 1L)),
    identical(plan$local_index$time, c(3L, 1L))
  )
  cat(
    "3. Envolvente mínima:",
    paste(plan$physical_count, collapse = " x "),
    "; amplificación:",
    plan$amplification,
    "\n"
  )
  cat(
    "4. Subsección local longitude:",
    paste(plan$local_index$longitude, collapse = ", "),
    "; time:",
    paste(plan$local_index$time, collapse = ", "),
    "\n"
  )
  stopifnot(
    identical(dimnames(selected)$longitude, c("-78", "-80")),
    identical(
      dimnames(selected)$variable,
      c("oxygen", "temperature")
    )
  )
  cat("5. Orden descendente y variables reordenadas conservados.\n")

  estimated_bytes <- oceancube:::.cube_estimated_bytes(x_netcdf)
  stopifnot(identical(estimated_bytes, 768))
  cat("6. Memoria lógica mínima estimada:", estimated_bytes, "bytes.\n")

  x_memory <- cube_collect(x_netcdf)
  stopifnot(
    identical(oceancube:::.cube_backend(x_netcdf), "netcdf"),
    identical(oceancube:::.cube_backend(x_memory), "memory"),
    identical(
      oceancube:::.cube_read(x_memory, index = index),
      selected
    ),
    identical(
      oceancube:::.cube_read(x_memory),
      oceancube:::.cube_read(x_netcdf)
    )
  )
  cat("7. cube_collect y equivalencia con memory comprobados.\n")

  values <- oceancube:::.cube_read(x_memory)
  stopifnot(unlink(file) == 0L, !file.exists(file))
  stopifnot(identical(oceancube:::.cube_read(x_memory), values))
  cat("8. El cubo recopilado funciona sin el archivo fuente.\n")

  modified <- oceancube:::.cube_write_block(
    x_memory,
    values = array(999, dim = rep(1L, 5L)),
    start = rep(1L, 5L)
  )
  stopifnot(
    identical(
      oceancube:::.cube_read(modified)[1, 1, 1, 1, 1],
      999
    ),
    identical(
      oceancube:::.cube_read(x_memory)[1, 1, 1, 1, 1],
      11111
    )
  )
  cat("9. Modificación independiente en memoria comprobada.\n")

  provenance <- x_memory$provenance$cube_collect
  stopifnot(
    identical(provenance$operation, "cube_collect"),
    identical(provenance$source_backend, "netcdf"),
    identical(provenance$target_backend, "memory")
  )
  cat("10. Procedencia de la materialización comprobada.\n")

  rds <- tempfile(tmpdir = tempdir(), fileext = ".rds")
  on.exit(unlink(rds), add = TRUE)
  saveRDS(x_memory, rds)
  restored <- readRDS(rds)
  stopifnot(
    identical(oceancube:::.cube_backend(restored), "memory"),
    identical(
      oceancube:::.cube_read(restored),
      oceancube:::.cube_read(x_memory)
    )
  )
  cat("11. Serialización y restauración comprobadas.\n")
  cat(
    paste0(
      "ÍNDICES NO CONTIGUOS Y CUBE_COLLECT COMPLETADOS: ",
      "todas las comprobaciones fueron satisfactorias.\n"
    )
  )
})
