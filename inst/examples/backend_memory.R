# Backend en memoria: lectura completa, lectura por bloques y escritura
# Ejecute este script desde la raíz del repositorio. No usa internet ni escribe
# archivos. Las funciones demostradas son internas y no forman parte de la API.

if (!exists("ocean_cube", mode = "function")) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Se necesita 'devtools' para cargar la copia local de oceancube.")
  }
  devtools::load_all(".", quiet = TRUE)
}

# Se define sólo cuando el ejemplo del contrato no se ejecutó previamente.
if (!exists("make_baseline_cube", mode = "function")) {
  make_baseline_cube <- function() {
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
      dim = c(
        longitude = 3,
        latitude = 2,
        depth = 2,
        time = 4,
        variable = 2
      )
    )

    # Valor = 10000*m + 1000*l + 100*k + 10*j + i.
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
      lon = longitude,
      lat = latitude,
      depth = depth,
      time = time,
      data = values,
      vars = variable,
      units = c(temperature = "degC", oxygen = "mmol m-3"),
      source = "backend-memory-example"
    )
  }
}

x <- make_baseline_cube()
backend <- oceancube:::.cube_backend(x)
message("Backend detectado: ", backend)
stopifnot(identical(backend, "memory"))

# Lectura completa: el resultado coincide exactamente con el array almacenado.
all_values <- oceancube:::.cube_read(x)
message("Lectura completa: ", paste(dim(all_values), collapse = " x "))
stopifnot(identical(all_values, x$data))

# Los índices son posiciones 1-based, no valores de las coordenadas.
one_cell <- oceancube:::.cube_read(
  x,
  index = list(
    longitude = 1,
    latitude = 1,
    depth = 1,
    time = 1,
    variable = 1
  )
)
message("Celda [1,1,1,1,1]: ", one_cell[1, 1, 1, 1, 1])
stopifnot(
  identical(unname(dim(one_cell)), rep(1L, 5L)),
  identical(one_cell[1, 1, 1, 1, 1], 11111)
)

# Un bloque rectangular usa un inicio y una longitud por cada eje canónico.
start <- c(2, 1, 1, 2, 1)
count <- c(2, 2, 1, 2, 2)
block <- oceancube:::.cube_read_block(x, start = start, count = count)
direct_block <- x$data[2:3, 1:2, 1, 2:3, 1:2, drop = FALSE]
message("Bloque leído: ", paste(dim(block), collapse = " x "))
stopifnot(
  identical(block, direct_block),
  identical(unname(dim(block)), c(2L, 2L, 1L, 2L, 2L))
)

# La escritura devuelve otro cubo y reemplaza un bloque 1 x 2 x 1 x 2 x 1.
replacement <- array(-999, dim = c(1, 2, 1, 2, 1))
original_data <- x$data
y <- oceancube:::.cube_write_block(
  x,
  values = replacement,
  start = c(2, 1, 1, 2, 1)
)
written <- oceancube:::.cube_read_block(
  y,
  start = c(2, 1, 1, 2, 1),
  count = c(1, 2, 1, 2, 1)
)
message(
  "Escritura copy-on-modify: bloque nuevo=-999; original intacto=",
  identical(x$data, original_data)
)
stopifnot(
  identical(x$data, original_data),
  all(written == -999),
  identical(x$lon, y$lon),
  identical(dimnames(x$data), dimnames(y$data)),
  isTRUE(oceancube:::.check_cube(y))
)

# Un índice fuera de rango produce un error controlado antes de leer.
index_error <- tryCatch(
  {
    oceancube:::.cube_read(x, index = list(time = 100))
    NA_character_
  },
  error = conditionMessage
)
message("Índice inválido rechazado: ", index_error)
stopifnot(
  grepl("index\\$time", index_error),
  grepl("between 1 and 4", index_error),
  grepl("received: 100", index_error)
)

message("BACKEND MEMORY COMPLETADO: todas las comprobaciones fueron satisfactorias.")
