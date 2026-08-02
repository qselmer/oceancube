# Contrato formal de la clase ocean_cube
# Ejecute este script desde la raíz del repositorio. No usa internet ni escribe
# archivos. Si el paquete local aún no está cargado, lo carga con devtools.

if (!exists("ocean_cube", mode = "function")) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Se necesita 'devtools' para cargar la copia local de oceancube.")
  }
  devtools::load_all(".", quiet = TRUE)
}

# Construye el mismo cubo determinista usado por la línea base científica.
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

  # Cada índice queda visible en el valor: 10000*m + 1000*l + 100*k + 10*j + i.
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
    source = "contract-example"
  )
}

# 1. Objeto válido, clase y dimensiones canónicas.
x <- make_baseline_cube()
print(x)
message("Clase: ", paste(class(x), collapse = ", "))
message("Dimensiones: ", paste(dim(x$data), collapse = " x "))
stopifnot(
  inherits(x, "ocean_cube"),
  identical(unname(dim(x$data)), c(3L, 2L, 2L, 4L, 2L))
)

# 2. Backend actual. El triple colon accede a una función interna; es útil para
# esta demostración, pero no debe usarse normalmente en análisis de usuario.
backend <- oceancube:::.cube_backend(x)
message("Backend: ", backend)
stopifnot(identical(backend, "memory"))

# 3. Validación interna. Devuelve TRUE de forma invisible si el contrato se cumple.
validation <- oceancube:::.check_cube(x)
stopifnot(isTRUE(validation))
message("Validación interna: OK")

# 4. Error controlado: se permutan longitud y latitud en el array, mientras las
# coordenadas siguen esperando 3 x 2. El mensaje muestra forma esperada y obtenida.
wrong_dimensions <- array(
  seq_len(96),
  dim = c(2, 3, 2, 4, 2)
)
dimension_error <- tryCatch(
  {
    ocean_cube(
      lon = x$lon,
      lat = x$lat,
      depth = x$depth,
      time = x$time,
      data = wrong_dimensions,
      vars = x$vars,
      units = x$units
    )
    NA_character_
  },
  error = conditionMessage
)
message("Error dimensional esperado: ", dimension_error)
stopifnot(grepl("expected", dimension_error), grepl("obtained", dimension_error))

# 5. Un cubo superficial usa un único nivel NA y una tercera dimensión de uno.
surface <- ocean_cube(
  lon = c(-80, -79),
  lat = c(-12, -11),
  depth = NA_real_,
  time = as.Date("2020-01-01"),
  data = array(1:4, dim = c(2, 2, 1, 1, 1)),
  vars = "temperature",
  units = "degC"
)
message(
  "Superficie válida: depth=", surface$depth,
  "; dimensiones=", paste(dim(surface$data), collapse = " x ")
)
stopifnot(is.na(surface$depth), dim(surface$data)[3] == 1L)

# 6. Un vector de profundidad vacío no representa superficie y se rechaza.
empty_depth_error <- tryCatch(
  {
    ocean_cube(
      lon = c(-80, -79),
      lat = c(-12, -11),
      depth = numeric(0),
      time = as.Date("2020-01-01"),
      data = array(numeric(0), dim = c(2, 2, 0, 1, 1)),
      vars = "temperature"
    )
    NA_character_
  },
  error = conditionMessage
)
message("Profundidad vacía rechazada: ", empty_depth_error)
stopifnot(grepl("depth", empty_depth_error), grepl("must not be empty", empty_depth_error))

message("CONTRATO OCEAN_CUBE COMPLETADO: todas las comprobaciones fueron satisfactorias.")
