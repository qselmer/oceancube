# Línea base reproducible de oceancube
# Ejecute este archivo desde la raíz del repositorio. No usa internet, datos
# privados ni números aleatorios, y no escribe archivos.

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Se necesita el paquete 'devtools' para cargar la copia local.")
}

# Carga el código local para comprobar exactamente el estado del repositorio.
devtools::load_all(".", quiet = TRUE)

comprobar <- function(condicion, mensaje) {
  if (!isTRUE(condicion)) {
    stop("FALLO: ", mensaje, call. = FALSE)
  }
  message("OK: ", mensaje)
}

# Define las cinco coordenadas del contrato público:
# longitud x latitud x profundidad x tiempo x variable.
longitude <- c(-80, -79, -78)
latitude <- c(-12, -11)
depth <- c(0, 50)
time <- as.Date(c(
  "2020-01-01", "2020-02-01",
  "2021-01-01", "2021-02-01"
))
variable <- c("temperature", "oxygen")

dims <- c(
  longitude = length(longitude),
  latitude = length(latitude),
  depth = length(depth),
  time = length(time),
  variable = length(variable)
)

# La fórmula hace que cada celda pueda calcularse a mano:
# 10000*m + 1000*l + 100*k + 10*j + i.
values <- array(NA_real_, dim = dims)
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

cube <- ocean_cube(
  lon = longitude,
  lat = latitude,
  depth = depth,
  time = time,
  data = values,
  vars = variable,
  units = c(temperature = "degC", oxygen = "mmol m-3"),
  source = "deterministic-baseline"
)

# Inspecciona la estructura y exige dimensiones 3 x 2 x 2 x 4 x 2.
print(cube)
print(summary(cube))
message("Dimensiones: ", paste(dim(cube$data), collapse = " x "))
comprobar(
  identical(unname(dim(cube$data)), c(3L, 2L, 2L, 4L, 2L)),
  "el orden y las dimensiones del cubo son correctos"
)

# Comprueba dos esquinas. Según la fórmula deben valer 11111 y 24223.
comprobar(cube$data[1, 1, 1, 1, 1] == 11111, "la primera esquina vale 11111")
comprobar(cube$data[3, 2, 2, 4, 2] == 24223, "la última esquina vale 24223")

# Para enero, la primera celda de temperatura contiene 11111 y 13111.
# Su climatología manual es (11111 + 13111) / 2 = 12111.
climatology <- clim_month(cube)
jan_expected <- mean(c(11111, 13111))
jan_obtained <- climatology$mean[1, 1, 1, 1, 1]
jan_comparison <- all.equal(jan_obtained, jan_expected)
print(jan_comparison)
comprobar(isTRUE(jan_comparison), "la climatología de enero vale 12111")

# La anomalía del primer enero debe ser 11111 - 12111 = -1000.
anomaly <- anom_diff(cube, climatology)
anom_expected <- 11111 - jan_expected
anom_obtained <- anomaly$data[1, 1, 1, 1, 1]
anom_comparison <- all.equal(anom_obtained, anom_expected)
print(anom_comparison)
comprobar(isTRUE(anom_comparison), "la anomalía absoluta vale -1000")

# Extrae por vecino más cercano. El tercer evento está fuera del dominio y,
# conforme al comportamiento actual, se asigna a la celda espacial de borde.
events <- data.frame(
  event_id = c("inside", "edge", "outside"),
  lon = c(-80, -78, -70),
  lat = c(-12, -11, 0),
  date = as.Date(c("2020-01-01", "2021-02-01", "2020-01-01"))
)
linked <- link_events(cube, events, vars = "temperature")
print(linked)
comprobar(
  identical(linked$event_id, events$event_id),
  "link_events conserva las filas originales"
)
comprobar(
  isTRUE(all.equal(linked$temperature_value, c(11111, 14123, 11123))),
  "link_events devuelve los tres valores nearest-neighbour esperados"
)

message("LÍNEA BASE COMPLETADA: todas las comprobaciones fueron satisfactorias.")
