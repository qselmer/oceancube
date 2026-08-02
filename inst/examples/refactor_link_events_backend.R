# Refactor de link_events mediante la capa interna del backend.
# Ejecute desde la raiz del repositorio. No usa internet, aleatoriedad ni
# escritura persistente.

if (!exists("ocean_cube", mode = "function")) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Se necesita 'devtools' para cargar la copia local de oceancube.")
  }
  devtools::load_all(".", quiet = TRUE)
}

comprobar_link <- function(condicion, mensaje) {
  if (!isTRUE(condicion)) {
    stop("FALLO: ", mensaje, call. = FALSE)
  }
  message("OK: ", mensaje)
}

# El cubo determinista sigue el orden:
# longitud x latitud x profundidad x tiempo x variable.
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
    longitude = length(longitude),
    latitude = length(latitude),
    depth = length(depth),
    time = length(time),
    variable = length(variable)
  )
)

# La formula permite calcular cada valor sin depender de link_events().
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

x <- ocean_cube(
  lon = longitude,
  lat = latitude,
  depth = depth,
  time = time,
  data = values,
  vars = variable,
  units = c(temperature = "degC", oxygen = "mmol m-3"),
  source = "link-events-backend-example"
)
x_before <- x
message("Backend de entrada: ", oceancube:::.cube_backend(x))

events <- data.frame(
  event_id = c("inside", "edge", "outside"),
  lon = c(-80, -78, -70),
  lat = c(-12, -11, 0),
  date = as.Date(c("2020-01-01", "2021-02-01", "2020-01-01"))
)

# Una coordenada es un valor fisico; un indice es su posicion en el array.
# La localizacion nearest-neighbour se hace independientemente por eje.
# No es interpolacion: se devuelve el valor de una sola celda existente.
longitude_index <- vapply(
  events$lon,
  function(value) which.min(abs(longitude - value)),
  integer(1)
)
latitude_index <- vapply(
  events$lat,
  function(value) which.min(abs(latitude - value)),
  integer(1)
)
time_index <- vapply(
  events$date,
  function(value) which.min(abs(as.integer(time - value))),
  integer(1)
)
depth_index <- rep(1L, nrow(events))

selected_indices <- data.frame(
  event_id = events$event_id,
  longitude_index = longitude_index,
  latitude_index = latitude_index,
  depth_index = depth_index,
  time_index = time_index
)
print(selected_indices)

linked <- link_events(x, events, vars = "temperature")
print(linked)

# Comparacion independiente con el array determinista. El evento exterior
# conserva la politica publica: usa la celda de borde mas cercana, sin una
# distancia espacial maxima.
manual_temperature <- vapply(
  seq_len(nrow(events)),
  function(row) {
    values[
      longitude_index[row],
      latitude_index[row],
      depth_index[row],
      time_index[row],
      1L
    ]
  },
  numeric(1)
)
comprobar_link(
  identical(manual_temperature, c(11111, 14123, 11123)),
  "los valores manuales interior, borde y exterior son reproducibles"
)
comprobar_link(
  identical(linked$temperature_value, manual_temperature),
  "link_events coincide con la indexacion manual"
)
comprobar_link(
  identical(linked$.oceancube_lon, c(-80, -78, -78)) &&
    identical(linked$.oceancube_lat, c(-12, -11, -11)),
  "las coordenadas seleccionadas son las esperadas"
)

# La tolerancia temporal se expresa en dias y el limite es inclusivo.
tolerance_event <- data.frame(
  event_id = "one-day-away",
  lon = -80,
  lat = -12,
  date = as.Date("2020-01-02")
)
exact_only <- link_events(
  x,
  tolerance_event,
  vars = "temperature",
  time_tolerance = 0L
)
within_one_day <- link_events(
  x,
  tolerance_event,
  vars = "temperature",
  time_tolerance = 1L
)
comprobar_link(
  is.na(exact_only$temperature_value),
  "tolerancia cero rechaza una diferencia de un dia"
)
comprobar_link(
  identical(within_one_day$temperature_value, 11111) &&
    identical(within_one_day$.oceancube_dt_days, 1L),
  "tolerancia uno acepta exactamente un dia"
)

# Una lectura puntual conserva cinco dimensiones internamente y puede incluir
# todas las variables solicitadas. link_events las convierte en columnas.
point_array <- oceancube:::.cube_read(
  x,
  index = list(
    longitude = 1L,
    latitude = 1L,
    depth = 1L,
    time = 1L,
    variable = c(2L, 1L)
  )
)
comprobar_link(
  identical(unname(dim(point_array)), c(1L, 1L, 1L, 1L, 2L)),
  "la lectura puntual conserva las cinco dimensiones"
)

multiple <- link_events(
  x,
  events,
  vars = c("oxygen", "temperature"),
  prefix = "observed",
  keep_grid = FALSE
)
comprobar_link(
  identical(
    multiple$observed_oxygen,
    c(21111, 24123, 21123)
  ) &&
    identical(
      multiple$observed_temperature,
      c(11111, 14123, 11123)
    ),
  "la seleccion multiple respeta el orden solicitado"
)
comprobar_link(
  !any(startsWith(names(multiple), ".oceancube_")),
  "keep_grid = FALSE omite las columnas diagnosticas"
)
comprobar_link(
  all(
    c(
      ".oceancube_lon", ".oceancube_lat", ".oceancube_depth",
      ".oceancube_date", ".oceancube_dt_days"
    ) %in% names(linked)
  ),
  "keep_grid = TRUE conserva las columnas diagnosticas"
)
comprobar_link(
  identical(x, x_before),
  "el cubo original permanece intacto"
)

message(
  "REFACTOR LINK-EVENTS COMPLETADO: ",
  "todas las comprobaciones fueron satisfactorias."
)
