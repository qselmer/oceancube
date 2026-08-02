# Refactor de coast_dist y cierre de accesos directos al almacenamiento.
# Ejecute desde la raiz del repositorio. No usa internet, aleatoriedad ni
# escritura persistente.

if (!exists("ocean_cube", mode = "function")) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Se necesita 'devtools' para cargar la copia local de oceancube.")
  }
  devtools::load_all(".", quiet = TRUE)
}
if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Se necesita el paquete sugerido 'sf' para este ejemplo.")
}

comprobar_coast <- function(condicion, mensaje) {
  if (!isTRUE(condicion)) {
    stop("FALLO: ", mensaje, call. = FALSE)
  }
  message("OK: ", mensaje)
}

# Cubo determinista: longitud x latitud x profundidad x tiempo x variable.
longitude <- c(-80, -79, -78)
latitude <- c(-12, -11)
depth <- c(0, 50)
time <- as.Date(c(
  "2020-01-01", "2020-02-01",
  "2021-01-01", "2021-02-01"
))
variable <- c("temperature", "oxygen")
values <- array(
  seq_len(3 * 2 * 2 * 4 * 2),
  dim = c(
    longitude = 3,
    latitude = 2,
    depth = 2,
    time = 4,
    variable = 2
  )
)

x <- ocean_cube(
  lon = longitude,
  lat = latitude,
  depth = depth,
  time = time,
  vars = variable,
  data = values,
  units = c(temperature = "degC", oxygen = "mmol m-3"),
  source = "coast-dist-backend-example"
)
x_before <- x
values_before <- oceancube:::.cube_read(x)

message("Longitudes: ", paste(x$lon, collapse = ", "))
message("Latitudes: ", paste(x$lat, collapse = ", "))
message("Backend de entrada: ", oceancube:::.cube_backend(x))

# Costa sencilla: una linea vertical un grado al oeste de la primera columna.
# coast_dist transforma la geometria a EPSG:4326 y calcula distancia geodesica
# con sf. La salida numerica se expresa contractualmente en millas nauticas.
coast <- sf::st_sfc(
  sf::st_linestring(
    matrix(
      c(-81, -13, -81, -10),
      ncol = 2,
      byrow = TRUE
    )
  ),
  crs = 4326
)

x_dc <- coast_dist(x, coast)
print(x_dc$dc)
message("Forma de dc: ", paste(dim(x_dc$dc), collapse = " x "))
message("Tipo de dc: ", typeof(x_dc$dc))
message(
  "Unidad contractual: millas nauticas; atributo units presente: ",
  !is.null(attr(x_dc$dc, "units"))
)
message(
  "Rango de distancias: ",
  paste(format(range(x_dc$dc), digits = 8), collapse = " a "),
  " millas nauticas"
)

comprobar_coast(
  identical(dim(x_dc$dc), c(3L, 2L)),
  "dc conserva el orden longitude x latitude"
)
comprobar_coast(
  all(is.finite(x_dc$dc)) && all(x_dc$dc >= 0),
  "las distancias son finitas y no negativas"
)
comprobar_coast(
  all(apply(x_dc$dc, 2, function(column) all(diff(column) > 0))),
  "la distancia aumenta al alejarse hacia el este"
)
comprobar_coast(
  identical(oceancube:::.cube_read(x_dc), values_before),
  "coast_dist no cambia los valores oceanograficos"
)
comprobar_coast(
  identical(x, x_before),
  "el cubo original permanece intacto"
)
comprobar_coast(
  identical(oceancube:::.cube_backend(x_dc), "memory"),
  "el backend memory se conserva"
)

# La forma logica se deriva de las coordenadas. La forma fisica la consulta el
# backend memory sin leer el contenido completo.
logical_shape <- oceancube:::.cube_shape(x)
storage_shape <- oceancube:::.cube_storage_shape(x)
message("Forma logica: ", paste(logical_shape, collapse = " x "))
message("Forma fisica: ", paste(storage_shape, collapse = " x "))
comprobar_coast(
  identical(unname(logical_shape), unname(storage_shape)),
  "las formas logica y fisica coinciden"
)

# coast_dist usa geometria, no temperatura, oxigeno ni otras variables.
different_values <- array(-999L, dim = unname(logical_shape))
x_different <- oceancube:::.cube_write_block(
  x,
  values = different_values,
  start = rep(1L, 5L),
  count = unname(logical_shape)
)
x_different_dc <- coast_dist(x_different, coast)
comprobar_coast(
  identical(x_dc$dc, x_different_dc$dc),
  "cubos con iguales coordenadas y valores distintos producen el mismo dc"
)

message(
  "REFACTOR COAST-DIST COMPLETADO: ",
  "todas las comprobaciones fueron satisfactorias."
)
