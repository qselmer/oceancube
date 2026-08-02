# Refactor vertical y anual mediante la capa interna del backend.
# Ejecute desde la raíz del repositorio. No usa internet, aleatoriedad ni
# escritura persistente.

if (!exists("ocean_cube", mode = "function")) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Se necesita 'devtools' para cargar la copia local de oceancube.")
  }
  devtools::load_all(".", quiet = TRUE)
}

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
      source = "layer-annual-backend-example"
    )
  }
}

x <- make_baseline_cube()
original_data <- x$data
message("Backend de entrada: ", oceancube:::.cube_backend(x))

# Los bordes inferidos para profundidades 0 y 50 son 0, 25 y 75.
# La capa solicitada 0-50 usa pesos de 25 para ambos niveles.
layer <- layer_mean(x, depth = c(0, 50))
surface_value <- 11111
deep_value <- 11211
expected_layer <- (surface_value * 25 + deep_value * 25) / 50
observed_layer <- layer$data[1, 1, 1, 1, 1]
layer_comparison <- all.equal(observed_layer, expected_layer)

message(
  "Capa [lon=1, lat=1, layer=1, time=1, variable=1]: manual=",
  expected_layer,
  "; función=", observed_layer
)
message("all.equal(capa): ", layer_comparison)
message("Backend de la capa: ", oceancube:::.cube_backend(layer))

# annual_index resume longitud, latitud y tiempos del año, pero mantiene una
# fila independiente por profundidad y variable. Aquí se localiza depth=0 y
# temperature, es decir, las posiciones depth=1 y variable=1 del cubo.
indicators <- annual_index(x)
time_2020 <- x$time < as.Date("2021-01-01")
time_2021 <- x$time >= as.Date("2021-01-01")
expected_2020 <- mean(x$data[, , 1, time_2020, 1])
expected_2021 <- mean(x$data[, , 1, time_2021, 1])

selected <- indicators$var == "temperature" & indicators$depth == 0
observed_annual <- indicators$mean_value[selected]
expected_annual <- c(expected_2020, expected_2021)
annual_comparison <- all.equal(observed_annual, expected_annual)

message(
  "Índice 2020 depth=0 temperature: manual=",
  expected_2020,
  "; función=", observed_annual[[1]]
)
message(
  "Índice 2021 depth=0 temperature: manual=",
  expected_2021,
  "; función=", observed_annual[[2]]
)
message("all.equal(índices anuales): ", annual_comparison)
message("Cubo original intacto: ", identical(x$data, original_data))

stopifnot(
  identical(oceancube:::.cube_backend(x), "memory"),
  identical(oceancube:::.cube_backend(layer), "memory"),
  isTRUE(layer_comparison),
  isTRUE(annual_comparison),
  identical(expected_layer, 11161),
  identical(expected_annual, c(11617, 13617)),
  identical(x$data, original_data)
)

message("REFACTOR LAYER-ANNUAL COMPLETADO: todas las comprobaciones fueron satisfactorias.")
