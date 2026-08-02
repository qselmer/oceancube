# Refactor temporal: los algoritmos leen mediante la capa interna del backend.
# Ejecute este script desde la raíz del repositorio. No usa internet, no genera
# números aleatorios y no escribe archivos.

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
      source = "temporal-backend-example"
    )
  }
}

x <- make_baseline_cube()
message("Backend de entrada: ", oceancube:::.cube_backend(x))
stopifnot(identical(oceancube:::.cube_backend(x), "memory"))

# Las funciones temporales piden los valores a .cube_read(); no necesitan saber
# si el backend los guarda en un array residente o en otro soporte futuro.
monthly <- to_month(x)
clim <- clim_month(x)
anom <- anom_diff(x, clim)
z <- anom_z(x, clim)
sn <- signal_noise(x, clim)

message(
  "Conversión mensual: ",
  paste(dim(monthly$data), collapse = " x "),
  "; backend=", oceancube:::.cube_backend(monthly)
)
message(
  "Climatología mensual: ",
  paste(dim(clim$mean), collapse = " x "),
  "; mean/sd/n son arrays residentes en memoria"
)

# Celda localizada por posiciones:
# longitud 1, latitud 1, profundidad 1, enero, variable 1.
january_values <- c(11111, 13111)
expected_mean <- mean(january_values)
observed_mean <- clim$mean[1, 1, 1, 1, 1]

# Para las anomalías, la cuarta posición es el primer tiempo: 2020-01-01.
expected_difference <- january_values[[1]] - expected_mean
expected_z <- expected_difference / stats::sd(january_values)
expected_signal_noise <- abs(expected_z)

observed_difference <- anom$data[1, 1, 1, 1, 1]
observed_z <- z$data[1, 1, 1, 1, 1]
observed_signal_noise <- sn$data[1, 1, 1, 1, 1]

message("Media enero: manual=", expected_mean, "; función=", observed_mean)
message(
  "Anomalía absoluta: manual=", expected_difference,
  "; función=", observed_difference
)
message("Anomalía z: manual=", expected_z, "; función=", observed_z)
message(
  "Señal/ruido: manual=", expected_signal_noise,
  "; función=", observed_signal_noise
)

mean_comparison <- all.equal(observed_mean, expected_mean)
difference_comparison <- all.equal(observed_difference, expected_difference)
z_comparison <- all.equal(observed_z, expected_z)
signal_noise_comparison <- all.equal(
  observed_signal_noise,
  expected_signal_noise
)

message("all.equal(media): ", mean_comparison)
message("all.equal(anomalía): ", difference_comparison)
message("all.equal(z): ", z_comparison)
message("all.equal(señal/ruido): ", signal_noise_comparison)

stopifnot(
  isTRUE(mean_comparison),
  isTRUE(difference_comparison),
  isTRUE(z_comparison),
  isTRUE(signal_noise_comparison),
  identical(oceancube:::.cube_backend(monthly), "memory"),
  identical(oceancube:::.cube_backend(anom), "memory"),
  identical(oceancube:::.cube_backend(z), "memory"),
  identical(oceancube:::.cube_backend(sn), "memory")
)

message(
  "Backends de salidas ocean_cube: monthly=",
  oceancube:::.cube_backend(monthly),
  ", anom=", oceancube:::.cube_backend(anom),
  ", z=", oceancube:::.cube_backend(z),
  ", sn=", oceancube:::.cube_backend(sn)
)

message("REFACTOR TEMPORAL COMPLETADO: todas las comprobaciones fueron satisfactorias.")
