.make_baseline_fixture <- function() {
  longitude <- c(-80, -79, -78)
  latitude <- c(-12, -11)
  depth <- c(0, 50)
  time <- as.Date(c(
    "2020-01-01",
    "2020-02-01",
    "2021-01-01",
    "2021-02-01"
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
    ),
    dimnames = list(
      longitude = as.character(longitude),
      latitude = as.character(latitude),
      depth = as.character(depth),
      time = as.character(time),
      variable = variable
    )
  )

  # Input formula: 10000*m + 1000*l + 100*k + 10*j + i.
  for (i in seq_along(longitude)) {
    for (j in seq_along(latitude)) {
      for (k in seq_along(depth)) {
        for (l in seq_along(time)) {
          for (m in seq_along(variable)) {
            values[i, j, k, l, m] <-
              10000 * m +
              1000 * l +
              100 * k +
              10 * j +
              i
          }
        }
      }
    }
  }

  values_before <- values
  cube <- ocean_cube(
    lon = longitude,
    lat = latitude,
    depth = depth,
    time = time,
    data = values,
    vars = variable,
    units = c(temperature = "degC", oxygen = "mmol m-3")
  )

  list(
    cube = cube,
    values = values,
    values_before = values_before,
    longitude = longitude,
    latitude = latitude,
    depth = depth,
    time = time,
    variable = variable
  )
}
