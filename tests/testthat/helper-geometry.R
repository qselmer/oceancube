geometry_test_cube <- function(
    lon = structure(c(0, 1), bounds = c(-0.5, 0.5, 1.5)),
    lat = structure(c(0, 1), bounds = c(-0.5, 0.5, 1.5)),
    depth = structure(c(5, 15), units = "m", positive = "down")) {
  values <- array(
    seq_len(length(lon) * length(lat) * length(depth)),
    dim = c(length(lon), length(lat), length(depth), 1L, 1L)
  )
  ocean_cube(
    lon = lon,
    lat = lat,
    depth = depth,
    time = as.Date("2020-01-01"),
    data = values,
    vars = "temperature"
  )
}
