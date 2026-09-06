d2a_gallery_cube <- function() {
  longitude <- c(-84, -82.5, -80, -77)
  latitude <- c(-16, -13, -11, -8.5)
  depth <- c(0, 10, 35, 90, 180)
  time <- as.Date("2021-01-01") + c(0, 8, 23, 47, 82, 128)
  values <- array(
    NA_real_,
    dim = c(length(longitude), length(latitude), length(depth), length(time), 1L)
  )
  for (i in seq_along(longitude)) {
    for (j in seq_along(latitude)) {
      for (k in seq_along(depth)) {
        for (m in seq_along(time)) {
          values[i, j, k, m, 1L] <-
            21 + 0.55 * (i - 1) + 0.35 * (j - 1) - 0.035 * depth[[k]] +
            1.4 * sin((m - 1) * pi / 2.5)
        }
      }
    }
  }
  values[3L, 3L, 3:4, 3:4, 1L] <- NA_real_
  ocean_cube(
    lon = longitude,
    lat = latitude,
    depth = depth,
    time = time,
    data = values,
    vars = "temperature",
    units = "degC",
    dataset_id = "d2a-hovmoller-simulated-v1"
  )
}

d2a_save_plot <- function(plot, output) {
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = output,
    plot = plot,
    width = 6,
    height = 4,
    units = "in",
    dpi = 300,
    device = "png",
    bg = "white"
  )
}
