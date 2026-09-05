d1b_gallery_cube <- function() {
  longitude <- c(-80, -79, -78, -77)
  latitude <- c(-12, -11, -10)
  depth <- c(0, 25, 50, 100)
  time <- as.Date("2020-01-01") + 0:4
  values <- array(
    seq_len(length(longitude) * length(latitude) * length(depth) * length(time)),
    dim = c(length(longitude), length(latitude), length(depth), length(time), 1L)
  )
  ocean_cube(
    lon = longitude, lat = latitude, depth = depth, time = time,
    data = values, vars = "temperature", units = "degC",
    dataset_id = "d1b-gallery-deterministic-v1"
  )
}

d1b_gallery_path <- function() {
  data.frame(
    longitude = c(-80, -79, -78, -77),
    latitude = rep(-11, 4L)
  )
}

d1b_save_plot <- function(plot, output) {
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = output, plot = plot, width = 6, height = 4,
    units = "in", dpi = 300, device = "png", bg = "white"
  )
}
