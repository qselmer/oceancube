library(oceancube)
source("dev/gallery/visualization/scripts/d2a-fixture.R")
cube <- d2a_gallery_cube()
plot <- viz.hovmoller(
  cube,
  "temperature",
  axis = "depth",
  longitude = -80,
  latitude = -11,
  title = "Simulated time-depth temperature",
  subtitle = "Stored centres; irregular time and depth; grey cells are missing",
  caption = "Simulated stored values; no interpolation"
)
d2a_save_plot(
  plot,
  "dev/gallery/visualization/static/d2a-hovmoller-time-depth.png"
)
