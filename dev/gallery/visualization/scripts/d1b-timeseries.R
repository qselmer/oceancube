library(oceancube)
source("dev/gallery/visualization/scripts/d1b-fixture.R")
cube <- d1b_gallery_cube()
plot <- viz.timeseries(
  cube, "temperature", longitude = -79, latitude = -11, depth = 0,
  points = TRUE, title = "Deterministic stored time series"
)
d1b_save_plot(plot, "dev/gallery/visualization/static/d1b-timeseries.png")
