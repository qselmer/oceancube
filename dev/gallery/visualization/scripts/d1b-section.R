library(oceancube)
source("dev/gallery/visualization/scripts/d1b-fixture.R")
cube <- d1b_gallery_cube()
plot <- viz.section(
  cube, "temperature", section = "longitude-depth",
  time = cube$time[[1L]], latitude = -11,
  title = "Deterministic longitude-depth section"
)
d1b_save_plot(plot, "dev/gallery/visualization/static/d1b-section.png")
