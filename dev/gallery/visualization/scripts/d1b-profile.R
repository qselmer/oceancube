library(oceancube)
source("dev/gallery/visualization/scripts/d1b-fixture.R")
cube <- d1b_gallery_cube()
plot <- viz.profile(
  cube, "temperature", longitude = -79, latitude = -11,
  time = cube$time[[1L]], title = "Deterministic vertical profile"
)
d1b_save_plot(plot, "dev/gallery/visualization/static/d1b-profile.png")
