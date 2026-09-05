library(oceancube)
source("dev/gallery/visualization/scripts/d1b-fixture.R")
cube <- d1b_gallery_cube()
plot <- viz.transect(
  cube, d1b_gallery_path(), "temperature", time = cube$time[[1L]],
  mode = "section", title = "Deterministic matched transect"
)
d1b_save_plot(plot, "dev/gallery/visualization/static/d1b-transect.png")
