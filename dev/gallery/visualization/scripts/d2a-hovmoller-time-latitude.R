library(oceancube)
source("dev/gallery/visualization/scripts/d2a-fixture.R")
cube <- d2a_gallery_cube()
plot <- viz.hovmoller(
  cube,
  "temperature",
  axis = "latitude",
  longitude = -80,
  depth = 35,
  title = "Simulated time-latitude temperature",
  subtitle = "Irregular stored centres",
  caption = paste0(
    "Grey = stored NA; blank = no stored centre;\n",
    "Tile extent is display-only; no interpolation."
  )
)
d2a_save_plot(
  plot,
  "dev/gallery/visualization/static/d2a-hovmoller-time-latitude.png"
)
