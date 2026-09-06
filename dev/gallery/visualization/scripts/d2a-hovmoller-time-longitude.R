library(oceancube)
source("dev/gallery/visualization/scripts/d2a-fixture.R")
cube <- d2a_gallery_cube()
plot <- viz.hovmoller(
  cube,
  "temperature",
  axis = "longitude",
  latitude = -11,
  depth = 35,
  title = "Simulated time-longitude temperature",
  subtitle = "Stored centres; irregular time and longitude; grey cells are missing",
  caption = "Simulated stored values; no interpolation"
)
d2a_save_plot(
  plot,
  "dev/gallery/visualization/static/d2a-hovmoller-time-longitude.png"
)
