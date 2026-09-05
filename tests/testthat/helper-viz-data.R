viz_data_test_cube <- function() {
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
    dataset_id = "viz-data-deterministic-v1"
  )
}

viz_data_test_path <- function() {
  data.frame(
    longitude = c(-80, -79, -78, -77),
    latitude = rep(-11, 4L)
  )
}

viz_plot_semantics <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  list(
    class = class(plot),
    data = plot$data,
    build_data = built$data,
    labels = plot$labels,
    layer_count = length(plot$layers),
    geom_classes = vapply(
      plot$layers, function(layer) class(layer$geom)[[1L]], character(1)
    ),
    scale_classes = vapply(
      plot$scales$scales, function(scale) class(scale)[[1L]], character(1)
    ),
    coordinate_class = class(plot$coordinates)[[1L]]
  )
}
