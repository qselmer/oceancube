args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: parity-worker.R <installed-library> <output-rds>")
}
library_path <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
.libPaths(c(library_path, .libPaths()))
suppressPackageStartupMessages(library(oceancube, lib.loc = library_path))
suppressPackageStartupMessages(library(ggplot2))

make_cube <- function(surface = FALSE) {
  longitude <- c(-80, -79, -78, -77)
  latitude <- c(-12, -11, -10)
  depth <- if (surface) NA_real_ else c(0, 25, 50, 100)
  time <- as.Date("2020-01-01") + 0:4
  values <- array(
    seq_len(length(longitude) * length(latitude) * length(depth) * length(time)),
    dim = c(length(longitude), length(latitude), length(depth), length(time), 1L)
  )
  ocean_cube(
    lon = longitude, lat = latitude, depth = depth, time = time,
    data = values, vars = "temperature", units = "degC",
    dataset_id = "d1b-parity-deterministic-v1"
  )
}

path <- data.frame(
  longitude = c(-80, -79, -78, -77), latitude = rep(-11, 4L)
)
cube <- make_cube()

capture_warnings <- function(expression) {
  warnings <- list()
  value <- withCallingHandlers(
    expression,
    warning = function(condition) {
      warnings[[length(warnings) + 1L]] <<- list(
        message = conditionMessage(condition), class = class(condition)
      )
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}

capture_error <- function(expression) {
  tryCatch(
    {
      force(expression)
      list(message = NA_character_, class = character())
    },
    error = function(condition) list(
      message = strsplit(
        conditionMessage(condition), "\nCaused by error", fixed = TRUE
      )[[1L]][[1L]],
      class = class(condition)
    )
  )
}

plot_summary <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  labels <- as.list(plot$labels)
  attributes(labels) <- list(names = names(labels))
  plot_attributes <- attributes(plot)
  plot_attributes <- plot_attributes[
    grepl("^oceancube_", names(plot_attributes))
  ]
  list(
    class = class(plot),
    data = plot$data,
    build_data = built$data,
    labels = labels,
    layer_count = length(plot$layers),
    geom_classes = vapply(
      plot$layers, function(layer) class(layer$geom)[[1L]], character(1)
    ),
    mappings = lapply(plot$mapping, function(mapping) {
      paste(deparse(mapping), collapse = " ")
    }),
    scale_classes = vapply(
      plot$scales$scales, function(scale) class(scale)[[1L]], character(1)
    ),
    coordinate_class = class(plot$coordinates)[[1L]],
    oceancube_attributes = plot_attributes
  )
}

calls <- list(
  viz.map = quote(viz.map(
    cube, "temperature", time = cube$time[[1L]], depth = 0,
    limits = c(10, 100), title = "Map", subtitle = "Stored layer",
    caption = "D1B parity"
  )),
  viz.profile = quote(viz.profile(
    cube, "temperature", longitude = -79, latitude = -11,
    time = cube$time[[1L]], limits = c(10, 180), reverse_depth = TRUE,
    points = TRUE, title = "Profile"
  )),
  viz.section = quote(viz.section(
    cube, "temperature", section = "longitude-depth",
    time = cube$time[[1L]], latitude = -11, reverse_depth = TRUE,
    title = "Section"
  )),
  viz.transect = quote(viz.transect(
    cube, path, "temperature", time = cube$time[[1L]], mode = "section",
    distance = "requested", reverse_depth = TRUE, title = "Transect"
  )),
  viz.timeseries = quote(viz.timeseries(
    cube, "temperature", longitude = -79, latitude = -11, depth = 0,
    points = TRUE, title = "Series"
  ))
)

plots <- lapply(calls, function(call) capture_warnings(eval(call)))
signatures <- lapply(names(calls), function(name) {
  function_object <- getExportedValue("oceancube", name)
  list(names = names(formals(function_object)),
       text = paste(deparse(args(function_object)), collapse = " "))
})
names(signatures) <- names(calls)

errors <- list(
  missing_variable = capture_error(viz.map(cube, "missing")),
  invalid_time = capture_error(viz.map(cube, "temperature", time = as.Date("1999-01-01"), depth = 0)),
  invalid_depth = capture_error(viz.map(cube, "temperature", time = cube$time[[1L]], depth = 12)),
  insufficient_profile_depths = capture_error(viz.profile(make_cube(TRUE), "temperature", -79, -11, cube$time[[1L]])),
  invalid_section_geometry = capture_error(viz.section(cube, "temperature", section = "longitude-depth", time = cube$time[[1L]], longitude = -79, latitude = -11)),
  invalid_transect_path = capture_error(viz.transect(cube, path[1L, , drop = FALSE], "temperature", time = cube$time[[1L]])),
  invalid_match = capture_error(viz.timeseries(cube, "temperature", -79, -11, 0, match = "invalid")),
  invalid_limits = capture_error(viz.timeseries(cube, "temperature", -79, -11, 0, limits = c(2, 2)))
)

result <- list(
  package_version = as.character(utils::packageVersion("oceancube")),
  exports = sort(getNamespaceExports("oceancube")),
  signatures = signatures,
  plots = lapply(plots, function(item) plot_summary(item$value)),
  warnings = lapply(plots, `[[`, "warnings"),
  errors = errors
)
saveRDS(result, args[[2L]], version = 3L)
