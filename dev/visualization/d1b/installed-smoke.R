args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Usage: installed-smoke.R <installed-library> <output-dir> <result-csv>")
}
library_path <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
.libPaths(c(library_path, .libPaths()))
suppressPackageStartupMessages(library(oceancube, lib.loc = library_path))
output_dir <- args[[2L]]
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

longitude <- c(-80, -79, -78, -77)
latitude <- c(-12, -11, -10)
depth <- c(0, 25, 50, 100)
time <- as.Date("2020-01-01") + 0:4
values <- array(
  seq_len(length(longitude) * length(latitude) * length(depth) * length(time)),
  dim = c(length(longitude), length(latitude), length(depth), length(time), 1L)
)
cube <- ocean_cube(
  lon = longitude, lat = latitude, depth = depth, time = time,
  data = values, vars = "temperature", units = "degC",
  dataset_id = "d1b-installed-smoke-v1"
)
path <- data.frame(longitude = longitude, latitude = rep(-11, 4L))
plots <- list(
  viz.map = viz.map(cube, "temperature", cube$time[[1L]], 0),
  viz.profile = viz.profile(cube, "temperature", -79, -11, cube$time[[1L]]),
  viz.section = viz.section(cube, "temperature", time = cube$time[[1L]], latitude = -11),
  viz.transect = viz.transect(cube, path, "temperature", time = cube$time[[1L]], mode = "section"),
  viz.timeseries = viz.timeseries(cube, "temperature", -79, -11, 0)
)
for (name in names(plots)) {
  ggplot2::ggsave(
    file.path(output_dir, paste0(name, ".png")), plots[[name]],
    width = 4, height = 3, units = "in", dpi = 100, bg = "white"
  )
}
result <- data.frame(
  function_name = names(plots),
  return_class = vapply(plots, function(plot) class(plot)[[1L]], character(1)),
  inherits_ggplot = vapply(plots, inherits, logical(1), "ggplot"),
  rendered = file.exists(file.path(output_dir, paste0(names(plots), ".png"))),
  internal_triple_colon_calls = 0L,
  installed_exports = length(getNamespaceExports("oceancube")),
  version = as.character(utils::packageVersion("oceancube")),
  status = "PASS",
  stringsAsFactors = FALSE
)
result$status[!result$inherits_ggplot | !result$rendered |
                result$installed_exports != 48L |
                result$version != "0.2.0.9000"] <- "FAIL"
utils::write.csv(result, args[[3L]], row.names = FALSE)
if (any(result$status != "PASS")) stop("Installed public smoke failed.")
cat("INSTALLED_PUBLIC_SMOKE=PASS\n")
