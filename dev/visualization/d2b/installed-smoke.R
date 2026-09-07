args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: installed-smoke.R <installed-library> <result-csv>")
}
library_path <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
.libPaths(c(library_path, .libPaths()))
suppressPackageStartupMessages(library(oceancube, lib.loc = library_path))

repo_root <- normalizePath(file.path(dirname(args[[2L]]), "..", "..", ".."),
                           winslash = "/", mustWork = TRUE)
benchmark <- file.path(repo_root, "dev", "data", "visualization", "benchmark",
                       "oceancube-viz-benchmark-v1.nc")
cube <- cube_open(benchmark, vars = c("pottmp", "salt"),
                  dataset_id = "oceancube-viz-benchmark-v1")
time <- cube$time[[1L]]
depth <- cube$depth[[1L]]

lon <- seq(-3, 3)
lat <- seq(-3, 3)
values <- outer(lon, lat, `+`)
signed <- ocean_cube(
  lon = lon, lat = lat, depth = 5, time = as.Date("2024-01-01"),
  data = array(values, dim = c(7L, 7L, 1L, 1L, 1L)),
  vars = "signed", units = "1", dataset_id = "d2b-installed-smoke-v1"
)

plots <- list(
  default_field = viz.map(cube, "pottmp", time = time, depth = depth),
  contour = viz.map(cube, "pottmp", time = time, depth = depth,
                    style = "contour"),
  field_contour = viz.map(cube, "pottmp", time = time, depth = depth,
                          style = "field_contour"),
  sequential = viz.map(cube, "salt", time = time, depth = depth,
                       scale_class = "sequential"),
  diverging = viz.map(signed, "signed", scale_class = "diverging", center = 0),
  longitude_source = viz.map(cube, "pottmp", time = time, depth = depth,
                             longitude_display = "source"),
  longitude_neg180 = viz.map(cube, "pottmp", time = time, depth = depth,
                             longitude_display = "neg180_180")
)
rendered <- vapply(seq_along(plots), function(index) {
  path <- file.path(tempdir(), paste0("d2b-installed-", index, ".png"))
  ggplot2::ggsave(path, plots[[index]], width = 4, height = 3,
                  units = "in", dpi = 100, bg = "white")
  file.exists(path) && file.info(path)$size > 0
}, logical(1L))

result <- data.frame(
  case = names(plots),
  inherits_ggplot = vapply(plots, inherits, logical(1L), "ggplot"),
  rendered = rendered,
  installed_exports = length(getNamespaceExports("oceancube")),
  version = as.character(utils::packageVersion("oceancube")),
  internal_triple_colon_calls = 0L,
  status = "PASS",
  stringsAsFactors = FALSE
)
result$status[!result$inherits_ggplot | !result$rendered |
                result$installed_exports != 49L |
                result$version != "0.2.0.9000"] <- "FAIL"
utils::write.csv(result, args[[2L]], row.names = FALSE)
if (any(result$status != "PASS")) stop("Installed D2B public smoke failed.")
cat("D2B_INSTALLED_PUBLIC_SMOKE=PASS\n")
