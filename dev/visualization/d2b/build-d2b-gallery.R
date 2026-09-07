#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Usage: build-d2b-gallery.R <installed-library>")
}
library_path <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
.libPaths(c(library_path, .libPaths()))
suppressPackageStartupMessages(library(oceancube, lib.loc = library_path))
if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop("patchwork is required only to reproduce the governed D2B composition examples.")
}
if (!requireNamespace("openssl", quietly = TRUE)) {
  stop("openssl is required to fingerprint governed gallery artifacts.")
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- normalizePath(sub("^--file=", "", script_arg[[1L]]),
                             winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_file), "..", "..", ".."),
                           winslash = "/", mustWork = TRUE)
benchmark_path <- file.path(
  repo_root, "dev", "data", "visualization", "benchmark",
  "oceancube-viz-benchmark-v1.nc"
)
output_dir <- file.path(repo_root, "dev", "gallery", "visualization", "static")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

oc <- function(name) getExportedValue("oceancube", name)
cube <- oc("cube_open")(
  benchmark_path, vars = c("pottmp", "salt"),
  source = "NOAA-PSL-GODAS", dataset_id = "oceancube-viz-benchmark-v1"
)
selected_time <- cube$time[[1L]]
selected_depth <- cube$depth[[1L]]
selected_longitude <- 278.5
selected_latitude <- cube$lat[[which.min(abs(cube$lat + 12))]]

real_caption <- paste0(
  "GODAS 2024, 5 m; stored centres; display wrap only.\n",
  "No interpolation."
)
plots <- list(
  `d2b-map-field-pottmp.png` = oc("viz.map")(
    cube, "pottmp", time = selected_time, depth = selected_depth,
    style = "field", scale_class = "sequential",
    longitude_display = "neg180_180", na.rm = FALSE,
    title = "Potential temperature — January 2024", caption = real_caption
  ),
  `d2b-map-field-salt.png` = oc("viz.map")(
    cube, "salt", time = selected_time, depth = selected_depth,
    style = "field", scale_class = "sequential",
    longitude_display = "neg180_180", na.rm = FALSE,
    title = "Salinity — January 2024", caption = real_caption
  ),
  `d2b-map-contour-pottmp.png` = oc("viz.map")(
    cube, "pottmp", time = selected_time, depth = selected_depth,
    style = "contour", scale_class = "sequential",
    longitude_display = "neg180_180", na.rm = FALSE,
    title = "Potential-temperature isolines — January 2024",
    caption = paste0(real_caption, " Isolines are renderer-only geometry.")
  ),
  `d2b-map-field-contour-pottmp.png` = oc("viz.map")(
    cube, "pottmp", time = selected_time, depth = selected_depth,
    style = "field_contour", scale_class = "sequential",
    longitude_display = "neg180_180", na.rm = FALSE,
    title = "pottmp field + isolines — Jan 2024",
    caption = paste0(real_caption, " Isolines are renderer-only geometry.")
  )
)

longitude <- seq(-84, -72, length.out = 13L)
latitude <- seq(-20, -4, length.out = 17L)
signed_values <- outer(
  longitude + 78, latitude + 12,
  function(lon, lat) lon / 6 - lat / 8
)
signed_cube <- oc("ocean_cube")(
  lon = longitude, lat = latitude, depth = 5,
  time = as.Date("2024-01-01"),
  data = array(signed_values, dim = c(13L, 17L, 1L, 1L, 1L)),
  vars = "signed_field", units = "1",
  dataset_id = "d2b-diverging-synthetic-v1"
)
plots[["d2b-map-diverging-synthetic.png"]] <- oc("viz.map")(
  signed_cube, "signed_field", style = "field_contour",
  scale_class = "diverging", center = 0,
  contour_breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5),
  title = "Synthetic signed field — explicit centre 0",
  caption = "Deterministic base-R HCL diverging scale; no hidden derivation"
)

map_for_composition <- oc("viz.map")(
  cube, "pottmp", time = selected_time, depth = selected_depth,
  style = "field", scale_class = "sequential",
  longitude_display = "neg180_180", na.rm = FALSE,
  title = "Surface context"
)
profile <- oc("viz.profile")(
  cube, "pottmp", longitude = selected_longitude,
  latitude = selected_latitude, time = selected_time,
  title = "Exact-grid profile"
)
timeseries <- oc("viz.timeseries")(
  cube, "pottmp", longitude = selected_longitude,
  latitude = selected_latitude, depth = selected_depth,
  title = "Monthly series"
) + ggplot2::theme(
  axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7)
)
plots[["d2b-compose-map-profile.png"]] <- patchwork::wrap_plots(
  list(map_for_composition, profile), widths = c(1.4, 1)
) + patchwork::plot_annotation(
  title = "GODAS map + profile",
  caption = "Independent public oceancube plots; no composition-time source read"
)
plots[["d2b-compose-map-timeseries.png"]] <- patchwork::wrap_plots(
  list(map_for_composition, timeseries), widths = c(1.4, 1)
) + patchwork::plot_annotation(
  title = "GODAS map + time series",
  caption = "Independent public oceancube plots; no composition-time source read"
)

for (name in names(plots)) {
  ggplot2::ggsave(
    file.path(output_dir, name), plots[[name]], width = 6, height = 4,
    units = "in", dpi = 300, device = "png", bg = "white"
  )
}

sha256_file <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  paste0(tolower(as.character(openssl::sha256(connection))))
}
paths <- file.path(output_dir, names(plots))
d2b_manifest <- data.frame(
  artifact = sub("\\.png$", "", names(plots)),
  path = file.path("dev", "gallery", "visualization", "static", names(plots)),
  bytes = as.numeric(file.info(paths)$size),
  sha256 = vapply(paths, sha256_file, character(1L)),
  review_status = "GENERATED_PENDING_MAINTAINER",
  reviewer = "",
  selected_time = format(selected_time, tz = "UTC", usetz = TRUE),
  selected_depth_m = selected_depth,
  installed_public_API = TRUE,
  internal_triple_colon_calls = 0L,
  stringsAsFactors = FALSE
)
utils::write.csv(
  d2b_manifest,
  file.path(repo_root, "dev", "visualization", "d2b", "d2b-gallery.csv"),
  row.names = FALSE, na = ""
)

manifest_path <- file.path(repo_root, "dev", "gallery", "visualization", "manifest.csv")
manifest <- utils::read.csv(manifest_path, check.names = FALSE,
                            colClasses = "character")
manifest <- manifest[!grepl("^D2B-", manifest$viz_id), , drop = FALSE]
styles <- c("field", "field", "contour", "field+contour", "diverging field+contour",
            "map+profile", "map+timeseries")
variables <- c("pottmp", "salt", "pottmp", "pottmp", "signed_field",
               "pottmp", "pottmp")
functions <- c(rep("viz.map", 5L), "viz.map + viz.profile",
               "viz.map + viz.timeseries")
rows <- data.frame(
  viz_id = paste0(
    "D2B-",
    toupper(sub("\\.png$", "", sub("^d2b-", "", names(plots))))
  ),
  `function` = functions,
  style = styles,
  renderer = "ggplot2; patchwork only for composition rows",
  dataset = c(rep("oceancube-viz-benchmark-v1", 4L),
              "d2b-diverging-synthetic-v1", rep("oceancube-viz-benchmark-v1", 2L)),
  variable = variables,
  purpose = "D2B technical visual-review candidate",
  script = "../../visualization/d2b/build-d2b-gallery.R",
  output_file = file.path("static", names(plots)), output_type = "PNG",
  width = "1800", height = "1200", dpi = "300",
  interactive = "FALSE", animated = "FALSE", `3d` = "FALSE",
  review_status = "GENERATED_PENDING_MAINTAINER", reviewer = "",
  review_date = "",
  notes = "Installed public oceancube scientific panels; offline; exact selections; no hidden science.",
  check.names = FALSE, stringsAsFactors = FALSE
)
rows <- rows[names(manifest)]
utils::write.csv(rbind(manifest, rows), manifest_path, row.names = FALSE, na = "")

cat("D2B_INSTALLED_GALLERY_SMOKE=PASS\n")
cat("D2B_GALLERY_ARTIFACTS=", length(plots), "\n", sep = "")
cat("D2B_GALLERY_STATUS=GENERATED_PENDING_MAINTAINER\n")
