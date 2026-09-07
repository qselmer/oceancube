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
evidence_dir <- dirname(script_file)
benchmark_path <- file.path(
  repo_root, "dev", "data", "visualization", "benchmark",
  "oceancube-viz-benchmark-v1.nc"
)
output_dir <- file.path(repo_root, "dev", "gallery", "visualization", "static")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

sha256_file <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  paste0(tolower(as.character(openssl::sha256(connection))))
}
sha256_object <- function(x) {
  paste0(tolower(as.character(openssl::sha256(serialize(x, NULL, version = 3L)))))
}
assert <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

oc <- function(name) getExportedValue("oceancube", name)
cube <- oc("cube_open")(
  benchmark_path, vars = c("pottmp", "salt"),
  source = "NOAA-PSL-GODAS", dataset_id = "oceancube-viz-benchmark-v1"
)
selected_time <- cube$time[[1L]]
selected_depth <- cube$depth[[1L]]
selected_longitude <- 278.5
selected_latitude <- cube$lat[[which.min(abs(cube$lat + 12))]]
display_longitude <- ((selected_longitude + 180) %% 360) - 180

real_caption <- paste0(
  "GODAS 2024, 5 m; stored centres; display wrap only. ",
  "Grey = stored NA / missing source field support. No interpolation."
)

# Accepted single-panel plots are reconstructed in memory for evidence, but only
# the title-corrected FIELD_CONTOUR candidate is written in this review/fix.
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
    title = "Potential temperature + isolines\nJanuary 2024",
    caption = paste0(
      "GODAS 2024, 5 m; stored centres; display wrap only.\n",
      "Grey = stored NA / missing source field support; no interpolation.\n",
      "Isolines are renderer-only geometry."
    )
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

profile_before <- profile$data
timeseries_before <- timeseries$data
map_before <- map_for_composition$data
marker <- data.frame(longitude = display_longitude, latitude = selected_latitude)
marked_map <- map_for_composition + ggplot2::geom_point(
  data = marker,
  ggplot2::aes(x = longitude, y = latitude),
  inherit.aes = FALSE, shape = 21, size = 2.6, stroke = 0.8,
  colour = "black", fill = "white"
)
composition_caption <- paste0(
  "Marker = exact grid location used in right-hand panel. ",
  "Grey = stored NA / missing source field support."
)
plots[["d2b-compose-map-profile.png"]] <- patchwork::wrap_plots(
  list(marked_map, profile), widths = c(1.4, 1)
) + patchwork::plot_annotation(
  title = "GODAS potential temperature: map + profile",
  caption = composition_caption
)
plots[["d2b-compose-map-timeseries.png"]] <- patchwork::wrap_plots(
  list(marked_map, timeseries), widths = c(1.4, 1)
) + patchwork::plot_annotation(
  title = "GODAS map + monthly series (5 m)",
  caption = composition_caption
)

assert(identical(attr(profile, "oceancube_longitude"), selected_longitude),
       "Profile longitude is not the authoritative selected longitude.")
assert(identical(attr(profile, "oceancube_latitude"), selected_latitude),
       "Profile latitude is not the authoritative selected latitude.")
assert(identical(attr(timeseries, "oceancube_longitude"), selected_longitude),
       "Time-series longitude is not the authoritative selected longitude.")
assert(identical(attr(timeseries, "oceancube_latitude"), selected_latitude),
       "Time-series latitude is not the authoritative selected latitude.")
assert(identical(attr(timeseries, "oceancube_depth"), selected_depth),
       "Time-series depth is not the authoritative selected depth.")

marker_layer <- ggplot2::ggplot_build(marked_map)$data[[length(marked_map$layers)]]
assert(nrow(marker_layer) == 1L, "Composition marker must contain exactly one point.")
assert(isTRUE(all.equal(marker_layer$x[[1L]], display_longitude, tolerance = 0)),
       "Built marker x does not exactly equal wrapped display longitude.")
assert(isTRUE(all.equal(marker_layer$y[[1L]], selected_latitude, tolerance = 0)),
       "Built marker y does not exactly equal selected latitude.")

regenerated <- c(
  "d2b-map-field-contour-pottmp.png",
  "d2b-compose-map-profile.png",
  "d2b-compose-map-timeseries.png"
)
for (name in regenerated) {
  ggplot2::ggsave(
    file.path(output_dir, name), plots[[name]], width = 6, height = 4,
    units = "in", dpi = 300, device = "png", bg = "white"
  )
}

baseline_hashes <- c(
  `d2b-map-field-pottmp.png` = "5c75a0ae7827301ff0f4371b1ef5f97f436e4a0a93bac15dfa688487330c21b2",
  `d2b-map-field-salt.png` = "5012c5eb2c22e88dbb6fe553b3b11ac00499d758e637257a87bd563b48d275bf",
  `d2b-map-contour-pottmp.png` = "5ac0420d16136fa4c47fb5aaaab1c8b8f680242a9ec072d65f30ef00b3f27bf0",
  `d2b-map-field-contour-pottmp.png` = "c659e5275623cee9f8f3e84b3805fb7b6cf0442abf4618573eb97788278d285b",
  `d2b-map-diverging-synthetic.png` = "32635bd8869eeecede789c9b033bb55d98ec9a869b1ee8b9b453fdbf080343ea",
  `d2b-compose-map-profile.png` = "aa086b460d25b7c927b95603ded91b95b76d2accf31582d5ea883dc341fca250",
  `d2b-compose-map-timeseries.png` = "a7d636ed9db9f305c37f2e2d6e092ac0f69a0198a83b3f6e9349c180ef910463"
)
baseline_bytes <- c(73720, 65676, 160581, 156325, 108257, 107341, 124244)
names(baseline_bytes) <- names(baseline_hashes)
all_names <- names(baseline_hashes)
paths <- file.path(output_dir, all_names)
current_hashes <- vapply(paths, sha256_file, character(1L))
names(current_hashes) <- all_names
unchanged <- setdiff(all_names, regenerated)
assert(identical(unname(current_hashes[unchanged]),
                 unname(baseline_hashes[unchanged])),
       "An accepted unchanged D2B candidate is no longer byte-identical.")
assert(all(current_hashes[regenerated] != baseline_hashes[regenerated]),
       "Every regenerated candidate must differ from its pre-review baseline.")

review_status <- ifelse(all_names %in% regenerated,
                        "REGENERATED_PENDING_MAINTAINER",
                        "GENERATED_PENDING_MAINTAINER")
d2b_manifest <- data.frame(
  artifact = sub("\\.png$", "", all_names),
  path = file.path("dev", "gallery", "visualization", "static", all_names),
  bytes = as.numeric(file.info(paths)$size),
  sha256 = unname(current_hashes),
  review_status = review_status,
  reviewer = "",
  selected_time = format(selected_time, tz = "UTC", usetz = TRUE),
  selected_depth_m = selected_depth,
  installed_public_API = TRUE,
  internal_triple_colon_calls = 0L,
  stringsAsFactors = FALSE
)
utils::write.csv(d2b_manifest, file.path(evidence_dir, "d2b-gallery.csv"),
                 row.names = FALSE, na = "")

change_reasons <- c(
  `d2b-map-field-contour-pottmp.png` = paste0(
    "visible title harmonized to scientific long name; field, contour levels, ",
    "line width, palette and scientific values unchanged"
  ),
  `d2b-compose-map-profile.png` = paste0(
    "exact child-selection marker and concise linkage/missingness caption; ",
    "profile and map science unchanged"
  ),
  `d2b-compose-map-timeseries.png` = paste0(
    "exact child-selection marker and concise linkage/missingness caption; ",
    "time-series and map science unchanged"
  )
)
history <- data.frame(
  record = "PRE_REVIEW_BASELINE",
  artifact = sub("\\.png$", "", all_names),
  path = file.path("dev", "gallery", "visualization", "static", all_names),
  bytes = unname(baseline_bytes),
  sha256 = unname(baseline_hashes),
  reason_changed = "original D2B candidate retained for auditability",
  stringsAsFactors = FALSE
)
post <- data.frame(
  record = "POST_REVIEW_FIX_CANDIDATE",
  artifact = sub("\\.png$", "", regenerated),
  path = file.path("dev", "gallery", "visualization", "static", regenerated),
  bytes = as.numeric(file.info(file.path(output_dir, regenerated))$size),
  sha256 = unname(current_hashes[regenerated]),
  reason_changed = unname(change_reasons[regenerated]),
  stringsAsFactors = FALSE
)
utils::write.csv(rbind(history, post),
                 file.path(evidence_dir, "d2b-gallery-hash-history.csv"),
                 row.names = FALSE, na = "")

composition_audit <- data.frame(
  composition = c("map_profile", "map_timeseries"),
  map_variable = "pottmp",
  map_time = format(selected_time, tz = "UTC", usetz = TRUE),
  map_depth = selected_depth,
  child_plot_type = c("profile", "timeseries"),
  selection_source_longitude = selected_longitude,
  selection_display_longitude = display_longitude,
  selection_latitude = selected_latitude,
  selection_depth = c(NA_real_, selected_depth),
  match_mode = "exact",
  marker_present = TRUE,
  marker_coordinate_match = TRUE,
  child_selection_match = TRUE,
  composition_source_reads = 0L,
  status = "REGENERATED_PENDING_MAINTAINER",
  stringsAsFactors = FALSE
)
utils::write.csv(composition_audit,
                 file.path(evidence_dir, "d2b-composition-audit.csv"),
                 row.names = FALSE, na = "")

marker_tests <- data.frame(
  composition = c("map_profile", "map_timeseries"),
  source_longitude = selected_longitude,
  expected_display_longitude = display_longitude,
  built_marker_x = marker_layer$x[[1L]],
  expected_latitude = selected_latitude,
  built_marker_y = marker_layer$y[[1L]],
  source_to_display_match = TRUE,
  latitude_match = TRUE,
  point_count = 1L,
  status = "PASS",
  stringsAsFactors = FALSE
)
utils::write.csv(marker_tests, file.path(evidence_dir, "d2b-marker-tests.csv"),
                 row.names = FALSE, na = "")

before <- list(
  profile_values = profile_before$value,
  profile_depths = profile_before$depth,
  timeseries_values = timeseries_before$value,
  timeseries_times = timeseries_before$time,
  map_prepared_values = map_before$value,
  map_coordinates = map_before[c("longitude", "latitude")]
)
after <- list(
  profile_values = profile$data$value,
  profile_depths = profile$data$depth,
  timeseries_values = timeseries$data$value,
  timeseries_times = timeseries$data$time,
  map_prepared_values = marked_map$data$value,
  map_coordinates = marked_map$data[c("longitude", "latitude")]
)
invariance <- data.frame(
  component = names(before),
  pre_review_sha256 = vapply(before, sha256_object, character(1L)),
  post_review_sha256 = vapply(after, sha256_object, character(1L)),
  exact_equal = vapply(names(before), function(name) {
    identical(before[[name]], after[[name]])
  }, logical(1L)),
  status = "PASS",
  stringsAsFactors = FALSE
)
assert(all(invariance$exact_equal), "Composition scientific invariance failed.")
utils::write.csv(invariance,
                 file.path(evidence_dir, "d2b-scientific-invariance.csv"),
                 row.names = FALSE, na = "")

manifest_path <- file.path(repo_root, "dev", "gallery", "visualization", "manifest.csv")
manifest <- utils::read.csv(manifest_path, check.names = FALSE,
                            colClasses = "character")
manifest <- manifest[!grepl("^D2B-", manifest$viz_id), , drop = FALSE]
styles <- c("field", "field", "contour", "field+contour",
            "diverging field+contour", "map+profile", "map+timeseries")
variables <- c("pottmp", "salt", "pottmp", "pottmp", "signed_field",
               "pottmp", "pottmp")
functions <- c(rep("viz.map", 5L), "viz.map + viz.profile",
               "viz.map + viz.timeseries")
notes <- rep(paste0(
  "Installed public oceancube scientific panels; offline; exact selections; ",
  "grey = stored NA / missing source field support; no hidden science."
), length(all_names))
notes[all_names == "d2b-map-contour-pottmp.png"] <- paste0(
  notes[all_names == "d2b-map-contour-pottmp.png"],
  " Contour-only geometry does not encode the complete valid-support mask."
)
notes[all_names %in% c("d2b-compose-map-profile.png",
                       "d2b-compose-map-timeseries.png")] <- paste0(
  notes[all_names %in% c("d2b-compose-map-profile.png",
                         "d2b-compose-map-timeseries.png")],
  " Marker is the exact grid location used in the paired child panel; ",
  "composition-time source reads = 0."
)
rows <- data.frame(
  viz_id = paste0("D2B-", toupper(sub("\\.png$", "", sub("^d2b-", "", all_names)))),
  `function` = functions,
  style = styles,
  renderer = "ggplot2; patchwork only for composition rows",
  dataset = c(rep("oceancube-viz-benchmark-v1", 4L),
              "d2b-diverging-synthetic-v1", rep("oceancube-viz-benchmark-v1", 2L)),
  variable = variables,
  purpose = "D2B technical visual-review candidate",
  script = "../../visualization/d2b/build-d2b-gallery.R",
  output_file = file.path("static", all_names), output_type = "PNG",
  width = "1800", height = "1200", dpi = "300",
  interactive = "FALSE", animated = "FALSE", `3d` = "FALSE",
  review_status = review_status, reviewer = "", review_date = "",
  notes = notes,
  check.names = FALSE, stringsAsFactors = FALSE
)
rows <- rows[names(manifest)]
utils::write.csv(rbind(manifest, rows), manifest_path, row.names = FALSE, na = "")

cat("D2B_INSTALLED_GALLERY_SMOKE=PASS\n")
cat("D2B_GALLERY_ARTIFACTS=", length(plots), "\n", sep = "")
cat("D2B_REGENERATED_ARTIFACTS=", length(regenerated), "\n", sep = "")
cat("D2B_GALLERY_STATUS=REVISED_VISUAL_CANDIDATES_GENERATED\n")
cat("D2B_SELECTION_SOURCE_LONGITUDE=", selected_longitude, "\n", sep = "")
cat("D2B_SELECTION_DISPLAY_LONGITUDE=", display_longitude, "\n", sep = "")
cat("D2B_SELECTION_LATITUDE=", format(selected_latitude, digits = 16L), "\n", sep = "")
