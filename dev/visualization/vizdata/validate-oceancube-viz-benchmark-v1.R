#!/usr/bin/env Rscript

# Offline validation and public-API smoke test for the canonical benchmark.

options(stringsAsFactors = FALSE, warn = 1)

required <- c("ncdf4", "openssl", "pkgload", "ggplot2")
missing_packages <- required[
  !vapply(required, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing validation package(s): ", paste(missing_packages, collapse = ", "))
}

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(script_argument)) stop("Cannot determine validator path")
script_file <- normalizePath(
  sub("^--file=", "", script_argument[[1L]]), winslash = "/", mustWork = TRUE
)
repo_root <- normalizePath(
  file.path(dirname(script_file), "..", "..", ".."),
  winslash = "/", mustWork = TRUE
)
benchmark_dir <- file.path(repo_root, "dev", "data", "visualization", "benchmark")
gallery_dir <- file.path(repo_root, "dev", "gallery", "visualization", "benchmark")
benchmark_path <- file.path(benchmark_dir, "oceancube-viz-benchmark-v1.nc")
manifest_path <- file.path(benchmark_dir, "oceancube-viz-benchmark-v1-manifest.csv")
variables_path <- file.path(benchmark_dir, "oceancube-viz-benchmark-v1-variables.csv")
coordinates_path <- file.path(benchmark_dir, "oceancube-viz-benchmark-v1-coordinates.csv")
parity_path <- file.path(benchmark_dir, "oceancube-viz-benchmark-v1-parity.csv")
smoke_path <- file.path(dirname(script_file), "d-vizdata-public-smoke.csv")
gallery_manifest_path <- file.path(gallery_dir, "manifest.csv")

fail <- function(...) stop(..., call. = FALSE)
assert <- function(ok, message) if (!isTRUE(ok)) fail(message)
sha256_file <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  paste0(tolower(as.character(openssl::sha256(connection))))
}
attribute <- function(nc, variable, name, default = NA) {
  value <- ncdf4::ncatt_get(nc, variable, name)
  if (isTRUE(value$hasatt)) value$value else default
}
format_hex <- function(values) {
  ifelse(is.na(values), "NA", sprintf("%a", as.numeric(values)))
}

for (path in c(benchmark_path, manifest_path, variables_path,
               coordinates_path, parity_path)) {
  assert(file.exists(path), paste("Missing benchmark artifact:", basename(path)))
}
assert(file.info(benchmark_path)$size <= 20 * 1024^2,
       "Benchmark exceeds the 20 MiB hard gate")

manifest <- utils::read.csv(manifest_path, check.names = FALSE)
variables <- utils::read.csv(variables_path, check.names = FALSE)
coordinates <- utils::read.csv(coordinates_path, check.names = FALSE)
parity <- utils::read.csv(parity_path, check.names = FALSE)
assert(nrow(manifest) == 1L, "Benchmark manifest must have one row")
assert(identical(manifest$benchmark_id, "oceancube-viz-benchmark"),
       "Benchmark ID mismatch")
assert(identical(as.character(manifest$benchmark_version), "1"),
       "Benchmark version mismatch")
assert(identical(manifest$benchmark_sha256, sha256_file(benchmark_path)),
       "Benchmark SHA-256 does not match the manifest")
assert(identical(manifest$redistribution_status, "PERMITTED_WITH_ATTRIBUTION"),
       "Redistribution is not certified")
assert(nrow(parity) == 200L && all(parity$exact_equal),
       "Source-value parity evidence must pass 100 samples per variable")

nc <- ncdf4::nc_open(benchmark_path)
on.exit(try(ncdf4::nc_close(nc), silent = TRUE), add = TRUE)
assert(identical(names(nc$dim), c("lon", "lat", "depth", "time")),
       "Required dimensions are absent or reordered")
assert(identical(names(nc$var), c("pottmp", "salt")),
       "Required variables or bounded variable set mismatch")
expected_dimensions <- c(lon = 12L, lat = 51L, depth = 18L, time = 12L)
actual_dimensions <- vapply(nc$dim, `[[`, numeric(1L), "len")
assert(identical(as.integer(actual_dimensions), as.integer(expected_dimensions)),
       "Benchmark dimensions indicate suspicious source expansion")
assert(identical(attribute(nc, "depth", "positive"), "down"),
       "Depth positive-down semantics are absent")
assert(identical(attribute(nc, "depth", "units"), "m"),
       "Depth units are not authoritative GODAS metres")
assert(identical(attribute(nc, "time", "units"),
                 "days since 1800-01-01 00:00:00"),
       "Time unit normalization contract mismatch")
assert(identical(attribute(nc, 0, "oceancube_source_time_units"),
                 "days since 1800-01-01 00:00:0.0"),
       "Original source time units were not retained")
assert(!isTRUE(ncdf4::ncatt_get(nc, "time", "calendar")$hasatt),
       "A calendar attribute was invented")

benchmark_coordinates <- lapply(c("lon", "lat", "depth", "time"), function(name) {
  as.numeric(nc$dim[[name]]$vals)
})
names(benchmark_coordinates) <- c("lon", "lat", "depth", "time")
assert(all(vapply(benchmark_coordinates, function(x) all(diff(x) > 0), logical(1L))),
       "Coordinates must be strictly increasing")
assert(identical(benchmark_coordinates$depth,
                 c(5, 15, 25, 35, 45, 55, 65, 75, 85,
                   95, 105, 125, 155, 195, 238, 303, 366, 459)),
       "Selected source depths changed")

benchmark_data <- lapply(c("pottmp", "salt"), function(variable) {
  ncdf4::ncvar_get(nc, variable, collapse_degen = FALSE)
})
names(benchmark_data) <- c("pottmp", "salt")
for (variable in names(benchmark_data)) {
  values <- benchmark_data[[variable]]
  assert(sum(is.finite(values)) > 0L, paste(variable, "has no finite values"))
  assert(!all(is.na(values)), paste(variable, "is entirely missing"))
  row <- variables[variables$variable == variable, , drop = FALSE]
  assert(nrow(row) == 1L, paste(variable, "variable manifest row missing"))
  assert(identical(as.numeric(row$n_finite), as.numeric(sum(is.finite(values)))),
         paste(variable, "finite count changed"))
  assert(identical(as.numeric(row$n_missing), as.numeric(sum(is.na(values)))),
         paste(variable, "missing count changed"))
}

scientific_lines <- c(
  "oceancube-viz-benchmark scientific content v1",
  unlist(lapply(sort(names(benchmark_coordinates)), function(name) {
    attrs <- c(
      units = as.character(attribute(nc, name, "units", "")),
      standard_name = as.character(attribute(nc, name, "standard_name", "")),
      axis = as.character(attribute(nc, name, "axis", "")),
      positive = as.character(attribute(nc, name, "positive", "")),
      calendar = as.character(attribute(nc, name, "calendar", ""))
    )
    c(
      paste0("coordinate:", name),
      paste(names(attrs), attrs, sep = "="),
      format_hex(benchmark_coordinates[[name]])
    )
  }), use.names = FALSE),
  unlist(lapply(sort(names(benchmark_data)), function(variable) {
    attrs <- c(
      units = as.character(attribute(nc, variable, "units", "")),
      long_name = as.character(attribute(nc, variable, "long_name", "")),
      var_desc = as.character(attribute(nc, variable, "var_desc", "")),
      statistic = as.character(attribute(nc, variable, "statistic", "")),
      missing_value = format_hex(attribute(nc, variable, "missing_value", NA_real_)),
      dimensions = paste(vapply(nc$var[[variable]]$dim, `[[`, character(1L), "name"), collapse = ",")
    )
    c(
      paste0("variable:", variable),
      paste(names(attrs), attrs, sep = "="),
      format_hex(benchmark_data[[variable]])
    )
  }), use.names = FALSE)
)
scientific_hash <- paste0(tolower(as.character(openssl::sha256(charToRaw(
  enc2utf8(paste(scientific_lines, collapse = "\n"))
)))))
assert(identical(manifest$scientific_content_sha256, scientific_hash),
       "Scientific content fingerprint mismatch")

global_attributes <- ncdf4::ncatt_get(nc, 0)
metadata_text <- paste(
  unlist(global_attributes, use.names = TRUE),
  unlist(lapply(c("pottmp", "salt"), function(x) ncdf4::ncatt_get(nc, x)),
         use.names = TRUE),
  collapse = "\n"
)
credential_pattern <- paste0(
  "(?i)(password|token|secret|authorization|bearer|apikey|api_key|username|cookie)",
  "[[:space:]]*[:=][[:space:]]*['\"]?[A-Za-z0-9+/_=-]{8,}"
)
assert(!grepl(credential_pattern, metadata_text, perl = TRUE),
       "Credential-like metadata detected")
private_path_pattern <- "(?i)(^|[[:space:]\"'=])([A-Z]:[/\\\\]|Users[/\\\\])"
assert(!grepl(private_path_pattern, metadata_text, perl = TRUE),
       "Private machine path detected in NetCDF metadata")

ncdf4::nc_close(nc)

pkgload::load_all(repo_root, quiet = TRUE)
oc <- function(name) getExportedValue("oceancube", name)
cube <- oc("cube_open")(
  benchmark_path,
  vars = c("pottmp", "salt"),
  source = "NOAA-PSL-GODAS",
  dataset_id = "oceancube-viz-benchmark-v1"
)
inspection <- oc("cube_inspect")(cube)
assert(identical(unname(inspection$dimensions), c(12L, 51L, 18L, 12L, 2L)),
       "Public cube inspection dimension mismatch")
assert(inherits(cube$time, "POSIXct") &&
         identical(as.character(cube$time), as.character(as.POSIXct(
           sprintf("2024-%02d-01", 1:12), tz = "UTC"
         ))), "Public time decoding failed")
assert(identical(inspection$time_summary$calendar, "standard"),
       "Public calendar resolution failed")

selected_longitude <- 278.5
selected_latitude <- cube$lat[[which.min(abs(cube$lat + 12))]]
selected_time <- cube$time[[7L]]
selected_depth <- cube$depth[[1L]]

extracted <- oc("cube_extract")(
  cube, longitude = selected_longitude, latitude = selected_latitude,
  depth = selected_depth, time = selected_time, variable = "pottmp",
  by = "value", match = "exact", mode = "table", format = "long"
)
assert(nrow(extracted) == 1L && is.finite(extracted$value),
       "Public cube_extract smoke failed")

plots <- list(
  map = oc("viz.map")(
    cube, "pottmp", time = selected_time, depth = selected_depth,
    title = "GODAS pottmp: Jul 2024 at 5 m",
    caption = "Source values; no interpolation"
  ),
  profile = oc("viz.profile")(
    cube, "pottmp", longitude = selected_longitude,
    latitude = selected_latitude, time = selected_time,
    title = "GODAS pottmp profile: Jul 2024",
    caption = "Exact grid point and native depths"
  ),
  section = oc("viz.section")(
    cube, "pottmp", section = "longitude-depth", time = selected_time,
    latitude = selected_latitude,
    title = "GODAS pottmp section: Jul 2024",
    caption = "Exact latitude; no interpolation"
  ),
  timeseries = oc("viz.timeseries")(
    cube, "pottmp", longitude = selected_longitude,
    latitude = selected_latitude, depth = selected_depth,
    title = "GODAS monthly pottmp: 2024"
  ),
  hovmoller_time_depth = oc("viz.hovmoller")(
    cube, "pottmp", axis = "depth", longitude = selected_longitude,
    latitude = selected_latitude,
    title = "GODAS pottmp: time vs depth",
    caption = "Fixed longitude and latitude; no reduction"
  ),
  hovmoller_time_latitude = oc("viz.hovmoller")(
    cube, "pottmp", axis = "latitude", longitude = selected_longitude,
    depth = selected_depth,
    title = "GODAS pottmp: time vs latitude",
    caption = "Fixed longitude and depth; no reduction"
  ),
  hovmoller_time_longitude = oc("viz.hovmoller")(
    cube, "pottmp", axis = "longitude", latitude = selected_latitude,
    depth = selected_depth,
    title = "GODAS pottmp: time vs longitude",
    caption = "Fixed latitude and depth; no reduction"
  )
)
assert(all(vapply(plots, inherits, logical(1L), "ggplot")),
       "One or more public visualization smokes failed")

dir.create(gallery_dir, recursive = TRUE, showWarnings = FALSE)
gallery_outputs <- c(
  map = "benchmark-map-temperature.png",
  profile = "benchmark-profile-temperature.png",
  section = "benchmark-section-temperature.png",
  hovmoller_time_depth = "benchmark-hovmoller-time-depth.png"
)
for (name in names(gallery_outputs)) {
  ggplot2::ggsave(
    file.path(gallery_dir, gallery_outputs[[name]]), plots[[name]],
    width = 7, height = 4.5, units = "in", dpi = 300,
    device = "png", bg = "white"
  )
}
gallery_paths <- file.path(gallery_dir, unname(gallery_outputs))
gallery_manifest <- data.frame(
  artifact = names(gallery_outputs),
  path = file.path(
    "dev/gallery/visualization/benchmark", unname(gallery_outputs)
  ),
  bytes = as.numeric(file.info(gallery_paths)$size),
  sha256 = vapply(gallery_paths, sha256_file, character(1L)),
  status = "BENCHMARK_PREVIEW_NOT_STYLE_CERTIFIED",
  stringsAsFactors = FALSE
)
utils::write.csv(gallery_manifest, gallery_manifest_path,
                 row.names = FALSE, na = "")

smoke <- data.frame(
  check = c(
    "cube_open", "cube_inspect", "cube_extract", "viz.map", "viz.profile",
    "viz.section", "viz.timeseries", "viz.hovmoller.time-depth",
    "viz.hovmoller.time-latitude", "viz.hovmoller.time-longitude",
    "TS_READY", "VOLUME_READY", "VECTOR_READY"
  ),
  status = c(rep("PASS", 10L), "TRUE", "TRUE", "FALSE"),
  detail = c(
    "NetCDF backend; pottmp and salt",
    "12 x 51 x 18 x 12 x 2",
    "one exact finite stored value",
    "one exact time and depth",
    "one exact longitude, latitude and time",
    "one exact time and latitude; stored lon-depth plane",
    "one exact longitude, latitude and depth",
    "fixed source longitude and latitude",
    "fixed source longitude and depth",
    "fixed source latitude and depth",
    "collocated pottmp and salt on all four coordinates",
    "pottmp and salt provide lon x lat x depth at every time",
    "u and v were not retained"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(smoke, smoke_path, row.names = FALSE, na = "")

d1b_expected <- c(
  "dev/gallery/visualization/static/d1b-map.png" = "36ae1322e1baf0ef077fb6952eb06a0ce517f3e3b7c727d3e39aa1154b63443a",
  "dev/gallery/visualization/static/d1b-profile.png" = "66c867d0a584ed703cb268cd16d01d26724e26cec70118d47a6bc713729c9008",
  "dev/gallery/visualization/static/d1b-section.png" = "aff7e2e730e01525f6c307744f0b53652c02c522a65bfb0ae56194192330ab61",
  "dev/gallery/visualization/static/d1b-transect.png" = "915f4dfcd5812538e1be3a5d85c77376f34dcccb7578ca5548b85bcfaf2ec310",
  "dev/gallery/visualization/static/d1b-timeseries.png" = "5cd8840aa524b110b953b8681db3929eacb6b1db0569156d3011d8972f17c4b4"
)
d2a_expected <- c(
  "dev/gallery/visualization/static/d2a-hovmoller-time-depth.png" = "0b06899297131e9c28c5bac807ef4d0b91ed7c729e149e6bae523e0d9fc67573",
  "dev/gallery/visualization/static/d2a-hovmoller-time-longitude.png" = "c254dea749cd982c45ef331d326138e75ea61abb1caf0625548a9786e80e47ee",
  "dev/gallery/visualization/static/d2a-hovmoller-time-latitude.png" = "04e84b01dffedfe60ef1d3ec994b4ecda12a8ff97680fa0783a8089de1eae97a"
)
check_hashes <- function(expected) {
  actual <- vapply(names(expected), function(path) {
    sha256_file(file.path(repo_root, path))
  }, character(1L))
  identical(unname(actual), unname(expected))
}
assert(check_hashes(d1b_expected), "D1B baseline hash regression")
assert(check_hashes(d2a_expected), "D2A approved hash regression")

text_candidates <- c(
  list.files(file.path(repo_root, "dev", "visualization", "vizdata"),
             recursive = TRUE, full.names = TRUE),
  list.files(benchmark_dir, pattern = "\\.(csv|md)$", full.names = TRUE),
  file.path(repo_root, "inst", "architecture",
            "oceancube-visualization-benchmark-v1.md")
)
text_candidates <- unique(text_candidates[file.exists(text_candidates)])
text_content <- paste(vapply(text_candidates, function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}, character(1L)), collapse = "\n")
assert(!grepl(credential_pattern, text_content, perl = TRUE),
       "Credential-like text detected in governed benchmark files")
assert(!grepl(private_path_pattern, text_content, perl = TRUE),
       "Private machine path detected in governed benchmark files")

cat("BENCHMARK_VALIDATOR: PASS\n")
cat("PUBLIC_OCEANCUBE_SMOKE: PASS (8 public functions)\n")
cat("REAL_HOVMOLLER_SMOKE: PASS (3/3 axes)\n")
cat("TS_READY=TRUE\n")
cat("VOLUME_READY=TRUE\n")
cat("VECTOR_READY=FALSE\n")
cat("D1B_BASELINE_HASHES=5/5 EXACT MATCH\n")
cat("D2A_APPROVED_HASHES=3/3 EXACT MATCH\n")
cat("CREDENTIAL_SCAN=PASS\n")
cat("PRIVATE_PATH_SCAN=PASS\n")
cat("GALLERY_STATUS=BENCHMARK_PREVIEW_NOT_STYLE_CERTIFIED\n")
