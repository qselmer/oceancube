#!/usr/bin/env Rscript

# Deterministically build the canonical real-ocean visualization benchmark.
#
# This script reads bounded OPeNDAP hyperslabs from the two 2024 NOAA PSL
# GODAS variables. It never downloads the global/year files, interpolates,
# averages, regrids, fills, derives, or changes decoded source values.

options(stringsAsFactors = FALSE, warn = 1)

required <- c("ncdf4", "openssl")
missing_packages <- required[
  !vapply(required, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing maintainer package(s): ", paste(missing_packages, collapse = ", "))
}

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(script_argument)) stop("Cannot determine the benchmark builder path")
script_file <- normalizePath(
  sub("^--file=", "", script_argument[[1L]]), winslash = "/", mustWork = TRUE
)
repo_root <- normalizePath(
  file.path(dirname(script_file), "..", "..", ".."),
  winslash = "/", mustWork = TRUE
)

args <- commandArgs(TRUE)
argument_value <- function(prefix, default) {
  hit <- grep(paste0("^", prefix, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^", prefix, "="), "", hit[[1L]]) else default
}
output_dir <- argument_value(
  "--output-dir",
  file.path(repo_root, "dev", "data", "visualization", "benchmark")
)
if (!grepl("^([A-Za-z]:)?[/\\\\]", output_dir)) {
  output_dir <- file.path(repo_root, output_dir)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

benchmark_id <- "oceancube-viz-benchmark"
benchmark_version <- "1"
access_date <- "2026-09-06"
source_provider <- "NOAA Physical Sciences Laboratory / NCEP"
source_product_id <- "NCEP-GODAS-2024-MONTHLY"
source_product_title <- "NCEP Global Ocean Data Assimilation System (GODAS), monthly 2024"
landing_page <- "https://psl.noaa.gov/data/gridded/data.godas.html"
license_name <- "NOAA public-domain data; GODAS Usage Restrictions: None"
license_url <- "https://psl.noaa.gov/data/gridded/data.godas.html"
source_urls <- c(
  pottmp = "https://psl.noaa.gov/thredds/dodsC/Datasets/godas/pottmp.2024.nc",
  salt = "https://psl.noaa.gov/thredds/dodsC/Datasets/godas/salt.2024.nc"
)
source_variables <- names(source_urls)
selected_depths <- c(
  5, 15, 25, 35, 45, 55, 65, 75, 85,
  95, 105, 125, 155, 195, 238, 303, 366, 459
)

benchmark_path <- file.path(output_dir, "oceancube-viz-benchmark-v1.nc")
manifest_path <- file.path(
  output_dir, "oceancube-viz-benchmark-v1-manifest.csv"
)
variables_path <- file.path(
  output_dir, "oceancube-viz-benchmark-v1-variables.csv"
)
coordinates_path <- file.path(
  output_dir, "oceancube-viz-benchmark-v1-coordinates.csv"
)
parity_path <- file.path(
  output_dir, "oceancube-viz-benchmark-v1-parity.csv"
)
checksums_path <- file.path(
  output_dir, "oceancube-viz-benchmark-v1-checksums.csv"
)

sha256_file <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  paste0(tolower(as.character(openssl::sha256(connection))))
}

attribute <- function(nc, variable, name, default = NA) {
  value <- ncdf4::ncatt_get(nc, variable, name)
  if (isTRUE(value$hasatt)) value$value else default
}

same_numeric <- function(x, y) {
  identical(is.na(x), is.na(y)) &&
    identical(as.numeric(x[!is.na(x)]), as.numeric(y[!is.na(y)]))
}

sources <- lapply(source_urls, ncdf4::nc_open)
names(sources) <- names(source_urls)
on.exit({
  for (source in sources) try(ncdf4::nc_close(source), silent = TRUE)
}, add = TRUE)

temperature <- sources[["pottmp"]]
salinity <- sources[["salt"]]
for (name in source_variables) {
  source <- sources[[name]]
  expected_dimensions <- c("time", "lat", "level", "lon")
  if (!setequal(names(source$dim), expected_dimensions)) {
    stop(name, " does not expose the expected GODAS coordinates")
  }
  if (!identical(vapply(source$var[[name]]$dim, `[[`, character(1L), "name"),
                 c("lon", "lat", "level", "time"))) {
    stop(name, " has an unexpected physical dimension order")
  }
  if (!identical(attribute(source, 0, "dataset_title"),
                 "NCEP Global Ocean Data Assimilation System (GODAS)")) {
    stop(name, " source identity changed")
  }
  if (!identical(attribute(source, 0, "Conventions"), "COARDS")) {
    stop(name, " source convention metadata changed")
  }
}

for (coordinate in c("lon", "lat", "level", "time")) {
  if (!same_numeric(temperature$dim[[coordinate]]$vals,
                    salinity$dim[[coordinate]]$vals)) {
    stop("GODAS temperature/salinity coordinate mismatch: ", coordinate)
  }
}

lon_source <- as.numeric(temperature$dim$lon$vals)
lat_source <- as.numeric(temperature$dim$lat$vals)
depth_source <- as.numeric(temperature$dim$level$vals)
time_source <- as.numeric(temperature$dim$time$vals)
source_time_units <- as.character(attribute(temperature, "time", "units"))
benchmark_time_units <- sub(
  "00:00:0\\.0$", "00:00:00", source_time_units
)
lon_index <- which(lon_source >= 276 & lon_source <= 288)
lat_index <- which(lat_source >= -20 & lat_source <= -3)
depth_index <- match(selected_depths, depth_source)
expected_time <- as.Date(sprintf("2024-%02d-01", 1:12))
decoded_time <- as.Date("1800-01-01") + time_source

if (!identical(length(lon_index), 12L) ||
    !identical(length(lat_index), 51L) ||
    anyNA(depth_index) ||
    !identical(decoded_time, expected_time)) {
  stop("GODAS 2024 coordinate selection contract failed")
}

lon <- lon_source[lon_index]
lat <- lat_source[lat_index]
depth <- depth_source[depth_index]
time <- time_source
max_depth_index <- max(depth_index)

read_source_block <- function(source, variable) {
  block <- ncdf4::ncvar_get(
    source,
    variable,
    start = c(min(lon_index), min(lat_index), 1L, 1L),
    count = c(length(lon_index), length(lat_index), max_depth_index, length(time)),
    collapse_degen = FALSE
  )
  block <- block[, , depth_index, , drop = FALSE]
  # The PSL OPeNDAP service can expose the documented `missing_value` either
  # as NA or as the finite sentinel, depending on the libnetcdf/DAP build.
  # Normalize only that exact authoritative sentinel to R's missing value.
  missing_value <- as.numeric(attribute(source, variable, "missing_value"))
  block[!is.na(block) & block == missing_value] <- NA_real_
  block
}

source_data <- lapply(source_variables, function(variable) {
  read_source_block(sources[[variable]], variable)
})
names(source_data) <- source_variables
expected_shape <- c(length(lon), length(lat), length(depth), length(time))
if (!all(vapply(source_data, function(x) identical(dim(x), expected_shape), logical(1L)))) {
  stop("Source subset shape contract failed")
}
if (any(vapply(source_data, function(x) sum(is.finite(x)) < 100L, logical(1L)))) {
  stop("A required variable has fewer than 100 finite retained values")
}

temporary_path <- paste0(benchmark_path, ".tmp")
if (file.exists(temporary_path)) unlink(temporary_path)

longitude_dimension <- ncdf4::ncdim_def(
  "lon", attribute(temperature, "lon", "units"), lon,
  longname = attribute(temperature, "lon", "long_name")
)
latitude_dimension <- ncdf4::ncdim_def(
  "lat", attribute(temperature, "lat", "units"), lat,
  longname = attribute(temperature, "lat", "long_name")
)
depth_dimension <- ncdf4::ncdim_def(
  "depth", attribute(temperature, "level", "units"), depth,
  longname = attribute(temperature, "level", "long_name")
)
time_dimension <- ncdf4::ncdim_def(
  "time", benchmark_time_units, time,
  longname = attribute(temperature, "time", "long_name")
)
dimensions <- list(
  longitude_dimension, latitude_dimension, depth_dimension, time_dimension
)

definitions <- lapply(source_variables, function(variable) {
  source <- sources[[variable]]
  ncdf4::ncvar_def(
    variable,
    attribute(source, variable, "units"),
    dimensions,
    missval = source$var[[variable]]$missval,
    longname = attribute(source, variable, "long_name"),
    prec = "float",
    compression = 9,
    chunksizes = c(length(lon), 17L, length(depth), 1L)
  )
})

output <- ncdf4::nc_create(temporary_path, definitions, force_v4 = TRUE)
output_closed <- FALSE
on.exit(if (!output_closed) try(ncdf4::nc_close(output), silent = TRUE), add = TRUE)
for (variable in source_variables) {
  ncdf4::ncvar_put(output, variable, source_data[[variable]])
}

copy_attributes <- function(source, source_variable, target_variable, names) {
  for (name in names) {
    value <- ncdf4::ncatt_get(source, source_variable, name)
    if (isTRUE(value$hasatt)) ncdf4::ncatt_put(output, target_variable, name, value$value)
  }
}
copy_attributes(temperature, "lon", "lon", c("standard_name", "axis", "GridType"))
copy_attributes(temperature, "lat", "lat", c("standard_name", "axis", "GridType"))
copy_attributes(temperature, "level", "depth", c("positive", "axis"))
copy_attributes(temperature, "time", "time", c(
  "standard_name", "axis", "info", "delta_t", "avg_period"
))
science_attributes <- c(
  "dataset", "var_desc", "level_desc", "statistic", "parent_stat",
  "sub_center", "center", "level_indicator", "gds_grid_type",
  "parameter_table_version", "parameter_number", "missing_value",
  "valid_range", "actual_range"
)
for (variable in source_variables) {
  copy_attributes(sources[[variable]], variable, variable, science_attributes)
}

global_attributes <- c(
  "creation_date", "sfcHeatFlux", "time_comment", "Conventions", "grib_file",
  "html_REFERENCES", "html_BACKGROUND", "html_GODAS", "comment", "title",
  "References", "dataset_title", "history"
)
for (name in global_attributes) {
  value <- ncdf4::ncatt_get(temperature, 0, name)
  if (isTRUE(value$hasatt)) ncdf4::ncatt_put(output, 0, name, value$value)
}
ncdf4::ncatt_put(output, 0, "oceancube_benchmark_id", benchmark_id)
ncdf4::ncatt_put(output, 0, "oceancube_benchmark_version", benchmark_version)
ncdf4::ncatt_put(output, 0, "oceancube_source_provider", source_provider)
ncdf4::ncatt_put(output, 0, "oceancube_source_product_id", source_product_id)
ncdf4::ncatt_put(output, 0, "oceancube_source_accessed", access_date)
ncdf4::ncatt_put(output, 0, "oceancube_source_url", paste(source_urls, collapse = ";"))
ncdf4::ncatt_put(output, 0, "oceancube_source_landing_page", landing_page)
ncdf4::ncatt_put(output, 0, "oceancube_source_license", license_name)
ncdf4::ncatt_put(output, 0, "oceancube_source_license_url", license_url)
ncdf4::ncatt_put(output, 0, "oceancube_subset_bbox", "276.5E/287.5E/-19.8337326N/-3.1671791N")
ncdf4::ncatt_put(output, 0, "oceancube_subset_time", "2024-01-01/2024-12-01; monthly means; source timestamps retained")
ncdf4::ncatt_put(output, 0, "oceancube_source_time_units", source_time_units)
ncdf4::ncatt_put(output, 0, "oceancube_time_units_normalization", "Lexically normalized terminal 00:00:0.0 to equivalent 00:00:00; numeric source time values unchanged")
ncdf4::ncatt_put(output, 0, "oceancube_subset_depth", paste(depth, collapse = ","))
ncdf4::ncatt_put(output, 0, "oceancube_subsampling", "FALSE; all source lon/lat coordinates in requested bounds retained")
ncdf4::ncatt_put(output, 0, "oceancube_generation_script", "dev/visualization/vizdata/build-oceancube-viz-benchmark-v1.R")
ncdf4::ncatt_put(output, 0, "oceancube_transformations", "none: no interpolation, regridding, averaging, filling, or derived variables")
ncdf4::ncatt_put(output, 0, "oceancube_redistribution", "PERMITTED_WITH_ATTRIBUTION; official GODAS usage restrictions: none")
ncdf4::nc_close(output)
output_closed <- TRUE

if (file.exists(benchmark_path)) unlink(benchmark_path)
if (!file.rename(temporary_path, benchmark_path)) {
  stop("Could not install deterministic benchmark output")
}

# Some ncdf4 builds replace NA with the target fill sentinel in the R object
# passed to ncvar_put(). Restore the decoded source representation before the
# parity calculation; this changes neither the source nor the written file.
for (variable in source_variables) {
  missing_value <- as.numeric(attribute(sources[[variable]], variable, "missing_value"))
  source_data[[variable]][
    !is.na(source_data[[variable]]) & source_data[[variable]] == missing_value
  ] <- NA_real_
}

benchmark <- ncdf4::nc_open(benchmark_path)
on.exit(try(ncdf4::nc_close(benchmark), silent = TRUE), add = TRUE)
benchmark_coordinates <- list(
  lon = as.numeric(benchmark$dim$lon$vals),
  lat = as.numeric(benchmark$dim$lat$vals),
  depth = as.numeric(benchmark$dim$depth$vals),
  time = as.numeric(benchmark$dim$time$vals)
)
source_coordinates <- list(lon = lon, lat = lat, depth = depth, time = time)
coordinate_parity <- vapply(names(source_coordinates), function(name) {
  identical(source_coordinates[[name]], benchmark_coordinates[[name]])
}, logical(1L))
if (!all(coordinate_parity)) {
  stop("Coordinate parity failed: ", paste(names(coordinate_parity)[!coordinate_parity], collapse = ", "))
}

benchmark_data <- lapply(source_variables, function(variable) {
  ncdf4::ncvar_get(benchmark, variable, collapse_degen = FALSE)
})
names(benchmark_data) <- source_variables

parity_rows <- lapply(source_variables, function(variable) {
  finite_index <- which(is.finite(source_data[[variable]]))
  sample_index <- finite_index[
    unique(as.integer(round(seq(1, length(finite_index), length.out = 100L))))
  ]
  source_value <- as.numeric(source_data[[variable]][sample_index])
  benchmark_value <- as.numeric(benchmark_data[[variable]][sample_index])
  exact_equal <- !is.na(source_value) & !is.na(benchmark_value) &
    source_value == benchmark_value
  data.frame(
    variable = variable,
    sample = seq_along(sample_index),
    array_index = sample_index,
    source_value = source_value,
    benchmark_value = benchmark_value,
    exact_equal = exact_equal,
    stringsAsFactors = FALSE
  )
})
parity <- do.call(rbind, parity_rows)
utils::write.csv(parity, parity_path, row.names = FALSE, na = "")
if (nrow(parity) < 200L || !all(parity$exact_equal)) {
  failed_by_variable <- aggregate(
    exact_equal ~ variable, parity, function(x) sum(!x)
  )
  stop(
    "Exact source-value parity failed: ",
    paste(failed_by_variable$variable, failed_by_variable$exact_equal,
          sep = "=", collapse = ", ")
  )
}

format_hex <- function(values) {
  ifelse(is.na(values), "NA", sprintf("%a", as.numeric(values)))
}
scientific_lines <- c(
  "oceancube-viz-benchmark scientific content v1",
  unlist(lapply(sort(names(benchmark_coordinates)), function(name) {
    attrs <- c(
      units = as.character(attribute(benchmark, name, "units", "")),
      standard_name = as.character(attribute(benchmark, name, "standard_name", "")),
      axis = as.character(attribute(benchmark, name, "axis", "")),
      positive = as.character(attribute(benchmark, name, "positive", "")),
      calendar = as.character(attribute(benchmark, name, "calendar", ""))
    )
    c(
      paste0("coordinate:", name),
      paste(names(attrs), attrs, sep = "="),
      format_hex(benchmark_coordinates[[name]])
    )
  }), use.names = FALSE),
  unlist(lapply(sort(source_variables), function(variable) {
    attrs <- c(
      units = as.character(attribute(benchmark, variable, "units", "")),
      long_name = as.character(attribute(benchmark, variable, "long_name", "")),
      var_desc = as.character(attribute(benchmark, variable, "var_desc", "")),
      statistic = as.character(attribute(benchmark, variable, "statistic", "")),
      missing_value = format_hex(attribute(benchmark, variable, "missing_value", NA_real_)),
      dimensions = paste(vapply(benchmark$var[[variable]]$dim, `[[`, character(1L), "name"), collapse = ",")
    )
    c(
      paste0("variable:", variable),
      paste(names(attrs), attrs, sep = "="),
      format_hex(benchmark_data[[variable]])
    )
  }), use.names = FALSE)
)
scientific_content_sha256 <- paste0(tolower(as.character(
  openssl::sha256(charToRaw(enc2utf8(paste(scientific_lines, collapse = "\n"))))
)))
benchmark_sha256 <- sha256_file(benchmark_path)
benchmark_bytes <- file.info(benchmark_path)$size
if (benchmark_bytes > 20 * 1024^2) stop("Benchmark exceeds the 20 MiB hard gate")

finite_statistics <- function(values) {
  finite <- values[is.finite(values)]
  c(
    minimum_finite = min(finite), maximum_finite = max(finite),
    median_finite = stats::median(finite), n_finite = length(finite),
    n_missing = sum(is.na(values)), fraction_missing = mean(is.na(values))
  )
}
statistics <- lapply(benchmark_data, finite_statistics)

source_request <- paste0(
  "OPeNDAP bounded hyperslabs: lon indices ", min(lon_index), "-", max(lon_index),
  "; lat indices ", min(lat_index), "-", max(lat_index),
  "; level indices 1-", max_depth_index, " then exact native levels ",
  paste(depth_index, collapse = ","), "; time indices 1-12"
)
manifest <- data.frame(
  benchmark_id = benchmark_id,
  benchmark_version = benchmark_version,
  provider = source_provider,
  product_id = source_product_id,
  product_title = source_product_title,
  source_url = landing_page,
  source_endpoint = paste(source_urls, collapse = ";"),
  license = license_name,
  license_url = license_url,
  redistribution_status = "PERMITTED_WITH_ATTRIBUTION",
  access_date = access_date,
  source_file_or_request = source_request,
  source_sha256_if_available = "NOT_APPLICABLE_REMOTE_OPENDAP_HYPERSLAB",
  benchmark_sha256 = benchmark_sha256,
  scientific_content_sha256 = scientific_content_sha256,
  benchmark_bytes = benchmark_bytes,
  remote_bytes_transferred_estimate = 2L * prod(c(
    length(lon_index), length(lat_index), max_depth_index, length(time)
  )) * 4L,
  local_source_bytes = 0L,
  longitude_min = min(lon), longitude_max = max(lon),
  latitude_min = min(lat), latitude_max = max(lat),
  depth_min = min(depth), depth_max = max(depth),
  time_from = as.character(min(decoded_time)),
  time_to = as.character(max(decoded_time)),
  n_lon = length(lon), n_lat = length(lat),
  n_depth = length(depth), n_time = length(time),
  variables = paste(source_variables, collapse = ";"),
  generation_script = "dev/visualization/vizdata/build-oceancube-viz-benchmark-v1.R",
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")

variable_manifest <- do.call(rbind, lapply(source_variables, function(variable) {
  source <- sources[[variable]]
  stat <- statistics[[variable]]
  data.frame(
    variable = variable,
    source_variable = variable,
    standard_name = as.character(attribute(source, variable, "standard_name", "")),
    long_name = as.character(attribute(source, variable, "long_name", "")),
    units = as.character(attribute(source, variable, "units", "")),
    data_type = "Float32",
    dimensions = "lon;lat;depth;time",
    missing_value = format(attribute(source, variable, "missing_value"), scientific = TRUE),
    minimum_finite = unname(stat[["minimum_finite"]]),
    maximum_finite = unname(stat[["maximum_finite"]]),
    median_finite = unname(stat[["median_finite"]]),
    n_finite = unname(stat[["n_finite"]]),
    n_missing = unname(stat[["n_missing"]]),
    fraction_missing = unname(stat[["fraction_missing"]]),
    stringsAsFactors = FALSE
  )
}))
utils::write.csv(variable_manifest, variables_path, row.names = FALSE, na = "")

spacing_type <- function(values) {
  differences <- diff(as.numeric(values))
  if (length(differences) <= 1L) return("REGULAR")
  tolerance <- sqrt(.Machine$double.eps) * max(c(1, abs(differences)))
  if (all(abs(differences - differences[[1L]]) <= tolerance)) "REGULAR" else "IRREGULAR"
}
coordinate_manifest <- data.frame(
  coordinate = c("lon", "lat", "depth", "time"),
  units = c(
    attribute(temperature, "lon", "units"),
    attribute(temperature, "lat", "units"),
    attribute(temperature, "level", "units"),
    benchmark_time_units
  ),
  standard_name = c(
    attribute(temperature, "lon", "standard_name", ""),
    attribute(temperature, "lat", "standard_name", ""),
    attribute(temperature, "level", "standard_name", ""),
    attribute(temperature, "time", "standard_name", "")
  ),
  axis = c(
    attribute(temperature, "lon", "axis", ""),
    attribute(temperature, "lat", "axis", ""),
    attribute(temperature, "level", "axis", ""),
    attribute(temperature, "time", "axis", "")
  ),
  positive = c("", "", attribute(temperature, "level", "positive", ""), ""),
  calendar = c("", "", "", attribute(temperature, "time", "calendar", "")),
  n = vapply(benchmark_coordinates, length, integer(1L)),
  minimum = vapply(benchmark_coordinates, min, numeric(1L)),
  maximum = vapply(benchmark_coordinates, max, numeric(1L)),
  monotonicity = vapply(benchmark_coordinates, function(values) {
    if (all(diff(values) > 0)) "STRICTLY_INCREASING" else if (all(diff(values) < 0)) "STRICTLY_DECREASING" else "NON_MONOTONIC"
  }, character(1L)),
  spacing_type = vapply(benchmark_coordinates, spacing_type, character(1L)),
  bounds_present = FALSE,
  stringsAsFactors = FALSE
)
utils::write.csv(coordinate_manifest, coordinates_path, row.names = FALSE, na = "")
ncdf4::nc_close(benchmark)

checksums <- data.frame(
  artifact = c(
    "oceancube-viz-benchmark-v1.nc",
    "oceancube-viz-benchmark-v1-manifest.csv",
    "build-oceancube-viz-benchmark-v1.R"
  ),
  sha256 = c(
    benchmark_sha256,
    sha256_file(manifest_path),
    sha256_file(script_file)
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(checksums, checksums_path, row.names = FALSE, na = "")

cat("BENCHMARK_PATH=", benchmark_path, "\n", sep = "")
cat("BENCHMARK_BYTES=", benchmark_bytes, "\n", sep = "")
cat("BENCHMARK_SHA256=", benchmark_sha256, "\n", sep = "")
cat("SCIENTIFIC_CONTENT_SHA256=", scientific_content_sha256, "\n", sep = "")
cat("SOURCE_VALUE_PARITY=PASS (100/100 per required variable)\n")
cat("COORDINATE_PARITY=PASS (4/4 exact)\n")
cat("NO_REGRIDDING_NO_INTERPOLATION_NO_AVERAGING=PASS\n")
cat("BUILD_OCEANCUBE_VIZ_BENCHMARK_V1: PASS\n")
