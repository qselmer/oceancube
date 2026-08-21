#!/usr/bin/env Rscript

# Deterministically derive the governed WOA23 temperature/salinity fixture.

options(stringsAsFactors = FALSE)

required <- c("ncdf4", "openssl")
missing <- required[!vapply(required, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing)) stop("Missing maintainer package(s): ", paste(missing, collapse = ", "))

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- sub("^--file=", "", script_arg[[1L]])
repo_root <- normalizePath(file.path(dirname(script_file), "..", ".."), winslash = "/")
args <- commandArgs(TRUE)
output_arg <- grep("^--output=", args, value = TRUE)
output <- if (length(output_arg)) {
  sub("^--output=", "", output_arg[[1L]])
} else {
  file.path(repo_root, "tests", "testthat", "fixtures", "real-data",
            "noaa-woa23-vertical-fv1.nc")
}
if (!grepl("^([A-Za-z]:)?[/\\\\]", output)) output <- file.path(repo_root, output)
output <- normalizePath(dirname(output), winslash = "/", mustWork = FALSE) |>
  file.path(basename(output))

cache_dir <- file.path(repo_root, "data-raw", "fixtures", "cache", "woa23")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)

sha256_file <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  paste0(tolower(as.character(openssl::sha256(con))), collapse = "")
}

temperature_url <- "https://www.ncei.noaa.gov/thredds-ocean/dodsC/woa23/DATA/temperature/netcdf/decav/1.00/woa23_decav_t00_01.nc"
salinity_url <- "https://www.ncei.noaa.gov/thredds-ocean/dodsC/woa23/DATA/salinity/netcdf/decav/1.00/woa23_decav_s00_01.nc"
common_constraint <- paste0(
  "lon[96:1:104],lat[72:1:83],depth[0:1:24],time[0:1:0],",
  "lon_bnds[96:1:104][0:1:1],lat_bnds[72:1:83][0:1:1],",
  "depth_bnds[0:1:24][0:1:1],climatology_bounds[0:1:0][0:1:1]"
)
source_urls <- c(
  temperature = paste0(temperature_url, ".dods?", common_constraint,
                       ",t_an[0:1:0][0:1:24][72:1:83][96:1:104]"),
  salinity = paste0(salinity_url, ".dods?", common_constraint,
                    ",s_an[0:1:0][0:1:24][72:1:83][96:1:104]")
)
source_paths <- file.path(cache_dir, c(
  "woa23_decav_t00_01-peru-upper.dods",
  "woa23_decav_s00_01-peru-upper.dods"
))
expected_source_sha256 <- c(
  temperature = "716eaa9d62ad8aaa29d4fd62ca6db9ed527e9cd2ceac0d352c4906f6c9f88c4b",
  salinity = "f1cf2eb2a28925f9a507076188dbdb2b4cfdf6ebf59b823b078dd4870c279fda"
)
for (i in seq_along(source_urls)) {
  if (!file.exists(source_paths[[i]]) ||
      !identical(sha256_file(source_paths[[i]]), expected_source_sha256[[i]])) {
    if (file.exists(source_paths[[i]])) unlink(source_paths[[i]])
    utils::download.file(source_urls[[i]], source_paths[[i]], mode = "wb",
                         method = "libcurl", quiet = FALSE)
  }
}
source_sha256 <- setNames(vapply(source_paths, sha256_file, character(1L)), names(source_urls))
if (!identical(unname(source_sha256), unname(expected_source_sha256))) {
  stop("WOA23 constrained DAP source checksum drift")
}

temperature <- ncdf4::nc_open(temperature_url)
salinity <- ncdf4::nc_open(salinity_url)
on.exit({
  try(ncdf4::nc_close(temperature), silent = TRUE)
  try(ncdf4::nc_close(salinity), silent = TRUE)
}, add = TRUE)

t_global <- ncdf4::ncatt_get(temperature, 0)
s_global <- ncdf4::ncatt_get(salinity, 0)
if (!identical(t_global$id, "woa23_decav_t00_01.nc") ||
    !identical(s_global$id, "woa23_decav_s00_01.nc") ||
    !identical(t_global$Conventions, "CF-1.6") ||
    !identical(s_global$Conventions, "CF-1.6") ||
    !grepl("openly available", t_global$license, fixed = TRUE) ||
    !grepl("openly available", s_global$license, fixed = TRUE) ||
    !grepl("10.25923/54bh-1613", t_global$references, fixed = TRUE) ||
    !grepl("10.25923/70qt-9574", s_global$references, fixed = TRUE)) {
  stop("WOA23 source identity, license, CF, or citation contract failed")
}
if (!identical(names(temperature$dim), c("depth", "lat", "lon", "nbounds", "time")) ||
    !all(c("t_an", "lat_bnds", "lon_bnds", "depth_bnds", "climatology_bounds", "crs") %in% names(temperature$var)) ||
    !all(c("s_an", "lat_bnds", "lon_bnds", "depth_bnds", "climatology_bounds", "crs") %in% names(salinity$var))) {
  stop("WOA23 dimension or variable inventory drift")
}

lon_i <- which(temperature$dim$lon$vals >= -84 & temperature$dim$lon$vals <= -75)
lat_i <- which(temperature$dim$lat$vals >= -18 & temperature$dim$lat$vals <= -6)
depth_target <- c(0, 10, 20, 50, 100, 200)
depth_i <- match(depth_target, temperature$dim$depth$vals)
upper_i <- seq_len(max(depth_i))
if (!identical(length(lon_i), 9L) || !identical(length(lat_i), 12L) || anyNA(depth_i) ||
    !identical(temperature$dim$lon$vals, salinity$dim$lon$vals) ||
    !identical(temperature$dim$lat$vals, salinity$dim$lat$vals) ||
    !identical(temperature$dim$depth$vals, salinity$dim$depth$vals) ||
    !identical(temperature$dim$time$vals, salinity$dim$time$vals)) {
  stop("WOA23 coordinate identity or approved selection contract failed")
}

lon <- as.numeric(temperature$dim$lon$vals[lon_i])
lat <- as.numeric(temperature$dim$lat$vals[lat_i])
depth <- as.numeric(temperature$dim$depth$vals[depth_i])
time <- as.numeric(temperature$dim$time$vals)
if (!identical(depth, depth_target)) stop("WOA23 selected depth values are not exact provider levels")

read_block <- function(nc, var, selectors) {
  dims <- nc$var[[var]]$dim
  dim_names <- vapply(dims, function(x) x$name, character(1L))
  start <- rep(1L, length(dims))
  count <- vapply(dims, function(x) x$len, integer(1L))
  for (nm in intersect(names(selectors), dim_names)) {
    idx <- selectors[[nm]]
    if (length(idx) > 1L && !identical(diff(idx), rep(1L, length(idx) - 1L))) {
      stop("Internal selector for ", nm, " is not contiguous")
    }
    at <- match(nm, dim_names)
    start[[at]] <- min(idx)
    count[[at]] <- length(idx)
  }
  ncdf4::ncvar_get(nc, var, start = start, count = count, collapse_degen = FALSE)
}

selectors_upper <- list(lon = lon_i, lat = lat_i, depth = upper_i, time = 1L)
t_upper <- read_block(temperature, "t_an", selectors_upper)
s_upper <- read_block(salinity, "s_an", selectors_upper)
t_an <- t_upper[, , depth_i, , drop = FALSE]
s_an <- s_upper[, , depth_i, , drop = FALSE]
lon_bnds <- read_block(temperature, "lon_bnds", list(lon = lon_i))
lat_bnds <- read_block(temperature, "lat_bnds", list(lat = lat_i))
depth_bnds_upper <- read_block(temperature, "depth_bnds", list(depth = upper_i))
depth_bnds <- depth_bnds_upper[, depth_i, drop = FALSE]
climatology_bounds <- read_block(temperature, "climatology_bounds", list(time = 1L))

if (!identical(dim(t_an), c(9L, 12L, 6L, 1L)) ||
    !identical(dim(s_an), c(9L, 12L, 6L, 1L)) ||
    !identical(as.numeric(time), 4614) ||
    !identical(as.numeric(climatology_bounds), c(4212, 5028)) ||
    !identical(ncdf4::ncatt_get(temperature, "time", "units")$value,
               "months since 1955-01-01 00:00:00") ||
    isTRUE(ncdf4::ncatt_get(temperature, "time", "calendar")$hasatt)) {
  stop("WOA23 data shape or provider-native climatological time contract failed")
}

tmp <- paste0(output, ".tmp")
if (file.exists(tmp)) unlink(tmp)
dnbounds <- ncdf4::ncdim_def("nbounds", "", 1:2, create_dimvar = FALSE)
dlon <- ncdf4::ncdim_def("lon", "degrees_east", lon, create_dimvar = TRUE)
dlat <- ncdf4::ncdim_def("lat", "degrees_north", lat, create_dimvar = TRUE)
ddepth <- ncdf4::ncdim_def("depth", "meters", depth, create_dimvar = TRUE)
dtime <- ncdf4::ncdim_def(
  "time", "months since 1955-01-01 00:00:00", time, create_dimvar = TRUE
)
fill <- ncdf4::ncatt_get(temperature, "t_an", "_FillValue")$value
defs <- list(
  ncdf4::ncvar_def("lon_bnds", "degrees_east", list(dnbounds, dlon), missval = NULL, prec = "float", compression = 9),
  ncdf4::ncvar_def("lat_bnds", "degrees_north", list(dnbounds, dlat), missval = NULL, prec = "float", compression = 9),
  ncdf4::ncvar_def("depth_bnds", "meters", list(dnbounds, ddepth), missval = NULL, prec = "float", compression = 9),
  ncdf4::ncvar_def("climatology_bounds", "months since 1955-01-01 00:00:00", list(dnbounds, dtime), missval = NULL, prec = "float", compression = 9),
  ncdf4::ncvar_def("crs", "", list(), missval = NULL, prec = "integer"),
  ncdf4::ncvar_def("t_an", "degrees_celsius", list(dlon, dlat, ddepth, dtime), missval = fill,
                   longname = ncdf4::ncatt_get(temperature, "t_an", "long_name")$value,
                   prec = "float", compression = 9, chunksizes = c(9L, 12L, 6L, 1L)),
  ncdf4::ncvar_def("s_an", "1", list(dlon, dlat, ddepth, dtime), missval = fill,
                   longname = ncdf4::ncatt_get(salinity, "s_an", "long_name")$value,
                   prec = "float", compression = 9, chunksizes = c(9L, 12L, 6L, 1L))
)
out <- ncdf4::nc_create(tmp, defs, force_v4 = TRUE)
closed <- FALSE
on.exit(if (!closed) try(ncdf4::nc_close(out), silent = TRUE), add = TRUE)
ncdf4::ncvar_put(out, "lon_bnds", lon_bnds)
ncdf4::ncvar_put(out, "lat_bnds", lat_bnds)
ncdf4::ncvar_put(out, "depth_bnds", depth_bnds)
ncdf4::ncvar_put(out, "climatology_bounds", climatology_bounds)
ncdf4::ncvar_put(out, "crs", 1L)
ncdf4::ncvar_put(out, "t_an", t_an)
ncdf4::ncvar_put(out, "s_an", s_an)

copy_attrs <- function(source, source_var, target, target_var, names) {
  a <- ncdf4::ncatt_get(source, source_var)
  for (nm in intersect(names, names(a))) ncdf4::ncatt_put(target, target_var, nm, a[[nm]])
}
coordinate_attrs <- c("standard_name", "long_name", "axis", "bounds", "positive", "climatology")
for (v in c("lon", "lat", "depth", "time")) copy_attrs(temperature, v, out, v, coordinate_attrs)
for (v in c("lon_bnds", "lat_bnds", "depth_bnds", "climatology_bounds")) {
  copy_attrs(temperature, v, out, v, "comment")
}
copy_attrs(temperature, "crs", out, "crs",
           c("grid_mapping_name", "epsg_code", "longitude_of_prime_meridian",
             "semi_major_axis", "inverse_flattening"))
science_attrs <- c("standard_name", "coordinates", "cell_methods", "grid_mapping")
copy_attrs(temperature, "t_an", out, "t_an", science_attrs)
copy_attrs(salinity, "s_an", out, "s_an", science_attrs)

for (nm in c("Conventions", "institution", "comment", "naming_authority", "project",
             "processing_level", "standard_name_vocabulary", "featureType", "cdm_data_type",
             "publisher_name", "publisher_url", "publisher_email", "ncei_template_version",
             "license", "Metadata_Conventions", "metadata_link")) {
  if (nm %in% names(t_global)) ncdf4::ncatt_put(out, 0, nm, t_global[[nm]])
}
ncdf4::ncatt_put(out, 0, "title", "Derived test subset of WOA23 annual 1-degree all-decades temperature and salinity")
ncdf4::ncatt_put(out, 0, "summary", "Provider-native annual 1955-2022 climatological temperature and practical salinity at six standard depths")
ncdf4::ncatt_put(out, 0, "references", paste(t_global$references, s_global$references, sep = "\n"))
ncdf4::ncatt_put(out, 0, "geospatial_lon_min", min(lon))
ncdf4::ncatt_put(out, 0, "geospatial_lon_max", max(lon))
ncdf4::ncatt_put(out, 0, "geospatial_lat_min", min(lat))
ncdf4::ncatt_put(out, 0, "geospatial_lat_max", max(lat))
ncdf4::ncatt_put(out, 0, "geospatial_vertical_min", min(depth))
ncdf4::ncatt_put(out, 0, "geospatial_vertical_max", max(depth))
ncdf4::ncatt_put(out, 0, "time_coverage_start", "1955-01-01")
ncdf4::ncatt_put(out, 0, "time_coverage_duration", "P68Y")
ncdf4::ncatt_put(out, 0, "time_coverage_resolution", "P01Y")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_id", "FIXTURE-VERTICAL-001")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_version", "fv1")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_role", "VERTICAL")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_derived", "Exact coordinate/depth subset of two provider files, combined without temporal reinterpretation")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_product", "NOAA/NCEI World Ocean Atlas 2023 full release v3.3 annual 1-degree all-decades")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_identifiers", "NCEI Accession 0270533 v3.3; woa23_decav_t00_01.nc; woa23_decav_s00_01.nc")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_dois", "10.25921/va26-hv25;10.25923/54bh-1613;10.25923/70qt-9574")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_urls", paste(c(temperature_url, salinity_url), collapse = ";"))
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_subset_urls", paste(source_urls, collapse = ";"))
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_sha256", paste(source_sha256, collapse = ";"))
ncdf4::ncatt_put(out, 0, "oceancube_fixture_access_date", "2026-08-21")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_redistribution", "YES-WITH-ATTRIBUTION")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_attribution", "Cite WOA23 dataset DOI and the temperature and salinity volume DOIs; this is a derived subset")
ncdf4::nc_close(out)
closed <- TRUE

if (file.exists(output)) unlink(output)
if (!file.rename(tmp, output)) stop("Failed to move deterministic WOA23 fixture into place")
fixture_sha256 <- sha256_file(output)
size_bytes <- file.info(output)$size

evidence <- data.frame(
  fixture_id = "FIXTURE-VERTICAL-001",
  source_files = "woa23_decav_t00_01.nc;woa23_decav_s00_01.nc",
  source_sha256 = paste(source_sha256, collapse = ";"),
  fixture_sha256 = fixture_sha256,
  size_bytes = size_bytes,
  output = normalizePath(output, winslash = "/"),
  stringsAsFactors = FALSE
)
utils::write.csv(evidence, file.path(cache_dir, "derivation-evidence.csv"), row.names = FALSE)
cat("WOA23_FIXTURE=", normalizePath(output, winslash = "/"), "\n", sep = "")
cat("WOA23_SHA256=", fixture_sha256, "\n", sep = "")
cat("WOA23_SIZE_BYTES=", size_bytes, "\n", sep = "")
