#!/usr/bin/env Rscript

# Deterministically derive the governed WOA23 January vertical fixture.

options(stringsAsFactors = FALSE)

required <- c("ncdf4", "openssl")
missing <- required[!vapply(required, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing)) stop("Missing maintainer package(s): ", paste(missing, collapse = ", "))

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- sub("^--file=", "", script_arg[[1L]])
repo_root <- normalizePath(file.path(dirname(script_file), "..", ".."), winslash = "/")
args <- commandArgs(TRUE)
value_arg <- function(prefix, default = NULL) {
  hit <- grep(paste0("^", prefix, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^", prefix, "="), "", hit[[1L]]) else default
}
output <- value_arg(
  "--output",
  file.path(repo_root, "tests", "testthat", "fixtures", "real-data",
            "noaa-woa23-monthly-vertical-fv1.nc")
)
if (!grepl("^([A-Za-z]:)?[/\\\\]", output)) output <- file.path(repo_root, output)
output <- file.path(
  normalizePath(dirname(output), winslash = "/", mustWork = FALSE),
  basename(output)
)
source_dir <- value_arg("--source-dir", file.path(
  repo_root, "data-raw", "fixtures", "cache", "woa23-monthly"
))
if (!grepl("^([A-Za-z]:)?[/\\\\]", source_dir)) source_dir <- file.path(repo_root, source_dir)
dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)

sha256_file <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  paste0(tolower(as.character(openssl::sha256(con))), collapse = "")
}

source_urls <- c(
  temperature = "https://www.ncei.noaa.gov/thredds-ocean/fileServer/woa23/DATA/temperature/netcdf/decav/1.00/woa23_decav_t01_01.nc",
  salinity = "https://www.ncei.noaa.gov/thredds-ocean/fileServer/woa23/DATA/salinity/netcdf/decav/1.00/woa23_decav_s01_01.nc"
)
source_paths <- file.path(source_dir, c(
  "woa23_decav_t01_01.nc", "woa23_decav_s01_01.nc"
))
names(source_paths) <- names(source_urls)
expected_source_sha256 <- c(
  temperature = "2311eb2642e8ebe4da8c9e122ffcb83acff66c000300280f907d20be9a659676",
  salinity = "437b2c2b472ceccbbecc563beefefe801557feaa6eb556c1f0075b39904105bf"
)
for (i in seq_along(source_paths)) {
  if (!file.exists(source_paths[[i]])) {
    utils::download.file(source_urls[[i]], source_paths[[i]], mode = "wb",
                         method = "libcurl", quiet = FALSE)
  }
}
source_sha256 <- setNames(vapply(source_paths, sha256_file, character(1L)), names(source_urls))
if (!identical(unname(source_sha256), unname(expected_source_sha256))) {
  stop("WOA23 January full-source checksum drift")
}

temperature <- ncdf4::nc_open(source_paths[["temperature"]])
salinity <- ncdf4::nc_open(source_paths[["salinity"]])
on.exit({
  try(ncdf4::nc_close(temperature), silent = TRUE)
  try(ncdf4::nc_close(salinity), silent = TRUE)
}, add = TRUE)
t_global <- ncdf4::ncatt_get(temperature, 0)
s_global <- ncdf4::ncatt_get(salinity, 0)
if (!identical(t_global$id, "woa23_decav_t01_01.nc") ||
    !identical(s_global$id, "woa23_decav_s01_01.nc") ||
    !identical(t_global$Conventions, "CF-1.6") ||
    !identical(s_global$Conventions, "CF-1.6") ||
    !grepl("openly available", t_global$license, fixed = TRUE) ||
    !grepl("openly available", s_global$license, fixed = TRUE) ||
    !grepl("10.25923/54bh-1613", t_global$references, fixed = TRUE) ||
    !grepl("10.25923/70qt-9574", s_global$references, fixed = TRUE)) {
  stop("WOA23 January source identity, license, CF, or citation contract failed")
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
  stop("WOA23 January coordinate identity or selection contract failed")
}

read_block <- function(nc, var, selectors) {
  dims <- nc$var[[var]]$dim
  dim_names <- vapply(dims, `[[`, character(1L), "name")
  start <- rep(1L, length(dims))
  count <- as.integer(vapply(dims, `[[`, numeric(1L), "len"))
  for (nm in intersect(names(selectors), dim_names)) {
    idx <- selectors[[nm]]
    at <- match(nm, dim_names)
    start[[at]] <- min(idx)
    count[[at]] <- length(idx)
  }
  ncdf4::ncvar_get(nc, var, start = start, count = count, collapse_degen = FALSE)
}

lon <- as.numeric(temperature$dim$lon$vals[lon_i])
lat <- as.numeric(temperature$dim$lat$vals[lat_i])
depth <- as.numeric(temperature$dim$depth$vals[depth_i])
time <- as.numeric(temperature$dim$time$vals)
selectors <- list(lon = lon_i, lat = lat_i, depth = upper_i, time = 1L)
t_upper <- read_block(temperature, "t_an", selectors)
s_upper <- read_block(salinity, "s_an", selectors)
t_an <- t_upper[, , depth_i, , drop = FALSE]
s_an <- s_upper[, , depth_i, , drop = FALSE]
lon_bnds <- read_block(temperature, "lon_bnds", list(lon = lon_i))
lat_bnds <- read_block(temperature, "lat_bnds", list(lat = lat_i))
depth_bnds <- read_block(temperature, "depth_bnds", list(depth = upper_i))[, depth_i, drop = FALSE]
climatology_bounds <- read_block(temperature, "climatology_bounds", list(time = 1L))
if (!identical(dim(t_an), c(9L, 12L, 6L, 1L)) ||
    !identical(dim(s_an), c(9L, 12L, 6L, 1L)) ||
    !identical(time, 396.5) ||
    !identical(as.numeric(climatology_bounds), c(0, 805)) ||
    !identical(as.numeric(depth), depth_target)) {
  stop("WOA23 January data shape or coordinate contract failed")
}
expected_bounds <- rbind(
  c(0, 2.5), c(7.5, 12.5), c(17.5, 22.5),
  c(47.5, 52.5), c(97.5, 112.5), c(187.5, 212.5)
)
if (!identical(t(depth_bnds), expected_bounds) ||
    !identical(ncdf4::ncatt_get(temperature, "depth", "positive")$value, "down") ||
    !identical(ncdf4::ncatt_get(temperature, "depth", "units")$value, "meters") ||
    !identical(ncdf4::ncatt_get(temperature, "depth", "standard_name")$value, "depth") ||
    !identical(ncdf4::ncatt_get(temperature, "time", "units")$value,
               "months since 1955-01-01 00:00:00") ||
    isTRUE(ncdf4::ncatt_get(temperature, "time", "calendar")$hasatt)) {
  stop("WOA23 January vertical/time source contract failed")
}

tmp <- paste0(output, ".tmp")
if (file.exists(tmp)) unlink(tmp)
dnbounds <- ncdf4::ncdim_def("nbounds", "", 1:2, create_dimvar = FALSE)
dlon <- ncdf4::ncdim_def("lon", "degrees_east", lon)
dlat <- ncdf4::ncdim_def("lat", "degrees_north", lat)
ddepth <- ncdf4::ncdim_def("depth", "meters", depth)
dtime <- ncdf4::ncdim_def("time", "months since 1955-01-01 00:00:00", time)
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

copy_attrs <- function(source, source_var, target, target_var, keep) {
  attrs <- ncdf4::ncatt_get(source, source_var)
  for (nm in intersect(keep, names(attrs))) ncdf4::ncatt_put(target, target_var, nm, attrs[[nm]])
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
ncdf4::ncatt_put(out, 0, "title", "Derived test subset of WOA23 January 1-degree all-decades temperature and salinity")
ncdf4::ncatt_put(out, 0, "summary", "Provider-native January climatological temperature and practical salinity at six standard depths")
ncdf4::ncatt_put(out, 0, "references", paste(t_global$references, s_global$references, sep = "\n"))
ncdf4::ncatt_put(out, 0, "time_coverage_start", "1955-01-01")
ncdf4::ncatt_put(out, 0, "time_coverage_end", "2022-12-31")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_id", "FIXTURE-VERTICAL-MONTHLY-001")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_version", "fv1")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_role", "VERTICAL-POSITIVE")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_derived", "Exact coordinate/depth subset of two official January provider files; no temporal or vertical metadata rewritten")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_product", "NOAA/NCEI World Ocean Atlas 2023 v3.3 January 1-degree all-decades")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_identifiers", "NCEI Accession 0270533 v3.3; woa23_decav_t01_01.nc; woa23_decav_s01_01.nc")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_dois", "10.25921/va26-hv25;10.25923/54bh-1613;10.25923/70qt-9574")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_urls", paste(source_urls, collapse = ";"))
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_sha256", paste(source_sha256, collapse = ";"))
ncdf4::ncatt_put(out, 0, "oceancube_fixture_access_date", "2026-08-29")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_redistribution", "YES-WITH-ATTRIBUTION")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_attribution", "Cite WOA23 dataset DOI and temperature/salinity volume DOIs; this is a derived subset")
ncdf4::nc_close(out)
closed <- TRUE

if (file.exists(output)) unlink(output)
if (!file.rename(tmp, output)) stop("Failed to move deterministic WOA23 monthly fixture")
fixture_sha256 <- sha256_file(output)
cat("WOA23_MONTHLY_FIXTURE=", normalizePath(output, winslash = "/"), "\n", sep = "")
cat("WOA23_MONTHLY_SHA256=", fixture_sha256, "\n", sep = "")
cat("WOA23_MONTHLY_SIZE_BYTES=", file.info(output)$size, "\n", sep = "")
