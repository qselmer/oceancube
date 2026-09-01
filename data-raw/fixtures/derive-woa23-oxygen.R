#!/usr/bin/env Rscript

# Deterministically derive the governed WOA23 January oxygen fixture for C7.

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
            "noaa-woa23-monthly-oxygen-fv1.nc")
)
if (!grepl("^([A-Za-z]:)?[/\\\\]", output)) output <- file.path(repo_root, output)
output <- file.path(
  normalizePath(dirname(output), winslash = "/", mustWork = FALSE),
  basename(output)
)
source_dir <- value_arg("--source-dir", file.path(
  repo_root, "data-raw", "fixtures", "cache", "woa23-oxygen"
))
if (!grepl("^([A-Za-z]:)?[/\\\\]", source_dir)) {
  source_dir <- file.path(repo_root, source_dir)
}
dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)

sha256_file <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  paste0(tolower(as.character(openssl::sha256(con))), collapse = "")
}

source_url <- paste0(
  "https://www.ncei.noaa.gov/thredds-ocean/fileServer/woa23/DATA/",
  "oxygen/netcdf/all/1.00/woa23_all_o01_01.nc"
)
source_path <- file.path(source_dir, "woa23_all_o01_01.nc")
expected_source_sha256 <-
  "bccd1abc57ab106e6564e0312d09de835985ce7a74e431f8834e021a10876591"
if (!file.exists(source_path)) {
  utils::download.file(source_url, source_path, mode = "wb",
                       method = "libcurl", quiet = FALSE)
}
source_sha256 <- sha256_file(source_path)
if (!identical(source_sha256, expected_source_sha256)) {
  stop("WOA23 January oxygen full-source checksum drift")
}

source <- ncdf4::nc_open(source_path)
closed_source <- FALSE
on.exit(if (!closed_source) try(ncdf4::nc_close(source), silent = TRUE), add = TRUE)
global <- ncdf4::ncatt_get(source, 0)
oxygen_attrs <- ncdf4::ncatt_get(source, "o_an")
if (!identical(global$id, "woa23_all_o01_01.nc") ||
    !identical(global$Conventions, "CF-1.6") ||
    !grepl("openly available", global$license, fixed = TRUE) ||
    !grepl("10.25923/rb67-ns53", global$references, fixed = TRUE) ||
    !identical(
      oxygen_attrs$standard_name,
      "moles_of_oxygen_per_unit_mass_in_sea_water"
    ) ||
    !identical(oxygen_attrs$units, "micromoles_per_kilogram") ||
    !grepl("depth: mean", oxygen_attrs$cell_methods, fixed = TRUE)) {
  stop("WOA23 oxygen identity, license, citation, or CF contract failed")
}

lon_target <- c(-83.5, -82.5, -81.5)
lat_target <- c(-17.5, -16.5, -15.5)
lon_i <- match(lon_target, source$dim$lon$vals)
lat_i <- match(lat_target, source$dim$lat$vals)
depth_i <- which(source$dim$depth$vals <= 1000)
if (anyNA(lon_i) || anyNA(lat_i) || length(depth_i) != 47L ||
    !identical(as.numeric(source$dim$depth$vals[depth_i]),
               c(seq(0, 100, 5), seq(125, 500, 25), seq(550, 1000, 50)))) {
  stop("WOA23 oxygen coordinate selection contract failed")
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
  ncdf4::ncvar_get(
    nc, var, start = start, count = count, collapse_degen = FALSE
  )
}

selectors <- list(lon = lon_i, lat = lat_i, depth = depth_i, time = 1L)
oxygen <- read_block(source, "o_an", selectors)
lon_bnds <- read_block(source, "lon_bnds", list(lon = lon_i))
lat_bnds <- read_block(source, "lat_bnds", list(lat = lat_i))
depth_bnds <- read_block(source, "depth_bnds", list(depth = depth_i))
climatology_bounds <- read_block(source, "climatology_bounds", list(time = 1L))
lon <- as.numeric(source$dim$lon$vals[lon_i])
lat <- as.numeric(source$dim$lat$vals[lat_i])
depth <- as.numeric(source$dim$depth$vals[depth_i])
time <- as.numeric(source$dim$time$vals)
if (!identical(dim(oxygen), c(3L, 3L, 47L, 1L)) ||
    !all(is.finite(oxygen)) || !identical(time, 336.5) ||
    !identical(as.numeric(climatology_bounds), c(0, 685)) ||
    !identical(ncdf4::ncatt_get(source, "depth", "positive")$value, "down") ||
    !identical(ncdf4::ncatt_get(source, "depth", "units")$value, "meters") ||
    !identical(ncdf4::ncatt_get(source, "time", "units")$value,
               "months since 1965-01-01 00:00:00") ||
    isTRUE(ncdf4::ncatt_get(source, "time", "calendar")$hasatt)) {
  stop("WOA23 oxygen payload, vertical, or time contract failed")
}
depth_bounds_matrix <- t(depth_bnds)
if (any(abs(depth_bounds_matrix[-1L, 1L] -
            depth_bounds_matrix[-nrow(depth_bounds_matrix), 2L]) > 1e-7)) {
  stop("WOA23 oxygen selected depth cells are not contiguous")
}

tmp <- paste0(output, ".tmp")
if (file.exists(tmp)) unlink(tmp)
dnbounds <- ncdf4::ncdim_def("nbounds", "", 1:2, create_dimvar = FALSE)
dlon <- ncdf4::ncdim_def("lon", "degrees_east", lon)
dlat <- ncdf4::ncdim_def("lat", "degrees_north", lat)
ddepth <- ncdf4::ncdim_def("depth", "meters", depth)
dtime <- ncdf4::ncdim_def(
  "time", "months since 1965-01-01 00:00:00", time
)
fill <- oxygen_attrs$`_FillValue`
defs <- list(
  ncdf4::ncvar_def("lon_bnds", "degrees_east", list(dnbounds, dlon),
                   missval = NULL, prec = "float", compression = 9),
  ncdf4::ncvar_def("lat_bnds", "degrees_north", list(dnbounds, dlat),
                   missval = NULL, prec = "float", compression = 9),
  ncdf4::ncvar_def("depth_bnds", "meters", list(dnbounds, ddepth),
                   missval = NULL, prec = "float", compression = 9),
  ncdf4::ncvar_def(
    "climatology_bounds", "months since 1965-01-01 00:00:00",
    list(dnbounds, dtime), missval = NULL, prec = "float", compression = 9
  ),
  ncdf4::ncvar_def("crs", "", list(), missval = NULL, prec = "integer"),
  ncdf4::ncvar_def(
    "o_an", "micromoles_per_kilogram", list(dlon, dlat, ddepth, dtime),
    missval = fill, longname = oxygen_attrs$long_name,
    prec = "float", compression = 9, chunksizes = c(3L, 3L, 47L, 1L)
  )
)
out <- ncdf4::nc_create(tmp, defs, force_v4 = TRUE)
closed_output <- FALSE
on.exit(if (!closed_output) try(ncdf4::nc_close(out), silent = TRUE), add = TRUE)
ncdf4::ncvar_put(out, "lon_bnds", lon_bnds)
ncdf4::ncvar_put(out, "lat_bnds", lat_bnds)
ncdf4::ncvar_put(out, "depth_bnds", depth_bnds)
ncdf4::ncvar_put(out, "climatology_bounds", climatology_bounds)
ncdf4::ncvar_put(out, "crs", 1L)
ncdf4::ncvar_put(out, "o_an", oxygen)

copy_attrs <- function(source_nc, source_var, target_nc, target_var, keep) {
  attrs <- ncdf4::ncatt_get(source_nc, source_var)
  for (nm in intersect(keep, names(attrs))) {
    ncdf4::ncatt_put(target_nc, target_var, nm, attrs[[nm]])
  }
}
coordinate_attrs <- c(
  "standard_name", "long_name", "axis", "bounds", "positive", "climatology"
)
for (v in c("lon", "lat", "depth", "time")) {
  copy_attrs(source, v, out, v, coordinate_attrs)
}
for (v in c("lon_bnds", "lat_bnds", "depth_bnds", "climatology_bounds")) {
  copy_attrs(source, v, out, v, "comment")
}
copy_attrs(
  source, "crs", out, "crs",
  c("grid_mapping_name", "epsg_code", "longitude_of_prime_meridian",
    "semi_major_axis", "inverse_flattening")
)
copy_attrs(
  source, "o_an", out, "o_an",
  c("standard_name", "coordinates", "cell_methods", "grid_mapping")
)
for (nm in c(
  "Conventions", "institution", "comment", "naming_authority", "project",
  "processing_level", "standard_name_vocabulary", "featureType",
  "cdm_data_type", "publisher_name", "publisher_url", "publisher_email",
  "ncei_template_version", "license", "Metadata_Conventions", "metadata_link"
)) {
  if (nm %in% names(global)) ncdf4::ncatt_put(out, 0, nm, global[[nm]])
}
ncdf4::ncatt_put(
  out, 0, "title",
  "Derived test subset of WOA23 January 1-degree dissolved oxygen"
)
ncdf4::ncatt_put(
  out, 0, "summary",
  "Provider-native January oxygen cell means at 47 contiguous standard depths through 1000 m off Peru"
)
ncdf4::ncatt_put(out, 0, "references", global$references)
ncdf4::ncatt_put(out, 0, "time_coverage_start", "1965-01-01")
ncdf4::ncatt_put(out, 0, "time_coverage_end", "2022-12-31")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_id", "FIXTURE-OXYGEN-C7-001")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_version", "fv1")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_role", "OXYGEN-VERTICAL-C7")
ncdf4::ncatt_put(
  out, 0, "oceancube_fixture_derived",
  "Exact coordinate/depth subset of the official WOA23 January oxygen file; no scientific or temporal metadata rewritten"
)
ncdf4::ncatt_put(
  out, 0, "oceancube_fixture_source_product",
  "NOAA/NCEI World Ocean Atlas 2023 v1.1 January 1-degree all-data dissolved oxygen"
)
ncdf4::ncatt_put(
  out, 0, "oceancube_fixture_source_identifiers",
  "woa23_all_o01_01.nc; gov.noaa.nodc:NCEI-WOA23"
)
ncdf4::ncatt_put(
  out, 0, "oceancube_fixture_source_dois",
  "10.25921/va26-hv25;10.25923/rb67-ns53"
)
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_url", source_url)
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_sha256", source_sha256)
ncdf4::ncatt_put(out, 0, "oceancube_fixture_access_date", "2026-09-01")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_redistribution", "YES-WITH-ATTRIBUTION")
ncdf4::ncatt_put(
  out, 0, "oceancube_fixture_attribution",
  "Cite WOA23 and Oxygen Volume 3 DOI; this is a derived offline test subset"
)
ncdf4::nc_close(out)
closed_output <- TRUE
ncdf4::nc_close(source)
closed_source <- TRUE

if (file.exists(output)) unlink(output)
if (!file.rename(tmp, output)) stop("Failed to move deterministic WOA23 oxygen fixture")
fixture_sha256 <- sha256_file(output)
cat("WOA23_OXYGEN_FIXTURE=", normalizePath(output, winslash = "/"), "\n", sep = "")
cat("WOA23_OXYGEN_SOURCE_SHA256=", source_sha256, "\n", sep = "")
cat("WOA23_OXYGEN_FIXTURE_SHA256=", fixture_sha256, "\n", sep = "")
cat("WOA23_OXYGEN_SIZE_BYTES=", file.info(output)$size, "\n", sep = "")
