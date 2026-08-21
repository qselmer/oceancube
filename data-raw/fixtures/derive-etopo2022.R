#!/usr/bin/env Rscript

# Deterministically derive the governed ETOPO 2022 bed-elevation fixture.

options(stringsAsFactors = FALSE)

required <- c("ncdf4", "openssl", "terra")
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
            "noaa-etopo2022-bathymetry-fv1.nc")
}
if (!grepl("^([A-Za-z]:)?[/\\\\]", output)) output <- file.path(repo_root, output)
output <- normalizePath(dirname(output), winslash = "/", mustWork = FALSE) |>
  file.path(basename(output))

cache_dir <- file.path(repo_root, "data-raw", "fixtures", "cache", "etopo2022")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)

sha256_file <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  paste0(tolower(as.character(openssl::sha256(con))), collapse = "")
}

source_url <- paste0(
  "https://gis.ngdc.noaa.gov/arcgis/rest/services/DEM_mosaics/DEM_all/",
  "ImageServer/exportImage?bbox=-84.00000,-18.00000,-75.00000,-6.00000",
  "&bboxSR=4326&size=540,720&imageSR=4326&format=tiff&pixelType=F32",
  "&interpolation=RSP_NearestNeighbor&compression=LZ77",
  "&renderingRule=%7B%22rasterFunction%22%3A%22none%22%7D",
  "&mosaicRule=%7B%22where%22%3A%22Name%3D%27ETOPO_2022_v1_60s_bed%27%22%7D",
  "&f=image"
)
source_path <- file.path(cache_dir, "ETOPO_2022_v1_60s_Peru_bed.tif")
expected_source_sha256 <- "2a53040c514393e8399d79d0bfe60fbe650119ed90ce0393413f7d23c8d15fb4"
if (!file.exists(source_path) || !identical(sha256_file(source_path), expected_source_sha256)) {
  if (file.exists(source_path)) unlink(source_path)
  utils::download.file(source_url, source_path, mode = "wb", method = "libcurl", quiet = FALSE)
}
source_sha256 <- sha256_file(source_path)
if (!identical(source_sha256, expected_source_sha256)) {
  stop("ETOPO provider subset checksum drift: expected ", expected_source_sha256,
       ", got ", source_sha256)
}

raster <- terra::rast(source_path)
extent <- as.vector(terra::ext(raster))
resolution <- terra::res(raster)
if (!isTRUE(all.equal(c(terra::ncol(raster), terra::nrow(raster), terra::nlyr(raster)),
                     c(540, 720, 1))) ||
    !isTRUE(all.equal(unname(extent), c(-84, -75, -18, -6), tolerance = 1e-10)) ||
    !isTRUE(all.equal(resolution, rep(1 / 60, 2L), tolerance = 1e-10)) ||
    !grepl("+proj=longlat", terra::crs(raster, proj = TRUE), fixed = TRUE)) {
  stop("ETOPO Grid Extract identity, extent, resolution, or CRS contract failed")
}

source_matrix <- terra::as.matrix(raster, wide = TRUE)
if (!identical(dim(source_matrix), c(720L, 540L)) || anyNA(source_matrix) ||
    !any(source_matrix < 0) || !any(source_matrix > 0)) {
  stop("ETOPO source values do not satisfy the expected land/ocean elevation contract")
}
lon <- terra::xFromCol(raster, seq_len(terra::ncol(raster)))
lat_desc <- terra::yFromRow(raster, seq_len(terra::nrow(raster)))
lat <- rev(lat_desc)
z <- t(source_matrix[nrow(source_matrix):1L, , drop = FALSE])
if (!identical(dim(z), c(length(lon), length(lat))) ||
    !isTRUE(all(diff(lon) > 0)) || !isTRUE(all(diff(lat) > 0))) {
  stop("ETOPO coordinate orientation contract failed")
}

tmp <- paste0(output, ".tmp")
if (file.exists(tmp)) unlink(tmp)
dlon <- ncdf4::ncdim_def("lon", "degrees_east", lon, create_dimvar = TRUE)
dlat <- ncdf4::ncdim_def("lat", "degrees_north", lat, create_dimvar = TRUE)
z_def <- ncdf4::ncvar_def(
  "z", "meters", list(dlon, dlat), missval = NULL,
  longname = "Elevation relative to the geoid", prec = "float",
  compression = 9, chunksizes = c(90L, 90L)
)
out <- ncdf4::nc_create(tmp, list(z_def), force_v4 = TRUE)
closed <- FALSE
on.exit(if (!closed) try(ncdf4::nc_close(out), silent = TRUE), add = TRUE)
ncdf4::ncvar_put(out, "z", z)

ncdf4::ncatt_put(out, "lon", "standard_name", "longitude")
ncdf4::ncatt_put(out, "lon", "long_name", "longitude")
ncdf4::ncatt_put(out, "lon", "axis", "X")
ncdf4::ncatt_put(out, "lat", "standard_name", "latitude")
ncdf4::ncatt_put(out, "lat", "long_name", "latitude")
ncdf4::ncatt_put(out, "lat", "axis", "Y")
ncdf4::ncatt_put(out, "z", "coordinates", "lat lon")
ncdf4::ncatt_put(out, 0, "title", "Derived test subset of ETOPO 2022 v1 60 arc-second bedrock elevation")
ncdf4::ncatt_put(out, 0, "institution", "NOAA National Centers for Environmental Information")
ncdf4::ncatt_put(out, 0, "source", "NCEI Grid Extract: ETOPO_2022_v1_60s_bed")
ncdf4::ncatt_put(out, 0, "references", "NOAA National Centers for Environmental Information (2022), ETOPO 2022 15 Arc-Second Global Relief Model, https://doi.org/10.25921/fd45-gt74")
ncdf4::ncatt_put(out, 0, "comment", "Elevation semantics and sign are preserved: ocean elevations are negative. Not for navigation.")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_id", "FIXTURE-BATHYMETRY-001")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_version", "fv1")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_role", "BATHYMETRY")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_derived", "Provider-side geographic subset on the native 60 arc-second grid; nearest-neighbour export; values and sign otherwise unchanged")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_product", "NOAA/NCEI ETOPO 2022 v1 60 arc-second bedrock elevation")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_identifier", "ETOPO_2022_v1_60s_N90W180_bed.nc; ETOPO_2022_v1_60s_bed")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_doi", "10.25921/fd45-gt74")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_url", source_url)
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_sha256", source_sha256)
ncdf4::ncatt_put(out, 0, "oceancube_fixture_access_date", "2026-08-21")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_license", "CC0-1.0")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_redistribution", "YES-WITH-ATTRIBUTION")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_horizontal_crs", "EPSG:4326")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_vertical_datum", "EGM2008")
ncdf4::nc_close(out)
closed <- TRUE

if (file.exists(output)) unlink(output)
if (!file.rename(tmp, output)) stop("Failed to move deterministic ETOPO fixture into place")
fixture_sha256 <- sha256_file(output)
size_bytes <- file.info(output)$size

evidence <- data.frame(
  fixture_id = "FIXTURE-BATHYMETRY-001",
  source_file = basename(source_path),
  source_sha256 = source_sha256,
  fixture_sha256 = fixture_sha256,
  size_bytes = size_bytes,
  output = normalizePath(output, winslash = "/"),
  stringsAsFactors = FALSE
)
utils::write.csv(evidence, file.path(cache_dir, "derivation-evidence.csv"), row.names = FALSE)
cat("ETOPO_FIXTURE=", normalizePath(output, winslash = "/"), "\n", sep = "")
cat("ETOPO_SHA256=", fixture_sha256, "\n", sep = "")
cat("ETOPO_SIZE_BYTES=", size_bytes, "\n", sep = "")
