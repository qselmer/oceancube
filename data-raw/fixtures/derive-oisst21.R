#!/usr/bin/env Rscript

# Deterministically derive the governed OISST v2.1 surface/time fixture.

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
            "noaa-oisst21-surface-time-fv1.nc")
}
if (!grepl("^([A-Za-z]:)?[/\\\\]", output)) output <- file.path(repo_root, output)
output <- normalizePath(dirname(output), winslash = "/", mustWork = FALSE) |>
  file.path(basename(output))

cache_dir <- file.path(repo_root, "data-raw", "fixtures", "cache", "oisst21")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)

sha256_file <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  paste0(tolower(as.character(openssl::sha256(con))), collapse = "")
}

download_checked <- function(url, path, expected_sha256) {
  if (!file.exists(path) || !identical(sha256_file(path), expected_sha256)) {
    if (file.exists(path)) unlink(path)
    utils::download.file(url, path, mode = "wb", method = "libcurl", quiet = FALSE)
  }
  actual <- sha256_file(path)
  if (!identical(actual, expected_sha256)) {
    stop("OISST source checksum drift for ", basename(path),
         ": expected ", expected_sha256, ", got ", actual)
  }
  actual
}

dates <- sprintf("202001%02d", 1:4)
filenames <- paste0("oisst-avhrr-v02r01.", dates, ".nc")
base_url <- "https://www.ncei.noaa.gov/thredds/fileServer/OisstBase/NetCDF/V2.1/AVHRR/202001"
urls <- paste(base_url, filenames, sep = "/")
expected_sha256 <- c(
  "11f1127a120f015a1412d44cccc5c94c364786f1f7b98615914ad5d418615ea4",
  "13b747752fa9d65b0b0ec8ae609b4770a96b718c63d364894c9fbbcc4d9c50a5",
  "52667e1e5c326f5ba746c0d4a101cc6541d09838e3738646185158df7fd98048",
  "2a6579f974c716109cd2cc42c1450a45fe607fb2b2c60cf09eae7b0df038c9db"
)
paths <- file.path(cache_dir, filenames)
source_sha256 <- mapply(download_checked, urls, paths, expected_sha256, USE.NAMES = FALSE)

vars <- c("sst", "anom", "err", "ice")
packed <- setNames(vector("list", length(vars)), vars)
source_ncs <- vector("list", length(paths))
on.exit({
  for (nc in source_ncs) if (!is.null(nc)) try(ncdf4::nc_close(nc), silent = TRUE)
}, add = TRUE)

lon <- lat <- zlev <- time <- NULL
for (i in seq_along(paths)) {
  nc <- ncdf4::nc_open(paths[[i]])
  source_ncs[[i]] <- nc
  global <- ncdf4::ncatt_get(nc, 0)
  expected_id <- filenames[[i]]
  if (!identical(global$id, expected_id) ||
      !identical(global$product_version, "Version v02r01") ||
      !grepl("Final", global$title, fixed = TRUE) ||
      !grepl("Final file", global$history, fixed = TRUE) ||
      !identical(global$metadata_link, "https://doi.org/10.25921/RE9P-PT57")) {
    stop("OISST source identity/final-file contract failed for ", expected_id)
  }
  if (!identical(vapply(nc$dim, function(x) x$len, integer(1L))[c("lon", "lat", "zlev", "time")],
                 c(lon = 1440L, lat = 720L, zlev = 1L, time = 1L)) ||
      !all(vars %in% names(nc$var))) {
    stop("Unexpected OISST dimensions or variables in ", expected_id)
  }
  for (v in vars) {
    attrs <- ncdf4::ncatt_get(nc, v)
    if (!identical(nc$var[[v]]$prec, "short") ||
        !isTRUE(all.equal(attrs$`_FillValue`, -999)) ||
        !isTRUE(all.equal(attrs$scale_factor, 0.01, tolerance = 1e-7)) ||
        !isTRUE(all.equal(attrs$add_offset, 0))) {
      stop("OISST packing contract failed for ", v, " in ", expected_id)
    }
  }

  lon_i <- which(nc$dim$lon$vals >= 276 & nc$dim$lon$vals <= 285)
  lat_i <- which(nc$dim$lat$vals >= -18 & nc$dim$lat$vals <= -6)
  if (!identical(length(lon_i), 36L) || !identical(length(lat_i), 48L)) {
    stop("Unexpected OISST subset dimensions in ", expected_id)
  }
  if (i == 1L) {
    lon <- nc$dim$lon$vals[lon_i]
    lat <- nc$dim$lat$vals[lat_i]
    zlev <- nc$dim$zlev$vals
    time <- numeric(length(paths))
    for (v in vars) packed[[v]] <- array(-999L, dim = c(length(lon), length(lat), 1L, length(paths)))
  } else if (!identical(lon, nc$dim$lon$vals[lon_i]) ||
             !identical(lat, nc$dim$lat$vals[lat_i]) ||
             !identical(zlev, nc$dim$zlev$vals)) {
    stop("OISST coordinate drift across daily granules")
  }
  time[[i]] <- nc$dim$time$vals[[1L]]
  for (v in vars) {
    block <- ncdf4::ncvar_get(
      nc, v,
      start = c(min(lon_i), min(lat_i), 1L, 1L),
      count = c(length(lon_i), length(lat_i), 1L, 1L),
      collapse_degen = FALSE,
      raw_datavals = TRUE
    )
    packed[[v]][, , , i] <- block
  }
}

first <- source_ncs[[1L]]
last_global <- ncdf4::ncatt_get(source_ncs[[length(source_ncs)]], 0)
time_units <- ncdf4::ncatt_get(first, "time", "units")$value

tmp <- paste0(output, ".tmp")
if (file.exists(tmp)) unlink(tmp)
dlon <- ncdf4::ncdim_def("lon", "degrees_east", lon, create_dimvar = TRUE)
dlat <- ncdf4::ncdim_def("lat", "degrees_north", lat, create_dimvar = TRUE)
dzlev <- ncdf4::ncdim_def("zlev", "meters", zlev, create_dimvar = TRUE)
dtime <- ncdf4::ncdim_def("time", time_units, time, create_dimvar = TRUE)
defs <- lapply(vars, function(v) {
  a <- ncdf4::ncatt_get(first, v)
  ncdf4::ncvar_def(v, a$units, list(dlon, dlat, dzlev, dtime),
                   missval = -999L, longname = a$long_name, prec = "short",
                   compression = 9, chunksizes = c(length(lon), length(lat), 1L, 1L))
})
out <- ncdf4::nc_create(tmp, defs, force_v4 = TRUE)
closed <- FALSE
on.exit(if (!closed) try(ncdf4::nc_close(out), silent = TRUE), add = TRUE)

for (v in vars) ncdf4::ncvar_put(out, v, packed[[v]])

copy_attrs <- function(source, source_var, target, target_var, names) {
  a <- ncdf4::ncatt_get(source, source_var)
  for (nm in intersect(names, names(a))) ncdf4::ncatt_put(target, target_var, nm, a[[nm]])
}
copy_attrs(first, "lon", out, "lon", c("long_name", "grids"))
copy_attrs(first, "lat", out, "lat", c("long_name", "grids"))
copy_attrs(first, "zlev", out, "zlev", c("long_name", "positive", "actual_range"))
copy_attrs(first, "time", out, "time", c("long_name"))
for (v in vars) {
  a <- ncdf4::ncatt_get(first, v)
  ncdf4::ncatt_put(out, v, "scale_factor", a$scale_factor, prec = "float")
  ncdf4::ncatt_put(out, v, "add_offset", a$add_offset, prec = "float")
  ncdf4::ncatt_put(out, v, "valid_min", a$valid_min, prec = "short")
  ncdf4::ncatt_put(out, v, "valid_max", a$valid_max, prec = "short")
}

g <- ncdf4::ncatt_get(first, 0)
global_names <- c(
  "source", "naming_authority", "cdm_data_type", "processing_level", "institution",
  "creator_url", "creator_email", "keywords", "keywords_vocabulary", "platform_vocabulary",
  "instrument", "instrument_vocabulary", "standard_name_vocabulary", "geospatial_lat_units",
  "geospatial_lat_resolution", "geospatial_lon_units", "geospatial_lon_resolution",
  "ncei_template_version", "Conventions", "history", "metadata_link", "sensor", "title",
  "references", "summary", "product_version", "platform", "comment"
)
for (nm in intersect(global_names, names(g))) ncdf4::ncatt_put(out, 0, nm, g[[nm]])
ncdf4::ncatt_put(out, 0, "geospatial_lon_min", min(lon))
ncdf4::ncatt_put(out, 0, "geospatial_lon_max", max(lon))
ncdf4::ncatt_put(out, 0, "geospatial_lat_min", min(lat))
ncdf4::ncatt_put(out, 0, "geospatial_lat_max", max(lat))
ncdf4::ncatt_put(out, 0, "time_coverage_start", g$time_coverage_start)
ncdf4::ncatt_put(out, 0, "time_coverage_end", last_global$time_coverage_end)
ncdf4::ncatt_put(out, 0, "oceancube_fixture_id", "FIXTURE-SURFACE-TIME-001")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_version", "fv1")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_role", "SURFACE-TIME")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_derived", "Exact coordinate subset and time concatenation of four provider final granules; scientific values unchanged")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_product", "NOAA/NCEI OISST AVHRR-only Final v2.1 / v02r01")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_doi", "10.25921/RE9P-PT57")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_urls", paste(urls, collapse = ";"))
ncdf4::ncatt_put(out, 0, "oceancube_fixture_source_sha256", paste(source_sha256, collapse = ";"))
ncdf4::ncatt_put(out, 0, "oceancube_fixture_access_date", "2026-08-21")
ncdf4::ncatt_put(out, 0, "oceancube_fixture_redistribution", "YES-WITH-ATTRIBUTION")
ncdf4::nc_close(out)
closed <- TRUE

if (file.exists(output)) unlink(output)
if (!file.rename(tmp, output)) stop("Failed to move deterministic OISST fixture into place")
fixture_sha256 <- sha256_file(output)
size_bytes <- file.info(output)$size

evidence <- data.frame(
  fixture_id = "FIXTURE-SURFACE-TIME-001",
  source_files = paste(filenames, collapse = ";"),
  source_sha256 = paste(source_sha256, collapse = ";"),
  fixture_sha256 = fixture_sha256,
  size_bytes = size_bytes,
  output = normalizePath(output, winslash = "/"),
  stringsAsFactors = FALSE
)
utils::write.csv(evidence, file.path(cache_dir, "derivation-evidence.csv"), row.names = FALSE)
cat("OISST_FIXTURE=", normalizePath(output, winslash = "/"), "\n", sep = "")
cat("OISST_SHA256=", fixture_sha256, "\n", sep = "")
cat("OISST_SIZE_BYTES=", size_bytes, "\n", sep = "")
