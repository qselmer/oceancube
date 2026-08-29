test_that("OISST uses one semantic zlev resolver in eager and deferred readers", {
  file <- test_path(
    "fixtures", "real-data", "noaa-oisst21-surface-time-fv1.nc"
  )
  vars <- c("sst", "anom", "err", "ice")

  eager <- read_nc(file, vars = vars)
  deferred <- cube_open(file, vars = NULL)
  collected <- cube_collect(deferred)

  expect_identical(eager$metadata$schema_name, "oceancube_metadata")
  expect_identical(eager$metadata$schema_version, "1.0.0")
  expect_identical(eager$metadata$cf$schema_name, "oceancube_cf_metadata")
  expect_identical(eager$metadata$cf$schema_version, "1.0.0")
  expect_identical(
    eager$metadata$cf$current$axes$depth$source_id,
    "zlev"
  )
  expect_identical(
    deferred$metadata$cf$current$axes$depth$source_id,
    "zlev"
  )
  expect_true(any(grepl(
    "positive=down", eager$metadata$cf$current$axes$depth$evidence,
    fixed = TRUE
  )))
  expect_identical(
    eager$metadata$cf$source,
    deferred$metadata$cf$source
  )
  expect_identical(deferred$metadata, collected$metadata)
  expect_identical(eager$lon, collected$lon)
  expect_identical(eager$lat, collected$lat)
  expect_identical(eager$depth, collected$depth)
  expect_identical(eager$time, collected$time)
  expect_identical(eager$vars, collected$vars)
  expect_identical(as.vector(is.na(eager$data)), as.vector(is.na(collected$data)))
  expect_equal(as.vector(eager$data), as.vector(collected$data), tolerance = 0)
})

test_that("explicit zlev remains supported and strong conflicts are fatal", {
  file <- test_path(
    "fixtures", "real-data", "noaa-oisst21-surface-time-fv1.nc"
  )
  explicit <- read_nc(file, vars = "sst", depth_name = "zlev")

  expect_identical(
    explicit$metadata$cf$current$axes$depth$method,
    "explicit"
  )

  conflicting <- make_cf_b2_fixture()
  withr::local_file(conflicting)
  nc <- ncdf4::nc_open(conflicting, write = TRUE)
  ncdf4::ncatt_put(nc, "lon", "units", "degrees_north")
  ncdf4::nc_close(nc)
  expect_error(
    read_nc(conflicting, vars = "temperature"),
    class = "oceancube_cf_axis_conflict"
  )
  expect_error(
    cube_open(conflicting, vars = "temperature", lon_name = "lon"),
    class = "oceancube_cf_axis_conflict"
  )
})

test_that("surface current metadata records an inserted singleton only", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  x <- cube_open(file, vars = "sst")

  expect_identical(x$metadata$cf$current$axes$depth$method, "inserted_singleton")
  expect_true(is.na(x$metadata$cf$current$axes$depth$source_id))
  expect_false("inserted_depth" %in% x$metadata$cf$source$dimensions$order)
  expect_identical(x$metadata$cf$current$variables, "sst")
  expect_true(all(c(
    "temperature", "oxygen", "sst", "chlorophyll"
  ) %in% x$metadata$cf$source$variables$order))
})

test_that("ETOPO and WOA metadata scan before representability and decoding", {
  fixture <- function(name) test_path("fixtures", "real-data", name)
  etopo_file <- fixture("noaa-etopo2022-bathymetry-fv1.nc")
  woa_file <- fixture("noaa-woa23-vertical-fv1.nc")
  etopo <- .cf_scan_netcdf(etopo_file)
  woa <- .cf_scan_netcdf(woa_file)

  expect_null(etopo$source$declaration$raw)
  expect_identical(etopo$source$dimensions$order, c("lon", "lat"))
  expect_true("z" %in% etopo$source$variables$order)
  expect_identical(
    .cf_attribute_value(etopo$source$variables$map$z$attributes, "coordinates"),
    "lat lon"
  )
  expect_error(read_nc(etopo_file), "Could not identify time", fixed = TRUE)
  expect_error(cube_open(etopo_file), "Cannot resolve the time dimension")

  expect_identical(woa$source$declaration$raw, "CF-1.6")
  expect_identical(
    .cf_attribute_value(woa$source$variables$map$time$attributes, "units"),
    "months since 1955-01-01 00:00:00"
  )
  expect_null(.cf_attribute_value(
    woa$source$variables$map$time$attributes, "calendar"
  ))
  climatology <- cf_link_records(woa, "climatology")
  expect_length(climatology, 1L)
  expect_identical(climatology[[1L]]$resolved_path, "climatology_bounds")
  expect_true(any(vapply(
    cf_link_records(woa, "bounds"), `[[`, character(1L), "resolved_path"
  ) == "depth_bnds"))
  expect_true(any(vapply(
    cf_link_records(woa, "grid_mapping"), `[[`, character(1L), "resolved_path"
  ) == "crs"))
  expect_match(
    .cf_attribute_value(woa$source$variables$map$t_an$attributes, "cell_methods"),
    "time: mean over years", fixed = TRUE
  )
  expect_error(
    read_nc(woa_file, vars = c("t_an", "s_an")),
    "Core will not infer a provider-specific offset correction", fixed = TRUE
  )
  expect_error(
    cube_open(woa_file, vars = c("t_an", "s_an")),
    "Core will not infer a provider-specific offset correction", fixed = TRUE
  )
})

test_that("serialization and collection retain exact canonical metadata", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  eager <- read_nc(
    file, vars = "temperature", lon_name = "longitude",
    lat_name = "latitude", depth_name = "depth", time_name = "time"
  )
  deferred <- cube_open(file, vars = "temperature")
  collected <- cube_collect(deferred)
  rds <- tempfile(fileext = ".rds")
  withr::local_file(rds)

  saveRDS(deferred, rds)
  restored <- readRDS(rds)

  expect_identical(eager$metadata, unserialize(serialize(eager$metadata, NULL)))
  expect_identical(deferred$metadata, restored$metadata)
  expect_identical(deferred$metadata, collected$metadata)
  expect_true(.cf_metadata_validate(restored$metadata))
  expect_false(.cf_contains_forbidden(restored$metadata))
})

test_that("selection preserves source and derivations are explicit", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  source <- cube_open(file, vars = c("temperature", "oxygen"))
  selected <- cube_slice(
    source, longitude = 1:2, variable = 1L, by = "index"
  )
  aggregated <- cube_aggregate_time(source, by = "day")

  expect_identical(
    source$metadata$cf$source,
    selected$metadata$cf$source
  )
  expect_identical(selected$metadata$cf$current$semantic_status, "CURRENT_SELECTION")
  expect_identical(selected$metadata$cf$current$variables, "temperature")
  expect_identical(
    source$metadata$cf$source,
    aggregated$metadata$cf$source
  )
  expect_identical(
    aggregated$metadata$cf$current$semantic_status,
    "DERIVATION_PENDING"
  )
  expect_identical(
    aggregated$metadata$cf$current$derivation$operation,
    "cube_aggregate_time"
  )
})

test_that("cube validation accepts absent metadata and rejects malformed metadata", {
  legacy <- .make_baseline_fixture()$cube
  legacy$metadata <- NULL
  expect_false(any(cube_validate(legacy)$status == "FAIL"))

  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  x <- cube_open(file, vars = "temperature")
  x$metadata$cf$schema_version <- "broken"
  report <- cube_validate(x)

  expect_identical(report$status[report$check == "cf_metadata"], "FAIL")
  expect_error(cube_validate(x, strict = TRUE), class = "oceancube_validation_error")
  expect_error(.check_cube(x), class = "oceancube_cf_metadata_error")
})

test_that("metadata size smoke contains descriptors rather than payload arrays", {
  oisst_file <- test_path(
    "fixtures", "real-data", "noaa-oisst21-surface-time-fv1.nc"
  )
  oisst <- cube_open(oisst_file, vars = NULL)
  synthetic_file <- make_cf_b2_fixture()
  withr::local_file(synthetic_file)
  synthetic <- read_nc(synthetic_file, vars = "temperature")

  oisst_metadata <- as.numeric(object.size(oisst$metadata))
  oisst_payload <- prod(as.double(.cube_shape(oisst))) * 8
  synthetic_metadata <- as.numeric(object.size(synthetic$metadata))
  synthetic_payload <- prod(as.double(.cube_shape(synthetic))) * 8

  expect_gt(oisst_metadata, 0)
  expect_gt(oisst_payload, 0)
  expect_gt(synthetic_metadata, 0)
  expect_gt(synthetic_payload, 0)
  expect_false(.cf_contains_forbidden(oisst$metadata))
  expect_false(.cf_contains_forbidden(synthetic$metadata))
  expect_true(is.finite(oisst_metadata / oisst_payload))
  expect_true(is.finite(synthetic_metadata / synthetic_payload))
})
