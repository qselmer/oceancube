test_that("the public API surface includes the approved 0.2 additions", {
  expected <- list(
    annual_index = c("x", "threshold_pos", "threshold_neg"),
    anom_diff = c("x", "clim"),
    anom_z = c("x", "clim"),
    clim_day = c("x", "period", "leap", "min_n"),
    clim_month = c("x", "period"),
    cm_connect = c("env", "required", "module", "verbose"),
    cm_setup = c("env", "install", "module", "verbose"),
    coast_dist = c("x", "coast"),
    crop_stock = c("x", "mask"),
    cube_cell_area = c("x", "unit"),
    cube_cell_volume = c("x", "depth_bounds", "unit"),
    cube_aggregate_time = c("x", "by", "method", "na.rm", "min_n", "diagnostics"),
    cube_anomaly = c("x", "climatology", "type"),
    cube_climatology = c("x", "by", "period", "leap", "min_n", "diagnostics"),
    cube_trend = c("x", "method", "period", "time_unit", "min_n", "diagnostics"),
    cube_collect = "x",
    cube_crop = c("x", "longitude", "latitude", "depth", "time",
                  "variable", "bbox", "outside"),
    cube_extract = c("x", "longitude", "latitude", "depth", "time",
                     "variable", "by", "match", "tolerance", "mode",
                     "format", "keep_index", "keep_distance"),
    cube_inspect = c("x", "missing"),
    cube_layer_thickness = c("x", "depth_bounds", "unit"),
    cube_mask = c("x", "polygons", "crs", "keep", "boundary"),
    cube_open = c("file", "vars", "lon_name", "lat_name", "depth_name",
                  "time_name", "source", "dataset_id"),
    cube_polygon_weights = c("x", "polygons", "id_col", "crs",
                             "dimension", "depth_bounds", "include_zero"),
    cube_slice = c("x", "longitude", "latitude", "depth", "time",
                   "variable", "by", "match", "tolerance"),
    cube_transect = c("x", "path", "lon_col", "lat_col", "id_col",
                      "depth", "time", "variable", "by", "match",
                      "tolerance", "mode", "format", "keep_index"),
    cube_validate = c("x", "strict"),
    download_nc = c("dataset_id", "vars", "lon", "lat", "time", "depth",
                    "outdir", "fmt", "overwrite", "skip_existing",
                    "dry_run", "filename", "verbose"),
    layer_mean = c("x", "depth"),
    layer_integral = c("x", "depth"),
    depth_sample = c("x", "depth", "method"),
    depth_gradient = c("x", "method"),
    depth_feature = c("x", "polarity", "support"),
    transition_layer = c("x", "diagnostic", "variable", "support"),
    oxygen_boundary = c("x", "threshold", "threshold_unit", "variable", "support"),
    mixed_layer_depth = c("x", "method", "variable", "reference_depth_m",
                          "threshold", "support"),
    thermodynamic_state = c("x", "salinity", "temperature", "pressure",
                            "reference_pressure_dbar"),
    stratification = c("x", "metric", "support"),
    link_events = c("x", "events", "lon_col", "lat_col", "date_col",
                    "depth_col", "vars", "prefix", "time_tolerance",
                    "keep_grid"),
    ocean_cube = c("lon", "lat", "time", "data", "depth", "vars",
                   "units", "source", "dataset_id", "spatial_extent",
                   "temporal_extent", "depth_extent", "mask", "dc",
                   "climatology", "anomaly", "provenance", "qa"),
    read_nc = c("file", "vars", "lon_name", "lat_name", "depth_name",
                "time_name", "source", "dataset_id"),
    signal_noise = c("x", "clim", "signed"),
    stock_mask = c("x", "stock", "lat", "dc", "depth"),
    to_month = c("x", "fun"),
    viz.map = c("x", "variable", "time", "depth", "limits", "na.rm",
                "coastline", "title", "subtitle", "caption"),
    viz.profile = c("x", "variable", "longitude", "latitude", "time",
                    "depth", "limits", "na.rm", "reverse_depth", "points",
                    "title", "subtitle", "caption"),
    viz.section = c("x", "variable", "section", "time", "longitude",
                    "latitude", "depth", "limits", "na.rm",
                    "reverse_depth", "title", "subtitle", "caption"),
    viz.timeseries = c("x", "variable", "longitude", "latitude", "depth",
                       "time_from", "time_to", "match", "tolerance",
                       "limits", "na.rm", "points", "title", "subtitle",
                       "caption"),
    viz.transect = c("x", "path", "variable", "time", "depth", "lon_col",
                     "lat_col", "id_col", "match", "tolerance", "mode",
                     "distance", "limits", "na.rm", "reverse_depth",
                     "points", "title", "subtitle", "caption")
  )

  exports <- getNamespaceExports("oceancube")
  expect_setequal(exports, names(expected))
  for (name in names(expected)) {
    expect_identical(names(formals(getExportedValue("oceancube", name))),
                     expected[[name]], info = name)
  }
})

test_that("materialized derivative objects serialize with metadata intact", {
  x <- .make_baseline_fixture()$cube
  x$provenance <- list(provider = "release-test", request = "offline")
  collected <- cube_collect(x)
  cropped <- cube_crop(x, longitude = c(-80, -79), variable = "oxygen")
  sliced <- cube_slice(x, longitude = -79, latitude = -11, by = "value")

  for (object in list(collected, cropped, sliced)) {
    restored <- unserialize(serialize(object, NULL))
    expect_identical(restored, object)
    expect_s3_class(restored, "ocean_cube")
    expect_identical(.cube_backend(restored), "memory")
  }
  expect_identical(collected$provenance, x$provenance)
  expect_identical(cropped$provenance$extensions$user, x$provenance)
  expect_identical(cropped$provenance$history[[1L]]$operation, "cube_crop")
})

test_that("mask and polygon-weight products are self-contained and serializable", {
  skip_if_not_installed("sf")
  x <- geometry_test_cube()
  polygon <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(-0.5, -0.5), c(0.5, -0.5), c(0.5, 0.5),
    c(-0.5, 0.5), c(-0.5, -0.5)
  ))), crs = 4326)
  masked <- cube_mask(x, polygon)
  weights <- cube_polygon_weights(x, polygon)

  masked_restored <- unserialize(serialize(masked, NULL))
  weights_restored <- unserialize(serialize(weights, NULL))
  expect_identical(masked_restored, masked)
  expect_identical(masked_restored$mask, masked$mask)
  expect_s3_class(masked_restored$mask, "ocean_mask")
  expect_identical(weights_restored, weights)
  expect_null(attr(weights_restored, "cube"))
})

test_that("public transformations do not mutate their input cube", {
  x <- .make_baseline_fixture()$cube
  before <- serialize(x, NULL)

  invisible(cube_collect(x))
  invisible(cube_slice(x, longitude = 2L, by = "index"))
  invisible(cube_crop(x, longitude = c(-80, -79)))
  invisible(cube_extract(x, longitude = -79, latitude = -11, by = "value"))
  invisible(suppressWarnings(clim_month(x)))
  invisible(to_month(x))

  expect_identical(serialize(x, NULL), before)
})
