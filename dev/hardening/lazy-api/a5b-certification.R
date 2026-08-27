options(warn = 1)
Sys.setlocale("LC_ALL", "English_United States.utf8")

devtools::load_all(".", quiet = TRUE)
source("tests/testthat/helper-netcdf-backend.R", local = globalenv())

elapsed_average <- function(fun, n = 5L) {
  unname(system.time(for (i in seq_len(n)) fun())[["elapsed"]]) / n
}

run_smoke <- function(fixture, file, vars, depth_name = NULL) {
  deferred <- cube_open(file, vars = vars, depth_name = depth_name)
  longitude_index <- seq_len(min(2L, length(deferred$lon)))
  latitude_index <- seq_len(min(2L, length(deferred$lat)))
  collected <- cube_collect(deferred)

  data.frame(
    fixture = fixture,
    cube_open_seconds = elapsed_average(function() {
      cube_open(file, vars = vars, depth_name = depth_name)
    }),
    small_crop_seconds = elapsed_average(function() {
      cube_crop(
        deferred,
        longitude = range(deferred$lon[longitude_index]),
        latitude = range(deferred$lat[latitude_index]),
        variable = deferred$vars[[1L]]
      )
    }),
    small_extract_seconds = elapsed_average(function() {
      cube_extract(
        deferred,
        longitude = longitude_index,
        latitude = latitude_index,
        depth = 1L,
        time = 1L,
        variable = 1L,
        by = "index"
      )
    }),
    cube_collect_seconds = elapsed_average(function() cube_collect(deferred)),
    descriptor_object_bytes = as.numeric(object.size(deferred)),
    collected_cube_bytes = as.numeric(object.size(collected)),
    stringsAsFactors = FALSE
  )
}

synthetic <- make_netcdf_backend_fixture()
on.exit(unlink(synthetic), add = TRUE)
oisst <- file.path(
  "tests", "testthat", "fixtures", "real-data",
  "noaa-oisst21-surface-time-fv1.nc"
)

results <- rbind(
  run_smoke(
    "synthetic-packed-permuted", synthetic,
    c("temperature", "oxygen")
  ),
  run_smoke("noaa-oisst21-fv1", oisst, NULL, "zlev")
)

a5a_exports <- c(
  "annual_index", "anom_diff", "anom_z", "clim_day", "clim_month",
  "cm_connect", "cm_setup", "coast_dist", "crop_stock",
  "cube_aggregate_time", "cube_anomaly", "cube_cell_area",
  "cube_cell_volume", "cube_climatology", "cube_collect", "cube_crop",
  "cube_extract", "cube_inspect", "cube_layer_thickness", "cube_mask",
  "cube_polygon_weights", "cube_slice", "cube_transect", "cube_trend",
  "cube_validate", "download_nc", "layer_mean", "link_events", "ocean_cube",
  "read_nc", "signal_noise", "stock_mask", "to_month", "viz.map",
  "viz.profile", "viz.section", "viz.timeseries", "viz.transect"
)

stopifnot(
  identical(names(formals(cube_open)), c(
    "file", "vars", "lon_name", "lat_name", "depth_name", "time_name",
    "source", "dataset_id"
  )),
  identical(cube_open(oisst, depth_name = "zlev")$vars,
            c("sst", "anom", "err", "ice")),
  identical(length(getNamespaceExports("oceancube")), 39L),
  identical(setdiff(getNamespaceExports("oceancube"), a5a_exports),
            "cube_open")
)

print(results, row.names = FALSE)
