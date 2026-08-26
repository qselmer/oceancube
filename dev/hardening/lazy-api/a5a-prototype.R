options(warn = 1)
Sys.setlocale("LC_ALL", "English_United States.utf8")

devtools::load_all(".", quiet = TRUE)
source("tests/testthat/helper-netcdf-backend.R", local = globalenv())

elapsed_average <- function(fun, n = 20L) {
  stopifnot(is.function(fun), length(formals(fun)) == 0L)
  unname(system.time(for (i in seq_len(n)) fun())[["elapsed"]]) / n
}

same_values <- function(left, right, tolerance = 0) {
  identical(dim(left), dim(right)) &&
    identical(is.na(as.vector(left)), is.na(as.vector(right))) &&
    isTRUE(all.equal(
      as.vector(left), as.vector(right), tolerance = tolerance,
      check.attributes = FALSE
    ))
}

same_cube_science <- function(left, right, tolerance = 0) {
  identical(left$lon, right$lon) &&
    identical(left$lat, right$lat) &&
    identical(left$depth, right$depth) &&
    identical(left$time, right$time) &&
    identical(left$vars, right$vars) &&
    same_values(left$data, right$data, tolerance = tolerance)
}

run_case <- function(id, file, vars, depth_name = NULL) {
  eager_seconds <- elapsed_average(
    function() read_nc(file, vars = vars, depth_name = depth_name)
  )
  deferred_seconds <- elapsed_average(
    function() .new_netcdf_cube(.new_netcdf_storage(
      file,
      variables = vars,
      depth_name = depth_name
    ))
  )

  eager <- read_nc(file, vars = vars, depth_name = depth_name)
  deferred <- .new_netcdf_cube(.new_netcdf_storage(
    file,
    variables = vars,
    depth_name = depth_name
  ))
  collected <- cube_collect(deferred)
  roundtrip <- readRDS(local({
    path <- tempfile(fileext = ".rds")
    saveRDS(deferred, path)
    path
  }))

  lon_idx <- unique(c(1L, min(2L, length(eager$lon))))
  lat_idx <- unique(c(1L, min(2L, length(eager$lat))))
  depth_idx <- 1L
  time_idx <- unique(c(1L, min(2L, length(eager$time))))
  var_idx <- 1L

  eager_slice <- cube_slice(
    eager, longitude = lon_idx, latitude = lat_idx, depth = depth_idx,
    time = time_idx, variable = var_idx, by = "index"
  )
  deferred_slice <- cube_slice(
    deferred, longitude = lon_idx, latitude = lat_idx, depth = depth_idx,
    time = time_idx, variable = var_idx, by = "index"
  )
  eager_crop <- cube_crop(
    eager,
    longitude = range(eager$lon[lon_idx]),
    latitude = range(eager$lat[lat_idx]),
    depth = range(eager$depth[depth_idx]),
    time = range(eager$time[time_idx]),
    variable = eager$vars[var_idx]
  )
  deferred_crop <- cube_crop(
    deferred,
    longitude = range(deferred$lon[lon_idx]),
    latitude = range(deferred$lat[lat_idx]),
    depth = range(deferred$depth[depth_idx]),
    time = range(deferred$time[time_idx]),
    variable = deferred$vars[var_idx]
  )
  eager_extract <- cube_extract(
    eager, longitude = lon_idx, latitude = lat_idx, depth = depth_idx,
    time = time_idx, variable = var_idx, by = "index", mode = "table",
    keep_index = TRUE
  )
  deferred_extract <- cube_extract(
    deferred, longitude = lon_idx, latitude = lat_idx, depth = depth_idx,
    time = time_idx, variable = var_idx, by = "index", mode = "table",
    keep_index = TRUE
  )
  path <- data.frame(
    station = paste0("S", seq_along(lon_idx)),
    longitude = lon_idx,
    latitude = lat_idx
  )
  eager_transect <- cube_transect(
    eager, path, id_col = "station", depth = depth_idx, time = time_idx[1L],
    variable = var_idx, by = "index", mode = "auto", keep_index = TRUE
  )
  deferred_transect <- cube_transect(
    deferred, path, id_col = "station", depth = depth_idx,
    time = time_idx[1L], variable = var_idx, by = "index", mode = "auto",
    keep_index = TRUE
  )
  eager_aggregate <- cube_aggregate_time(eager, by = "day")
  deferred_aggregate <- cube_aggregate_time(deferred, by = "day")

  crop_seconds <- elapsed_average(function() cube_crop(
      deferred,
      longitude = range(deferred$lon[lon_idx]),
      latitude = range(deferred$lat[lat_idx]),
      depth = range(deferred$depth[depth_idx]),
      time = range(deferred$time[time_idx]),
      variable = deferred$vars[var_idx]
    ))
  extract_seconds <- elapsed_average(function() cube_extract(
      deferred,
      longitude = lon_idx,
      latitude = lat_idx,
      depth = depth_idx,
      time = time_idx,
      variable = var_idx,
      by = "index",
      mode = "table",
      keep_index = TRUE
    ))
  collect_seconds <- elapsed_average(function() cube_collect(deferred))

  data.frame(
    fixture = id,
    dimensions = paste(unname(.cube_shape(deferred)), collapse = "x"),
    variables = paste(vars, collapse = ";"),
    eager_open_seconds = eager_seconds,
    deferred_open_seconds = deferred_seconds,
    deferred_object_bytes = as.numeric(object.size(deferred)),
    collected_object_bytes = as.numeric(object.size(collected)),
    full_payload_bytes = prod(.cube_shape(deferred)) * 8,
    crop_seconds = crop_seconds,
    extract_seconds = extract_seconds,
    collect_seconds = collect_seconds,
    coordinate_parity = identical(
      list(eager$lon, eager$lat, eager$depth, eager$time, eager$vars),
      list(deferred$lon, deferred$lat, deferred$depth, deferred$time,
           deferred$vars)
    ),
    full_value_parity = same_values(eager$data, collected$data),
    missingness_mismatch_count = sum(
      xor(is.na(as.vector(eager$data)), is.na(as.vector(collected$data)))
    ),
    slice_parity = same_cube_science(eager_slice, deferred_slice),
    crop_parity = same_cube_science(eager_crop, deferred_crop),
    extract_parity = isTRUE(all.equal(
      eager_extract, deferred_extract, tolerance = 0,
      check.attributes = FALSE
    )),
    transect_parity = isTRUE(all.equal(
      eager_transect, deferred_transect, tolerance = 0,
      check.attributes = FALSE
    )),
    aggregate_parity = same_cube_science(
      eager_aggregate, deferred_aggregate
    ),
    rds_parity = isTRUE(all.equal(
      .cube_read(roundtrip), .cube_read(deferred), tolerance = 0
    )),
    stringsAsFactors = FALSE
  )
}

synthetic <- make_netcdf_backend_fixture()
on.exit(unlink(synthetic), add = TRUE)
synthetic_result <- run_case(
  "synthetic-packed-permuted",
  synthetic,
  c("temperature", "oxygen")
)

oisst <- file.path(
  "tests", "testthat", "fixtures", "real-data",
  "noaa-oisst21-surface-time-fv1.nc"
)
oisst_result <- run_case(
  "noaa-oisst21-fv1",
  oisst,
  c("sst", "anom", "err", "ice"),
  depth_name = "zlev"
)

print(rbind(synthetic_result, oisst_result), row.names = FALSE)
