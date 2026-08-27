args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 8L) {
  stop(paste(
    "Usage: a6-worker.R SCENARIO TIER REP PACKAGE_LIB FIXTURE RESULT",
    "RPROFMEM STRESS_RESULT"
  ))
}

scenario <- args[[1L]]
tier_name <- args[[2L]]
replicate_id <- as.integer(args[[3L]])
package_lib <- args[[4L]]
fixture_file <- args[[5L]]
result_file <- args[[6L]]
rprofmem_file <- args[[7L]]
stress_file <- args[[8L]]

options(warn = 1, oceancube.a6 = TRUE)
suppressPackageStartupMessages(library(oceancube, lib.loc = package_lib))

tiers <- list(
  TINY = c(longitude = 6L, latitude = 4L, depth = 3L, time = 24L, variable = 2L),
  SMALL = c(longitude = 12L, latitude = 8L, depth = 4L, time = 48L, variable = 2L),
  MEDIUM = c(longitude = 36L, latitude = 24L, depth = 10L, time = 120L, variable = 3L),
  `LARGE-LOCAL` = c(longitude = 72L, latitude = 48L, depth = 20L, time = 365L, variable = 4L),
  OISST = c(longitude = 36L, latitude = 48L, depth = 1L, time = 4L, variable = 4L),
  CALIBRATION = c(longitude = 0L, latitude = 0L, depth = 0L, time = 0L, variable = 0L)
)
if (!tier_name %in% names(tiers)) stop("Unknown tier: ", tier_name)
tier <- tiers[[tier_name]]

make_values <- function(shape) {
  n <- prod(as.double(shape))
  index <- seq_len(n)
  values <- ((index %% 1009L) / 50) + sin(index %% 37L) +
    ((index %/% max(1L, shape[[1L]] * shape[[2L]] * shape[[3L]])) %% 12L) / 3
  values[index %% 97L == 0L] <- NA_real_
  array(values, dim = unname(shape))
}

make_memory_cube <- function(shape) {
  variables <- paste0("variable_", seq_len(shape[[5L]]))
  ocean_cube(
    lon = seq(-84, -75, length.out = shape[[1L]]),
    lat = seq(-18, -6, length.out = shape[[2L]]),
    depth = seq(0, 500, length.out = shape[[3L]]),
    time = seq.Date(as.Date("2000-01-01"), by = "month", length.out = shape[[4L]]),
    data = make_values(shape),
    vars = variables,
    units = stats::setNames(rep("1", shape[[5L]]), variables),
    source = "A6 deterministic synthetic fixture",
    dataset_id = paste0("a6-", tolower(tier_name))
  )
}

shape_of <- function(x) {
  if (inherits(x, "ocean_cube")) {
    return(paste(length(x$lon), length(x$lat), length(x$depth),
                 length(x$time), length(x$vars), sep = "x"))
  }
  if (is.data.frame(x)) return(paste0(nrow(x), "x", ncol(x)))
  if (is.array(x)) return(paste(dim(x), collapse = "x"))
  as.character(length(x))
}

values_of <- function(x) {
  if (inherits(x, "ocean_cube") && !is.null(x$data)) return(as.vector(x$data))
  if (is.data.frame(x) && "value" %in% names(x)) return(x$value)
  if (is.numeric(x)) return(as.vector(x))
  numeric()
}

signature_of <- function(x) {
  values <- values_of(x)
  finite <- values[is.finite(values)]
  paste(
    shape_of(x),
    length(values),
    length(finite),
    sum(is.na(values)),
    format(sum(finite), digits = 15, scientific = TRUE),
    format(if (length(finite)) mean(finite) else NA_real_, digits = 15, scientific = TRUE),
    sep = ":"
  )
}

read_metrics_of <- function(x) {
  metrics <- NULL
  if (inherits(x, "ocean_cube")) {
    metrics <- x$qa$selection$netcdf_read
    if (is.null(metrics)) metrics <- x$qa$crop$netcdf_read
  } else if (is.data.frame(x)) {
    qa <- attr(x, "oceancube_qa", exact = TRUE)
    metrics <- qa$extraction$netcdf_read
    if (is.null(metrics)) metrics <- qa$transect$netcdf_read
    if (is.null(metrics)) metrics <- qa$transect$backend
    if (is.null(metrics)) metrics <- qa$transect$physical_reads
  }
  if (is.null(metrics)) {
    return(c(logical = NA_real_, physical = NA_real_, amplification = NA_real_))
  }
  logical <- metrics$values_requested
  if (is.null(logical)) logical <- metrics$logical_values
  if (is.null(logical)) logical <- metrics$n_values_requested
  physical <- metrics$values_in_envelope
  if (is.null(physical)) physical <- metrics$physical_values
  if (is.null(physical)) physical <- metrics$n_values_read
  amplification <- metrics$amplification
  if (is.null(amplification)) amplification <- metrics$read_amplification
  if (is.null(amplification) && length(logical) && length(physical) && logical > 0) {
    amplification <- physical / logical
  }
  c(logical = as.numeric(logical %||% NA_real_),
    physical = as.numeric(physical %||% NA_real_),
    amplification = as.numeric(amplification %||% NA_real_))
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

profile_summary <- function(path) {
  if (!file.exists(path)) return(c(total = NA_real_, maximum = NA_real_, count = NA_real_))
  lines <- readLines(path, warn = FALSE)
  bytes <- suppressWarnings(as.numeric(sub("^([0-9]+).*$", "\\1", lines)))
  bytes <- bytes[is.finite(bytes)]
  if (!length(bytes)) return(c(total = 0, maximum = 0, count = 0))
  c(total = sum(bytes), maximum = max(bytes), count = length(bytes))
}

current_rss <- function() {
  command <- sprintf("(Get-Process -Id %d).WorkingSet64", Sys.getpid())
  answer <- suppressWarnings(system2(
    "powershell.exe", c("-NoProfile", "-Command", shQuote(command)),
    stdout = TRUE, stderr = FALSE
  ))
  suppressWarnings(as.numeric(tail(answer, 1L)))
}

is_memory <- startsWith(scenario, "memory_")
is_netcdf <- startsWith(scenario, "netcdf_")
is_oisst <- startsWith(scenario, "oisst_")
input <- NULL
climatology <- NULL

if (is_memory) {
  input <- make_memory_cube(tier)
  if (scenario %in% c("memory_anomaly_control", "memory_anomaly_difference",
                      "memory_anomaly_standardized")) {
    climatology <- suppressWarnings(cube_climatology(input, by = "month"))
  }
}
if (is_netcdf && scenario %in% c(
  "netcdf_deferred_control", "netcdf_cube_collect", "netcdf_slice",
  "netcdf_crop", "netcdf_extract", "netcdf_transect", "netcdf_stress"
)) {
  input <- cube_open(fixture_file)
}
if (is_oisst && scenario %in% c(
  "oisst_deferred_control", "oisst_crop", "oisst_extract", "oisst_collect"
)) {
  input <- cube_open(fixture_file, depth_name = "zlev")
}

gc(full = TRUE)
invisible(gc())

operation <- function() {
  switch(
    scenario,
    calibration_control = invisible(TRUE),
    calibration_allocate_128 = {
      allocation <- raw(128L * 1024L * 1024L)
      for (position in seq.int(1L, length(allocation), by = 4096L)) {
        allocation[[position]] <- as.raw(165L)
      }
      allocation
    },
    memory_input_control = input,
    memory_anomaly_control = climatology,
    memory_cube_aggregate_time = suppressWarnings(
      cube_aggregate_time(input, by = "year", method = "mean")
    ),
    memory_cube_climatology = suppressWarnings(
      cube_climatology(input, by = "month")
    ),
    memory_anomaly_difference = cube_anomaly(input, climatology, type = "difference"),
    memory_anomaly_standardized = cube_anomaly(input, climatology, type = "z"),
    memory_cube_trend = cube_trend(input, time_unit = "year"),
    netcdf_package_control = invisible(TRUE),
    netcdf_deferred_control = input,
    netcdf_cube_open = cube_open(fixture_file),
    netcdf_read_nc = read_nc(fixture_file),
    netcdf_cube_collect = cube_collect(input),
    netcdf_slice = cube_slice(
      input,
      longitude = unique(as.integer(round(seq(1, tier[[1L]], length.out = min(6L, tier[[1L]]))))),
      latitude = unique(as.integer(round(seq(1, tier[[2L]], length.out = min(5L, tier[[2L]]))))),
      depth = unique(as.integer(round(seq(1, tier[[3L]], length.out = min(3L, tier[[3L]]))))),
      time = unique(as.integer(round(seq(1, tier[[4L]], length.out = min(8L, tier[[4L]]))))),
      variable = 1L,
      by = "index"
    ),
    netcdf_crop = cube_crop(
      input,
      longitude = input$lon[c(max(1L, floor(tier[[1L]] * 0.35)), max(1L, ceiling(tier[[1L]] * 0.65)))],
      latitude = input$lat[c(max(1L, floor(tier[[2L]] * 0.35)), max(1L, ceiling(tier[[2L]] * 0.65)))],
      depth = input$depth[c(1L, max(1L, ceiling(tier[[3L]] / 2)))],
      time = input$time[c(max(1L, floor(tier[[4L]] * 0.35)), max(1L, ceiling(tier[[4L]] * 0.65)))],
      variable = input$vars[[1L]]
    ),
    netcdf_extract = cube_extract(
      input,
      longitude = unique(as.integer(round(seq(1, tier[[1L]], length.out = min(6L, tier[[1L]]))))),
      latitude = unique(as.integer(round(seq(1, tier[[2L]], length.out = min(5L, tier[[2L]]))))),
      depth = unique(as.integer(round(seq(1, tier[[3L]], length.out = min(3L, tier[[3L]]))))),
      time = unique(as.integer(round(seq(1, tier[[4L]], length.out = min(8L, tier[[4L]]))))),
      variable = 1L, by = "index", mode = "table", format = "long"
    ),
    netcdf_transect = {
      n_point <- min(12L, tier[[1L]], tier[[2L]])
      path <- data.frame(
        longitude = unique(as.integer(round(seq(1, tier[[1L]], length.out = n_point)))),
        latitude = unique(as.integer(round(seq(1, tier[[2L]], length.out = n_point))))
      )
      n_point <- min(nrow(path), length(path$latitude))
      path <- path[seq_len(n_point), , drop = FALSE]
      cube_transect(
        input, path, depth = seq_len(min(4L, tier[[3L]])),
        time = max(1L, ceiling(tier[[4L]] / 2)), variable = 1L,
        by = "index", mode = if (min(4L, tier[[3L]]) > 1L) "section" else "horizontal"
      )
    },
    oisst_package_control = invisible(TRUE),
    oisst_deferred_control = input,
    oisst_cube_open = cube_open(fixture_file, depth_name = "zlev"),
    oisst_crop = cube_crop(
      input,
      longitude = input$lon[c(8L, 20L)], latitude = input$lat[c(10L, 30L)],
      time = input$time[c(1L, 3L)], variable = "sst"
    ),
    oisst_extract = cube_extract(
      input, longitude = c(8L, 20L), latitude = c(10L, 30L),
      depth = 1L, time = c(1L, 3L), variable = 1L,
      by = "index", mode = "table"
    ),
    oisst_collect = cube_collect(input),
    netcdf_stress = {
      checkpoints <- c(0L, 1L, 5L, 10L, 20L, 30L, 40L, 50L, 60L, 75L)
      stress <- data.frame(
        scenario = scenario, tier = tier_name, iteration = checkpoints,
        current_rss_bytes = NA_real_, stringsAsFactors = FALSE
      )
      stress$current_rss_bytes[[1L]] <- current_rss()
      connection_start <- nrow(showConnections(all = TRUE))
      for (iteration in seq_len(75L)) {
        if (iteration %% 3L == 1L) {
          value <- cube_crop(
            input, longitude = input$lon[c(3L, 9L)],
            latitude = input$lat[c(2L, 7L)], time = input$time[c(2L, 12L)],
            variable = input$vars[[1L]]
          )
        } else if (iteration %% 3L == 2L) {
          value <- cube_extract(
            input, longitude = c(2L, 6L, 10L), latitude = c(2L, 5L, 8L),
            depth = c(1L, 3L), time = c(2L, 8L, 12L), variable = 1L,
            by = "index", mode = "table"
          )
        } else {
          path <- data.frame(longitude = c(2L, 6L, 10L), latitude = c(2L, 5L, 8L))
          value <- cube_transect(
            input, path, depth = c(1L, 3L), time = 8L, variable = 1L,
            by = "index", mode = "section"
          )
        }
        rm(value)
        if (iteration %in% checkpoints) {
          invisible(gc())
          stress$current_rss_bytes[stress$iteration == iteration] <- current_rss()
        }
      }
      descriptor_error_count <- 0L
      for (iteration in seq_len(50L)) {
        descriptor <- cube_open(fixture_file)
        tryCatch(
          cube_slice(descriptor, longitude = tier[[1L]] + 1L, by = "index"),
          error = function(error) {
            descriptor_error_count <<- descriptor_error_count + 1L
            invisible(NULL)
          }
        )
        rm(descriptor)
        if (iteration %% 10L == 0L) invisible(gc())
      }
      connection_end <- nrow(showConnections(all = TRUE))
      stress$connection_start <- connection_start
      stress$connection_end <- connection_end
      stress$connection_delta <- connection_end - connection_start
      stress$descriptor_cycles <- 50L
      stress$descriptor_expected_errors <- descriptor_error_count
      write.csv(stress, stress_file, row.names = FALSE, na = "")
      invisible(gc())
      renamed <- paste0(fixture_file, ".a6-move-probe")
      rename_out <- file.rename(fixture_file, renamed)
      rename_back <- if (rename_out) file.rename(renamed, fixture_file) else FALSE
      if (!rename_out || !rename_back) stop("NetCDF file-handle rename probe failed")
      list(stress = stress, rename_probe = TRUE)
    },
    stop("Unknown scenario: ", scenario)
  )
}

if (file.exists(rprofmem_file)) unlink(rprofmem_file)
utils::Rprofmem(rprofmem_file)
start <- proc.time()[["elapsed"]]
result <- operation()
elapsed <- proc.time()[["elapsed"]] - start
utils::Rprofmem(NULL)

profile <- profile_summary(rprofmem_file)
metrics <- read_metrics_of(result)
object_bytes <- as.numeric(object.size(result))
logical_values <- prod(as.double(tier))
if (identical(tier_name, "CALIBRATION")) logical_values <- 0

record <- data.frame(
  scenario = scenario,
  tier = tier_name,
  replicate = replicate_id,
  logical_values = logical_values,
  input_bytes = logical_values * 8,
  elapsed_seconds = elapsed,
  output_bytes = object_bytes,
  rprofmem_total_bytes = profile[["total"]],
  rprofmem_max_allocation_bytes = profile[["maximum"]],
  rprofmem_allocation_count = profile[["count"]],
  physical_values_read = metrics[["physical"]],
  logical_values_selected = metrics[["logical"]],
  read_amplification = metrics[["amplification"]],
  correctness_signature = signature_of(result),
  status = "PASS",
  stringsAsFactors = FALSE
)
write.csv(record, result_file, row.names = FALSE, na = "")
