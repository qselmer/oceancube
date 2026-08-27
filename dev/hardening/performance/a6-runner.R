#!/usr/bin/env Rscript

options(warn = 1)
root <- normalizePath(".", winslash = "/", mustWork = TRUE)
performance_dir <- file.path(root, "dev", "hardening", "performance")
artifact_dir <- file.path(root, "artifacts", "a6")
run_dir <- file.path(artifact_dir, "run")
fixture_dir <- file.path(artifact_dir, "fixtures")
library_dir <- file.path(artifact_dir, "library")
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(library_dir, recursive = TRUE, showWarnings = FALSE)

required_head <- "9613ab09c73434e034a6a16a405eb887773e5e34"
git <- function(...) trimws(system2("git", c(...), stdout = TRUE, stderr = TRUE))
head <- git("rev-parse", "HEAD")
runtime_diff <- system2(
  "git", c("diff", "--quiet", required_head, "--", "R", "NAMESPACE", "DESCRIPTION")
)
if (!identical(runtime_diff, 0L)) {
  stop("A6 requires R/, NAMESPACE and DESCRIPTION to match A5b ", required_head)
}
status <- git("status", "--porcelain")
outside_performance <- status[!grepl("dev/hardening/performance/", status, fixed = TRUE)]
if (length(outside_performance)) {
  stop("A6 runner requires a clean tree outside dev/hardening/performance")
}

rscript <- file.path(R.home("bin"), "x64", "Rscript.exe")
if (!file.exists(rscript)) rscript <- file.path(R.home("bin"), "Rscript.exe")
r_exe <- file.path(R.home("bin"), "R.exe")
worker <- file.path(performance_dir, "a6-worker.R")
monitor <- file.path(performance_dir, "a6-monitor.ps1")

install_log <- file.path(artifact_dir, "install.log")
install_status <- system2(
  r_exe,
  c("CMD", "INSTALL", "--no-multiarch", "--with-keep.source",
    paste0("--library=", shQuote(library_dir)), shQuote(root)),
  stdout = install_log, stderr = install_log
)
if (!identical(install_status, 0L)) stop("A6 isolated package installation failed")

tiers <- data.frame(
  tier = c("TINY", "SMALL", "MEDIUM"),
  longitude = c(6L, 12L, 36L),
  latitude = c(4L, 8L, 24L),
  depth = c(3L, 4L, 10L),
  time = c(24L, 48L, 120L),
  variable = c(2L, 2L, 3L),
  replicates = c(3L, 3L, 2L),
  stringsAsFactors = FALSE
)
tiers$logical_values <- with(tiers, longitude * latitude * depth * time * variable)
tiers$input_bytes <- tiers$logical_values * 8

make_netcdf_fixture <- function(row, file) {
  if (!requireNamespace("ncdf4", quietly = TRUE)) stop("ncdf4 is required")
  lon <- seq(-84, -75, length.out = row$longitude)
  lat <- seq(-18, -6, length.out = row$latitude)
  depth <- seq(0, 500, length.out = row$depth)
  time <- seq(0, by = 30, length.out = row$time)
  lon_dim <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  lat_dim <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  depth_dim <- ncdf4::ncdim_def("depth", "m", depth)
  time_dim <- ncdf4::ncdim_def("time", "days since 2000-01-01 00:00:00", time)
  definitions <- lapply(seq_len(row$variable), function(variable) {
    ncdf4::ncvar_def(
      paste0("variable_", variable), "1",
      list(lon_dim, lat_dim, depth_dim, time_dim),
      missval = -9999, prec = "double", compression = 4
    )
  })
  nc <- ncdf4::nc_create(file, definitions, force_v4 = TRUE)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncdf4::ncatt_put(nc, "longitude", "axis", "X")
  ncdf4::ncatt_put(nc, "latitude", "axis", "Y")
  ncdf4::ncatt_put(nc, "depth", "axis", "Z")
  ncdf4::ncatt_put(nc, "depth", "positive", "down")
  ncdf4::ncatt_put(nc, "time", "axis", "T")
  ncdf4::ncatt_put(nc, "time", "calendar", "proleptic_gregorian")
  shape <- c(row$longitude, row$latitude, row$depth, row$time)
  n <- prod(as.double(shape))
  for (variable in seq_len(row$variable)) {
    index <- seq_len(n)
    values <- ((index %% 1009L) / 50) + sin(index %% 37L) +
      ((index %/% max(1L, row$longitude * row$latitude * row$depth)) %% 12L) / 3 +
      variable / 10
    values[index %% 97L == 0L] <- NA_real_
    ncdf4::ncvar_put(nc, definitions[[variable]], array(values, dim = shape))
    rm(index, values)
    invisible(gc())
  }
  invisible(file)
}

fixture_files <- stats::setNames(
  file.path(fixture_dir, paste0("a6-", tolower(tiers$tier), ".nc")),
  tiers$tier
)
for (index in seq_len(nrow(tiers))) {
  if (file.exists(fixture_files[[tiers$tier[[index]]]])) {
    unlink(fixture_files[[tiers$tier[[index]]]])
  }
  make_netcdf_fixture(tiers[index, ], fixture_files[[tiers$tier[[index]]]])
}

scenario_definitions <- data.frame(
  scenario = c(
    "memory_input_control", "memory_anomaly_control",
    "memory_cube_aggregate_time", "memory_cube_climatology",
    "memory_anomaly_difference", "memory_anomaly_standardized", "memory_cube_trend",
    "netcdf_package_control", "netcdf_deferred_control", "netcdf_cube_open",
    "netcdf_read_nc", "netcdf_cube_collect", "netcdf_slice", "netcdf_crop",
    "netcdf_extract", "netcdf_transect"
  ),
  backend = c(rep("memory", 7L), rep("netcdf", 9L)),
  operation_class = c(
    "control", "control", rep("materializing", 5L),
    "control", "control", "descriptor", "materializing", "materializing",
    rep("bounded", 4L)
  ),
  control_scenario = c(
    NA, NA, "memory_input_control", "memory_input_control",
    "memory_anomaly_control", "memory_anomaly_control", "memory_input_control",
    NA, NA, "netcdf_package_control", "netcdf_package_control",
    "netcdf_deferred_control", "netcdf_deferred_control", "netcdf_deferred_control",
    "netcdf_deferred_control", "netcdf_deferred_control"
  ),
  stringsAsFactors = FALSE
)

scenario_rows <- do.call(rbind, lapply(seq_len(nrow(tiers)), function(index) {
  tier <- tiers[index, ]
  do.call(rbind, lapply(seq_len(nrow(scenario_definitions)), function(case) {
    data.frame(
      scenario_definitions[case, , drop = FALSE], tier = tier$tier,
      replicate = seq_len(tier$replicates),
      logical_values = tier$logical_values,
      input_bytes = tier$input_bytes,
      stringsAsFactors = FALSE, row.names = NULL
    )
  }))
}))

calibration_rows <- expand.grid(
  scenario = c("calibration_control", "calibration_allocate_128"),
  replicate = seq_len(3L), stringsAsFactors = FALSE
)
calibration_rows$tier <- "CALIBRATION"
calibration_rows$fixture <- "-"

oisst_file <- file.path(
  root, "tests", "testthat", "fixtures", "real-data",
  "noaa-oisst21-surface-time-fv1.nc"
)
oisst_rows <- data.frame(
  scenario = c(
    "oisst_package_control", "oisst_deferred_control", "oisst_cube_open",
    "oisst_crop", "oisst_extract", "oisst_collect"
  ),
  tier = "OISST", replicate = 1L, fixture = oisst_file,
  stringsAsFactors = FALSE
)

run_worker <- function(scenario, tier, replicate, fixture) {
  token <- paste(scenario, tolower(tier), replicate, sep = "-")
  args_file <- file.path(run_dir, paste0(token, "-args.txt"))
  result_file <- file.path(run_dir, paste0(token, "-result.csv"))
  rprofmem_file <- file.path(run_dir, paste0(token, "-rprofmem.out"))
  stress_file <- file.path(run_dir, paste0(token, "-stress.csv"))
  monitor_file <- file.path(run_dir, paste0(token, "-monitor.csv"))
  stdout_file <- file.path(run_dir, paste0(token, "-stdout.txt"))
  stderr_file <- file.path(run_dir, paste0(token, "-stderr.txt"))
  resume <- identical(Sys.getenv("OCEANCUBE_A6_RESUME"), "1")
  forced <- strsplit(Sys.getenv("OCEANCUBE_A6_FORCE_SCENARIO"), ",", fixed = TRUE)[[1L]]
  force_scenario <- scenario %in% forced[nzchar(forced)]
  stress_ready <- !identical(scenario, "netcdf_stress") || file.exists(stress_file)
  if (resume && !force_scenario && file.exists(result_file) && file.exists(monitor_file) && stress_ready) {
    result <- read.csv(result_file, stringsAsFactors = FALSE, check.names = FALSE)
    monitored <- read.csv(monitor_file, stringsAsFactors = FALSE, check.names = FALSE)
    return(cbind(result, monitored))
  }
  writeLines(
    c(scenario, tier, replicate, library_dir, fixture, result_file,
      rprofmem_file, stress_file),
    args_file, useBytes = TRUE
  )
  status <- system2(
    "pwsh.exe",
    c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(monitor),
      "-Rscript", shQuote(rscript), "-Worker", shQuote(worker),
      "-ArgumentsFile", shQuote(args_file), "-MonitorOutput", shQuote(monitor_file),
      "-Stdout", shQuote(stdout_file), "-Stderr", shQuote(stderr_file))
  )
  if (!identical(status, 0L) || !file.exists(result_file) || !file.exists(monitor_file)) {
    error_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
    stop("A6 worker failed for ", token, ": ", error_text)
  }
  result <- read.csv(result_file, stringsAsFactors = FALSE, check.names = FALSE)
  monitored <- read.csv(monitor_file, stringsAsFactors = FALSE, check.names = FALSE)
  cbind(result, monitored)
}

message("A6 calibration: 3 isolated control/allocation pairs")
calibration <- do.call(rbind, lapply(seq_len(nrow(calibration_rows)), function(index) {
  row <- calibration_rows[index, ]
  run_worker(row$scenario, row$tier, row$replicate, row$fixture)
}))
control_median <- median(calibration$peak_rss_bytes[calibration$scenario == "calibration_control"])
allocation_median <- median(calibration$peak_rss_bytes[calibration$scenario == "calibration_allocate_128"])
calibration_increment <- allocation_median - control_median
calibration_ratio <- calibration_increment / (128 * 1024^2)
if (!is.finite(calibration_ratio) || calibration_ratio < 0.70 || calibration_ratio > 1.50) {
  stop("STOP A1-009 CLOSURE: OS peak monitor calibration ratio = ", calibration_ratio)
}

scientific_inventory <- transform(
  scenario_rows,
  fixture = ifelse(backend == "netcdf", paste0("generated:", tolower(tier)), "in-process deterministic"),
  process_isolation = "fresh Rscript --vanilla",
  primary_metric = "OS peak process RSS",
  replicate_note = ifelse(
    tier == "MEDIUM",
    "2 replicates: contract minimum used because exact monthly climatology is the dominant resource case",
    "3 replicates"
  )
)
calibration_inventory <- data.frame(
  scenario = calibration_rows$scenario, backend = "process",
  operation_class = ifelse(calibration_rows$scenario == "calibration_control", "control", "calibration"),
  control_scenario = ifelse(calibration_rows$scenario == "calibration_control", NA, "calibration_control"),
  tier = calibration_rows$tier, replicate = calibration_rows$replicate,
  logical_values = 0, input_bytes = ifelse(
    calibration_rows$scenario == "calibration_allocate_128", 128 * 1024^2, 0
  ),
  fixture = "none", process_isolation = "fresh Rscript --vanilla",
  primary_metric = "OS peak process RSS", replicate_note = "3 calibration pairs",
  stringsAsFactors = FALSE
)
oisst_inventory <- data.frame(
  scenario = oisst_rows$scenario, backend = "netcdf",
  operation_class = c("control", "control", "descriptor", "bounded", "bounded", "materializing"),
  control_scenario = c(NA, NA, "oisst_package_control", "oisst_deferred_control",
                       "oisst_deferred_control", "oisst_deferred_control"),
  tier = "OISST", replicate = 1L, logical_values = 36L * 48L * 1L * 4L * 4L,
  input_bytes = 36L * 48L * 1L * 4L * 4L * 8L,
  fixture = "governed offline OISST fv1", process_isolation = "fresh Rscript --vanilla",
  primary_metric = "OS peak process RSS", replicate_note = "single regression smoke",
  stringsAsFactors = FALSE
)
stress_inventory <- data.frame(
  scenario = "netcdf_stress", backend = "netcdf", operation_class = "stress",
  control_scenario = NA, tier = "SMALL", replicate = 1L,
  logical_values = tiers$logical_values[tiers$tier == "SMALL"],
  input_bytes = tiers$input_bytes[tiers$tier == "SMALL"], fixture = "generated:small",
  process_isolation = "fresh Rscript --vanilla", primary_metric = "OS peak process RSS",
  replicate_note = "75 operations plus 50 descriptor/error cycles", stringsAsFactors = FALSE
)
large_inventory <- data.frame(
  scenario = "large_local_opt_in", backend = "memory/netcdf", operation_class = "opt-in",
  control_scenario = NA, tier = "LARGE-LOCAL", replicate = 0L,
  logical_values = 100915200, input_bytes = 807321600, fixture = "not generated",
  process_isolation = "not run", primary_metric = "OS peak process RSS",
  replicate_note = "SKIPPED: OCEANCUBE_RUN_LARGE_STRESS=1 was not set; not required for closure",
  stringsAsFactors = FALSE
)
write.csv(
  rbind(scientific_inventory, calibration_inventory, oisst_inventory,
        stress_inventory, large_inventory),
  file.path(performance_dir, "a6-scenarios.csv"), row.names = FALSE, na = ""
)

message("A6 scientific measurements: ", nrow(scenario_rows), " isolated workers")
results <- do.call(rbind, lapply(seq_len(nrow(scenario_rows)), function(index) {
  row <- scenario_rows[index, ]
  fixture <- if (identical(row$backend, "netcdf")) fixture_files[[row$tier]] else "-"
  message(sprintf("  %03d/%03d %s %s rep %d", index, nrow(scenario_rows),
                  row$scenario, row$tier, row$replicate))
  run_worker(row$scenario, row$tier, row$replicate, fixture)
}))

control_lookup <- scenario_definitions$control_scenario
names(control_lookup) <- scenario_definitions$scenario
results$control_scenario <- unname(control_lookup[results$scenario])
results$control_peak_rss_bytes <- NA_real_
for (index in seq_len(nrow(results))) {
  control <- results$control_scenario[[index]]
  if (is.na(control) || !nzchar(control)) next
  selected <- results$scenario == control & results$tier == results$tier[[index]] &
    results$replicate == results$replicate[[index]]
  if (sum(selected) != 1L) stop("Matched control lookup failed")
  results$control_peak_rss_bytes[[index]] <- results$peak_rss_bytes[selected]
}
results$incremental_peak_estimate_bytes <- pmax(
  0, results$peak_rss_bytes - results$control_peak_rss_bytes
)
results$incremental_peak_estimate_bytes[is.na(results$control_peak_rss_bytes)] <- NA_real_
results$input_peak_ratio <- results$incremental_peak_estimate_bytes / results$input_bytes
write.csv(results, file.path(performance_dir, "a6-peak-rss-results.csv"), row.names = FALSE, na = "")

operations <- scenario_definitions$scenario[scenario_definitions$operation_class != "control"]
summary_rows <- do.call(rbind, lapply(operations, function(operation) {
  do.call(rbind, lapply(tiers$tier, function(tier) {
    selected <- results[results$scenario == operation & results$tier == tier, ]
    data.frame(
      scenario = operation,
      tier = tier,
      replicates = nrow(selected),
      peak_rss_min_bytes = min(selected$peak_rss_bytes),
      peak_rss_median_bytes = median(selected$peak_rss_bytes),
      peak_rss_max_bytes = max(selected$peak_rss_bytes),
      incremental_peak_min_bytes = min(selected$incremental_peak_estimate_bytes),
      incremental_peak_median_bytes = median(selected$incremental_peak_estimate_bytes),
      incremental_peak_max_bytes = max(selected$incremental_peak_estimate_bytes),
      elapsed_median_seconds = median(selected$elapsed_seconds),
      output_bytes_median = median(selected$output_bytes),
      rprofmem_total_median_bytes = median(selected$rprofmem_total_bytes),
      physical_values_read_median = median(selected$physical_values_read, na.rm = TRUE),
      logical_values_selected_median = median(selected$logical_values_selected, na.rm = TRUE),
      read_amplification_median = median(selected$read_amplification, na.rm = TRUE),
      correctness_replicates_identical = length(unique(selected$correctness_signature)) == 1L,
      stringsAsFactors = FALSE
    )
  }))
}))
for (column in c("physical_values_read_median", "logical_values_selected_median", "read_amplification_median")) {
  summary_rows[[column]][is.nan(summary_rows[[column]])] <- NA_real_
}

scaling <- do.call(rbind, lapply(operations, function(operation) {
  rows <- summary_rows[summary_rows$scenario == operation, ]
  small <- rows[rows$tier == "SMALL", ]
  medium <- rows[rows$tier == "MEDIUM", ]
  value_ratio <- tiers$logical_values[tiers$tier == "MEDIUM"] /
    tiers$logical_values[tiers$tier == "SMALL"]
  small_peak <- small$incremental_peak_median_bytes
  medium_peak <- medium$incremental_peak_median_bytes
  alpha <- if (small_peak > 0 && medium_peak > 0) {
    log(medium_peak / small_peak) / log(value_ratio)
  } else {
    NA_real_
  }
  class <- scenario_definitions$operation_class[scenario_definitions$scenario == operation]
  classification <- if (!isTRUE(medium$correctness_replicates_identical)) {
    "RED"
  } else if (!is.finite(alpha)) {
    "AMBER"
  } else if (class == "bounded" && alpha <= 0.50) {
    "GREEN"
  } else if (class != "bounded" && alpha <= 1.35) {
    "GREEN"
  } else if (alpha <= 1.75) {
    "AMBER"
  } else {
    "RED"
  }
  data.frame(
    scenario = operation, operation_class = class,
    small_logical_values = tiers$logical_values[tiers$tier == "SMALL"],
    medium_logical_values = tiers$logical_values[tiers$tier == "MEDIUM"],
    logical_value_ratio = value_ratio,
    small_incremental_peak_median_bytes = small_peak,
    medium_incremental_peak_median_bytes = medium_peak,
    incremental_peak_ratio = if (small_peak > 0) medium_peak / small_peak else NA_real_,
    empirical_alpha = alpha,
    classification = classification,
    interpretation = if (classification == "AMBER") {
      "incremental estimate is near the monitor noise floor or scaling is uncertain; documented, not hidden"
    } else if (classification == "RED") {
      "apparently pathological scaling requires a separate remediation milestone"
    } else {
      "observed bounded/linear behavior within the A6 evidence policy"
    },
    stringsAsFactors = FALSE
  )
}))
write.csv(
  merge(summary_rows, scaling, by = "scenario", all.x = TRUE, sort = FALSE),
  file.path(performance_dir, "a6-scaling-results.csv"), row.names = FALSE, na = ""
)

message("A6 OISST offline smoke")
oisst_results <- do.call(rbind, lapply(seq_len(nrow(oisst_rows)), function(index) {
  row <- oisst_rows[index, ]
  run_worker(row$scenario, row$tier, row$replicate, row$fixture)
}))
write.csv(oisst_results, file.path(run_dir, "a6-oisst-results.csv"), row.names = FALSE, na = "")

attach_controls <- function(frame, lookup, requested_bytes = frame$input_bytes) {
  frame$control_scenario <- unname(lookup[frame$scenario])
  frame$control_peak_rss_bytes <- NA_real_
  for (index in seq_len(nrow(frame))) {
    control <- frame$control_scenario[[index]]
    if (is.na(control) || !nzchar(control)) next
    selected <- frame$scenario == control & frame$tier == frame$tier[[index]] &
      frame$replicate == frame$replicate[[index]]
    if (sum(selected) != 1L) stop("Supplementary matched control lookup failed")
    frame$control_peak_rss_bytes[[index]] <- frame$peak_rss_bytes[selected]
  }
  frame$incremental_peak_estimate_bytes <- pmax(
    0, frame$peak_rss_bytes - frame$control_peak_rss_bytes
  )
  frame$incremental_peak_estimate_bytes[is.na(frame$control_peak_rss_bytes)] <- NA_real_
  frame$input_peak_ratio <- frame$incremental_peak_estimate_bytes / requested_bytes
  frame
}
calibration_lookup <- c(
  calibration_control = NA_character_, calibration_allocate_128 = "calibration_control"
)
calibration_bytes <- ifelse(
  calibration$scenario == "calibration_allocate_128", 128 * 1024^2, NA_real_
)
calibration <- attach_controls(calibration, calibration_lookup, calibration_bytes)
oisst_lookup <- c(
  oisst_package_control = NA_character_, oisst_deferred_control = NA_character_,
  oisst_cube_open = "oisst_package_control", oisst_crop = "oisst_deferred_control",
  oisst_extract = "oisst_deferred_control", oisst_collect = "oisst_deferred_control"
)
oisst_results <- attach_controls(oisst_results, oisst_lookup)
write.csv(
  rbind(results, calibration, oisst_results),
  file.path(performance_dir, "a6-peak-rss-results.csv"), row.names = FALSE, na = ""
)

message("A6 repeated backend stress: 75 cycles")
stress_result <- run_worker("netcdf_stress", "SMALL", 1L, fixture_files[["SMALL"]])
stress_raw <- read.csv(file.path(run_dir, "netcdf_stress-small-1-stress.csv"), stringsAsFactors = FALSE)
stress_summary <- data.frame(
  scenario = "netcdf_stress",
  tier = "SMALL",
  cycles = 75L,
  rss_initial_bytes = stress_raw$current_rss_bytes[stress_raw$iteration == 0L],
  rss_final_bytes = stress_raw$current_rss_bytes[stress_raw$iteration == 75L],
  rss_max_checkpoint_bytes = max(stress_raw$current_rss_bytes),
  rss_growth_bytes = stress_raw$current_rss_bytes[stress_raw$iteration == 75L] -
    stress_raw$current_rss_bytes[stress_raw$iteration == 0L],
  connection_delta = stress_raw$connection_delta[[1L]],
  peak_rss_bytes = stress_result$peak_rss_bytes,
  file_rename_probe = TRUE,
  descriptor_error_cycles = stress_raw$descriptor_cycles[[1L]],
  descriptor_error_status = if (stress_raw$descriptor_expected_errors[[1L]] == 50L) "PASS" else "FAIL",
  classification = if (stress_raw$connection_delta[[1L]] == 0L &&
    stress_raw$descriptor_expected_errors[[1L]] == 50L) "GREEN" else "RED",
  stringsAsFactors = FALSE
)
stress_columns <- setdiff(names(stress_summary), c("scenario", "tier", "connection_delta"))
stress_output <- cbind(
  stress_raw,
  stress_summary[rep(1L, nrow(stress_raw)), stress_columns, drop = FALSE]
)
write.csv(
  stress_output, file.path(performance_dir, "a6-stress-results.csv"),
  row.names = FALSE, na = ""
)

ram_query <- "[int64][GC]::GetGCMemoryInfo().TotalAvailableMemoryBytes"
ram <- suppressWarnings(as.numeric(tail(system2(
  "pwsh.exe", c("-NoProfile", "-Command", shQuote(ram_query)),
  stdout = TRUE, stderr = FALSE
), 1L)))
if (!length(ram)) ram <- NA_real_
large_requested <- identical(Sys.getenv("OCEANCUBE_RUN_LARGE_STRESS"), "1")
environment <- data.frame(
  field = c(
    "git_sha", "R_version", "platform", "os", "architecture", "cpu_logical",
    "cpu_identifier", "physical_ram_bytes", "primary_metric", "monitor_calibration",
    "calibration_requested_bytes", "calibration_increment_bytes", "calibration_ratio",
    "fresh_process", "small_tiny_replicates", "medium_replicates", "large_local",
    "fixture_policy", "network", "measurement_date_utc"
  ),
  value = c(
    head, R.version.string, R.version$platform, Sys.info()[["sysname"]], R.version$arch,
    Sys.getenv("NUMBER_OF_PROCESSORS", unset = NA_character_),
    Sys.getenv("PROCESSOR_IDENTIFIER", unset = NA_character_), ram,
    "System.Diagnostics.Process.PeakWorkingSet64 from live x64 Rscript process",
    "PASS", 128 * 1024^2, calibration_increment, calibration_ratio,
    "Rscript --vanilla; exactly one scenario per worker", "3", "2",
    if (large_requested) "REQUESTED (run separately; not required for closure)" else
      "SKIPPED: explicit OCEANCUBE_RUN_LARGE_STRESS=1 opt-in absent; not required for closure",
    "deterministic memory fixture plus pre-built local NetCDF outside measured operation",
    "offline", format(Sys.time(), tz = "UTC", usetz = TRUE)
  ),
  stringsAsFactors = FALSE
)
write.csv(environment, file.path(performance_dir, "a6-environment.csv"), row.names = FALSE, na = "")

if (any(scaling$classification == "RED") || stress_summary$classification == "RED") {
  stop("A6 measurement completed with an unresolved RED classification")
}
message("A6 measurement completed; calibration ratio = ", format(calibration_ratio, digits = 4))
