#!/usr/bin/env Rscript

# oceancube 0.3.0-A1 bounded hardening baseline.
#
# This maintainer script writes only small CSV evidence under dev/hardening.
# It does not change package code, tests, dependencies, or public API.

options(stringsAsFactors = FALSE)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else NA_character_
repo_root <- if (!is.na(script_file)) {
  normalizePath(file.path(dirname(script_file), "..", ".."), winslash = "/")
} else {
  normalizePath(".", winslash = "/")
}
output_dir <- file.path(repo_root, "dev", "hardening")

write_evidence <- function(x, name) {
  utils::write.csv(
    x,
    file.path(output_dir, name),
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
}

collapse_or <- function(x, fallback = "NONE") {
  x <- unique(x[nzchar(x)])
  if (length(x)) paste(x, collapse = ";") else fallback
}

contains <- function(text, pattern) grepl(pattern, text, ignore.case = TRUE, perl = TRUE)

# Test inventory ------------------------------------------------------------

namespace_lines <- readLines(file.path(repo_root, "NAMESPACE"), warn = FALSE)
exports <- sub(
  '^export\\((?:"([^\"]+)"|([^\\)]+))\\)$',
  "\\1\\2",
  grep("^export\\(", namespace_lines, value = TRUE)
)
exports <- trimws(exports)

subsystem_patterns <- list(
  construction = "ocean_cube",
  validation = "cube_validate",
  inspection = "cube_inspect",
  `memory backend` = "\\.cube_(read|write|backend)|backend.?memory",
  `NetCDF backend` = "netcdf|ncdf4|\\.new_netcdf|\\.ncvar_get",
  selection = "selection|selector|\\.resolve_.*index",
  crop = "cube_crop",
  slice = "cube_slice",
  extract = "cube_extract",
  transect = "cube_transect",
  mask = "cube_mask",
  geometry = "cube_cell_area|cube_cell_volume|cube_layer_thickness|cube_polygon_weights",
  visualization = "viz\\.(map|profile|section|timeseries|transect)",
  `time/calendar` = "calendar|timezone|to_month|\\.canonicalize_time|\\.cf_time",
  aggregation = "cube_aggregate_time",
  climatology = "cube_climatology|clim_day|clim_month",
  anomaly = "cube_anomaly|anom_diff|anom_z",
  signal_noise = "signal_noise",
  trend = "cube_trend",
  `download/connection` = "download_nc|cm_connect|cm_setup",
  `legacy helpers` = "annual_index|stock_mask|crop_stock|coast_dist|layer_mean|link_events"
)

test_files <- sort(list.files(
  file.path(repo_root, "tests", "testthat"),
  pattern = "^test-.*\\.R$",
  full.names = TRUE
))

inventory_rows <- lapply(test_files, function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  text <- paste(lines, collapse = "\n")
  case_hits <- gregexpr('test_that\\s*\\(\\s*"[^"]+"', text, perl = TRUE)[[1L]]
  case_count <- if (identical(case_hits, -1L)) 0L else length(case_hits)
  expectation_hits <- gregexpr("\\bexpect_[A-Za-z0-9_]+\\s*\\(", text, perl = TRUE)[[1L]]
  expectation_calls <- if (identical(expectation_hits, -1L)) 0L else length(expectation_hits)
  used_exports <- exports[vapply(
    exports,
    function(fun) contains(text, paste0("(?<![A-Za-z0-9_.])", gsub("\\.", "\\\\.", fun), "\\s*\\(")),
    logical(1L)
  )]
  subsystems <- names(subsystem_patterns)[vapply(
    subsystem_patterns,
    function(pattern) contains(text, pattern),
    logical(1L)
  )]
  has_netcdf <- contains(text, "netcdf|ncdf4|\\.new_netcdf|\\.ncvar_get")
  has_memory <- contains(text, "memory|ocean_cube\\s*\\(|\\.make_baseline_fixture")
  edge <- contains(text, "expect_(error|warning)|empty|singleton|duplicate|unsorted|irregular|non.?finite|NA_|NaN|Inf|zero|negative|boundary|tolerance")
  property <- contains(text, "quickcheck|hedgehog|set\\.seed|runif|rnorm|sample\\s*\\(")
  invariance <- contains(text, "preserv|unchanged|does not mutate|remain(s)? (identical|unchanged)|serialize\\(.*before|canonical axis order")
  performance <- contains(text, "system\\.time|microbenchmark|bench::|Rprof|elapsed|allocation")
  stress <- contains(text, "stress|large[-_ ]local|very large|1e[6-9]")
  real_data <- contains(text, "Copernicus|CMEMS|NOAA|GEBCO|ERA5|satellite|real[-_ ]data")
  parity <- has_netcdf && has_memory && contains(text, "equivalent|identical|matches memory|equal memory|from_memory|memory_result")

  taxonomy <- character()
  if (!has_netcdf && !real_data && !performance && !stress) taxonomy <- c(taxonomy, "UNIT")
  if (contains(text, "contract|preserv|class\\(|provenance|units|metadata|canonical|shape")) taxonomy <- c(taxonomy, "CONTRACT")
  if (contains(text, "regression|baseline|compatib|legacy|historical")) taxonomy <- c(taxonomy, "REGRESSION")
  if (edge) taxonomy <- c(taxonomy, "EDGE")
  if (parity) taxonomy <- c(taxonomy, "BACKEND-PARITY")
  if (has_netcdf || contains(text, "ggplot|sf::|do.call\\(|readRDS|saveRDS")) taxonomy <- c(taxonomy, "INTEGRATION")
  if (real_data) taxonomy <- c(taxonomy, "REAL-DATA")
  if (property) taxonomy <- c(taxonomy, "PROPERTY")
  if (invariance) taxonomy <- c(taxonomy, "INVARIANCE")
  if (performance) taxonomy <- c(taxonomy, "PERFORMANCE")
  if (stress) taxonomy <- c(taxonomy, "STRESS")

  data.frame(
    test_file = file.path("tests", "testthat", basename(path)),
    subsystem = collapse_or(subsystems, "unclassified"),
    public_function = collapse_or(used_exports),
    backend = if (has_netcdf && has_memory) "memory;netcdf" else if (has_netcdf) "netcdf" else "memory/not-applicable",
    contract_type = collapse_or(taxonomy),
    edge_case = edge,
    memory_test = contains(text, "object\\.size|estimated_bytes|memory allocation|mem_used|Rprofmem"),
    netcdf_test = has_netcdf,
    real_data_test = real_data,
    property_test = property,
    invariance_test = invariance,
    performance_test = performance,
    case_count = case_count,
    expectation_calls_static = expectation_calls,
    assertion_families = collapse_or(unique(sub("\\s*\\($", "", regmatches(text, gregexpr("expect_[A-Za-z0-9_]+\\s*\\(", text, perl = TRUE))[[1L]]))),
    stringsAsFactors = FALSE
  )
})
test_inventory <- do.call(rbind, inventory_rows)
write_evidence(test_inventory, "test-inventory.csv")

taxonomy_names <- c(
  "UNIT", "CONTRACT", "REGRESSION", "EDGE", "BACKEND-PARITY",
  "INTEGRATION", "REAL-DATA", "PROPERTY", "INVARIANCE", "PERFORMANCE", "STRESS"
)
taxonomy_summary <- data.frame(
  taxonomy = taxonomy_names,
  files = vapply(taxonomy_names, function(x) sum(grepl(paste0("(^|;)", x, "(;|$)"), test_inventory$contract_type)), integer(1L)),
  cases_in_files = vapply(taxonomy_names, function(x) sum(test_inventory$case_count[grepl(paste0("(^|;)", x, "(;|$)"), test_inventory$contract_type)]), integer(1L)),
  note = "Non-exclusive file-level classification from test bodies and assertions",
  stringsAsFactors = FALSE
)
write_evidence(taxonomy_summary, "test-taxonomy-summary.csv")

# Coverage import -----------------------------------------------------------

coverage_root <- Sys.getenv("OCEANCUBE_A1_COVERAGE_ROOT", unset = "")
coverage_overall <- NA_real_
function_count <- NA_integer_
function_any_coverage <- NA_real_
untested_exports <- character()
critical_low_helpers <- character()
if (nzchar(coverage_root)) {
  coverage_files <- utils::read.csv(file.path(coverage_root, "coverage-by-file.csv"), check.names = FALSE)
  coverage_files$class <- cut(
    coverage_files$coverage_percent,
    breaks = c(-Inf, 70, 80, 90, Inf),
    labels = c("priority-audit", "weak", "acceptable-review", "strong"),
    right = FALSE
  )
  write_evidence(coverage_files, "coverage-by-file.csv")
  coverage_functions <- utils::read.csv(file.path(coverage_root, "coverage-by-function.csv"), check.names = FALSE)
  coverage_functions$class <- cut(
    coverage_functions$coverage_percent,
    breaks = c(-Inf, 70, 80, 90, Inf),
    labels = c("priority-audit", "weak", "acceptable-review", "strong"),
    right = FALSE
  )
  write_evidence(coverage_functions, "coverage-by-function.csv")
  function_count <- nrow(coverage_functions)
  function_any_coverage <- 100 * mean(coverage_functions$coverage_percent > 0)
  export_coverage <- coverage_functions[coverage_functions$function_name %in% exports, , drop = FALSE]
  untested_exports <- export_coverage$function_name[export_coverage$coverage_percent == 0]
  critical_low_helpers <- coverage_functions$function_name[
    grepl("backend|read|block|index|time|range|bounds|validate", coverage_functions$function_name, ignore.case = TRUE) &
      coverage_functions$coverage_percent < 70
  ]
  coverage_rds <- readRDS(file.path(coverage_root, "coverage.rds"))
  coverage_overall <- as.numeric(covr::percent_coverage(coverage_rds))
}

# Bounded timing and allocation baseline -----------------------------------

if (!requireNamespace("devtools", quietly = TRUE)) stop("devtools is required for this maintainer baseline")
devtools::load_all(repo_root, quiet = TRUE)

set.seed(303001L)

make_tier <- function(tier) {
  dims <- switch(
    tier,
    TINY = c(longitude = 6L, latitude = 4L, depth = 3L, time = 24L, variable = 2L),
    SMALL = c(longitude = 12L, latitude = 8L, depth = 4L, time = 48L, variable = 2L),
    stop("Unknown tier")
  )
  lon <- seq(-82, -76, length.out = dims[["longitude"]])
  lat <- seq(-15, -8, length.out = dims[["latitude"]])
  depth <- seq(0, 75, length.out = dims[["depth"]])
  time <- seq(as.Date("2019-01-01"), by = "month", length.out = dims[["time"]])
  vars <- c("temperature", "oxygen")
  n <- prod(dims)
  time_index <- rep(seq_len(dims[["time"]]), each = prod(dims[1:3]), times = dims[["variable"]])
  values <- array(seq_len(n) / n + time_index * 0.25, dim = unname(dims))
  ocean_cube(
    lon = lon,
    lat = lat,
    depth = depth,
    time = time,
    data = values,
    vars = vars,
    units = c(temperature = "degC", oxygen = "mmol m-3"),
    source = "deterministic synthetic A1",
    dataset_id = tolower(tier),
    provenance = list(source_identity = paste0("synthetic-a1-", tolower(tier)))
  )
}

allocation_bytes <- function(fun) {
  if (!isTRUE(capabilities("profmem"))) return(NA_real_)
  path <- tempfile("oceancube-a1-profmem-", fileext = ".out")
  on.exit(unlink(path), add = TRUE)
  Rprofmem(path)
  on.exit(Rprofmem(NULL), add = TRUE)
  invisible(fun())
  Rprofmem(NULL)
  lines <- readLines(path, warn = FALSE)
  values <- suppressWarnings(as.numeric(sub(" .*", "", lines)))
  sum(values, na.rm = TRUE)
}

measure_case <- function(tier, operation, cube, fun, iterations = 3L) {
  timings <- numeric(iterations)
  result <- NULL
  for (i in seq_len(iterations)) {
    gc()
    timings[[i]] <- system.time(result <- suppressWarnings(fun()))[["elapsed"]]
  }
  allocated <- allocation_bytes(function() suppressWarnings(fun()))
  input_bytes <- as.numeric(object.size(cube))
  output_bytes <- as.numeric(object.size(result))
  data.frame(
    tier = tier,
    backend = "memory",
    operation = operation,
    iterations = iterations,
    input_cells = prod(dim(cube$data)),
    input_bytes = input_bytes,
    median_elapsed_seconds = stats::median(timings),
    min_elapsed_seconds = min(timings),
    allocated_bytes = allocated,
    allocation_input_ratio = allocated / input_bytes,
    output_bytes = output_bytes,
    peak_bytes = NA_real_,
    seed = 303001L,
    stringsAsFactors = FALSE
  )
}

benchmark_rows <- list()
for (tier in c("TINY", "SMALL")) {
  cube <- make_tier(tier)
  climatology <- suppressWarnings(cube_climatology(cube, "month"))
  lon <- cube$lon
  lat <- cube$lat
  path <- data.frame(
    longitude = lon[c(1L, ceiling(length(lon) / 2), length(lon))],
    latitude = lat[c(1L, ceiling(length(lat) / 2), length(lat))]
  )
  depth_bounds <- structure(
    c(
      0,
      (cube$depth[-length(cube$depth)] + cube$depth[-1L]) / 2,
      cube$depth[[length(cube$depth)]] +
        (cube$depth[[length(cube$depth)]] - cube$depth[[length(cube$depth) - 1L]]) / 2
    ),
    units = "m"
  )
  operations <- list(
    cube_validate = function() cube_validate(cube),
    cube_slice = function() cube_slice(
      cube,
      longitude = seq_len(ceiling(length(lon) / 2)),
      latitude = seq_len(ceiling(length(lat) / 2)),
      time = seq_len(ceiling(length(cube$time) / 2)),
      variable = 1L,
      by = "index"
    ),
    cube_crop = function() cube_crop(
      cube,
      longitude = range(lon[2:(length(lon) - 1L)]),
      latitude = range(lat[2:(length(lat) - 1L)]),
      time = range(cube$time[seq_len(ceiling(length(cube$time) / 2))]),
      variable = "temperature"
    ),
    cube_extract = function() cube_extract(
      cube,
      longitude = lon[[2L]], latitude = lat[[2L]], depth = cube$depth[[1L]],
      time = cube$time[[1L]], variable = "temperature"
    ),
    cube_transect = function() cube_transect(
      cube, path, depth = cube$depth, time = cube$time[[1L]],
      variable = "temperature", match = "exact", mode = "section"
    ),
    cube_collect = function() cube_collect(cube),
    cube_aggregate_time = function() cube_aggregate_time(cube, "year"),
    cube_climatology = function() cube_climatology(cube, "month"),
    cube_anomaly = function() cube_anomaly(cube, climatology),
    cube_trend = function() cube_trend(cube),
    cube_cell_area = function() cube_cell_area(cube),
    cube_cell_volume = function() cube_cell_volume(cube, depth_bounds = depth_bounds)
  )
  for (operation in names(operations)) {
    benchmark_rows[[length(benchmark_rows) + 1L]] <- measure_case(
      tier, operation, cube, operations[[operation]]
    )
  }
}
benchmarks <- do.call(rbind, benchmark_rows)

cell_ratio <- with(benchmarks, input_cells[tier == "SMALL"][[1L]] / input_cells[tier == "TINY"][[1L]])
benchmarks$empirical_scaling <- NA_character_
for (operation in unique(benchmarks$operation)) {
  rows <- which(benchmarks$operation == operation)
  tiny_time <- benchmarks$median_elapsed_seconds[rows[benchmarks$tier[rows] == "TINY"]]
  small_time <- benchmarks$median_elapsed_seconds[rows[benchmarks$tier[rows] == "SMALL"]]
  label <- if (!length(tiny_time) || !length(small_time) || tiny_time <= 0) {
    "unknown"
  } else if (small_time / tiny_time < 2) {
    "approximately constant over tested range"
  } else if (small_time / tiny_time <= 2.5 * cell_ratio) {
    "approximately linear over tested range"
  } else {
    "super-linear signal over tested range; repeat before inference"
  }
  benchmarks$empirical_scaling[rows] <- label
}
write_evidence(benchmarks, "hardening-baseline.csv")

# Provenance field audit ----------------------------------------------------

cube <- make_tier("TINY")
climatology <- suppressWarnings(cube_climatology(cube, "month"))
objects <- list(
  construction = cube,
  selection = cube_slice(cube, longitude = 1:3, by = "index"),
  crop = cube_crop(cube, longitude = range(cube$lon[2:5])),
  extract = cube_extract(
    cube, longitude = cube$lon[[1L]], latitude = cube$lat[[1L]],
    depth = cube$depth[[1L]], time = cube$time[[1L]], variable = cube$vars[[1L]]
  ),
  aggregation = suppressWarnings(cube_aggregate_time(cube, "year")),
  climatology = climatology,
  anomaly = cube_anomaly(cube, climatology),
  trend = cube_trend(cube),
  collect = cube_collect(cube)
)

get_provenance <- function(name, object) {
  if (name == "extract") attr(object, "oceancube_provenance", exact = TRUE) else object$provenance
}
recursive_names <- function(x) {
  if (!is.list(x)) return(character())
  unique(c(names(x), unlist(lapply(x, recursive_names), use.names = FALSE)))
}

candidate_fields <- list(
  schema_version = "schema_version",
  operation_recorded = "operation|function_name",
  parameters = "parameters|arguments|selectors|by|method",
  `parent/source` = "parent|source|source_provenance",
  source_identity = "source_identity|dataset_id|file_identity",
  package_version = "package_version",
  backend = "backend|source_backend|backend_from",
  timestamp = "timestamp|date|time_recorded",
  scientific_method = "scientific_method|method|sd_method|weighting"
)

provenance_rows <- lapply(names(objects), function(name) {
  provenance <- get_provenance(name, objects[[name]])
  fields <- recursive_names(provenance)
  present <- vapply(candidate_fields, function(pattern) any(grepl(paste0("^(", pattern, ")$"), fields)), logical(1L))
  data.frame(
    operation = name,
    top_level_fields = collapse_or(names(provenance)),
    nested_fields = collapse_or(fields),
    as.list(present),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
})
provenance_audit <- do.call(rbind, provenance_rows)
write_evidence(provenance_audit, "provenance-audit.csv")

# Reproducibility summary ---------------------------------------------------

git_sha <- tryCatch(
  system2("git", c("-C", shQuote(repo_root), "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[[1L]],
  error = function(e) NA_character_
)
description <- read.dcf(file.path(repo_root, "DESCRIPTION"))
dependency_names <- unique(c(
  names(tools:::.split_dependencies(description[1L, "Imports"])),
  names(tools:::.split_dependencies(description[1L, "Suggests"]))
))
dependency_versions <- vapply(dependency_names, function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) as.character(utils::packageVersion(pkg)) else "not-installed"
}, character(1L))

summary_rows <- data.frame(
  category = c(
    "test-inventory", "test-inventory", "test-inventory",
    "coverage", "coverage", "coverage",
    rep("coverage-gap", length(untested_exports)),
    rep("critical-helper-gap", length(critical_low_helpers)),
    "environment", "environment", "environment", "environment", "environment"
  ),
  metric = c(
    "test_files", "test_that_cases", "static_expectation_calls",
    "overall_line_coverage", "functions_total", "functions_with_any_coverage",
    untested_exports,
    critical_low_helpers,
    "commit", "R_version", "platform", "CPU", "dependency_versions"
  ),
  value = c(
    nrow(test_inventory), sum(test_inventory$case_count), sum(test_inventory$expectation_calls_static),
    coverage_overall, function_count, function_any_coverage,
    rep(0, length(untested_exports)),
    rep("below-70-percent", length(critical_low_helpers)),
    git_sha, R.version.string, R.version$platform,
    Sys.getenv("PROCESSOR_IDENTIFIER", unset = Sys.info()[["machine"]]),
    paste(paste(names(dependency_versions), dependency_versions, sep = "="), collapse = ";")
  ),
  unit = c(
    "files", "cases", "static calls", "percent", "functions", "percent",
    rep("percent", length(untested_exports)),
    rep("classification", length(critical_low_helpers)),
    "SHA", "text", "text", "text", "versions"
  ),
  status = c(
    "BASELINE", "BASELINE", "BASELINE",
    if (coverage_overall >= 90) "STRONG" else "REVIEW",
    "BASELINE", if (function_any_coverage >= 90) "STRONG" else "REVIEW",
    rep("TEST-GAP", length(untested_exports)),
    rep("TEST-GAP", length(critical_low_helpers)),
    rep("RECORDED", 5L)
  ),
  notes = "A1 local bounded baseline; percentages do not imply contract completeness",
  stringsAsFactors = FALSE
)
write_evidence(summary_rows, "baseline-summary.csv")

cat("HARDENING_BASELINE: PASS\n")
cat("test_files=", nrow(test_inventory), " test_cases=", sum(test_inventory$case_count), "\n", sep = "")
cat("coverage=", sprintf("%.4f", coverage_overall), "\n", sep = "")
cat("benchmark_rows=", nrow(benchmarks), "\n", sep = "")
