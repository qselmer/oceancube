test_that("B3 publishes a versioned deterministic supported-subset contract", {
  file <- make_cf_b3_fixture()
  withr::local_file(file)
  first <- .cf_scan_netcdf(file)
  second <- .cf_scan_netcdf(file)
  summary <- first$interpretation$supported_subset

  expect_identical(first$interpretation, second$interpretation)
  expect_identical(first$diagnostics, second$diagnostics)
  expect_identical(summary, second$interpretation$supported_subset)
  expect_identical(summary$reference, "CF-1.13")
  expect_identical(summary$definition_version, "1.0.0")
  expect_identical(summary$validator_version, "1.0.0")
  expect_identical(summary$validation_scope, "oceancube_supported_subset")
  expect_identical(summary$status, "PASS")
  expect_identical(summary$fail, 0L)
  expect_identical(
    first$interpretation$contract$public_claim,
    "CF-aware; supports a documented subset of CF 1.13"
  )
  expect_true(all(c(
    "Conventions", "dimension_structure", "coordinate_variables",
    "auxiliary_coordinates", "bounds", "climatology",
    "ancillary_variables", "cell_measures", "grid_mapping_simple",
    "grid_mapping_extended", "formula_terms", "cell_methods",
    "standard_name", "units", "fill_and_missing", "valid_ranges",
    "flags", "groups_and_paths"
  ) %in% names(first$interpretation$contract$constructs)))
})

test_that("diagnostics use the bounded plain-R B3 schema", {
  file <- make_cf_b3_fixture()
  withr::local_file(file)
  cf <- .cf_scan_netcdf(file)
  fields <- c(
    "code", "severity", "status", "scope", "source_id", "attribute",
    "rule_id", "rule_kind", "message", "cf_section",
    "blocking_for_current_cube", "requires_data_values"
  )

  expect_true(all(vapply(cf$diagnostics, function(x) {
    all(fields %in% names(x))
  }, logical(1L))))
  expect_setequal(unique(vapply(cf$diagnostics, `[[`, "", "severity")),
                  c("INFO", "DEFERRED"))
  expect_true(all(vapply(cf$diagnostics, `[[`, "", "status") %in%
                    c("PASS", "FAIL", "DEFERRED", "NOT_APPLICABLE")))
  expect_true(all(vapply(cf$diagnostics, `[[`, "", "rule_kind") %in%
                    c("REQUIREMENT", "RECOMMENDATION", "OCEANCUBE-SAFETY")))
  expect_false(.cf_contains_forbidden(cf$diagnostics))
  expect_identical(cf, unserialize(serialize(cf, NULL)))

  malformed <- cf
  malformed$diagnostics[[1L]]$severity <- "FATAL"
  expect_error(
    .cf_validate_cf(malformed, require_current = FALSE),
    "diagnostic record"
  )
})

test_that("the valid CF-rich fixture exercises the supported structural subset", {
  file <- make_cf_b3_fixture()
  withr::local_file(file)
  cf <- .cf_scan_netcdf(file)

  expect_length(cf_b3_failures(cf), 0L)
  expect_identical(
    cf$interpretation$coordinates$map$lon$classification,
    "DIMENSION_COORDINATE"
  )
  expect_identical(
    cf$interpretation$coordinates$map$auxlat$classification,
    "AUXILIARY_COORDINATE"
  )
  expect_identical(
    cf$interpretation$cell_methods$map$temperature$status,
    "SIMPLE_RECOGNIZED"
  )
  expect_identical(
    cf$interpretation$standard_names$map$temperature$status,
    "PRESENT_UNVALIDATED"
  )
  expect_true(all(c(
    "CFB3_COORDINATES_DIMENSIONS", "CFB3_ANCILLARY_VARIABLES_DIMENSIONS",
    "CFB3_BOUNDS_DIMENSIONS", "CFB3_CLIMATOLOGY_STRUCTURE",
    "CFB3_CELL_MEASURE_DIMENSIONS", "CFB3_GRID_MAPPING_CONTAINER",
    "CFB3_FORMULA_SEMANTICS", "CFB3_FLAGS_STRUCTURE"
  ) %in% cf_b3_codes(cf)))
  expect_true(any(vapply(cf$diagnostics, function(x) {
    identical(x$code, "CFB3_BOUNDS_VALUE_CHECK") &&
      identical(x$status, "DEFERRED") && isTRUE(x$requires_data_values)
  }, logical(1L))))
  expect_true(any(vapply(cf$diagnostics, function(x) {
    identical(x$code, "CFB3_CELL_MEASURE_UNITS") &&
      identical(x$status, "DEFERRED")
  }, logical(1L))))
})

test_that("focused invalid fixtures produce their stable supported-rule failure", {
  expected <- c(
    coordinates_missing = "CFB3_COORDINATES_RESOLUTION",
    coordinates_dimensions = "CFB3_COORDINATES_DIMENSIONS",
    ancillary_dimensions = "CFB3_ANCILLARY_VARIABLES_DIMENSIONS",
    bounds_missing = "CFB3_BOUNDS_RESOLUTION",
    bounds_dimensions = "CFB3_BOUNDS_DIMENSIONS",
    climatology_dimensions = "CFB3_CLIMATOLOGY_STRUCTURE",
    cell_measure_keyword = "CFB3_CELL_MEASURE_KEYWORD",
    cell_measure_missing = "CFB3_CELL_MEASURES_RESOLUTION",
    grid_mapping_missing = "CFB3_GRID_MAPPING_RESOLUTION",
    grid_mapping_name_missing = "CFB3_GRID_MAPPING_CONTAINER",
    formula_missing = "CFB3_FORMULA_TERMS_RESOLUTION",
    axis_conflict = "CFB3_AXIS_EVIDENCE",
    valid_range_conflict = "CFB3_VALID_RANGE_EXCLUSIVE"
  )
  for (case in names(expected)) {
    file <- make_cf_b3_fixture(case)
    withr::local_file(file, .local_envir = environment())
    cf <- .cf_scan_netcdf(file)
    failures <- cf_b3_failures(cf)
    expect_length(failures, 1L)
    expect_identical(failures[[1L]]$code, unname(expected[[case]]))
    expect_identical(cf$interpretation$supported_subset$status, "FAIL")
  }
})

test_that("supported-subset validation performs no scientific payload reads", {
  file <- make_cf_b3_fixture()
  withr::local_file(file)
  reads <- 0L
  local_mocked_bindings(
    ncvar_get = function(...) {
      reads <<- reads + 1L
      stop("B3 validator attempted a payload read")
    },
    .package = "ncdf4"
  )

  cf <- .cf_scan_netcdf(file)
  expect_identical(reads, 0L)
  expect_identical(cf$interpretation$supported_subset$status, "PASS")
})

test_that("real fixtures separate metadata interpretation from cube support", {
  fixture <- function(name) test_path("fixtures", "real-data", name)
  oisst_file <- fixture("noaa-oisst21-surface-time-fv1.nc")
  etopo_file <- fixture("noaa-etopo2022-bathymetry-fv1.nc")
  woa_file <- fixture("noaa-woa23-vertical-fv1.nc")
  oisst <- .cf_scan_netcdf(oisst_file)
  etopo <- .cf_scan_netcdf(etopo_file)
  woa <- .cf_scan_netcdf(woa_file)

  expect_identical(oisst$interpretation$supported_subset$status, "PASS")
  expect_identical(etopo$interpretation$supported_subset$status, "PASS")
  expect_identical(
    etopo$interpretation$supported_subset$declaration_status,
    "NOT_DECLARED"
  )
  expect_identical(woa$interpretation$supported_subset$status, "PASS")
  expect_identical(
    woa$interpretation$cell_methods$map$t_an$status,
    "COMPLEX_DEFERRED"
  )
  expect_identical(
    .cf_attribute_value(woa$source$variables$map$t_an$attributes, "cell_methods"),
    "area: mean depth: mean time: mean within years time: mean over years"
  )
  expect_length(cf_b3_failures(oisst), 0L)
  expect_length(cf_b3_failures(etopo), 0L)
  expect_length(cf_b3_failures(woa), 0L)
  expect_error(read_nc(etopo_file), "Could not identify time", fixed = TRUE)
  expect_error(cube_open(etopo_file), "Cannot resolve the time dimension")
  expect_error(read_nc(woa_file, vars = c("t_an", "s_an")),
               "Time coordinate units must match", fixed = TRUE)
  expect_error(cube_open(woa_file, vars = c("t_an", "s_an")),
               "Time coordinate units must match", fixed = TRUE)
})

test_that("eager deferred and collected cubes preserve B3 source semantics", {
  file <- test_path(
    "fixtures", "real-data", "noaa-oisst21-surface-time-fv1.nc"
  )
  variables <- c("sst", "anom", "err", "ice")
  eager <- read_nc(file, vars = variables)
  deferred <- cube_open(file, vars = variables)
  collected <- cube_collect(deferred)
  source_diagnostics <- function(x) x$metadata$cf$diagnostics[vapply(
    x$metadata$cf$diagnostics, function(item) identical(item$scope, "SOURCE"),
    logical(1L)
  )]

  expect_identical(eager$metadata$cf$source, deferred$metadata$cf$source)
  expect_identical(
    eager$metadata$cf$interpretation$supported_subset,
    deferred$metadata$cf$interpretation$supported_subset
  )
  expect_identical(
    eager$metadata$cf$interpretation$contract,
    deferred$metadata$cf$interpretation$contract
  )
  expect_identical(source_diagnostics(eager), source_diagnostics(deferred))
  expect_identical(deferred$metadata, collected$metadata)
  expect_length(cf_b3_failures(eager$metadata$cf), 0L)
  expect_length(cf_b3_failures(deferred$metadata$cf), 0L)
  expect_identical(as.vector(is.na(eager$data)), as.vector(is.na(collected$data)))
  expect_equal(as.vector(eager$data), as.vector(collected$data), tolerance = 0)
})

test_that("current validation is separate and derivations remain pending", {
  file <- make_netcdf_backend_fixture()
  withr::local_file(file)
  source <- cube_open(file, vars = c("temperature", "oxygen"))
  selected <- cube_slice(source, variable = 1L, by = "index")
  aggregated <- cube_aggregate_time(source, by = "day")

  expect_identical(
    source$metadata$cf$interpretation$current$status,
    "CURRENT_SUPPORTED_SUBSET"
  )
  expect_true(any(vapply(source$metadata$cf$diagnostics, function(x) {
    identical(x$scope, "CURRENT") && identical(x$status, "PASS")
  }, logical(1L))))
  expect_identical(
    selected$metadata$cf$interpretation$current$status,
    "CURRENT_SELECTION"
  )
  expect_identical(
    selected$metadata$cf$interpretation$current$variables,
    "temperature"
  )
  expect_identical(
    aggregated$metadata$cf$interpretation$current$status,
    "DERIVATION_PENDING"
  )
  expect_identical(
    source$metadata$cf$interpretation$supported_subset,
    aggregated$metadata$cf$interpretation$supported_subset
  )
  expect_identical(
    source$metadata$cf$source,
    aggregated$metadata$cf$source
  )
})

test_that("B3 metadata remains serializable and path-private", {
  file <- make_cf_b3_fixture()
  withr::local_file(file)
  cf <- .cf_scan_netcdf(file)
  rds <- tempfile(fileext = ".rds")
  withr::local_file(rds)
  saveRDS(cf, rds)
  restored <- readRDS(rds)
  text <- capture.output(str(cf, max.level = 8L))

  expect_identical(cf, restored)
  expect_identical(cf$interpretation, restored$interpretation)
  expect_identical(cf$diagnostics, restored$diagnostics)
  expect_false(any(grepl(normalizePath(file, winslash = "/"), text, fixed = TRUE)))
  expect_false(any(grepl(Sys.info()[["nodename"]], text, fixed = TRUE)))
})
