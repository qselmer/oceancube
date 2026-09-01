global_exit_cube <- function(provenance = NULL, posixct = FALSE) {
  time <- seq(as.Date("2018-01-01"), as.Date("2021-12-01"), by = "month")
  if (isTRUE(posixct)) {
    time <- as.POSIXct(paste(time, "12:34:56"), tz = "UTC")
  }
  month <- as.integer(format(time, "%m"))
  year <- as.integer(format(time, "%Y")) - 2018L
  values <- rep(10 + month / 10 + year / 4, each = 4L)
  ocean_cube(
    lon = c(-80, -79), lat = c(-12, -11), depth = 0,
    time = time, vars = "sst",
    data = array(values, c(2, 2, 1, length(time), 1)),
    units = c(sst = "degC"), source = "global-exit-source",
    dataset_id = "a4-exit-fixture", provenance = provenance
  )
}

global_exit_polygon <- function() {
  sf::st_sfc(sf::st_polygon(list(rbind(
    c(-80.5, -12.5), c(-78.5, -12.5), c(-78.5, -10.5),
    c(-80.5, -10.5), c(-80.5, -12.5)
  ))), crs = 4326)
}

global_exit_provenance <- function(x, location) {
  if (identical(location, "field")) x$provenance else {
    attr(x, "oceancube_provenance", exact = TRUE)
  }
}

test_that("all certified output classes expose one complete V1 contract", {
  skip_if_not_installed("sf")
  cube <- global_exit_cube()
  climatology <- suppressWarnings(clim_month(cube))
  anomaly <- anom_diff(cube, climatology)
  cube$dc <- matrix(c(1, 2, 3, 4), 2, 2)
  mask <- stock_mask(cube, stock = "certification", dc = c(0, 4))
  stock <- crop_stock(cube, mask)
  extraction <- cube_extract(
    cube, longitude = -80, latitude = -12, depth = 0,
    variable = "sst", mode = "series", match = "exact"
  )
  path <- data.frame(id = c("a", "b"), longitude = c(-80, -79),
                     latitude = c(-12, -11))
  transect <- cube_transect(
    cube, path, id_col = "id", depth = 0, time = cube$time[[1L]],
    variable = "sst", match = "exact", mode = "horizontal"
  )
  weights <- cube_polygon_weights(cube, global_exit_polygon())
  outputs <- list(
    ocean_cube = list(value = cube, location = "field"),
    stock_cube = list(value = stock, location = "field"),
    ocean_anom = list(value = anomaly, location = "field"),
    ocean_clim = list(value = climatology, location = "field"),
    extract_table = list(value = extraction, location = "attribute"),
    transect_table = list(value = transect, location = "attribute"),
    polygon_weight_table = list(value = weights, location = "attribute")
  )

  for (name in names(outputs)) {
    value <- outputs[[name]]$value
    provenance <- global_exit_provenance(value, outputs[[name]]$location)
    expect_true(.provenance_validate(provenance, strict = TRUE)$valid,
                info = name)
    expect_named(provenance, c(
      "schema_version", "source", "time", "history", "lineages", "extensions"
    ), ignore.order = FALSE, info = name)
    expect_identical(provenance$schema_version, "1.0.0", info = name)
    expect_identical(provenance$source$identity$dataset_id,
                     "a4-exit-fixture", info = name)
    software <- unlist(lapply(provenance$history, function(record) {
      paste(record$software$package, record$software$version)
    }))
    expect_true(all(software == "oceancube 0.2.0.9000"), info = name)
    restored <- unserialize(serialize(value, NULL))
    expect_identical(
      .provenance_semantic(global_exit_provenance(restored, outputs[[name]]$location)),
      .provenance_semantic(provenance), info = name
    )
  }
  expect_identical(attr(weights, "provenance"),
                   attr(weights, "oceancube_provenance"))
  expect_null(attr(extraction, "provenance", exact = TRUE))
  expect_null(attr(transect, "provenance", exact = TRUE))
})

test_that("validation classification and graph failures are deterministic", {
  valid <- provenance_test_v1()
  legacy <- list(parent = list(function_name = "read_nc"))
  opaque <- list(project_note = "safe")
  future <- valid
  future$schema_version <- "2.0.0"
  expect_identical(.provenance_validate(valid)$kind, "v1")
  expect_identical(.provenance_validate(legacy)$kind, "legacy")
  expect_identical(.provenance_validate(opaque)$kind, "opaque_user")
  expect_identical(.provenance_validate(future)$kind, "future_schema")

  malformed <- list()
  malformed$random_id <- valid
  malformed$random_id$history[[1L]]$id <- "uuid-random"
  malformed$duplicate_id <- .provenance_append(valid, "cube_crop")
  malformed$duplicate_id$history[[2L]]$id <- "op_001"
  with_lineage <- .provenance_merge_lineages(valid, valid, "secondary")$provenance
  malformed$lineage_id <- with_lineage
  names(malformed$lineage_id$lineages) <- "lineage_random"
  malformed$lineage_ref <- with_lineage
  malformed$lineage_ref$history[[1L]]$inputs[[1L]]$lineage_ref <- "lineage_999"
  malformed$entity_ref <- valid
  malformed$entity_ref$history[[1L]]$inputs[[1L]]$entity_ref <- "op_999:output"
  for (name in names(malformed)) {
    expect_identical(.provenance_validate(malformed[[name]])$kind,
                     "malformed_v1", info = name)
    expect_error(.provenance_validate(malformed[[name]], strict = TRUE),
                 class = "oceancube_provenance_error", info = name)
  }
})

test_that("privacy failures reject sentinel secrets without disclosing values", {
  names <- c("token", "password", "api_key", "authorization", "bearer")
  for (name in names) {
    sentinel <- paste0("SENTINEL-", toupper(name), "-DO-NOT-PRINT")
    value <- setNames(list(sentinel), name)
    message <- tryCatch(
      .provenance_normalize(value),
      error = function(error) conditionMessage(error)
    )
    expect_type(message, "character")
    expect_false(grepl(sentinel, message, fixed = TRUE), info = name)
  }
  signed <- provenance_test_v1()
  signed$source$locator <- list(
    type = "url", value = "https://example.test/data?signature=SENTINEL-SIGNATURE",
    basename = "data", portable = TRUE
  )
  message <- tryCatch(
    .provenance_validate(signed, strict = TRUE),
    error = function(error) conditionMessage(error)
  )
  expect_false(grepl("SENTINEL-SIGNATURE", message, fixed = TRUE))
  unsafe <- list(globalenv(), identity, quote(x + 1), expression(x + 1), Inf)
  for (value in unsafe) {
    expect_error(.provenance_normalize(list(value = value)),
                 class = "oceancube_provenance_unsafe")
  }
})

test_that("representative 0.2 operation families migrate once and safely", {
  named_steps <- c(
    "cube_slice", "cube_crop", "cube_extract", "cube_collect",
    "cube_aggregate_time", "cube_climatology", "cube_anomaly",
    "signal_noise", "cube_trend", "cube_mask"
  )
  function_steps <- c(
    "read_nc", "cube_transect", "coast_dist", "layer_mean", "crop_stock"
  )
  for (operation in named_steps) {
    legacy <- setNames(list(list(operation = operation, marker = "safe")), operation)
    before <- serialize(legacy, NULL)
    migrated <- .provenance_normalize(legacy, provenance_test_context())
    expect_identical(serialize(legacy, NULL), before, info = operation)
    expect_identical(vapply(migrated$history, `[[`, character(1L), "operation"),
                     operation, info = operation)
  }
  for (operation in function_steps) {
    legacy <- list(
      package = "oceancube", package_version = "0.2.0.9000",
      function_name = operation, arguments = list(marker = "safe")
    )
    migrated <- .provenance_normalize(legacy, provenance_test_context())
    expect_identical(vapply(migrated$history, `[[`, character(1L), "operation"),
                     operation, info = operation)
  }
  source <- list(
    package = "oceancube", package_version = "0.2.0.9000",
    function_name = "read_nc", arguments = list(vars = "sst")
  )
  climatology <- list(cube_climatology = list(by = "month"))
  legacy_anomaly <- list(
    parent = list(source = source, climatology = climatology),
    cube_anomaly = list(type = "difference"), safe_note = "retain"
  )
  before <- serialize(legacy_anomaly, NULL)
  migrated <- .provenance_normalize(legacy_anomaly, provenance_test_context())
  expect_identical(serialize(legacy_anomaly, NULL), before)
  expect_identical(vapply(migrated$history, `[[`, character(1L), "operation"),
                   c("read_nc", "cube_anomaly"))
  expect_named(migrated$lineages, "lineage_001")
  expect_identical(vapply(migrated$lineages$lineage_001$history, `[[`,
                          character(1L), "operation"), "cube_climatology")
  expect_identical(migrated$extensions$legacy$safe_note, "retain")
})

test_that("Date and UTC POSIXct pipelines preserve temporal meaning", {
  for (posixct in c(FALSE, TRUE)) {
    cube <- global_exit_cube(posixct = posixct)
    cropped <- cube_crop(cube, time = range(cube$time)[c(2L, 2L)])
    sliced <- cube_slice(cube, time = cube$time[c(2L, 7L, 19L)], by = "value")
    aggregated <- suppressWarnings(cube_aggregate_time(cube, "month"))
    climatology <- suppressWarnings(cube_climatology(aggregated, "month"))
    anomaly <- cube_anomaly(aggregated, climatology)
    trend <- cube_trend(anomaly, time_unit = "year")
    expected_class <- if (posixct) "POSIXct" else "Date"
    expect_s3_class(cropped$time, expected_class)
    expect_s3_class(sliced$time, expected_class)
    if (posixct) {
      expect_identical(attr(sliced$time, "tzone"), "UTC")
      expect_true(any(format(sliced$time, "%H:%M:%S", tz = "UTC") != "00:00:00"))
    }
    expect_identical(aggregated$provenance$time$current$kind, "historical")
    expect_identical(climatology$provenance$time$current$kind,
                     "recurring_climatology")
    expect_identical(anomaly$provenance$time$current$kind, "historical")
    expect_identical(trend$provenance$time$current$kind, "trend_anchor")
    restored <- unserialize(serialize(sliced, NULL))
    expect_identical(restored$time, sliced$time)
  }
})

test_that("offline OISST lineage survives both certified end-to-end paths", {
  fixture <- testthat::test_path(
    "fixtures", "real-data", "noaa-oisst21-surface-time-fv1.nc"
  )
  raw <- read_nc(
    fixture, vars = "sst", depth_name = "zlev",
    source = "NOAA OISST", dataset_id = "oisst-v2.1"
  )
  raw_values <- .cube_read(raw)
  cropped <- cube_crop(raw, longitude = range(raw$lon), latitude = range(raw$lat))
  sliced <- cube_slice(cropped, time = raw$time[[1L]], by = "value")
  aggregated <- suppressWarnings(cube_aggregate_time(cropped, "month"))
  extracted <- cube_extract(
    aggregated, longitude = aggregated$lon[[1L]],
    latitude = aggregated$lat[[1L]], depth = aggregated$depth[[1L]],
    variable = "sst", mode = "series", match = "exact"
  )
  trend <- cube_trend(cropped, time_unit = "day", min_n = 3L)
  products <- list(raw, cropped, sliced, aggregated, trend)
  for (product in products) {
    expect_true(.provenance_validate(product$provenance, strict = TRUE)$valid)
    expect_identical(product$provenance$source$identity$dataset_id, "oisst-v2.1")
  }
  extract_provenance <- attr(extracted, "oceancube_provenance")
  expect_true(.provenance_validate(extract_provenance, strict = TRUE)$valid)
  expect_identical(extract_provenance$source$identity$dataset_id, "oisst-v2.1")
  expect_identical(.cube_read(cropped), raw_values)
  expect_identical(.cube_read(sliced), raw_values[, , , 1L, , drop = FALSE])
  expect_true(all(raw$lon >= 0 & raw$lon <= 360))
  expect_identical(provenance_operations(trend),
                   c("read_nc", "cube_crop", "cube_trend"))
})

test_that("V1 growth stays linear through 50 operations and one-copy lineage", {
  context <- provenance_test_context()
  provenance <- .provenance_normalize(NULL, context)
  checkpoints <- c(1L, 5L, 10L, 25L, 50L)
  sizes <- numeric(length(checkpoints))
  for (i in seq_len(max(checkpoints))) {
    provenance <- .provenance_append(provenance, "cube_slice", context = context)
    if (i %in% checkpoints) sizes[match(i, checkpoints)] <- length(serialize(provenance, NULL))
  }
  expect_identical(vapply(provenance$history, `[[`, character(1L), "id"),
                   sprintf("op_%03d", seq_len(50L)))
  per_operation <- diff(sizes) / diff(checkpoints)
  expect_lt(max(per_operation) / min(per_operation), 1.1)

  secondary <- provenance_test_v1("cube_climatology")
  once <- .provenance_merge_lineages(provenance, secondary, "climatology")
  twice <- .provenance_merge_lineages(once$provenance, secondary, "climatology")
  expect_named(once$provenance$lineages, "lineage_001")
  expect_length(twice$provenance$lineages, 1L)
  expect_identical(once$refs[[1L]], twice$refs[[1L]])
})

test_that("A4 exit machine-readable inventories are exhaustive and closed", {
  test_root <- normalizePath(testthat::test_path(), winslash = "/")
  candidates <- c(
    file.path(test_root, "..", ".."),
    file.path(test_root, "..", "..", "00_pkg_src", "oceancube")
  )
  roots <- candidates[dir.exists(file.path(candidates, "R"))]
  expect_gt(length(roots), 0L)
  root <- roots[[1L]]
  evidence <- file.path(root, "dev", "hardening", "provenance")
  if (!dir.exists(evidence)) {
    expect_identical(as.character(utils::packageVersion("oceancube")),
                     "0.2.0.9000")
    return(invisible())
  }
  producer <- read.csv(file.path(evidence, "a4-exit-producer-scan.csv"),
                       stringsAsFactors = FALSE, check.names = FALSE)
  output <- read.csv(file.path(evidence, "a4-exit-output-matrix.csv"),
                     stringsAsFactors = FALSE, check.names = FALSE)
  certification <- read.csv(file.path(evidence, "a4-exit-certification.csv"),
                            stringsAsFactors = FALSE, check.names = FALSE)
  findings <- read.csv(file.path(evidence, "a4-exit-open-findings.csv"),
                       stringsAsFactors = FALSE, check.names = FALSE)
  expect_named(producer, c(
    "file", "symbol", "pattern", "classification", "active_runtime_producer",
    "legacy_parser", "compatibility_alias", "test_support",
    "remediation_required", "notes"
  ), ignore.order = FALSE)
  expect_false(any(
    producer$active_runtime_producer == "TRUE" &
      grepl("legacy producer", producer$classification, fixed = TRUE)
  ))
  expect_setequal(output$output_class, c(
    "ocean_cube", "stock_cube", "ocean_anom", "ocean_clim",
    "cube_extract data.frame", "cube_transect data.frame",
    "cube_polygon_weights data.frame"
  ))
  expect_true(all(output$result == "PASS"))
  expect_true(all(certification$result %in% c("PASS", "OPEN", "NOT_APPLICABLE")))
  expect_identical(findings$status[findings$id == "A1-003"], "CLOSED")
  expect_identical(findings$status[findings$id == "A4B3B-001"], "OPEN")

  mapping <- read.csv(file.path(evidence, "operation-mapping.csv"),
                      stringsAsFactors = FALSE, check.names = FALSE)
  mapping_status <- ifelse(
    mapping$current_operation %in% c(
      "to_month", "clim_month", "clim_day", "anom_diff", "anom_z"
    ), "compatibility-only", ifelse(
      mapping$current_operation == "ocean_cube", "not a producer", "implemented"
    )
  )
  expect_identical(nrow(mapping), 21L)
  expect_true(all(mapping_status %in% c(
    "implemented", "compatibility-only", "future/deferred", "not a producer"
  )))
  expect_false(any(is.na(mapping_status) | !nzchar(mapping_status)))
  field <- read.csv(file.path(evidence, "a4b1-field-coverage.csv"),
                    stringsAsFactors = FALSE, check.names = FALSE)
  expect_identical(field$fields_with_executable_evidence[field$requirement == "total"],
                   75L)
  expect_identical(field$unexercised_fields[field$requirement == "total"], 0L)
  migration <- read.csv(file.path(evidence, "migration-matrix.csv"),
                        stringsAsFactors = FALSE, check.names = FALSE)
  expect_identical(nrow(migration), 40L)
  expect_false(any(!nzchar(migration$v1_destination) |
                     !nzchar(migration$ordering_rule) |
                     !nzchar(migration$notes)))
})

test_that("runtime source contains no active legacy provenance producer", {
  test_root <- normalizePath(testthat::test_path(), winslash = "/")
  candidates <- c(
    file.path(test_root, "..", ".."),
    file.path(test_root, "..", "..", "00_pkg_src", "oceancube")
  )
  roots <- candidates[dir.exists(file.path(candidates, "R"))]
  expect_gt(length(roots), 0L)
  root <- roots[[1L]]
  files <- list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE,
                      recursive = TRUE)
  source <- unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE)
  make_definitions <- grep("\\.make_provenance\\s*<-\\s*function", source,
                           value = TRUE, perl = TRUE)
  make_calls <- grep("\\.make_provenance\\(", source, value = TRUE)
  expect_length(make_definitions, 1L)
  expect_length(make_calls, 0L)
  expect_length(grep("attr\\([^,]+, [\"']oceancube_provenance[\"']\\) <-",
                     source, perl = TRUE), 8L)
  expect_length(grep("attr\\([^,]+, [\"']provenance[\"']\\) <-",
                     source, perl = TRUE), 1L)
})
