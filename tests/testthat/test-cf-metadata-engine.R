test_that("the production scanner preserves the canonical source schema", {
  file <- make_cf_b2_fixture()
  withr::local_file(file)

  cf <- .cf_scan_netcdf(file)
  source <- cf$source

  expect_identical(cf$schema_name, "oceancube_cf_metadata")
  expect_identical(cf$schema_version, "1.0.0")
  expect_null(cf$current)
  expect_identical(source$declaration$raw, "CF-1.13, ACDD-1.3")
  expect_identical(source$declaration$declared_cf, "1.13")
  expect_identical(
    source$dimensions$order,
    names(source$dimensions$map)
  )
  expect_identical(source$variables$order, names(source$variables$map))
  expect_true(all(c(
    "temperature", "lon_bnds", "g1/x", "g2/x", "g1/dup", "g2/dup"
  ) %in% source$variables$order))
  expect_identical(
    source$variables$map$temperature$source_primitive_type,
    "float"
  )
  expect_identical(
    source$variables$map$temperature$primitive_type_status,
    "available"
  )
  expect_identical(
    source$variables$map$lon$primitive_type_status,
    "unavailable"
  )
  expect_true(.cf_validate_cf(cf, require_current = FALSE))
})

test_that("unknown global and variable attributes remain exact and ordered", {
  file <- make_cf_b2_fixture()
  withr::local_file(file)
  cf <- .cf_scan_netcdf(file)

  global <- cf_attribute_record_by_name(
    cf$source$global_attributes, "custom_provider_attribute"
  )
  variable <- cf_attribute_record_by_name(
    cf$source$variables$map$temperature$attributes,
    "custom_variable_attribute"
  )

  expect_identical(global$raw_value, "keep this exact value")
  expect_identical(global$r_type, "character")
  expect_identical(global$r_class, "character")
  expect_identical(global$owner_path, "/")
  expect_identical(global$scope, "global")
  expect_identical(global$primitive_type_status, "unavailable")
  expect_identical(variable$raw_value, "preserve me")
  expect_identical(variable$owner_path, "temperature")
  expect_true(all(diff(vapply(
    cf$source$global_attributes, `[[`, integer(1L), "source_order"
  )) > 0L))
})

test_that("all supported links keep raw and interpreted relationships", {
  file <- make_cf_b2_fixture()
  withr::local_file(file)
  cf <- .cf_scan_netcdf(file)
  links <- cf$source$links

  expect_true(all(.cf_link_families() %in%
    vapply(links, `[[`, character(1L), "attribute")))
  expect_true(all(c(
    "RESOLVED", "MISSING_TARGET", "SELF_REFERENCE",
    "DUPLICATE_REFERENCE", "AMBIGUOUS", "DEFERRED_EXTENDED"
  ) %in% vapply(links, `[[`, character(1L), "status")))

  extended <- cf_link_records(cf, "grid_mapping")
  extended <- extended[vapply(
    extended, `[[`, character(1L), "status"
  ) == "DEFERRED_EXTENDED"]
  expect_length(extended, 1L)
  expect_identical(extended[[1L]]$raw_value, "crs: lon lat")
  expect_true(is.na(extended[[1L]]$resolved_path))

  formula <- cf_link_records(cf, "formula_terms")
  expect_true(any(vapply(formula, `[[`, character(1L), "status") ==
    "SELF_REFERENCE"))
  expect_true("formula_term" %in% cf$source$variables$map$sigma$roles)
  expect_true("quality_flag" %in% cf$source$variables$map$qc$roles)
  expect_true("ancillary" %in% cf$source$variables$map$qc$roles)
})

test_that("scanner is deterministic, path-private and reads no variable payload", {
  file <- make_cf_b2_fixture()
  withr::local_file(file)
  reads <- 0L
  local_mocked_bindings(
    ncvar_get = function(...) {
      reads <<- reads + 1L
      stop("scanner attempted a data read")
    },
    .package = "ncdf4"
  )

  first <- .cf_scan_netcdf(file)
  second <- .cf_scan_netcdf(file)

  expect_identical(reads, 0L)
  expect_identical(first, second)
  expect_identical(first, unserialize(serialize(first, NULL)))
  expect_false(.cf_contains_forbidden(first))
  expect_false(any(grepl(
    normalizePath(file, winslash = "/"),
    capture.output(str(first)), fixed = TRUE
  )))
})

test_that("group identities remain path-qualified and distinct", {
  file <- make_cf_b2_fixture()
  withr::local_file(file)
  cf <- .cf_scan_netcdf(file)

  expect_true(all(c("g1/x", "g2/x") %in% cf$source$dimensions$order))
  expect_true(all(c("g1/dup", "g2/dup") %in% cf$source$variables$order))
  expect_identical(anyDuplicated(cf$source$variables$order), 0L)
  ambiguous <- cf_link_records(cf, "coordinates")
  ambiguous <- ambiguous[vapply(
    ambiguous, `[[`, character(1L), "status"
  ) == "AMBIGUOUS"]
  expect_length(ambiguous, 1L)
  expect_setequal(
    ambiguous[[1L]]$candidate_target_paths,
    c("g1/dup", "g2/dup")
  )
})

test_that("metadata validators reject malformed or live canonical state", {
  file <- make_cf_b2_fixture()
  withr::local_file(file)
  cf <- .cf_scan_netcdf(file)
  resolutions <- .cf_resolve_axes(cf, "temperature")
  metadata <- .cf_wrap_metadata(
    .cf_build_current(cf, "temperature", resolutions, explicit_depth = TRUE)
  )

  expect_true(.cf_metadata_validate(metadata))
  bad_version <- metadata
  bad_version$cf$schema_version <- "2.0.0"
  expect_error(.cf_metadata_validate(bad_version), "schema")
  bad_order <- metadata
  bad_order$cf$source$variables$order[[1L]] <- "missing"
  expect_error(.cf_metadata_validate(bad_order), "order and map")
  bad_role <- metadata
  bad_role$cf$source$variables$map$temperature$roles <- "not_a_role"
  expect_error(.cf_metadata_validate(bad_role), "variable descriptor")
  bad_attribute_order <- metadata
  attributes <- bad_attribute_order$cf$source$variables$map$temperature$attributes
  attributes[[1L]]$source_order <- 99L
  bad_attribute_order$cf$source$variables$map$temperature$attributes <- attributes
  expect_error(.cf_metadata_validate(bad_attribute_order), "attribute order")
  bad_current_link <- metadata
  bad_current_link$cf$current$links <- as.integer(
    length(metadata$cf$source$links) + 1L
  )
  expect_error(.cf_metadata_validate(bad_current_link), "current metadata view")
  bad_array <- metadata
  bad_array$cf$diagnostics$payload <- array(1, dim = c(1, 1))
  expect_error(.cf_metadata_validate(bad_array), "plain-R")
})

test_that("axis resolver records conflicts, overrides and weak fallback", {
  file <- make_cf_b2_fixture()
  withr::local_file(file)
  cf <- .cf_scan_netcdf(file)

  conflict <- cf
  units <- cf_attribute_record_by_name(
    conflict$source$variables$map$lon$attributes, "units"
  )
  units$raw_value <- "degrees_north"
  names <- vapply(
    conflict$source$variables$map$lon$attributes,
    `[[`, character(1L), "name"
  )
  conflict$source$variables$map$lon$attributes[[which(names == "units")]] <- units
  found <- .cf_resolve_axes(conflict, "temperature")
  expect_identical(found$longitude$status, "CONFLICT")
  expect_identical(
    .cf_resolve_axes(conflict, "temperature", lon_name = "lon")$longitude$status,
    "CONFLICT"
  )

  weak <- cf
  weak$source$variables$map$lon$attributes <- list()
  overridden <- .cf_resolve_axes(weak, "temperature", lon_name = "lon")
  expect_identical(overridden$longitude$status, "RESOLVED")
  expect_identical(overridden$longitude$method, "explicit")
  expect_identical(overridden$longitude$diagnostic, "OVERRIDDEN")
})
