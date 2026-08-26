table_v1_cube <- function(provenance = NULL) {
  ocean_cube(
    lon = c(-80, -79), lat = c(-12, -11), depth = c(0, 50),
    time = as.Date(c("2020-01-01", "2020-02-01")), vars = "sst",
    data = array(seq_len(16), c(2, 2, 2, 2, 1)),
    units = c(sst = "degC"), source = "table-v1-source",
    dataset_id = "table-v1-fixture", provenance = provenance
  )
}

table_v1_outputs <- function(x = table_v1_cube()) {
  path <- data.frame(
    station = c("a", "b"), longitude = c(-80, -79),
    latitude = c(-12, -11)
  )
  extraction <- cube_extract(
    x, longitude = -80, latitude = -12, depth = 0,
    variable = "sst", mode = "series", match = "exact"
  )
  transect <- cube_transect(
    x, path, id_col = "station", depth = 0, time = x$time[[1L]],
    variable = "sst", match = "exact", mode = "horizontal"
  )
  list(extraction = extraction, transect = transect, path = path)
}

test_that("extraction and transect tables append one complete V1 operation", {
  x <- cube_crop(table_v1_cube(), longitude = c(-80, -79))
  before <- serialize(x, NULL)
  outputs <- table_v1_outputs(x)

  expect_identical(serialize(x, NULL), before)
  for (name in c("extraction", "transect")) {
    value <- outputs[[name]]
    provenance <- attr(value, "oceancube_provenance", exact = TRUE)
    expect_true(.provenance_validate(provenance, strict = TRUE)$valid,
                info = name)
    expect_identical(provenance$source$identity, x$provenance$source$identity)
    expect_identical(length(provenance$history),
                     length(x$provenance$history) + 1L)
    expect_identical(vapply(provenance$history, `[[`, character(1L), "id"),
                     sprintf("op_%03d", seq_along(provenance$history)))
    expect_null(provenance$parent)
    expect_setequal(names(provenance), c(
      "schema_version", "source", "time", "history", "lineages", "extensions"
    ))
  }

  extract_op <- tail(attr(outputs$extraction, "oceancube_provenance")$history,
                     1L)[[1L]]
  transect_op <- tail(attr(outputs$transect, "oceancube_provenance")$history,
                      1L)[[1L]]
  expect_identical(extract_op$operation, "cube_extract")
  expect_identical(extract_op$scientific_method$id,
                   "oceancube:coordinate_extraction")
  expect_identical(extract_op$parameters$resolved$selected_counts,
                   c(longitude = 1L, latitude = 1L, depth = 1L,
                     time = 2L, variable = 1L))
  expect_identical(transect_op$operation, "cube_transect")
  expect_identical(transect_op$scientific_method$id,
                   "oceancube:haversine_transect")
  expect_match(transect_op$parameters$resolved$distance_method,
               "6371.0088", fixed = TRUE)
  expect_identical(nrow(attr(outputs$transect, "oceancube_path")), 2L)
  expect_identical(attr(outputs$transect, "oceancube_path")$point_id,
                   outputs$path$station)
})

test_that("table QA is separated from semantic V1", {
  outputs <- table_v1_outputs()
  extraction_qa <- attr(outputs$extraction, "oceancube_qa")$extraction
  transect_qa <- attr(outputs$transect, "oceancube_qa")$transect
  extraction_provenance <- attr(outputs$extraction, "oceancube_provenance")
  transect_provenance <- attr(outputs$transect, "oceancube_provenance")

  expect_true(is.list(extraction_qa))
  expect_true(is.list(transect_qa))
  expect_true("netcdf_read" %in% names(extraction_qa))
  expect_true("physical_reads" %in% names(transect_qa))
  expect_false("netcdf_read" %in% names(extraction_provenance))
  expect_false("physical_reads" %in% names(transect_provenance))
  expect_true(is.list(attr(outputs$extraction, "oceancube_selection")))
  expect_true(is.data.frame(attr(outputs$transect, "oceancube_path")))
})

test_that("table V1 survives serialize and RDS round trips", {
  outputs <- table_v1_outputs()
  for (name in c("extraction", "transect")) {
    value <- outputs[[name]]
    restored <- unserialize(serialize(value, NULL))
    expect_identical(restored, value, info = paste(name, "serialize"))
    path <- tempfile(fileext = ".rds")
    withr::local_file(path)
    saveRDS(value, path)
    restored_rds <- readRDS(path)
    expect_identical(restored_rds, value, info = paste(name, "RDS"))
    expect_identical(
      .provenance_semantic(attr(restored_rds, "oceancube_provenance")),
      .provenance_semantic(attr(value, "oceancube_provenance"))
    )
  }
})

test_that("table producers lazily normalize legacy and opaque provenance", {
  legacy <- table_v1_cube()
  legacy$provenance <- .make_provenance(
    "read_nc", args = list(vars = "sst"), extra = list(marker = "legacy")
  )
  opaque <- table_v1_cube()
  opaque$provenance <- list(project = "safe-user-project")

  for (x in list(legacy, opaque)) {
    before <- serialize(x, NULL)
    value <- cube_extract(x, longitude = -80, mode = "table")
    provenance <- attr(value, "oceancube_provenance")
    expect_identical(serialize(x, NULL), before)
    expect_true(.provenance_validate(provenance, strict = TRUE)$valid)
    expect_identical(tail(provenance$history, 1L)[[1L]]$operation,
                     "cube_extract")
  }
  opaque_result <- cube_extract(opaque, longitude = -80, mode = "table")
  expect_identical(
    attr(opaque_result, "oceancube_provenance")$extensions$user,
    opaque$provenance
  )
})

test_that("table semantic provenance excludes private execution material", {
  outputs <- table_v1_outputs()
  text <- paste(capture.output(dput(lapply(
    outputs[c("extraction", "transect")],
    function(x) .provenance_semantic(attr(x, "oceancube_provenance"))
  ))), collapse = "")
  forbidden <- c(
    Sys.info()[["user"]], Sys.info()[["nodename"]],
    normalizePath(tempdir(), winslash = "/"), "credential", "signed_url"
  )
  forbidden <- forbidden[!is.na(forbidden) & nzchar(forbidden)]
  expect_false(any(vapply(forbidden, grepl, logical(1L),
                          x = text, fixed = TRUE)))
})
