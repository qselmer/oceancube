remaining_v1_cube <- function(provenance = NULL) {
  ocean_cube(
    lon = c(-80, -79), lat = c(-12, -11), depth = c(0, 10, 30),
    time = as.Date(c("2020-01-01", "2020-02-01")), vars = "sst",
    data = array(seq_len(24), c(2, 2, 3, 2, 1)),
    units = c(sst = "degC"), source = "remaining-v1-source",
    dataset_id = "remaining-v1-fixture", provenance = provenance
  )
}

test_that("layer_mean appends bounded V1 and preserves time", {
  x <- remaining_v1_cube()
  before <- serialize(x, NULL)
  result <- layer_mean(x, c(0, 10, 30))
  operation <- tail(result$provenance$history, 1L)[[1L]]

  expect_identical(serialize(x, NULL), before)
  expect_true(.provenance_validate(result$provenance, strict = TRUE)$valid)
  expect_identical(operation$operation, "layer_mean")
  expect_identical(operation$scientific_method$id,
                   "oceancube:depth_layer_mean")
  expect_identical(operation$parameters$resolved$n_layers, 2L)
  expect_identical(operation$parameters$resolved$layer_centers, c(5, 20))
  expect_identical(result$time, x$time)
  expect_identical(result$provenance$time, x$provenance$time)
  expect_identical(length(result$provenance$history),
                   length(x$provenance$history) + 1L)
})

test_that("crop_stock appends bounded mask V1 and preserves stock-cube metadata", {
  x <- remaining_v1_cube()
  x$dc <- matrix(c(1, 2, 3, 4), 2, 2)
  mask <- stock_mask(
    x, stock = "anchovy", lat = c(-12, -11.5), dc = c(0, 3),
    depth = c(0, 10)
  )
  before <- serialize(x, NULL)
  mask_before <- serialize(mask, NULL)
  result <- crop_stock(x, mask)
  operation <- tail(result$provenance$history, 1L)[[1L]]

  expect_identical(serialize(x, NULL), before)
  expect_identical(serialize(mask, NULL), mask_before)
  expect_s3_class(result, "stock_cube")
  expect_identical(result$mask, mask)
  expect_identical(result$dc, x$dc)
  expect_identical(result$time, x$time)
  expect_identical(result$source, x$source)
  expect_identical(result$dataset_id, x$dataset_id)
  expect_identical(operation$operation, "crop_stock")
  expect_identical(operation$scientific_method$id, "oceancube:stock_mask")
  expect_identical(operation$parameters$resolved$mask_dimensions,
                   c(longitude = 2L, latitude = 2L, depth = 3L))
  expect_identical(operation$parameters$resolved$kept_cells,
                   as.integer(sum(mask$mask)))
  expect_false(any(vapply(operation$parameters$resolved, is.array, logical(1L))))
})

test_that("remaining cube producers normalize legacy inputs without mutation", {
  x <- remaining_v1_cube()
  x$provenance <- .make_provenance(
    "read_nc", args = list(vars = "sst"), extra = list(marker = "legacy")
  )
  before <- serialize(x, NULL)
  layer <- layer_mean(x, c(0, 30))
  mask <- stock_mask(x, stock = "legacy-stock")
  stock <- crop_stock(x, mask)

  expect_identical(serialize(x, NULL), before)
  for (value in list(layer, stock)) {
    expect_true(.provenance_validate(value$provenance, strict = TRUE)$valid)
    expect_null(value$provenance$parent)
  }
  expect_identical(tail(layer$provenance$history, 1L)[[1L]]$operation,
                   "layer_mean")
  expect_identical(tail(stock$provenance$history, 1L)[[1L]]$operation,
                   "crop_stock")
})

test_that("remaining cube V1 is deterministic and serializable", {
  first <- layer_mean(remaining_v1_cube(), c(0, 30))
  second <- layer_mean(remaining_v1_cube(), c(0, 30))
  expect_identical(.provenance_semantic(first$provenance),
                   .provenance_semantic(second$provenance))
  expect_identical(unserialize(serialize(first, NULL)), first)
  path <- tempfile(fileext = ".rds")
  withr::local_file(path)
  saveRDS(first, path)
  expect_identical(readRDS(path), first)
})

test_that("active remaining producer files contain no recursive legacy writes", {
  test_root <- normalizePath(testthat::test_path(), winslash = "/")
  candidates <- c(
    file.path(test_root, "..", ".."),
    file.path(test_root, "..", "..", "00_pkg_src", "oceancube")
  )
  package_root <- candidates[dir.exists(file.path(candidates, "R"))][[1L]]
  files <- file.path(
    package_root,
    "R", c("cube_extract.R", "cube_transect.R", "cube_polygon_weights.R",
           "layer_mean.R", "coast_dist.R", "stock_mask.R")
  )
  source <- lapply(files, readLines, warn = FALSE)
  expect_false(any(vapply(source, function(x) any(grepl(
    "\\.make_provenance\\(|list\\(parent\\s*=|source_provenance",
    x, perl = TRUE
  )), logical(1L))))
  expect_identical(sum(vapply(source, function(x) sum(grepl(
    "attr\\(base, \\\"provenance\\\"\\)", x
  )), integer(1L))), 1L)
})
