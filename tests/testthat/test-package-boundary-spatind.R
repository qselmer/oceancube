test_that("only the four approved geometry primitives extend the API", {
  exports_before <- c(
    "annual_index", "anom_diff", "anom_z", "clim_day", "clim_month",
    "cm_connect", "cm_setup", "coast_dist", "crop_stock", "cube_collect",
    "cube_crop", "cube_extract", "cube_inspect", "cube_mask", "cube_slice",
    "cube_transect", "download_nc", "layer_mean", "link_events",
    "cube_validate", "ocean_cube", "read_nc", "signal_noise", "stock_mask",
    "to_month", "viz.map", "viz.profile", "viz.section", "viz.transect"
  )
  approved <- c(
    "cube_cell_area",
    "cube_layer_thickness",
    "cube_cell_volume",
    "cube_polygon_weights"
  )
  observed <- getNamespaceExports("oceancube")

  expect_setequal(observed, c(exports_before, approved))
  expect_setequal(setdiff(observed, exports_before), approved)
})

test_that("indicator and summary APIs remain outside oceancube", {
  forbidden <- c(
    "cube_polygon_summary",
    "cube_indicator_2d",
    "cube_indicator_3d",
    "cube_center_of_gravity",
    "cube_area_occupied",
    "cube_volume_occupied"
  )
  expect_false(any(forbidden %in% getNamespaceExports("oceancube")))
})

test_that("new APIs expose geometry arguments but no indicator inputs", {
  expected <- list(
    cube_cell_area = c("x", "unit"),
    cube_layer_thickness = c("x", "depth_bounds", "unit"),
    cube_cell_volume = c("x", "depth_bounds", "unit"),
    cube_polygon_weights = c(
      "x", "polygons", "id_col", "crs", "dimension",
      "depth_bounds", "include_zero"
    )
  )
  for (name in names(expected)) {
    expect_identical(names(formals(getExportedValue("oceancube", name))),
                     expected[[name]])
  }
})
