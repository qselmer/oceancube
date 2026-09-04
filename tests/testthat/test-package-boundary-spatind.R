test_that("only approved APIs extend the boundary", {
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
    "cube_polygon_weights",
    "cube_aggregate_time",
    "cube_anomaly",
    "cube_climatology",
    "cube_trend",
    "viz.timeseries",
    "cube_open",
    "layer_integral",
    "depth_sample",
    "depth_gradient",
    "depth_feature",
    "transition_layer",
    "oxygen_boundary",
    "mixed_layer_depth",
    "thermodynamic_state",
    "stratification"
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

test_that("oceancube owns descriptive per-cell trends but not spatial inference", {
  expect_true("cube_trend" %in% getNamespaceExports("oceancube"))
  expect_false(any(c(
    "cube_trend_significance", "cube_change", "cube_breakpoints",
    "cube_regimes", "cube_spatial_trend"
  ) %in% getNamespaceExports("oceancube")))
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
