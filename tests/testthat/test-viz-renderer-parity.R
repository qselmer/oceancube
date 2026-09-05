test_that("public wrappers and explicit prepare-render paths are equivalent", {
  cube <- viz_data_test_cube()
  path <- viz_data_test_path()
  public <- list(
    viz.map(cube, "temperature", cube$time[[1L]], 0,
            limits = c(10, 100), title = "Map"),
    viz.profile(cube, "temperature", -79, -11, cube$time[[1L]],
                limits = c(10, 180), points = TRUE),
    viz.section(cube, "temperature", time = cube$time[[1L]], latitude = -11),
    viz.transect(cube, path, "temperature", time = cube$time[[1L]],
                 mode = "section"),
    viz.timeseries(cube, "temperature", -79, -11, 0, points = TRUE)
  )
  prepared <- list(
    .viz_prepare_map(cube, "temperature", cube$time[[1L]], 0,
                     limits = c(10, 100), title = "Map"),
    .viz_prepare_profile(cube, "temperature", -79, -11, cube$time[[1L]],
                         limits = c(10, 180), points = TRUE),
    .viz_prepare_section(cube, "temperature", time = cube$time[[1L]],
                         latitude = -11),
    .viz_prepare_transect(cube, path, "temperature", time = cube$time[[1L]],
                          mode = "section"),
    .viz_prepare_timeseries(cube, "temperature", -79, -11, 0, points = TRUE)
  )
  internal <- lapply(prepared, .viz_render_ggplot)

  expect_true(all(vapply(public, inherits, logical(1), "ggplot")))
  for (index in seq_along(public)) {
    expect_identical(viz_plot_semantics(public[[index]]),
                     viz_plot_semantics(internal[[index]]))
  }
})

test_that("renderer is deterministic and performs no scientific selection", {
  cube <- viz_data_test_cube()
  prepared <- .viz_prepare_section(
    cube, "temperature", time = cube$time[[1L]], latitude = -11
  )
  local_mocked_bindings(
    cube_extract = function(...) stop("renderer attempted cube_extract"),
    cube_transect = function(...) stop("renderer attempted cube_transect"),
    .package = "oceancube"
  )

  first <- .viz_render_ggplot(prepared)
  second <- .viz_render_ggplot(prepared)
  expect_identical(viz_plot_semantics(first), viz_plot_semantics(second))
})

test_that("all five public signatures remain frozen", {
  expected <- list(
    viz.map = c("x", "variable", "time", "depth", "limits", "na.rm",
                "coastline", "title", "subtitle", "caption"),
    viz.profile = c("x", "variable", "longitude", "latitude", "time",
                    "depth", "limits", "na.rm", "reverse_depth", "points",
                    "title", "subtitle", "caption"),
    viz.section = c("x", "variable", "section", "time", "longitude",
                    "latitude", "depth", "limits", "na.rm", "reverse_depth",
                    "title", "subtitle", "caption"),
    viz.transect = c("x", "path", "variable", "time", "depth", "lon_col",
                     "lat_col", "id_col", "match", "tolerance", "mode",
                     "distance", "limits", "na.rm", "reverse_depth", "points",
                     "title", "subtitle", "caption"),
    viz.timeseries = c("x", "variable", "longitude", "latitude", "depth",
                       "time_from", "time_to", "match", "tolerance", "limits",
                       "na.rm", "points", "title", "subtitle", "caption")
  )
  for (name in names(expected)) {
    expect_identical(names(formals(get(name))), expected[[name]])
  }
})
