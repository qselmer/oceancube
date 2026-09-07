d2b_map_cube <- function(longitude = 0:6, latitude = 0:6,
                         values = NULL, dataset_id = "d2b-map-synthetic-v1") {
  if (is.null(values)) {
    values <- outer(longitude, latitude, function(x, y) x + y)
  }
  ocean_cube(
    lon = longitude,
    lat = latitude,
    depth = 0,
    time = as.Date("2024-01-01"),
    data = array(values, dim = c(length(longitude), length(latitude), 1, 1, 1)),
    vars = "field",
    units = "1",
    dataset_id = dataset_id
  )
}

test_that("D2B appends the minimum map contract without changing exports", {
  expected_original <- c(
    "x", "variable", "time", "depth", "limits", "na.rm", "coastline",
    "title", "subtitle", "caption"
  )
  expected_added <- c(
    "style", "scale_class", "center", "contour_breaks",
    "longitude_display"
  )

  expect_identical(names(formals(viz.map))[seq_along(expected_original)],
                   expected_original)
  expect_identical(tail(names(formals(viz.map)), length(expected_added)),
                   expected_added)
  expect_identical(length(getNamespaceExports("oceancube")), 49L)
  expect_false("viz.compose" %in% getNamespaceExports("oceancube"))
})

test_that("FIELD preserves the historical renderer and default scale", {
  cube <- d2b_map_cube()
  default <- viz.map(cube, "field")
  explicit <- viz.map(
    cube, "field", style = "field",
    scale_class = "unspecified_continuous", longitude_display = "source"
  )

  expect_s3_class(default, "ggplot")
  expect_true(inherits(default$layers[[1L]]$geom, "GeomRaster"))
  expect_identical(viz_plot_semantics(default), viz_plot_semantics(explicit))
  expect_identical(attr(default, "oceancube_map_style"), "FIELD")
  expect_identical(attr(default, "oceancube_contour_geometry"), "NONE")
})

test_that("contour styles change only renderer state", {
  cube <- d2b_map_cube()
  field <- .viz_prepare_map(
    cube, "field", style = "field", scale_class = "sequential", na.rm = FALSE
  )
  contour <- .viz_prepare_map(
    cube, "field", style = "contour", scale_class = "sequential",
    contour_breaks = c(3, 6, 9), na.rm = FALSE
  )
  combined <- .viz_prepare_map(
    cube, "field", style = "field_contour", scale_class = "sequential",
    contour_breaks = c(3, 6, 9), na.rm = FALSE
  )

  for (candidate in list(contour, combined)) {
    expect_identical(candidate$data, field$data)
    expect_identical(candidate$coordinates, field$coordinates)
    expect_identical(candidate$selection, field$selection)
    expect_identical(candidate$variables, field$variables)
    expect_identical(candidate$provenance, field$provenance)
    expect_identical(candidate$qa, field$qa)
  }
  contour_plot <- .viz_render_ggplot(contour)
  combined_plot <- .viz_render_ggplot(combined)
  expect_true(inherits(contour_plot$layers[[1L]]$geom, "GeomPath"))
  expect_true(inherits(combined_plot$layers[[1L]]$geom, "GeomRaster"))
  expect_true(inherits(combined_plot$layers[[2L]]$geom, "GeomPath"))
  expect_identical(attr(contour_plot, "oceancube_contour_geometry"),
                   "DISPLAY_CONTOUR_GEOMETRY")
  expect_identical(contour$renderer_hints$contour_break_rule,
                   "EXPLICIT_USER_LEVELS")
})

test_that("automatic contour breaks are deterministic and device independent", {
  cube <- d2b_map_cube()
  first <- .viz_prepare_map(cube, "field", style = "contour")
  second <- .viz_prepare_map(cube, "field", style = "contour")

  expect_identical(first$renderer_hints$contour_breaks,
                   second$renderer_hints$contour_breaks)
  expect_identical(first$renderer_hints$contour_break_rule,
                   "PRETTY_FINITE_DISPLAY_RANGE_N7")
  expect_true(all(first$renderer_hints$contour_breaks > min(first$data$value)))
  expect_true(all(first$renderer_hints$contour_breaks < max(first$data$value)))
})

test_that("FILLED_CONTOUR remains explicitly deferred", {
  expect_error(
    viz.map(d2b_map_cube(), "field", style = "filled_contour"),
    "reserved but deferred",
    class = "oceancube_viz_style_error"
  )
})

test_that("diverging maps require a finite compatible explicit center", {
  longitude <- 0:6
  latitude <- 0:6
  values <- outer(longitude - 3, latitude - 3, function(x, y) x + y)
  cube <- d2b_map_cube(longitude, latitude, values)
  plot <- viz.map(cube, "field", scale_class = "diverging", center = 0)
  prepared <- .viz_prepare_map(
    cube, "field", scale_class = "diverging", center = 0
  )

  expect_s3_class(plot, "ggplot")
  expect_identical(prepared$scale$classification, "DIVERGING")
  expect_identical(prepared$scale$centre, 0)
  expect_identical(.viz_map_palette_resolve(prepared$scale)$engine,
                   "grDevices_hcl_Blue-Red_3")
  expect_error(viz.map(cube, "field", scale_class = "diverging"),
               "explicit finite", class = "oceancube_viz_scale_error")
  expect_error(viz.map(cube, "field", scale_class = "diverging", center = Inf),
               "explicit finite", class = "oceancube_viz_scale_error")
  expect_error(viz.map(cube, "field", scale_class = "diverging", center = 8),
               "strictly inside", class = "oceancube_viz_scale_error")
  expect_error(viz.map(cube, "field", scale_class = "sequential", center = 0),
               "available only", class = "oceancube_viz_scale_error")
})

test_that("sequential and diverging defaults do not consult optional packages", {
  sequential <- .viz_prepare_map(
    d2b_map_cube(), "field", scale_class = "sequential"
  )
  signed_values <- outer(0:6 - 3, 0:6 - 3, function(x, y) x + y)
  diverging <- .viz_prepare_map(
    d2b_map_cube(values = signed_values), "field",
    scale_class = "diverging", center = 0
  )

  expect_identical(sequential$scale$palette, "viridis_D")
  expect_identical(diverging$scale$palette, "base_hcl_Blue-Red_3")
  expect_identical(.viz_map_palette_resolve(sequential$scale)$engine,
                   "ggplot2_viridis_D")
  expect_identical(
    .viz_map_palette_resolve(diverging$scale)$colours,
    grDevices::hcl.colors(3L, "Blue-Red 3")
  )
})

test_that("longitude wrapping is display only and dateline safe", {
  cube <- d2b_map_cube(longitude = c(276.5, 277.5, 278.5))
  prepared <- .viz_prepare_map(
    cube, "field", longitude_display = "neg180_180"
  )
  wrapped <- .viz_render_ggplot(prepared)
  source <- viz.map(cube, "field", longitude_display = "source")
  zero <- viz.map(
    d2b_map_cube(longitude = c(-83.5, -82.5, -81.5)), "field",
    longitude_display = "zero_360"
  )

  expect_identical(range(prepared$data$longitude), c(276.5, 278.5))
  expect_identical(range(wrapped$data$longitude), c(-83.5, -81.5))
  expect_identical(range(source$data$longitude), c(276.5, 278.5))
  expect_identical(range(zero$data$longitude), c(276.5, 278.5))
  expect_identical(attr(wrapped, "oceancube_source_longitude_range"),
                   c(276.5, 278.5))
  expect_identical(attr(wrapped, "oceancube_display_longitude_range"),
                   c(-83.5, -81.5))
  expect_error(
    viz.map(
      d2b_map_cube(longitude = c(170, 179, 181, 190)), "field",
      longitude_display = "neg180_180"
    ),
    "dateline discontinuity",
    class = "oceancube_viz_longitude_error"
  )
})

test_that("data-frame coastlines follow an explicit longitude display", {
  cube <- d2b_map_cube(longitude = c(276.5, 277.5, 278.5))
  coast <- data.frame(
    longitude = c(276.5, 277.5, 278.5),
    latitude = c(1, 2, 3),
    group = 1L
  )
  plot <- viz.map(
    cube, "field", coastline = coast, longitude_display = "neg180_180"
  )
  built <- ggplot2::ggplot_build(plot)

  expect_identical(range(built$data[[2L]]$x), c(-83.5, -81.5))
})

test_that("NA holes are retained and contour pieces do not bridge them", {
  values <- outer(0:8, 0:8, function(x, y) x)
  values[3:7, 3:7] <- NA_real_
  cube <- d2b_map_cube(0:8, 0:8, values)
  field <- .viz_prepare_map(cube, "field", style = "field", na.rm = FALSE)
  contour <- .viz_prepare_map(
    cube, "field", style = "contour", contour_breaks = c(1.5, 3.5, 6.5),
    na.rm = FALSE
  )
  combined <- .viz_prepare_map(
    cube, "field", style = "field_contour",
    contour_breaks = c(1.5, 3.5, 6.5), na.rm = FALSE
  )
  geometry <- .viz_map_contour_data(
    contour$data, contour$renderer_hints$contour_breaks
  )

  expect_identical(sum(is.na(field$data$value)), 25L)
  expect_identical(contour$data, field$data)
  expect_identical(combined$data, field$data)
  crossing_piece <- vapply(split(geometry, geometry$piece), function(piece) {
    inside_x <- any(piece$longitude > 2 & piece$longitude < 6)
    spans_hole <- min(piece$latitude) < 2 && max(piece$latitude) > 6
    inside_x && spans_hole
  }, logical(1))
  expect_false(any(crossing_piece))
})

test_that("display limits never mutate prepared scientific values", {
  cube <- d2b_map_cube()
  unlimited <- .viz_prepare_map(cube, "field")
  limited <- .viz_prepare_map(cube, "field", limits = c(2, 8))

  expect_identical(limited$data, unlimited$data)
  expect_identical(limited$provenance, unlimited$provenance)
  expect_identical(limited$qa, unlimited$qa)
})

test_that("all implemented styles support bounded NetCDF preparation and removal", {
  file <- make_netcdf_backend_fixture()
  cube <- .new_netcdf_cube(
    .new_netcdf_storage(file, c("temperature", "oxygen"))
  )
  styles <- c("field", "contour", "field_contour")
  prepared <- lapply(styles, function(style) {
    .viz_prepare_map(
      cube, "temperature", cube$time[[1L]], cube$depth[[1L]],
      style = style, na.rm = FALSE
    )
  })

  for (item in prepared) {
    expect_identical(
      item$qa$extraction$netcdf_read$physical_count,
      c(longitude = 3L, latitude = 2L, depth = 1L, time = 1L)
    )
    expect_false(isTRUE(item$qa$extraction$netcdf_read$full_cube_read))
  }
  expect_identical(unlink(file), 0L)
  rendered <- lapply(prepared, .viz_render_ggplot)
  expect_true(all(vapply(rendered, inherits, logical(1), "ggplot")))
})

test_that("MAP_LAYER D2B state serializes and remains ggplot-modifiable", {
  prepared <- .viz_prepare_map(
    d2b_map_cube(), "field", style = "field_contour",
    scale_class = "sequential"
  )
  restored <- unserialize(serialize(prepared, NULL))
  file <- tempfile(fileext = ".rds")
  withr::local_file(file)
  saveRDS(prepared, file)

  expect_identical(restored, prepared)
  expect_identical(readRDS(file), prepared)
  expect_true(.validate_oceancube_viz_data(restored))
  plot <- .viz_render_ggplot(restored)
  expect_s3_class(plot, "ggplot")
  modified <- plot + ggplot2::theme_bw()
  expect_s3_class(modified, "ggplot")
})
