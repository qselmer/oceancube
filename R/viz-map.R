#' Draw a static map from one ocean-cube layer
#'
#' `viz.map()` selects exactly one variable, time, and depth layer through
#' [cube_extract()] and draws its stored grid without interpolation. Regular
#' grids use a raster layer and irregular grids use tiles. Explicit contour
#' styles calculate renderer-only isoline geometry; they never interpolate,
#' regrid, smooth, fill, or replace the prepared scientific values. NetCDF
#' inputs read only the selected layer rather than materializing the complete
#' cube.
#'
#' @param x A valid `<ocean_cube>`.
#' @param variable A single non-missing variable name present in `x`.
#' @param time A single stored `Date` or `POSIXct` value. May be `NULL` only
#'   when the cube has one time value.
#' @param depth A single stored numeric depth. May be `NULL` only for a surface
#'   cube or a cube with one depth value.
#' @param limits `NULL` or two finite numeric fill-scale limits in increasing
#'   order. Values outside the limits are squished to the scale boundary and
#'   rows are not removed.
#' @param na.rm A single non-missing logical value. If `TRUE`, missing cells are
#'   removed before plotting.
#' @param coastline Optional coastline supplied as an `sf`/`sfc` object or a
#'   data frame with `longitude`, `latitude`, and `group` columns. Coordinates
#'   and CRS are used as supplied and are not transformed.
#' @param title,subtitle,caption Optional character scalars used as plot labels.
#' @param style Map rendering style. `"field"` preserves the historical direct
#'   raster/tile rendering. `"contour"` draws display-only isolines and
#'   `"field_contour"` overlays those isolines on the direct field.
#'   `"filled_contour"` is reserved but currently errors because its missing-
#'   support and continuous-scale contract is not yet certified.
#' @param scale_class Scientific colour-scale class. The historical
#'   `"unspecified_continuous"` default preserves the existing scale exactly.
#'   `"sequential"` uses deterministic viridis option D. `"diverging"` uses a
#'   deterministic base-R HCL palette and requires an explicit `center`.
#' @param center `NULL` or one finite scientifically meaningful centre. It is
#'   required for `scale_class = "diverging"`, must lie strictly inside the
#'   effective display range, and is invalid for other scale classes. Zero is
#'   never assumed.
#' @param contour_breaks `NULL` or at least one finite, strictly increasing
#'   contour level. When `NULL`, explicit contour styles use deterministic
#'   `pretty()` breaks from the finite display range with `n = 7`.
#' @param longitude_display Display-only longitude convention: `"source"`
#'   preserves stored coordinates, `"neg180_180"` wraps to [-180, 180), and
#'   `"zero_360"` wraps to [0, 360). Scientific coordinates, selection,
#'   provenance, and QA are unchanged. A wrap that introduces an internal
#'   dateline discontinuity fails explicitly.
#'
#' @return A `ggplot` object with selected variable, time, depth, and backend
#'   recorded in `oceancube_*` attributes.
#' @export
#' @importFrom rlang .data
#' @seealso [cube_validate()], [cube_extract()], [cube_slice()]
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   values <- array(1:6, dim = c(3, 2, 1, 1, 1))
#'   cube <- ocean_cube(
#'     lon = c(-80, -79, -78), lat = c(-12, -11), depth = 0,
#'     time = as.Date("2020-01-01"), data = values,
#'     vars = "temperature", units = "degC"
#'   )
#'   viz.map(cube, "temperature", title = "Surface temperature")
#'   viz.map(
#'     cube, "temperature", style = "field_contour",
#'     scale_class = "sequential", contour_breaks = c(2, 4)
#'   )
#'
#'   signed <- cube
#'   signed$data <- signed$data - 3.5
#'   viz.map(signed, "temperature", scale_class = "diverging", center = 0)
#' }
viz.map <- function(x, variable, time = NULL, depth = NULL, limits = NULL,
                    na.rm = TRUE, coastline = NULL, title = NULL,
                    subtitle = NULL, caption = NULL,
                    style = c("field", "contour", "field_contour",
                              "filled_contour"),
                    scale_class = c("unspecified_continuous", "sequential",
                                    "diverging"),
                    center = NULL, contour_breaks = NULL,
                    longitude_display = c("source", "neg180_180", "zero_360")) {
  prepared <- .viz_prepare_map(
    x = x, variable = variable, time = time, depth = depth, limits = limits,
    na.rm = na.rm, coastline = coastline, title = title,
    subtitle = subtitle, caption = caption, style = style,
    scale_class = scale_class, center = center,
    contour_breaks = contour_breaks, longitude_display = longitude_display
  )
  .viz_render_ggplot(prepared)
}

.viz_prepare_map <- function(x, variable, time = NULL, depth = NULL,
                             limits = NULL, na.rm = TRUE, coastline = NULL,
                             title = NULL, subtitle = NULL, caption = NULL,
                             style = c("field", "contour", "field_contour",
                                       "filled_contour"),
                             scale_class = c("unspecified_continuous", "sequential",
                                             "diverging"),
                             center = NULL, contour_breaks = NULL,
                             longitude_display = c("source", "neg180_180",
                                                   "zero_360")) {
  cube_validate(x, strict = TRUE)

  abort_viz <- function(message, class = "oceancube_viz_error", parent = NULL) {
    rlang::abort(
      message,
      class = unique(c(class, "oceancube_viz_error")),
      parent = parent
    )
  }
  validate_label <- function(value, argument) {
    if (!is.null(value) &&
        (!is.character(value) || length(value) != 1L || is.na(value))) {
      abort_viz(paste0("`", argument, "` must be NULL or one non-missing character string."))
    }
    invisible(TRUE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    abort_viz("Package `ggplot2` is required to use viz.map().")
  }
  if (!is.character(variable) || length(variable) != 1L ||
      is.na(variable) || !nzchar(variable)) {
    abort_viz(
      "`variable` must be one non-empty, non-missing character string.",
      "oceancube_viz_selection_error"
    )
  }
  if (!variable %in% x$vars) {
    abort_viz(
      paste0(
        "Variable `", variable, "` is not present in the cube. Available variables: ",
        paste0("`", x$vars, "`", collapse = ", "), "."
      ),
      "oceancube_viz_selection_error"
    )
  }

  if (is.null(time)) {
    if (length(x$time) != 1L) {
      abort_viz(
        "`time` must select exactly one stored value when the cube contains multiple times.",
        "oceancube_viz_selection_error"
      )
    }
  } else if (length(time) != 1L || anyNA(time)) {
    abort_viz(
      "`time` must be NULL or one non-missing stored time value.",
      "oceancube_viz_selection_error"
    )
  }

  if (is.null(depth)) {
    if (length(x$depth) != 1L) {
      abort_viz(
        "`depth` must select exactly one stored value when the cube contains multiple depths.",
        "oceancube_viz_selection_error"
      )
    }
  } else if (!is.numeric(depth) || length(depth) != 1L ||
             is.nan(depth) || (!is.na(depth) && !is.finite(depth))) {
    abort_viz(
      "`depth` must be NULL or one finite stored numeric value; use NA_real_ only for a surface cube.",
      "oceancube_viz_selection_error"
    )
  }

  if (!is.null(limits) &&
      (!is.numeric(limits) || !is.null(dim(limits)) || length(limits) != 2L ||
         any(!is.finite(limits)) || limits[[1L]] >= limits[[2L]])) {
    abort_viz("`limits` must be NULL or two finite numeric values with min < max.")
  }
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    abort_viz("`na.rm` must be one non-missing logical value.")
  }
  validate_label(title, "title")
  validate_label(subtitle, "subtitle")
  validate_label(caption, "caption")

  style <- match.arg(style)
  scale_class <- match.arg(scale_class)
  longitude_display <- match.arg(longitude_display)
  if (identical(style, "filled_contour")) {
    abort_viz(
      paste0(
        "`style = \"filled_contour\"` is reserved but deferred: its ",
        "continuous-scale and missing-support semantics are not certified in D2B."
      ),
      "oceancube_viz_style_error"
    )
  }
  contour_style <- style %in% c("contour", "field_contour")
  if (!is.null(contour_breaks)) {
    if (!is.numeric(contour_breaks) || !is.null(dim(contour_breaks)) ||
        !length(contour_breaks) || any(!is.finite(contour_breaks)) ||
        any(diff(contour_breaks) <= 0)) {
      abort_viz(
        "`contour_breaks` must be NULL or finite, strictly increasing numeric levels.",
        "oceancube_viz_style_error"
      )
    }
    if (!contour_style) {
      abort_viz(
        "`contour_breaks` is available only for an explicit contour style.",
        "oceancube_viz_style_error"
      )
    }
  }

  coastline_type <- "none"
  if (!is.null(coastline)) {
    if (inherits(coastline, "sf") || inherits(coastline, "sfc")) {
      if (!requireNamespace("sf", quietly = TRUE)) {
        abort_viz(
          "Package `sf` is required to render an sf/sfc coastline.",
          "oceancube_viz_dependency_error"
        )
      }
      coastline_type <- "sf"
    } else if (is.data.frame(coastline)) {
      required <- c("longitude", "latitude", "group")
      missing_columns <- setdiff(required, names(coastline))
      coastline_ok <- length(missing_columns) == 0L &&
        is.numeric(coastline$longitude) && is.numeric(coastline$latitude) &&
        all(is.finite(coastline$longitude)) && all(is.finite(coastline$latitude)) &&
        !anyNA(coastline$group)
      if (!coastline_ok) {
        abort_viz(
          "A coastline data frame must contain finite numeric `longitude` and `latitude` plus non-missing `group`.",
          "oceancube_viz_data_error"
        )
      }
      coastline_type <- "data.frame"
    } else {
      abort_viz(
        "`coastline` must be NULL, an sf/sfc object, or a data frame with longitude, latitude, and group.",
        "oceancube_viz_data_error"
      )
    }
  }

  extracted <- tryCatch(
    cube_extract(
      x,
      depth = depth,
      time = time,
      variable = variable,
      by = "value",
      match = "exact",
      mode = "table",
      format = "long"
    ),
    error = function(error) {
      abort_viz(
        paste0("Could not select the requested map layer: ", conditionMessage(error)),
        "oceancube_viz_selection_error",
        parent = error
      )
    }
  )

  if (!is.data.frame(extracted) || nrow(extracted) == 0L) {
    abort_viz("The selected map layer is empty.", "oceancube_viz_data_error")
  }
  required_output <- c("longitude", "latitude", "depth", "time", "unit", "value")
  if (!all(required_output %in% names(extracted))) {
    abort_viz(
      "cube_extract() returned an incomplete map layer.",
      "oceancube_viz_data_error"
    )
  }
  if (!is.numeric(extracted$value)) {
    abort_viz("The selected map values must be numeric.", "oceancube_viz_data_error")
  }
  if (length(unique(extracted$time)) != 1L || length(unique(extracted$depth)) != 1L) {
    abort_viz(
      "The selected data do not represent exactly one time and depth layer.",
      "oceancube_viz_selection_error"
    )
  }

  selected_time <- unique(extracted$time)
  selected_depth <- unique(extracted$depth)
  backend <- attr(extracted, "oceancube_backend", exact = TRUE)
  layer <- extracted[, c("longitude", "latitude", "value"), drop = FALSE]
  rownames(layer) <- NULL
  if (anyDuplicated(layer[c("longitude", "latitude")])) {
    abort_viz(
      "The selected layer contains duplicate longitude x latitude cells; aggregation is not performed.",
      "oceancube_viz_data_error"
    )
  }
  if (isTRUE(na.rm)) {
    layer <- layer[!is.na(layer$value), , drop = FALSE]
    rownames(layer) <- NULL
  }
  if (nrow(layer) == 0L) {
    abort_viz("The selected map layer is empty after removing missing values.", "oceancube_viz_data_error")
  }

  finite_values <- layer$value[is.finite(layer$value)]
  if (!length(finite_values) &&
      (contour_style || identical(scale_class, "diverging"))) {
    abort_viz(
      "The requested contour or diverging map requires finite values.",
      "oceancube_viz_data_error"
    )
  }
  effective_range <- if (!is.null(limits)) {
    limits
  } else if (length(finite_values)) {
    range(finite_values)
  } else {
    c(NA_real_, NA_real_)
  }
  scale <- .viz_map_scale_spec(
    classification = toupper(scale_class), limits = limits,
    centre = center, effective_range = effective_range
  )
  contour_spec <- .viz_map_contour_spec(
    values = finite_values, limits = limits, breaks = contour_breaks,
    required = contour_style
  )

  regular_axis <- function(values) {
    values <- sort(unique(as.numeric(values)))
    if (length(values) <= 2L) return(TRUE)
    differences <- diff(values)
    reference <- differences[[1L]]
    tolerance <- sqrt(.Machine$double.eps) *
      max(c(1, abs(reference), abs(differences)))
    all(abs(differences - reference) <= tolerance)
  }
  regular_grid <- regular_axis(layer$longitude) && regular_axis(layer$latitude)

  units <- unique(as.character(extracted$unit))
  units <- units[!is.na(units) & nzchar(units)]
  scale_title <- if (length(units) == 1L) {
    paste0(variable, " (", units, ")")
  } else {
    variable
  }

  roles <- .viz_named_roles(
    x = "longitude", y = "latitude", value = "value",
    longitude = "longitude", latitude = "latitude"
  )
  .new_oceancube_viz_data(
    kind = "MAP_LAYER",
    data = layer,
    roles = roles,
    variables = .viz_variable_metadata(x, variable, units),
    coordinates = .viz_coordinate_metadata(
      layer, roles, list(longitude = attr(x$lon, "units", exact = TRUE),
                         latitude = attr(x$lat, "units", exact = TRUE))
    ),
    selection = attr(extracted, "oceancube_selection", exact = TRUE),
    time = .viz_time_metadata(selected_time, time),
    depth = .viz_depth_metadata(
      selected_depth, FALSE, attr(x$depth, "units", exact = TRUE)
    ),
    source_semantics = .viz_source_semantics(x),
    geometry = list(
      x = "longitude", y = "latitude", value = "value",
      regular_grid = regular_grid
    ),
    projection = list(source_crs = NULL, target_crs = NULL, status = "UNKNOWN"),
    scale = scale,
    support = list(
      rows = nrow(layer), missing_values = sum(is.na(layer$value)),
      backend = backend, selection_status = "SELECTED",
      geometry = "STORED_CENTRES",
      scientific_bounds = NULL,
      explicit_cell_bounds_runtime = "DEFERRED_NOT_CERTIFIED_D2B",
      display_footprint = .viz_map_display_footprint(
        layer$longitude, layer$latitude
      )
    ),
    provenance = .viz_private_state(
      attr(extracted, "oceancube_provenance", exact = TRUE)
    ),
    qa = .viz_private_state(attr(extracted, "oceancube_qa", exact = TRUE)),
    renderer_hints = list(
      title = title, subtitle = subtitle, caption = caption,
      na.rm = na.rm, coastline = coastline, coastline_type = coastline_type,
      value_label = scale_title, map_style = toupper(style),
      contour_breaks = contour_spec$breaks,
      contour_break_rule = contour_spec$rule,
      longitude_display = toupper(longitude_display),
      plot_attributes = list(
        oceancube_variable = variable,
        oceancube_time = selected_time,
        oceancube_depth = selected_depth,
        oceancube_backend = backend,
        oceancube_map_style = toupper(style),
        oceancube_scale_class = scale$classification,
        oceancube_longitude_display = toupper(longitude_display)
      )
    )
  )
}
