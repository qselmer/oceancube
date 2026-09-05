#' Draw a static transect from an ocean cube
#'
#' `viz.transect()` delegates ordered path matching and scientific selection to
#' [cube_transect()], then draws either a distance-by-depth section or a
#' distance-by-value horizontal transect. It never interpolates values or
#' densifies the supplied path.
#'
#' @param x A valid `<ocean_cube>` using the memory or NetCDF backend.
#' @param path An ordered path accepted by [cube_transect()]. It is passed
#'   unchanged; `sf`/`sfc` paths are rejected by the extraction contract.
#' @param variable One non-empty, non-missing variable name.
#' @param time `NULL` for a singleton time axis or one stored time value.
#' @param depth Optional stored depth selector passed directly to
#'   [cube_transect()]. A section requires at least two depths; a horizontal
#'   transect requires exactly one physical depth or the surface depth.
#' @param lon_col,lat_col Column names containing path longitude and latitude.
#' @param id_col Optional path column used as point identifiers.
#' @param match Matching method passed explicitly to [cube_transect()]. The new
#'   visualization API defaults to exact matching. Nearest matching may warn
#'   when no scientific `tolerance` is supplied.
#' @param tolerance Optional nearest-matching tolerance passed unchanged to
#'   [cube_transect()].
#' @param mode Plot mode. `"auto"` chooses a section for two or more depths and
#'   a horizontal plot for one physical depth or a surface cube.
#' @param distance Cumulative distance used on the x axis. `"requested"`, the
#'   default, follows the user path; `"matched"` follows the matched grid path.
#'   Point-to-cell `match_distance_km` is diagnostic and is never the x axis.
#' @param limits `NULL` or two finite numeric scale limits in increasing order.
#'   Values outside the limits are squished to the boundary without removing
#'   observations.
#' @param na.rm A single non-missing logical value. Missing values are removed
#'   before plotting when `TRUE`; they are never replaced or interpolated.
#' @param reverse_depth A single non-missing logical value. In section mode,
#'   `TRUE` places the surface at the top and greater depths downward.
#' @param points A single non-missing logical value. In horizontal mode, `TRUE`
#'   adds points to the line; it does not alter a section plot.
#' @param title,subtitle,caption Optional character scalars used as plot labels.
#'
#' @return A `ggplot` object with the resolved variable, time, mode, distance,
#'   depth range, backend, matching settings, maximum point-to-cell distance,
#'   and path point count recorded in `oceancube_*` attributes.
#'
#' @details
#' `cube_transect()` is the sole data-selection and coordinate-matching layer.
#' `viz.transect()` does not recalculate distances, access a backend reader,
#' aggregate duplicate cells, interpolate, or reorder the requested path.
#' Regular distance-by-depth grids use `geom_raster()` and irregular grids use
#' `geom_tile()`. Repeated path positions that create duplicate distance-depth
#' cells cannot define a two-dimensional section and are rejected; remove the
#' zero-length segment or choose the alternative distance metric when valid.
#' NetCDF extraction remains selective through [cube_transect()].
#'
#' @export
#' @seealso [cube_transect()], [viz.map()], [viz.section()], [viz.profile()]
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   values <- array(1:12, dim = c(3, 1, 4, 1, 1))
#'   cube <- ocean_cube(
#'     lon = c(-80, -79, -78), lat = -11, depth = c(0, 25, 50, 100),
#'     time = as.Date("2020-01-01"), data = values,
#'     vars = "temperature", units = "degC"
#'   )
#'   path <- data.frame(
#'     longitude = c(-80, -79, -78), latitude = rep(-11, 3)
#'   )
#'   viz.transect(cube, path, "temperature", depth = 0)
#'   viz.transect(cube, path, "temperature", mode = "section")
#' }
viz.transect <- function(
    x,
    path,
    variable,
    time = NULL,
    depth = NULL,
    lon_col = "longitude",
    lat_col = "latitude",
    id_col = NULL,
    match = c("exact", "nearest"),
    tolerance = NULL,
    mode = c("auto", "section", "horizontal"),
    distance = c("requested", "matched"),
    limits = NULL,
    na.rm = TRUE,
    reverse_depth = TRUE,
    points = TRUE,
    title = NULL,
    subtitle = NULL,
    caption = NULL) {
  prepared <- .viz_prepare_transect(
    x = x, path = path, variable = variable, time = time, depth = depth,
    lon_col = lon_col, lat_col = lat_col, id_col = id_col, match = match,
    tolerance = tolerance, mode = mode, distance = distance, limits = limits,
    na.rm = na.rm, reverse_depth = reverse_depth, points = points,
    title = title, subtitle = subtitle, caption = caption
  )
  .viz_render_ggplot(prepared)
}

.viz_prepare_transect <- function(
    x,
    path,
    variable,
    time = NULL,
    depth = NULL,
    lon_col = "longitude",
    lat_col = "latitude",
    id_col = NULL,
    match = c("exact", "nearest"),
    tolerance = NULL,
    mode = c("auto", "section", "horizontal"),
    distance = c("requested", "matched"),
    limits = NULL,
    na.rm = TRUE,
    reverse_depth = TRUE,
    points = TRUE,
    title = NULL,
    subtitle = NULL,
    caption = NULL) {
  abort_viz <- function(message, class = "oceancube_viz_error", parent = NULL) {
    rlang::abort(
      message,
      class = unique(c(class, "oceancube_viz_error")),
      parent = parent
    )
  }
  match_choice <- function(value, argument, choices) {
    tryCatch(
      base::match.arg(value, choices),
      error = function(error) {
        abort_viz(
          paste0(
            "`", argument, "` must be one of ",
            paste0("\"", choices, "\"", collapse = ", "), "."
          ),
          "oceancube_viz_selection_error",
          parent = error
        )
      }
    )
  }
  validate_label <- function(value, argument) {
    if (!is.null(value) &&
        (!is.character(value) || length(value) != 1L || is.na(value))) {
      abort_viz(paste0(
        "`", argument,
        "` must be NULL or one non-missing character string."
      ))
    }
    invisible(TRUE)
  }
  regular_axis <- function(values) {
    values <- sort(unique(as.numeric(values)))
    if (length(values) <= 2L) return(TRUE)
    differences <- diff(values)
    reference <- differences[[1L]]
    tolerance <- sqrt(.Machine$double.eps) *
      max(c(1, abs(reference), abs(differences)))
    all(abs(differences - reference) <= tolerance)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    abort_viz("Package `ggplot2` is required to use viz.transect().")
  }
  if (!is.character(variable) || length(variable) != 1L ||
      is.na(variable) || !nzchar(variable)) {
    abort_viz(
      "`variable` must be one non-empty, non-missing character string.",
      "oceancube_viz_selection_error"
    )
  }
  match <- match_choice(match, "match", c("exact", "nearest"))
  mode <- match_choice(mode, "mode", c("auto", "section", "horizontal"))
  distance <- match_choice(
    distance, "distance", c("requested", "matched")
  )
  if (!is.null(limits) &&
      (!is.numeric(limits) || !is.null(dim(limits)) || length(limits) != 2L ||
         any(!is.finite(limits)) || limits[[1L]] >= limits[[2L]])) {
    abort_viz("`limits` must be NULL or two finite numeric values with min < max.")
  }
  for (flag in c("na.rm", "reverse_depth", "points")) {
    value <- get(flag, inherits = FALSE)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      abort_viz(paste0("`", flag, "` must be one non-missing logical value."))
    }
  }
  validate_label(title, "title")
  validate_label(subtitle, "subtitle")
  validate_label(caption, "caption")

  extracted <- cube_transect(
    x = x,
    path = path,
    lon_col = lon_col,
    lat_col = lat_col,
    id_col = id_col,
    depth = depth,
    time = time,
    variable = variable,
    by = "value",
    match = match,
    tolerance = tolerance,
    mode = mode,
    format = "long",
    keep_index = FALSE
  )

  if (!is.data.frame(extracted) || nrow(extracted) == 0L) {
    abort_viz("cube_transect() returned an empty transect.",
              "oceancube_viz_data_error")
  }
  required <- c(
    "point_id", "point_order", "longitude_requested",
    "latitude_requested", "longitude", "latitude", "match_distance_km",
    "requested_distance_km", "matched_distance_km", "depth", "time",
    "variable", "unit", "value"
  )
  if (!all(required %in% names(extracted))) {
    abort_viz("cube_transect() returned incomplete transect data.",
              "oceancube_viz_data_error")
  }
  if (!is.numeric(extracted$value)) {
    abort_viz("The selected transect values must be numeric.",
              "oceancube_viz_data_error")
  }
  distance_columns <- c(
    "requested_distance_km", "matched_distance_km", "match_distance_km"
  )
  bad_distance <- vapply(
    extracted[distance_columns],
    function(value) !is.numeric(value) || anyNA(value) || any(!is.finite(value)),
    logical(1)
  )
  if (any(bad_distance)) {
    abort_viz(
      "Transect distance columns must be finite, non-missing numeric values.",
      "oceancube_viz_data_error"
    )
  }
  if (!is.numeric(extracted$point_order) || anyNA(extracted$point_order) ||
      any(!is.finite(extracted$point_order))) {
    abort_viz("`point_order` must contain finite numeric positions.",
              "oceancube_viz_data_error")
  }
  selected_variables <- unique(as.character(extracted$variable))
  if (length(selected_variables) != 1L || is.na(selected_variables) ||
      !identical(selected_variables, variable)) {
    abort_viz("The transect must contain exactly the requested variable.",
              "oceancube_viz_selection_error")
  }
  selected_time <- unique(extracted$time)
  if (length(selected_time) != 1L || anyNA(selected_time)) {
    abort_viz("The transect must contain exactly one non-missing time.",
              "oceancube_viz_selection_error")
  }

  selected_depth <- unique(extracted$depth)
  surface <- length(selected_depth) == 1L && is.na(selected_depth[[1L]]) &&
    !is.nan(selected_depth[[1L]])
  if (!is.numeric(selected_depth) ||
      (!surface && (anyNA(selected_depth) || any(!is.finite(selected_depth))))) {
    abort_viz(
      "The selected depths must be finite numeric values or one surface NA depth.",
      "oceancube_viz_data_error"
    )
  }
  point_orders <- sort(unique(extracted$point_order))
  if (length(point_orders) < 2L) {
    abort_viz(
      "A transect visualization requires at least two ordered path points; use viz.profile() for one point.",
      "oceancube_viz_selection_error"
    )
  }
  resolved_mode <- if (identical(mode, "auto")) {
    if (isTRUE(surface) || length(selected_depth) == 1L) {
      "horizontal"
    } else {
      "section"
    }
  } else {
    mode
  }
  if (identical(resolved_mode, "section") &&
      (isTRUE(surface) || length(selected_depth) < 2L)) {
    abort_viz("Section mode requires at least two finite, unique depths.",
              "oceancube_viz_selection_error")
  }
  if (identical(resolved_mode, "horizontal") &&
      !(isTRUE(surface) || length(selected_depth) == 1L)) {
    abort_viz(
      "Horizontal mode requires exactly one physical depth or a surface cube; select `depth` explicitly.",
      "oceancube_viz_selection_error"
    )
  }

  distance_column <- paste0(distance, "_distance_km")
  distances_by_point <- lapply(
    point_orders,
    function(point) unique(extracted[[distance_column]][
      extracted$point_order == point
    ])
  )
  if (any(lengths(distances_by_point) != 1L)) {
    abort_viz("Each path point must have one cumulative plotting distance.",
              "oceancube_viz_data_error")
  }
  path_distances <- vapply(distances_by_point, `[[`, numeric(1), 1L)
  if (any(diff(path_distances) < 0)) {
    abort_viz("Cumulative transect distance must be monotonic non-decreasing in point_order.",
              "oceancube_viz_data_error")
  }

  backend <- attr(extracted, "oceancube_backend", exact = TRUE)
  if (identical(resolved_mode, "section")) {
    if (anyDuplicated(extracted[c("point_order", "depth")]) ||
        nrow(extracted) != length(point_orders) * length(selected_depth)) {
      abort_viz(
        "Section data must contain exactly one row per point_order x depth; aggregation is not performed.",
        "oceancube_viz_data_error"
      )
    }
    if (anyDuplicated(extracted[c(distance_column, "depth")])) {
      abort_viz(
        paste(
          "The path contains non-unique distance positions for a two-dimensional section.",
          "Remove zero-length segments or choose the alternative distance metric when appropriate; aggregation and jitter are not performed."
        ),
        "oceancube_viz_data_error"
      )
    }
    regular_grid <- regular_axis(extracted[[distance_column]]) &&
      regular_axis(extracted$depth)
    depth_order <- match(extracted$depth, selected_depth)
    layer <- extracted[order(extracted$point_order, depth_order),
                       c("point_order", distance_column, "depth", "value"),
                       drop = FALSE]
  } else {
    if (anyDuplicated(extracted$point_order) ||
        nrow(extracted) != length(point_orders)) {
      abort_viz(
        "Horizontal data must contain exactly one row per point_order; aggregation is not performed.",
        "oceancube_viz_data_error"
      )
    }
    layer <- extracted[order(extracted$point_order),
                       c("point_order", distance_column, "value"),
                       drop = FALSE]
  }
  rownames(layer) <- NULL
  if (isTRUE(na.rm)) {
    layer <- layer[!is.na(layer$value), , drop = FALSE]
    rownames(layer) <- NULL
  }
  if (nrow(layer) == 0L) {
    abort_viz(
      "The selected transect is empty after removing missing values.",
      "oceancube_viz_data_error"
    )
  }

  units <- unique(as.character(extracted$unit))
  units <- units[!is.na(units) & nzchar(units)]
  value_label <- if (length(units) == 1L) {
    paste0(variable, " (", units, ")")
  } else {
    variable
  }
  depth_unit <- as.character(attr(x$depth, "units", exact = TRUE))
  depth_label <- if (length(depth_unit) == 1L && !is.na(depth_unit) &&
                     nzchar(depth_unit)) {
    paste0("Depth (", depth_unit, ")")
  } else {
    "Depth"
  }
  distance_label <- if (identical(distance, "requested")) {
    "Distance along requested transect (km)"
  } else {
    "Distance along matched grid path (km)"
  }

  depth_range <- if (isTRUE(surface)) {
    c(NA_real_, NA_real_)
  } else {
    range(selected_depth)
  }
  roles <- .viz_named_roles(
    x = distance_column,
    y = if (identical(resolved_mode, "section")) "depth" else "value",
    value = "value",
    group = "point_order",
    depth = if (identical(resolved_mode, "section")) "depth" else NULL,
    distance = distance_column
  )
  kind <- if (identical(resolved_mode, "section")) {
    "TRANSECT_SECTION"
  } else {
    "TRANSECT_LINE"
  }
  .new_oceancube_viz_data(
    kind = kind,
    data = layer,
    roles = roles,
    variables = .viz_variable_metadata(x, variable, units),
    coordinates = .viz_coordinate_metadata(
      layer, roles,
      list(distance = "km", depth = attr(x$depth, "units", exact = TRUE))
    ),
    selection = attr(extracted, "oceancube_selection", exact = TRUE),
    time = .viz_time_metadata(selected_time, time),
    depth = .viz_depth_metadata(selected_depth, reverse_depth,
                                attr(x$depth, "units", exact = TRUE)),
    source_semantics = .viz_source_semantics(x),
    geometry = list(
      x = distance_column,
      y = if (identical(resolved_mode, "section")) "depth" else "value",
      value = "value", distance_column = distance_column,
      distance_semantics = distance, regular_grid = if (exists("regular_grid")) {
        regular_grid
      } else {
        FALSE
      }
    ),
    projection = list(source_crs = NULL, target_crs = NULL,
                      status = "NOT_APPLICABLE"),
    scale = list(classification = "UNSPECIFIED_CONTINUOUS", limits = limits),
    support = list(
      rows = nrow(layer), missing_values = sum(is.na(layer$value)),
      backend = backend, selection_status = "SELECTED",
      maximum_match_distance_km = max(extracted$match_distance_km),
      path_points = length(point_orders)
    ),
    provenance = .viz_private_state(
      attr(extracted, "oceancube_provenance", exact = TRUE)
    ),
    qa = .viz_private_state(attr(extracted, "oceancube_qa", exact = TRUE)),
    renderer_hints = list(
      title = title, subtitle = subtitle, caption = caption, na.rm = na.rm,
      points = points, value_label = value_label, depth_label = depth_label,
      distance_label = distance_label,
      plot_attributes = list(
        oceancube_variable = variable,
        oceancube_time = selected_time,
        oceancube_mode = resolved_mode,
        oceancube_distance = distance,
        oceancube_depth_range = depth_range,
        oceancube_backend = backend,
        oceancube_match = match,
        oceancube_tolerance = tolerance,
        oceancube_max_match_distance_km = max(extracted$match_distance_km),
        oceancube_path_points = length(point_orders)
      )
    )
  )
}
