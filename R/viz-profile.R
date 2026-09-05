#' Draw a vertical profile from an ocean cube
#'
#' `viz.profile()` selects one variable at exactly one longitude, latitude,
#' and time through [cube_extract()], then draws the stored value-by-depth
#' profile without interpolation or aggregation. NetCDF inputs read only the
#' selected profile rather than materializing the complete cube.
#'
#' @param x A valid `<ocean_cube>`.
#' @param variable A single non-missing variable name present in `x`.
#' @param longitude A single stored longitude. May be `NULL` only when the cube
#'   contains one longitude.
#' @param latitude A single stored latitude. May be `NULL` only when the cube
#'   contains one latitude.
#' @param time A single stored `Date` or `POSIXct` value. May be `NULL` only
#'   when the cube contains one time value.
#' @param depth Optional finite, unique stored depth values. `NULL` retains all
#'   depths. At least two levels must remain after selection.
#' @param limits `NULL` or two finite numeric limits for the value axis in
#'   increasing order. Values outside the limits are squished to the scale
#'   boundary without removing rows.
#' @param na.rm A single non-missing logical value. If `TRUE`, missing values
#'   are removed before plotting; they are never replaced with zero.
#' @param reverse_depth A single non-missing logical value. If `TRUE`, smaller
#'   depth values appear at the top of the plot.
#' @param points A single non-missing logical value. If `TRUE`, points are
#'   drawn over the profile line.
#' @param title,subtitle,caption Optional character scalars used as plot labels.
#'
#' @return A `ggplot` object with the selected variable, longitude, latitude,
#'   time, depth range, and backend recorded in `oceancube_*` attributes.
#'
#' @details
#' Longitude, latitude, time, variable, and optional depth selectors use exact
#' stored-value matching. A surface cube or a selection with fewer than two
#' distinct finite depth levels is rejected. Duplicate extracted depths are
#' rejected rather than combined. The input cube is not modified.
#'
#' @export
#' @seealso [viz.map()], [viz.section()], [cube_extract()], [cube_inspect()]
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   values <- array(c(18, 16, 13, 9), dim = c(1, 1, 4, 1, 1))
#'   cube <- ocean_cube(
#'     lon = -79, lat = -11, depth = c(0, 25, 50, 100),
#'     time = as.Date("2020-01-01"), data = values,
#'     vars = "temperature", units = "degC"
#'   )
#'   viz.profile(cube, "temperature")
#' }
viz.profile <- function(
    x,
    variable,
    longitude = NULL,
    latitude = NULL,
    time = NULL,
    depth = NULL,
    limits = NULL,
    na.rm = TRUE,
    reverse_depth = TRUE,
    points = TRUE,
    title = NULL,
    subtitle = NULL,
    caption = NULL) {
  prepared <- .viz_prepare_profile(
    x = x, variable = variable, longitude = longitude, latitude = latitude,
    time = time, depth = depth, limits = limits, na.rm = na.rm,
    reverse_depth = reverse_depth, points = points, title = title,
    subtitle = subtitle, caption = caption
  )
  .viz_render_ggplot(prepared)
}

.viz_prepare_profile <- function(
    x,
    variable,
    longitude = NULL,
    latitude = NULL,
    time = NULL,
    depth = NULL,
    limits = NULL,
    na.rm = TRUE,
    reverse_depth = TRUE,
    points = TRUE,
    title = NULL,
    subtitle = NULL,
    caption = NULL) {
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
      abort_viz(paste0(
        "`", argument,
        "` must be NULL or one non-missing character string."
      ))
    }
    invisible(TRUE)
  }
  validate_selector <- function(value, argument) {
    if (!is.null(value) &&
        (!is.numeric(value) || length(value) != 1L ||
           is.na(value) || !is.finite(value))) {
      abort_viz(
        paste0("`", argument, "` must be NULL or one finite stored numeric value."),
        "oceancube_viz_selection_error"
      )
    }
    invisible(TRUE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    abort_viz("Package `ggplot2` is required to use viz.profile().")
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

  validate_selector(longitude, "longitude")
  validate_selector(latitude, "latitude")
  if (is.null(longitude) && length(x$lon) != 1L) {
    abort_viz(
      "`longitude` must select exactly one stored value when the cube contains multiple longitudes.",
      "oceancube_viz_selection_error"
    )
  }
  if (is.null(latitude) && length(x$lat) != 1L) {
    abort_viz(
      "`latitude` must select exactly one stored value when the cube contains multiple latitudes.",
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

  if (length(x$depth) == 1L && is.na(x$depth[[1L]]) && !is.nan(x$depth[[1L]])) {
    abort_viz(
      "A surface cube has no numeric depth axis for a vertical profile.",
      "oceancube_viz_selection_error"
    )
  }
  if (length(x$depth) < 2L) {
    abort_viz(
      "A vertical profile requires at least two depth levels.",
      "oceancube_viz_selection_error"
    )
  }
  if (!is.numeric(x$depth) || any(!is.finite(x$depth)) || anyDuplicated(x$depth)) {
    abort_viz(
      "The cube depth axis must contain finite, unique numeric values.",
      "oceancube_viz_selection_error"
    )
  }
  if (!is.null(depth) &&
      (!is.numeric(depth) || length(depth) < 2L || any(!is.finite(depth)) ||
         anyDuplicated(depth))) {
    abort_viz(
      "`depth` must be NULL or at least two finite, unique stored numeric values.",
      "oceancube_viz_selection_error"
    )
  }

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

  extracted <- tryCatch(
    cube_extract(
      x,
      longitude = longitude,
      latitude = latitude,
      depth = depth,
      time = time,
      variable = variable,
      by = "value",
      match = "exact",
      mode = "profile",
      format = "long"
    ),
    error = function(error) {
      abort_viz(
        paste0("Could not select the requested vertical profile: ", conditionMessage(error)),
        "oceancube_viz_selection_error",
        parent = error
      )
    }
  )

  if (!is.data.frame(extracted) || nrow(extracted) == 0L) {
    abort_viz("The selected vertical profile is empty.", "oceancube_viz_data_error")
  }
  required_output <- c(
    "longitude", "latitude", "depth", "time", "unit", "value"
  )
  if (!all(required_output %in% names(extracted))) {
    abort_viz(
      "cube_extract() returned an incomplete vertical profile.",
      "oceancube_viz_data_error"
    )
  }
  if (!is.numeric(extracted$value)) {
    abort_viz("The selected profile values must be numeric.", "oceancube_viz_data_error")
  }
  if (length(unique(extracted$longitude)) != 1L ||
      length(unique(extracted$latitude)) != 1L ||
      length(unique(extracted$time)) != 1L) {
    abort_viz(
      "The selected data do not represent exactly one longitude, latitude, and time.",
      "oceancube_viz_selection_error"
    )
  }
  selected_depth <- unique(extracted$depth)
  if (length(selected_depth) < 2L || any(!is.finite(selected_depth)) ||
      anyDuplicated(selected_depth)) {
    abort_viz(
      "The selected profile must contain at least two finite, unique depth levels.",
      "oceancube_viz_selection_error"
    )
  }

  selected_longitude <- unique(extracted$longitude)
  selected_latitude <- unique(extracted$latitude)
  selected_time <- unique(extracted$time)
  backend <- attr(extracted, "oceancube_backend", exact = TRUE)
  profile <- extracted[, c("depth", "value"), drop = FALSE]
  rownames(profile) <- NULL
  if (anyDuplicated(profile$depth)) {
    abort_viz(
      "The selected profile contains duplicate depth rows; aggregation is not performed.",
      "oceancube_viz_data_error"
    )
  }
  profile <- profile[order(profile$depth), , drop = FALSE]
  rownames(profile) <- NULL
  if (isTRUE(na.rm)) {
    profile <- profile[!is.na(profile$value), , drop = FALSE]
    rownames(profile) <- NULL
  }
  if (nrow(profile) == 0L) {
    abort_viz(
      "The selected vertical profile is empty after removing missing values.",
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
  depth_unit <- attr(x$depth, "units", exact = TRUE)
  depth_unit <- as.character(depth_unit)
  depth_label <- if (length(depth_unit) == 1L && !is.na(depth_unit) &&
                     nzchar(depth_unit)) {
    paste0("Depth (", depth_unit, ")")
  } else {
    "Depth"
  }

  roles <- .viz_named_roles(
    x = "value", y = "depth", value = "value", depth = "depth"
  )
  .new_oceancube_viz_data(
    kind = "PROFILE",
    data = profile,
    roles = roles,
    variables = .viz_variable_metadata(x, variable, units),
    coordinates = .viz_coordinate_metadata(
      profile, roles, list(depth = attr(x$depth, "units", exact = TRUE))
    ),
    selection = attr(extracted, "oceancube_selection", exact = TRUE),
    time = .viz_time_metadata(selected_time, time),
    depth = .viz_depth_metadata(selected_depth, reverse_depth,
                                attr(x$depth, "units", exact = TRUE)),
    source_semantics = .viz_source_semantics(x),
    geometry = list(x = "value", y = "depth", value = "value"),
    projection = list(source_crs = NULL, target_crs = NULL,
                      status = "NOT_APPLICABLE"),
    scale = list(classification = "UNSPECIFIED_CONTINUOUS", limits = limits),
    support = list(
      rows = nrow(profile), missing_values = sum(is.na(profile$value)),
      backend = backend, selection_status = "SELECTED"
    ),
    provenance = .viz_private_state(
      attr(extracted, "oceancube_provenance", exact = TRUE)
    ),
    qa = .viz_private_state(attr(extracted, "oceancube_qa", exact = TRUE)),
    renderer_hints = list(
      title = title, subtitle = subtitle, caption = caption, na.rm = na.rm,
      points = points, value_label = value_label, depth_label = depth_label,
      plot_attributes = list(
        oceancube_variable = variable,
        oceancube_longitude = as.numeric(selected_longitude),
        oceancube_latitude = as.numeric(selected_latitude),
        oceancube_time = selected_time,
        oceancube_depth_range = range(selected_depth),
        oceancube_backend = backend
      )
    )
  )
}
