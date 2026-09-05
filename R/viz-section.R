#' Draw a static vertical section from an ocean cube
#'
#' `viz.section()` selects one vertical plane through [cube_extract()] and
#' draws the stored horizontal-by-depth grid without interpolation or
#' aggregation. Longitude-depth sections fix one latitude; latitude-depth
#' sections fix one longitude. NetCDF inputs read only the selected plane.
#'
#' @param x A valid `<ocean_cube>`.
#' @param variable A single non-missing variable name present in `x`.
#' @param section Section orientation: `"longitude-depth"` or
#'   `"latitude-depth"`.
#' @param time A single stored `Date` or `POSIXct` value. May be `NULL` only
#'   when the cube has one time value.
#' @param longitude A single stored longitude for a latitude-depth section.
#'   Must be `NULL` for a longitude-depth section and may be `NULL` for a
#'   latitude-depth section only when the cube has one longitude.
#' @param latitude A single stored latitude for a longitude-depth section.
#'   Must be `NULL` for a latitude-depth section and may be `NULL` for a
#'   longitude-depth section only when the cube has one latitude.
#' @param depth Optional finite, unique stored depth values. `NULL` retains all
#'   depths. A section must contain at least two depth levels.
#' @param limits `NULL` or two finite numeric fill-scale limits in increasing
#'   order. Values outside the limits are squished to the scale boundary and
#'   rows are not removed.
#' @param na.rm A single non-missing logical value. If `TRUE`, missing cells are
#'   removed before plotting.
#' @param reverse_depth A single non-missing logical value. If `TRUE`, smaller
#'   depth values appear at the top of the plot.
#' @param title,subtitle,caption Optional character scalars used as plot labels.
#'
#' @return A `ggplot` object with the selected variable, time, section, fixed
#'   coordinate, depth range, and backend recorded in `oceancube_*` attributes.
#' @export
#' @seealso [cube_validate()], [cube_extract()], [viz.map()]
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   values <- array(1:12, dim = c(3, 1, 4, 1, 1))
#'   cube <- ocean_cube(
#'     lon = c(-80, -79, -78), lat = -11, depth = c(0, 25, 50, 100),
#'     time = as.Date("2020-01-01"), data = values,
#'     vars = "temperature", units = "degC"
#'   )
#'   viz.section(cube, "temperature")
#' }
viz.section <- function(
    x,
    variable,
    section = c("longitude-depth", "latitude-depth"),
    time = NULL,
    longitude = NULL,
    latitude = NULL,
    depth = NULL,
    limits = NULL,
    na.rm = TRUE,
    reverse_depth = TRUE,
    title = NULL,
    subtitle = NULL,
    caption = NULL) {
  prepared <- .viz_prepare_section(
    x = x, variable = variable, section = section, time = time,
    longitude = longitude, latitude = latitude, depth = depth,
    limits = limits, na.rm = na.rm, reverse_depth = reverse_depth,
    title = title, subtitle = subtitle, caption = caption
  )
  .viz_render_ggplot(prepared)
}

.viz_prepare_section <- function(
    x,
    variable,
    section = c("longitude-depth", "latitude-depth"),
    time = NULL,
    longitude = NULL,
    latitude = NULL,
    depth = NULL,
    limits = NULL,
    na.rm = TRUE,
    reverse_depth = TRUE,
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
      abort_viz(paste0("`", argument, "` must be NULL or one non-missing character string."))
    }
    invisible(TRUE)
  }
  validate_fixed <- function(value, argument) {
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
    abort_viz("Package `ggplot2` is required to use viz.section().")
  }
  section <- tryCatch(
    base::match.arg(section),
    error = function(error) {
      abort_viz(
        "`section` must be either \"longitude-depth\" or \"latitude-depth\".",
        "oceancube_viz_selection_error",
        parent = error
      )
    }
  )
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

  if (length(x$depth) == 1L && is.na(x$depth[[1L]]) && !is.nan(x$depth[[1L]])) {
    abort_viz(
      "A surface cube has no numeric depth axis for a vertical section.",
      "oceancube_viz_selection_error"
    )
  }
  if (length(x$depth) < 2L) {
    abort_viz(
      "A vertical section requires at least two depth levels.",
      "oceancube_viz_selection_error"
    )
  }
  if (!is.numeric(x$depth) || any(!is.finite(x$depth)) || anyDuplicated(x$depth)) {
    abort_viz(
      "The cube depth axis must contain finite, unique numeric values.",
      "oceancube_viz_selection_error"
    )
  }
  if (!is.null(depth)) {
    if (!is.numeric(depth) || length(depth) < 2L || any(!is.finite(depth)) ||
        anyDuplicated(depth)) {
      abort_viz(
        "`depth` must be NULL or at least two finite, unique stored numeric values.",
        "oceancube_viz_selection_error"
      )
    }
  }

  validate_fixed(longitude, "longitude")
  validate_fixed(latitude, "latitude")
  if (identical(section, "longitude-depth")) {
    if (!is.null(longitude)) {
      abort_viz(
        "`longitude` must be NULL because longitude varies in a longitude-depth section.",
        "oceancube_viz_selection_error"
      )
    }
    if (is.null(latitude) && length(x$lat) != 1L) {
      abort_viz(
        "`latitude` must select exactly one stored value for a longitude-depth section.",
        "oceancube_viz_selection_error"
      )
    }
    horizontal <- "longitude"
    fixed_axis <- "latitude"
    fixed_selector <- latitude
  } else {
    if (!is.null(latitude)) {
      abort_viz(
        "`latitude` must be NULL because latitude varies in a latitude-depth section.",
        "oceancube_viz_selection_error"
      )
    }
    if (is.null(longitude) && length(x$lon) != 1L) {
      abort_viz(
        "`longitude` must select exactly one stored value for a latitude-depth section.",
        "oceancube_viz_selection_error"
      )
    }
    horizontal <- "latitude"
    fixed_axis <- "longitude"
    fixed_selector <- longitude
  }

  if (!is.null(limits) &&
      (!is.numeric(limits) || !is.null(dim(limits)) || length(limits) != 2L ||
         any(!is.finite(limits)) || limits[[1L]] >= limits[[2L]])) {
    abort_viz("`limits` must be NULL or two finite numeric values with min < max.")
  }
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    abort_viz("`na.rm` must be one non-missing logical value.")
  }
  if (!is.logical(reverse_depth) || length(reverse_depth) != 1L ||
      is.na(reverse_depth)) {
    abort_viz("`reverse_depth` must be one non-missing logical value.")
  }
  validate_label(title, "title")
  validate_label(subtitle, "subtitle")
  validate_label(caption, "caption")

  extracted <- tryCatch(
    cube_extract(
      x,
      longitude = if (identical(fixed_axis, "longitude")) fixed_selector else NULL,
      latitude = if (identical(fixed_axis, "latitude")) fixed_selector else NULL,
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
        paste0("Could not select the requested vertical section: ", conditionMessage(error)),
        "oceancube_viz_selection_error",
        parent = error
      )
    }
  )

  if (!is.data.frame(extracted) || nrow(extracted) == 0L) {
    abort_viz("The selected vertical section is empty.", "oceancube_viz_data_error")
  }
  required_output <- c(horizontal, fixed_axis, "depth", "time", "unit", "value")
  if (!all(required_output %in% names(extracted))) {
    abort_viz(
      "cube_extract() returned an incomplete vertical section.",
      "oceancube_viz_data_error"
    )
  }
  if (!is.numeric(extracted$value)) {
    abort_viz("The selected section values must be numeric.", "oceancube_viz_data_error")
  }
  if (length(unique(extracted$time)) != 1L ||
      length(unique(extracted[[fixed_axis]])) != 1L) {
    abort_viz(
      "The selected data do not represent exactly one time and fixed coordinate.",
      "oceancube_viz_selection_error"
    )
  }
  selected_depth <- unique(extracted$depth)
  if (length(selected_depth) < 2L || any(!is.finite(selected_depth)) ||
      anyDuplicated(selected_depth)) {
    abort_viz(
      "The selected section must contain at least two finite, unique depth levels.",
      "oceancube_viz_selection_error"
    )
  }

  selected_time <- unique(extracted$time)
  selected_fixed <- unique(extracted[[fixed_axis]])
  backend <- attr(extracted, "oceancube_backend", exact = TRUE)
  layer <- extracted[, c(horizontal, "depth", "value"), drop = FALSE]
  rownames(layer) <- NULL
  if (anyDuplicated(layer[c(horizontal, "depth")])) {
    abort_viz(
      paste0(
        "The selected section contains duplicate ", horizontal,
        " x depth cells; aggregation is not performed."
      ),
      "oceancube_viz_data_error"
    )
  }
  if (isTRUE(na.rm)) {
    layer <- layer[!is.na(layer$value), , drop = FALSE]
    rownames(layer) <- NULL
  }
  if (nrow(layer) == 0L) {
    abort_viz(
      "The selected vertical section is empty after removing missing values.",
      "oceancube_viz_data_error"
    )
  }

  regular_grid <- regular_axis(layer[[horizontal]]) && regular_axis(layer$depth)
  units <- unique(as.character(extracted$unit))
  units <- units[!is.na(units) & nzchar(units)]
  scale_title <- if (length(units) == 1L) {
    paste0(variable, " (", units, ")")
  } else {
    variable
  }

  roles <- .viz_named_roles(
    x = horizontal, y = "depth", value = "value", depth = "depth",
    longitude = if (identical(horizontal, "longitude")) "longitude" else NULL,
    latitude = if (identical(horizontal, "latitude")) "latitude" else NULL
  )
  .new_oceancube_viz_data(
    kind = "SECTION",
    data = layer,
    roles = roles,
    variables = .viz_variable_metadata(x, variable, units),
    coordinates = .viz_coordinate_metadata(
      layer, roles,
      list(longitude = attr(x$lon, "units", exact = TRUE),
           latitude = attr(x$lat, "units", exact = TRUE),
           depth = attr(x$depth, "units", exact = TRUE))
    ),
    selection = attr(extracted, "oceancube_selection", exact = TRUE),
    time = .viz_time_metadata(selected_time, time),
    depth = .viz_depth_metadata(selected_depth, reverse_depth,
                                attr(x$depth, "units", exact = TRUE)),
    source_semantics = .viz_source_semantics(x),
    geometry = list(
      x = horizontal, y = "depth", value = "value", horizontal = horizontal,
      fixed_axis = fixed_axis, fixed_coordinate = as.numeric(selected_fixed),
      regular_grid = regular_grid
    ),
    projection = list(source_crs = NULL, target_crs = NULL,
                      status = "NOT_APPLICABLE"),
    scale = list(classification = "UNSPECIFIED_CONTINUOUS", limits = limits),
    support = list(
      rows = nrow(layer), missing_values = sum(is.na(layer$value)),
      backend = backend, selection_status = "SELECTED"
    ),
    provenance = .viz_private_state(
      attr(extracted, "oceancube_provenance", exact = TRUE)
    ),
    qa = .viz_private_state(attr(extracted, "oceancube_qa", exact = TRUE)),
    renderer_hints = list(
      title = title, subtitle = subtitle, caption = caption, na.rm = na.rm,
      value_label = scale_title,
      plot_attributes = list(
        oceancube_variable = variable,
        oceancube_time = selected_time,
        oceancube_section = section,
        oceancube_fixed_coordinate = stats::setNames(
          as.numeric(selected_fixed), fixed_axis
        ),
        oceancube_depth_range = range(selected_depth),
        oceancube_backend = backend
      )
    )
  )
}
