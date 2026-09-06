#' Draw a Hovmoller diagram from one ocean-cube plane
#'
#' `viz.hovmoller()` selects one variable over stored time and exactly one
#' longitude, latitude, or depth axis. Every other non-singleton dimension must
#' be selected explicitly. It never averages, aggregates, interpolates,
#' smooths, fills, calculates a climatology or anomaly, or estimates a trend.
#'
#' @param x A valid `<ocean_cube>` using the memory or NetCDF backend.
#' @param variable Exactly one non-empty, non-missing variable name present in
#'   `x`.
#' @param axis Coordinate retained with time: `"longitude"`, `"latitude"`, or
#'   `"depth"`. Time is always displayed from left to right on the x axis.
#' @param longitude,latitude,depth Stored coordinate selectors for dimensions
#'   not named by `axis`. A selector may be `NULL` only when that dimension is
#'   singleton. The selector for the displayed axis must remain `NULL`, so the
#'   complete stored axis is retained without hidden reduction.
#' @param time_from,time_to Optional scalar `Date` or `POSIXct` bounds compatible
#'   with the cube time axis. The represented interval is inclusive and no
#'   temporal resampling or regularization is performed.
#' @param match Matching method for fixed coordinates. `"exact"` is the
#'   default; `"nearest"` selects stored coordinates without interpolation.
#' @param tolerance Optional nearest-matching tolerance passed unchanged to
#'   [cube_extract()].
#' @param limits `NULL` or two finite numeric fill-scale limits in increasing
#'   order. Limits affect display only and never remove or modify values.
#' @param na.rm A single non-missing logical value. `FALSE` preserves missing
#'   cells for explicit display; `TRUE` removes missing rows without filling.
#' @param reverse_depth A single non-missing logical value. For a time-depth
#'   diagram, `TRUE` displays positive-down depth with the surface at the top.
#'   Scientific depth values are never changed.
#' @param title,subtitle,caption Optional character scalars used as plot labels.
#'
#' @return A modifiable `ggplot` object carrying bounded `oceancube_*`
#'   attributes for the selected variable, axis, represented time range, fixed
#'   coordinates, depth, backend, source semantics, and prepared-data kind.
#'
#' @details
#' The prepared data are the exact time-by-coordinate Cartesian support returned
#' by [cube_extract()]. Irregular stored spacing remains irregular and is drawn
#' with a deterministic display-only footprint positioned at stored coordinate
#' centres; subtle boundaries distinguish stored `NA` tiles from regions with
#' no stored centre. Gaps are not stretched into a continuous raster and no
#' scientific cell bounds are invented. The display label normalizes the
#' unambiguous `degC` alias to `°C`; stored unit metadata remain unchanged.
#' Calendar-aware non-base time axes are rejected explicitly because the D2A
#' ggplot renderer cannot represent them without inventing Gregorian dates.
#'
#' @export
#' @importFrom rlang .data
#' @seealso [cube_extract()], [viz.timeseries()], [viz.section()]
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   values <- array(1:12, dim = c(1, 1, 3, 4, 1))
#'   cube <- ocean_cube(
#'     lon = -79, lat = -11, depth = c(0, 25, 75),
#'     time = as.Date("2020-01-01") + c(0, 1, 3, 6),
#'     data = values, vars = "temperature", units = "degC"
#'   )
#'   viz.hovmoller(cube, "temperature", axis = "depth")
#' }
viz.hovmoller <- function(
    x,
    variable,
    axis = c("longitude", "latitude", "depth"),
    longitude = NULL,
    latitude = NULL,
    depth = NULL,
    time_from = NULL,
    time_to = NULL,
    match = c("exact", "nearest"),
    tolerance = NULL,
    limits = NULL,
    na.rm = FALSE,
    reverse_depth = TRUE,
    title = NULL,
    subtitle = NULL,
    caption = NULL) {
  prepared <- .viz_prepare_hovmoller(
    x = x, variable = variable, axis = axis,
    longitude = longitude, latitude = latitude, depth = depth,
    time_from = time_from, time_to = time_to, match = match,
    tolerance = tolerance, limits = limits, na.rm = na.rm,
    reverse_depth = reverse_depth, title = title, subtitle = subtitle,
    caption = caption
  )
  .viz_render_ggplot(prepared)
}

.viz_prepare_hovmoller <- function(
    x,
    variable,
    axis = c("longitude", "latitude", "depth"),
    longitude = NULL,
    latitude = NULL,
    depth = NULL,
    time_from = NULL,
    time_to = NULL,
    match = c("exact", "nearest"),
    tolerance = NULL,
    limits = NULL,
    na.rm = FALSE,
    reverse_depth = TRUE,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    scale_class = "UNSPECIFIED_CONTINUOUS",
    scale_centre = NULL,
    palette = "viridis") {
  cube_validate(x, strict = TRUE)
  .calendar_operation_unsupported(x, "viz.hovmoller")

  match_choice <- function(value, argument, choices) {
    tryCatch(
      base::match.arg(value, choices),
      error = function(error) {
        .viz_abort(
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
      .viz_abort(
        paste0("`", argument, "` must be NULL or one non-missing character string.")
      )
    }
    invisible(TRUE)
  }
  validate_selector <- function(value, argument) {
    if (!is.null(value) &&
        (!is.numeric(value) || !is.null(dim(value)) || length(value) != 1L ||
         is.na(value) || !is.finite(value))) {
      .viz_abort(
        paste0("`", argument, "` must be NULL or one finite stored numeric value."),
        "oceancube_viz_selection_error"
      )
    }
    invisible(TRUE)
  }
  time_class <- function(value) {
    if (inherits(value, "Date")) return("Date")
    if (inherits(value, "POSIXct")) return("POSIXct")
    NULL
  }
  validate_time_bound <- function(value, argument, expected_class) {
    if (is.null(value)) return(invisible(TRUE))
    if (!inherits(value, expected_class) || !is.null(dim(value)) ||
        length(value) != 1L || is.na(value) || !is.finite(as.numeric(value))) {
      .viz_abort(
        paste0(
          "`", argument, "` must be NULL or one finite ", expected_class,
          " value compatible with the cube time axis."
        ),
        "oceancube_viz_selection_error"
      )
    }
    invisible(TRUE)
  }
  regular_axis <- function(values) {
    values <- as.numeric(values)
    if (length(values) <= 2L) return(TRUE)
    differences <- diff(values)
    reference <- differences[[1L]]
    tolerance_value <- sqrt(.Machine$double.eps) *
      max(c(1, abs(reference), abs(differences)))
    all(abs(differences - reference) <= tolerance_value)
  }
  axis_values <- function(name) {
    switch(name, longitude = x$lon, latitude = x$lat, depth = x$depth)
  }
  axis_units <- function(name) {
    values <- axis_values(name)
    attr(values, "units", exact = TRUE)
  }
  axis_label <- function(name) {
    label <- switch(name, longitude = "Longitude", latitude = "Latitude", depth = "Depth")
    units <- .viz_display_unit(axis_units(name))
    if (is.null(units) || length(units) != 1L || is.na(units) || !nzchar(units)) {
      label
    } else {
      paste0(label, " (", units, ")")
    }
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .viz_abort("Package `ggplot2` is required to use viz.hovmoller().")
  }
  if (!is.character(variable) || !is.null(dim(variable)) ||
      length(variable) != 1L || is.na(variable) || !nzchar(variable)) {
    .viz_abort(
      "`variable` must be one non-empty, non-missing character string.",
      "oceancube_viz_selection_error"
    )
  }
  if (!variable %in% x$vars) {
    .viz_abort(
      paste0(
        "Variable `", variable, "` is not present in the cube. Available variables: ",
        paste0("`", x$vars, "`", collapse = ", "), "."
      ),
      "oceancube_viz_selection_error"
    )
  }

  axis <- match_choice(axis, "axis", c("longitude", "latitude", "depth"))
  match <- match_choice(match, "match", c("exact", "nearest"))
  selectors <- list(longitude = longitude, latitude = latitude, depth = depth)
  for (name in names(selectors)) validate_selector(selectors[[name]], name)
  if (!is.null(selectors[[axis]])) {
    .viz_abort(
      paste0(
        "`", axis, "` must be NULL when `axis = \"", axis,
        "\"`; the displayed coordinate retains its complete stored support."
      ),
      "oceancube_viz_selection_error"
    )
  }
  coordinate_lengths <- c(
    longitude = length(x$lon), latitude = length(x$lat), depth = length(x$depth)
  )
  for (name in setdiff(names(selectors), axis)) {
    if (is.null(selectors[[name]]) && coordinate_lengths[[name]] != 1L) {
      .viz_abort(
        paste0(
          "`", name, "` must select exactly one stored value because it is a ",
          "non-display dimension with ", coordinate_lengths[[name]], " positions; ",
          "viz.hovmoller() never performs hidden reduction."
        ),
        "oceancube_viz_selection_error"
      )
    }
  }
  displayed_values <- axis_values(axis)
  if (length(displayed_values) < 2L ||
      (identical(axis, "depth") && any(!is.finite(displayed_values)))) {
    .viz_abort(
      paste0("The displayed `", axis, "` axis must contain at least two finite stored positions."),
      "oceancube_viz_selection_error"
    )
  }
  if (length(x$time) < 2L) {
    .viz_abort(
      "A Hovmoller diagram requires at least two stored time positions.",
      "oceancube_viz_selection_error"
    )
  }
  for (flag in c("na.rm", "reverse_depth")) {
    value <- get(flag, inherits = FALSE)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      .viz_abort(paste0("`", flag, "` must be one non-missing logical value."))
    }
  }
  for (name in c("title", "subtitle", "caption")) {
    validate_label(get(name, inherits = FALSE), name)
  }
  scale <- .viz_scale_spec(
    classification = scale_class,
    limits = limits,
    centre = scale_centre,
    palette = palette
  )

  expected_time_class <- time_class(x$time)
  if (is.null(expected_time_class)) {
    .viz_abort(
      "The cube time axis must inherit from Date or POSIXct.",
      "oceancube_viz_selection_error"
    )
  }
  validate_time_bound(time_from, "time_from", expected_time_class)
  validate_time_bound(time_to, "time_to", expected_time_class)
  if (identical(expected_time_class, "POSIXct")) {
    if (!is.null(time_from)) time_from <- .as_utc_posixct(time_from)
    if (!is.null(time_to)) time_to <- .as_utc_posixct(time_to)
  }
  if (!is.null(time_from) && !is.null(time_to) && time_from > time_to) {
    .viz_abort(
      "`time_from` must be earlier than or equal to `time_to`.",
      "oceancube_viz_selection_error"
    )
  }
  keep_time <- rep(TRUE, length(x$time))
  if (!is.null(time_from)) keep_time <- keep_time & x$time >= time_from
  if (!is.null(time_to)) keep_time <- keep_time & x$time <= time_to
  selected_time <- x$time[keep_time]
  if (length(selected_time) < 2L) {
    .viz_abort(
      "The requested interval must contain at least two stored time positions.",
      "oceancube_viz_selection_error"
    )
  }

  extracted <- tryCatch(
    cube_extract(
      x,
      longitude = longitude,
      latitude = latitude,
      depth = depth,
      time = selected_time,
      variable = variable,
      by = "value",
      match = match,
      tolerance = tolerance,
      mode = "table",
      format = "long",
      keep_index = FALSE,
      keep_distance = identical(match, "nearest")
    ),
    error = function(error) {
      .viz_abort(
        paste0("Could not select the requested Hovmoller support: ", conditionMessage(error)),
        "oceancube_viz_selection_error",
        parent = error
      )
    }
  )
  if (!is.data.frame(extracted) || nrow(extracted) == 0L) {
    .viz_abort("The selected Hovmoller support is empty.", "oceancube_viz_data_error")
  }
  required <- c(
    "longitude", "latitude", "depth", "time", "variable", "unit", "value"
  )
  if (!all(required %in% names(extracted))) {
    .viz_abort(
      "cube_extract() returned incomplete Hovmoller data.",
      "oceancube_viz_data_error"
    )
  }
  if (!is.numeric(extracted$value)) {
    .viz_abort("The selected Hovmoller values must be numeric.", "oceancube_viz_data_error")
  }
  if (!inherits(extracted$time, expected_time_class) || anyNA(extracted$time)) {
    .viz_abort(
      "The selected Hovmoller times are missing or incompatible with the cube time axis.",
      "oceancube_viz_data_error"
    )
  }
  if (!identical(unique(as.character(extracted$variable)), variable)) {
    .viz_abort(
      "The selected Hovmoller data must contain exactly the requested variable.",
      "oceancube_viz_selection_error"
    )
  }
  for (name in setdiff(names(selectors), axis)) {
    values <- unique(extracted[[name]])
    is_surface <- identical(name, "depth") && length(values) == 1L &&
      is.numeric(values) && is.na(values[[1L]]) && !is.nan(values[[1L]])
    if (length(values) != 1L || (!is_surface &&
        (!is.numeric(values) || is.na(values) || !is.finite(values)))) {
      .viz_abort(
        paste0("The selected data must contain exactly one fixed `", name, "` coordinate."),
        "oceancube_viz_selection_error"
      )
    }
  }
  selected_axis <- unique(extracted[[axis]])
  if (!is.numeric(selected_axis) || length(selected_axis) < 2L ||
      anyNA(selected_axis) || any(!is.finite(selected_axis))) {
    .viz_abort(
      paste0("The selected Hovmoller `", axis, "` support is invalid."),
      "oceancube_viz_data_error"
    )
  }
  if (anyDuplicated(extracted[c("time", axis)])) {
    .viz_abort(
      paste0(
        "The selected support contains duplicate time x ", axis,
        " cells; aggregation is not performed."
      ),
      "oceancube_viz_data_error"
    )
  }
  display_footprint <- .viz_display_footprint(selected_time, selected_axis)

  backend <- attr(extracted, "oceancube_backend", exact = TRUE)
  selection <- attr(extracted, "oceancube_selection", exact = TRUE)
  provenance <- .viz_private_state(
    attr(extracted, "oceancube_provenance", exact = TRUE)
  )
  qa <- .viz_private_state(attr(extracted, "oceancube_qa", exact = TRUE))
  hovmoller <- extracted[, c("time", axis, "value"), drop = FALSE]
  rownames(hovmoller) <- NULL
  if (isTRUE(na.rm)) {
    hovmoller <- hovmoller[!is.na(hovmoller$value), , drop = FALSE]
    rownames(hovmoller) <- NULL
  }
  if (nrow(hovmoller) == 0L) {
    .viz_abort(
      "The selected Hovmoller support is empty after removing missing values.",
      "oceancube_viz_data_error"
    )
  }

  units <- unique(as.character(extracted$unit))
  units <- units[!is.na(units) & nzchar(units)]
  variable_metadata <- .viz_variable_metadata(x, variable, units)
  variable_label <- if (!is.na(variable_metadata$long_name) &&
      nzchar(variable_metadata$long_name)) {
    variable_metadata$long_name
  } else {
    variable
  }
  value_label <- if (!is.na(variable_metadata$units) &&
      nzchar(variable_metadata$units)) {
    paste0(variable_label, " (", .viz_display_unit(variable_metadata$units), ")")
  } else {
    variable_label
  }
  fixed_coordinates <- lapply(
    setdiff(names(selectors), axis),
    function(name) unique(extracted[[name]])
  )
  names(fixed_coordinates) <- setdiff(names(selectors), axis)
  selected_depth <- unique(extracted$depth)
  roles_args <- list(x = "time", y = axis, value = "value", time = "time")
  roles_args[[axis]] <- axis
  roles <- do.call(.viz_named_roles, roles_args)
  source_semantics <- .viz_source_semantics(x)
  plot_attributes <- list(
    oceancube_variable = variable,
    oceancube_axis = axis,
    oceancube_time_range = range(selected_time),
    oceancube_fixed_coordinates = fixed_coordinates,
    oceancube_depth = selected_depth,
    oceancube_backend = backend,
    oceancube_match = match,
    oceancube_tolerance = tolerance,
    oceancube_source_semantics = source_semantics,
    oceancube_prepared_kind = "HOVMOLLER",
    oceancube_selection = selection,
    oceancube_provenance = provenance,
    oceancube_qa = qa
  )

  .new_oceancube_viz_data(
    kind = "HOVMOLLER",
    data = hovmoller,
    roles = roles,
    variables = variable_metadata,
    coordinates = .viz_coordinate_metadata(
      hovmoller,
      roles,
      c(list(time = attr(x$time, "units", exact = TRUE)),
        stats::setNames(list(axis_units(axis)), axis))
    ),
    selection = selection,
    time = .viz_time_metadata(
      selected_time,
      list(from = time_from, to = time_to)
    ),
    depth = .viz_depth_metadata(
      selected_depth,
      identical(axis, "depth") && isTRUE(reverse_depth),
      attr(x$depth, "units", exact = TRUE)
    ),
    source_semantics = source_semantics,
    geometry = list(
      x = "time",
      y = axis,
      value = "value",
      axis = axis,
      support = "STORED_CENTRES",
      renderer_geometry = "DISPLAY_ONLY_POINT_CENTRED_TILES_WITH_VISIBLE_GAPS",
      regular_time = regular_axis(selected_time),
      regular_axis = regular_axis(selected_axis)
    ),
    projection = list(
      source_crs = NULL, target_crs = NULL, status = "NOT_APPLICABLE"
    ),
    scale = scale,
    support = list(
      rows = nrow(hovmoller),
      missing_values = sum(is.na(hovmoller$value)),
      backend = backend,
      selection_status = "SELECTED",
      fixed_coordinates = fixed_coordinates,
      no_hidden_reduction = TRUE,
      geometry = "STORED_CENTRES",
      display_footprint = display_footprint,
      scientific_bounds = NULL,
      bounds_authority = "NONE_AVAILABLE_FOR_BOTH_PLOTTED_AXES",
      explicit_cell_bounds_runtime = "DEFERRED_NOT_CERTIFIED_D2A"
    ),
    provenance = provenance,
    qa = qa,
    renderer_hints = list(
      title = title,
      subtitle = subtitle,
      caption = caption,
      na.rm = na.rm,
      value_label = value_label,
      axis_label = axis_label(axis),
      plot_attributes = plot_attributes
    )
  )
}
