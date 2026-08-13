#' Draw a raw point time series from an ocean cube
#'
#' `viz.timeseries()` selects one variable at exactly one longitude, latitude,
#' and depth (or the singleton surface) through [cube_extract()], then draws
#' the stored values against time. It does not aggregate, smooth, interpolate,
#' impute, or otherwise transform the selected observations.
#'
#' @param x A valid `<ocean_cube>` using the memory or NetCDF backend.
#' @param variable Exactly one non-empty, non-missing variable name present in
#'   `x`.
#' @param longitude A single stored longitude. May be `NULL` only when the cube
#'   contains one longitude.
#' @param latitude A single stored latitude. May be `NULL` only when the cube
#'   contains one latitude.
#' @param depth A single stored depth. May be `NULL` only for a singleton depth
#'   axis, including a singleton `NA_real_` surface depth.
#' @param time_from,time_to Optional scalar `Date` or `POSIXct` bounds compatible
#'   with the cube time axis. Together they define the inclusive closed interval
#'   `[time_from, time_to]`; a `NULL` bound leaves that side open.
#' @param match Matching method passed explicitly to [cube_extract()]. Exact
#'   stored-value matching is the default; `"nearest"` selects a stored grid
#'   cell without interpolation.
#' @param tolerance Optional nearest-matching tolerance passed unchanged to
#'   [cube_extract()].
#' @param limits `NULL` or two finite numeric value-axis limits in increasing
#'   order. Values outside the limits are squished to the scale boundary and
#'   rows are not removed.
#' @param na.rm A single non-missing logical value. If `FALSE`, the default,
#'   missing values remain in the plotted data and interrupt the raw line. If
#'   `TRUE`, missing rows are excluded and separated observations may therefore
#'   appear connected; values are never imputed or replaced with zero.
#' @param points A single non-missing logical value. If `TRUE`, points are drawn
#'   over the line.
#' @param title,subtitle,caption Optional character scalars used as plot labels.
#'
#' @return A `ggplot` object. The selected variable, matched coordinates, depth,
#'   represented time range, number of temporal rows, backend, matching settings,
#'   and point-to-cell distance are recorded in `oceancube_*` attributes.
#'
#' @details
#' The simple series contract is exactly one variable, one longitude, one
#' latitude, one depth or surface, and multiple stored time positions. A bounded
#' interval is inclusive. Canonical cube time is already unique and strictly
#' increasing; extracted rows are plotted in stable chronological order and are
#' never averaged or deduplicated. POSIXct bounds use instant semantics and may
#' be expressed in a display timezone other than canonical UTC.
#'
#' Data selection is delegated to `cube_extract(mode = "series")`. For a lazy
#' NetCDF cube, a unique bounded time selection reads only the selected point,
#' depth, times, and variable (or the backend's minimal physical envelope), not
#' the complete cube.
#'
#' @export
#' @seealso [cube_extract()], [cube_crop()], [viz.profile()], [viz.transect()],
#'   [viz.map()]
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   values <- array(c(18, 17, NA, 15), dim = c(1, 1, 1, 4, 1))
#'   cube <- ocean_cube(
#'     lon = -79, lat = -11, depth = 0,
#'     time = as.Date("2020-01-01") + 0:3,
#'     data = values, vars = "temperature", units = "degC"
#'   )
#'   viz.timeseries(cube, "temperature")
#'   viz.timeseries(
#'     cube, "temperature",
#'     time_from = as.Date("2020-01-02"),
#'     time_to = as.Date("2020-01-03"),
#'     points = TRUE
#'   )
#' }
viz.timeseries <- function(
    x,
    variable,
    longitude = NULL,
    latitude = NULL,
    depth = NULL,
    time_from = NULL,
    time_to = NULL,
    match = c("exact", "nearest"),
    tolerance = NULL,
    limits = NULL,
    na.rm = FALSE,
    points = FALSE,
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
  validate_numeric_selector <- function(value, argument) {
    if (!is.null(value) &&
        (!is.numeric(value) || !is.null(dim(value)) || length(value) != 1L ||
           is.na(value) || !is.finite(value))) {
      abort_viz(
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
  time_zone <- function(value) {
    zone <- attr(value, "tzone", exact = TRUE)
    if (is.null(zone) || length(zone) == 0L || is.na(zone[[1L]])) "" else zone[[1L]]
  }
  validate_time_bound <- function(value, argument, expected_class, expected_zone) {
    if (is.null(value)) return(invisible(TRUE))
    valid <- inherits(value, expected_class) && is.null(dim(value)) &&
      length(value) == 1L && !is.na(value) && is.finite(as.numeric(value))
    if (!valid) {
      abort_viz(
        paste0(
          "`", argument, "` must be NULL or one finite ", expected_class,
          " value compatible with the cube time axis."
        ),
        "oceancube_viz_selection_error"
      )
    }
    invisible(TRUE)
  }
  horizontal_distance_km <- function(lon_from, lat_from, lon_to, lat_to) {
    radians <- pi / 180
    delta_lon <- (lon_to - lon_from) * radians
    delta_lat <- (lat_to - lat_from) * radians
    lat1 <- lat_from * radians
    lat2 <- lat_to * radians
    haversine <- sin(delta_lat / 2)^2 +
      cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
    haversine <- pmax(0, pmin(1, haversine))
    6371.0088 * 2 * atan2(sqrt(haversine), sqrt(1 - haversine))
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    abort_viz("Package `ggplot2` is required to use viz.timeseries().")
  }
  if (!is.character(variable) || !is.null(dim(variable)) ||
      length(variable) != 1L || is.na(variable) || !nzchar(variable)) {
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

  validate_numeric_selector(longitude, "longitude")
  validate_numeric_selector(latitude, "latitude")
  validate_numeric_selector(depth, "depth")
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
  if (is.null(depth) && length(x$depth) != 1L) {
    abort_viz(
      "`depth` must select exactly one stored value when the cube contains multiple depths.",
      "oceancube_viz_selection_error"
    )
  }

  match <- match_choice(match, "match", c("exact", "nearest"))
  if (!is.null(limits) &&
      (!is.numeric(limits) || !is.null(dim(limits)) || length(limits) != 2L ||
         any(!is.finite(limits)) || limits[[1L]] >= limits[[2L]])) {
    abort_viz("`limits` must be NULL or two finite numeric values with min < max.")
  }
  for (flag in c("na.rm", "points")) {
    value <- get(flag, inherits = FALSE)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      abort_viz(paste0("`", flag, "` must be one non-missing logical value."))
    }
  }
  validate_label(title, "title")
  validate_label(subtitle, "subtitle")
  validate_label(caption, "caption")

  expected_time_class <- time_class(x$time)
  if (is.null(expected_time_class)) {
    abort_viz(
      "The cube time axis must inherit from Date or POSIXct.",
      "oceancube_viz_selection_error"
    )
  }
  expected_zone <- if (identical(expected_time_class, "POSIXct")) {
    time_zone(x$time)
  } else {
    ""
  }
  validate_time_bound(time_from, "time_from", expected_time_class, expected_zone)
  validate_time_bound(time_to, "time_to", expected_time_class, expected_zone)
  if (identical(expected_time_class, "POSIXct")) {
    if (!is.null(time_from)) time_from <- .as_utc_posixct(time_from)
    if (!is.null(time_to)) time_to <- .as_utc_posixct(time_to)
  }
  if (!is.null(time_from) && !is.null(time_to) && time_from > time_to) {
    abort_viz(
      "`time_from` must be earlier than or equal to `time_to`.",
      "oceancube_viz_selection_error"
    )
  }

  bounded <- !is.null(time_from) || !is.null(time_to)
  keep_time <- rep(TRUE, length(x$time))
  if (!is.null(time_from)) keep_time <- keep_time & x$time >= time_from
  if (!is.null(time_to)) keep_time <- keep_time & x$time <= time_to
  selected_time <- x$time[keep_time]
  if (length(selected_time) == 0L) {
    abort_viz(
      "No stored time values fall within the requested interval.",
      "oceancube_viz_selection_error"
    )
  }

  duplicated_selected_time <- isTRUE(bounded) &&
    anyDuplicated(as.numeric(selected_time)) > 0L
  extract_time <- if (!isTRUE(bounded) || duplicated_selected_time) {
    NULL
  } else {
    selected_time
  }

  extracted <- tryCatch(
    cube_extract(
      x,
      longitude = longitude,
      latitude = latitude,
      depth = depth,
      time = extract_time,
      variable = variable,
      by = "value",
      match = match,
      tolerance = tolerance,
      mode = "series",
      format = "long",
      keep_index = FALSE,
      keep_distance = identical(match, "nearest")
    ),
    error = function(error) {
      abort_viz(
        paste0("Could not select the requested raw time series: ", conditionMessage(error)),
        "oceancube_viz_selection_error",
        parent = error
      )
    }
  )

  if (isTRUE(duplicated_selected_time) && is.data.frame(extracted)) {
    retained_attributes <- attributes(extracted)[c(
      "oceancube_backend", "oceancube_shape", "oceancube_selection",
      "units", "oceancube_provenance"
    )]
    extracted <- extracted[keep_time, , drop = FALSE]
    for (attribute in names(retained_attributes)) {
      attr(extracted, attribute) <- retained_attributes[[attribute]]
    }
  }

  if (!is.data.frame(extracted) || nrow(extracted) == 0L) {
    abort_viz("The selected raw time series is empty.", "oceancube_viz_data_error")
  }
  required <- c(
    "longitude", "latitude", "depth", "time", "variable", "unit", "value"
  )
  if (!all(required %in% names(extracted))) {
    abort_viz(
      "cube_extract() returned incomplete raw time-series data.",
      "oceancube_viz_data_error"
    )
  }
  if (!is.numeric(extracted$value)) {
    abort_viz(
      "The selected time-series values must be numeric.",
      "oceancube_viz_data_error"
    )
  }
  if (!inherits(extracted$time, expected_time_class) || anyNA(extracted$time)) {
    abort_viz(
      "The selected time-series times are missing or incompatible with the cube time axis.",
      "oceancube_viz_data_error"
    )
  }
  selected_longitude <- unique(extracted$longitude)
  selected_latitude <- unique(extracted$latitude)
  selected_depth <- unique(extracted$depth)
  selected_variable <- unique(as.character(extracted$variable))
  if (length(selected_longitude) != 1L || !is.numeric(selected_longitude) ||
      is.na(selected_longitude) || !is.finite(selected_longitude)) {
    abort_viz(
      "The selected data must contain exactly one finite longitude.",
      "oceancube_viz_selection_error"
    )
  }
  if (length(selected_latitude) != 1L || !is.numeric(selected_latitude) ||
      is.na(selected_latitude) || !is.finite(selected_latitude)) {
    abort_viz(
      "The selected data must contain exactly one finite latitude.",
      "oceancube_viz_selection_error"
    )
  }
  surface <- length(selected_depth) == 1L && is.numeric(selected_depth) &&
    is.na(selected_depth[[1L]]) && !is.nan(selected_depth[[1L]])
  if (length(selected_depth) != 1L || !is.numeric(selected_depth) ||
      (!surface && (is.na(selected_depth) || !is.finite(selected_depth)))) {
    abort_viz(
      "The selected data must contain exactly one finite depth or one surface depth.",
      "oceancube_viz_selection_error"
    )
  }
  if (length(selected_variable) != 1L || is.na(selected_variable) ||
      !identical(selected_variable, variable)) {
    abort_viz(
      "The selected data must contain exactly the requested variable.",
      "oceancube_viz_selection_error"
    )
  }

  backend <- attr(extracted, "oceancube_backend", exact = TRUE)
  selection_metadata <- attr(extracted, "oceancube_selection", exact = TRUE)
  provenance <- attr(extracted, "oceancube_provenance", exact = TRUE)
  represented_time_range <- range(extracted$time)
  n_time <- as.integer(nrow(extracted))

  requested_longitude <- if ("longitude_requested" %in% names(extracted)) {
    unique(extracted$longitude_requested)
  } else {
    selected_longitude
  }
  requested_latitude <- if ("latitude_requested" %in% names(extracted)) {
    unique(extracted$latitude_requested)
  } else {
    selected_latitude
  }
  if (length(requested_longitude) != 1L || length(requested_latitude) != 1L ||
      anyNA(c(requested_longitude, requested_latitude)) ||
      any(!is.finite(c(requested_longitude, requested_latitude)))) {
    abort_viz(
      "Nearest-match diagnostics must identify one finite requested location.",
      "oceancube_viz_data_error"
    )
  }
  match_distance_km <- if (identical(match, "nearest")) {
    as.numeric(horizontal_distance_km(
      requested_longitude, requested_latitude,
      selected_longitude, selected_latitude
    ))
  } else {
    0
  }

  stable_position <- seq_len(nrow(extracted))
  series <- extracted[
    order(as.numeric(extracted$time), stable_position, method = "radix"),
    c("time", "value"),
    drop = FALSE
  ]
  rownames(series) <- NULL
  if (isTRUE(na.rm)) {
    series <- series[!is.na(series$value), , drop = FALSE]
    rownames(series) <- NULL
  }
  if (nrow(series) == 0L) {
    abort_viz(
      "The selected raw time series is empty after removing missing values.",
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

  plot <- ggplot2::ggplot(
    series,
    ggplot2::aes(x = .data$time, y = .data$value)
  ) +
    ggplot2::geom_line(na.rm = na.rm)
  if (isTRUE(points)) {
    plot <- plot + ggplot2::geom_point(na.rm = na.rm)
  }
  plot <- plot +
    ggplot2::scale_y_continuous(
      limits = limits,
      oob = function(values, range) {
        pmax(range[[1L]], pmin(range[[2L]], values))
      }
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = "Time",
      y = value_label
    )

  attr(plot, "oceancube_variable") <- variable
  attr(plot, "oceancube_longitude") <- as.numeric(selected_longitude)
  attr(plot, "oceancube_latitude") <- as.numeric(selected_latitude)
  attr(plot, "oceancube_depth") <- selected_depth
  attr(plot, "oceancube_time_range") <- represented_time_range
  attr(plot, "oceancube_n_time") <- n_time
  attr(plot, "oceancube_backend") <- backend
  attr(plot, "oceancube_match") <- match
  attr(plot, "oceancube_tolerance") <- tolerance
  attr(plot, "oceancube_match_distance_km") <- match_distance_km
  attr(plot, "oceancube_selection") <- selection_metadata
  attr(plot, "oceancube_provenance") <- provenance
  if ("longitude_requested" %in% names(extracted)) {
    attr(plot, "oceancube_longitude_requested") <- requested_longitude
    attr(plot, "oceancube_longitude_distance") <-
      unique(extracted$longitude_distance)
  }
  if ("latitude_requested" %in% names(extracted)) {
    attr(plot, "oceancube_latitude_requested") <- requested_latitude
    attr(plot, "oceancube_latitude_distance") <-
      unique(extracted$latitude_distance)
  }
  if ("depth_requested" %in% names(extracted)) {
    attr(plot, "oceancube_depth_requested") <- unique(extracted$depth_requested)
    attr(plot, "oceancube_depth_distance") <- unique(extracted$depth_distance)
  }
  plot
}
