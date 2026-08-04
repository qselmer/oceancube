#' Draw a static map from one ocean-cube layer
#'
#' `viz.map()` selects exactly one variable, time, and depth layer through
#' [cube_extract()] and draws its stored grid without interpolation. Regular
#' grids use a raster layer and irregular grids use tiles. NetCDF inputs read
#' only the selected layer rather than materializing the complete cube.
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
#' }
viz.map <- function(x, variable, time = NULL, depth = NULL, limits = NULL,
                    na.rm = TRUE, coastline = NULL, title = NULL,
                    subtitle = NULL, caption = NULL) {
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

  coastline_type <- "none"
  if (!is.null(coastline)) {
    if (inherits(coastline, "sf") || inherits(coastline, "sfc")) {
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

  plot <- ggplot2::ggplot(
    layer,
    ggplot2::aes(
      x = .data$longitude,
      y = .data$latitude,
      fill = .data$value
    )
  )
  if (regular_grid) {
    plot <- plot + ggplot2::geom_raster(na.rm = na.rm)
  } else {
    plot <- plot + ggplot2::geom_tile(na.rm = na.rm)
  }
  plot <- plot +
    ggplot2::scale_fill_continuous(
      name = scale_title,
      limits = limits,
      oob = function(values, range) {
        pmax(range[[1L]], pmin(range[[2L]], values))
      }
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption,
                  x = "Longitude", y = "Latitude")

  if (identical(coastline_type, "sf")) {
    coastline_data <- if (inherits(coastline, "sfc")) {
      sf::st_sf(geometry = coastline)
    } else {
      coastline
    }
    plot <- plot + ggplot2::geom_sf(
      data = coastline_data,
      inherit.aes = FALSE,
      fill = NA,
      colour = "black"
    )
  } else if (identical(coastline_type, "data.frame")) {
    plot <- plot + ggplot2::geom_path(
      data = coastline,
      mapping = ggplot2::aes(
        x = .data$longitude,
        y = .data$latitude,
        group = .data$group
      ),
      inherit.aes = FALSE,
      colour = "black"
    )
  }

  plot <- suppressMessages(plot + ggplot2::coord_equal(expand = FALSE))

  attr(plot, "oceancube_variable") <- variable
  attr(plot, "oceancube_time") <- selected_time
  attr(plot, "oceancube_depth") <- selected_depth
  attr(plot, "oceancube_backend") <- backend
  plot
}
