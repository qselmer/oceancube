# Internal renderer-neutral visualization data contract --------------------

.viz_data_schema_name <- "oceancube_viz_data"
.viz_data_schema_version <- "1.0.0"
.viz_data_kinds <- c(
  "MAP_LAYER", "PROFILE", "SECTION", "TRANSECT_SECTION",
  "TRANSECT_LINE", "TIMESERIES"
)
.viz_data_role_names <- c(
  "x", "y", "value", "group", "time", "depth", "longitude",
  "latitude", "distance"
)

.viz_abort <- function(message, class = "oceancube_viz_error", parent = NULL) {
  rlang::abort(
    message,
    class = unique(c(class, "oceancube_viz_error")),
    parent = parent
  )
}

.viz_squish <- function(values, range) {
  pmax(range[[1L]], pmin(range[[2L]], values))
}

.viz_named_roles <- function(...) {
  supplied <- list(...)
  unknown <- setdiff(names(supplied), .viz_data_role_names)
  if (length(unknown)) {
    .viz_abort(
      paste0("Unknown visualization role(s): ", paste(unknown, collapse = ", "), "."),
      "oceancube_viz_data_error"
    )
  }
  roles <- stats::setNames(vector("list", length(.viz_data_role_names)),
                           .viz_data_role_names)
  roles[names(supplied)] <- supplied
  roles
}

.viz_variable_metadata <- function(x, variable, units = character()) {
  units <- unique(as.character(units))
  units <- units[!is.na(units) & nzchar(units)]
  unit <- if (length(units) == 1L) units[[1L]] else NA_character_

  variable_metadata <- NULL
  if (is.list(x$metadata) && is.list(x$metadata$variables)) {
    map <- x$metadata$variables$map
    if (is.list(map) && variable %in% names(map)) {
      variable_metadata <- map[[variable]]
    }
  }
  attributes <- if (is.list(variable_metadata$attributes)) {
    variable_metadata$attributes
  } else {
    list()
  }
  scalar_character <- function(value) {
    if (is.null(value) || length(value) != 1L || is.na(value)) NA_character_
    else as.character(value)
  }

  list(
    name = variable,
    units = unit,
    standard_name = scalar_character(attributes$standard_name),
    long_name = scalar_character(attributes$long_name),
    value_semantics = "CONTINUOUS",
    descriptor_identity = if (!is.null(x$dataset_id) &&
      length(x$dataset_id) == 1L && !is.na(x$dataset_id)) {
      as.character(x$dataset_id)
    } else {
      NA_character_
    }
  )
}

.viz_source_semantics <- function(x) {
  derived <- inherits(x, c("ocean_anom", "ocean_clim"))
  if (isTRUE(derived)) {
    return(list(
      rendered_from = "DERIVED_FIELD",
      classification_status = "RESOLVED_FROM_CLASS",
      authority = class(x)[[1L]]
    ))
  }
  list(
    rendered_from = NA_character_,
    classification_status = "UNRESOLVED",
    authority = NA_character_
  )
}

.viz_private_state <- function(value) {
  if (is.character(value)) {
    absolute <- grepl("^[A-Za-z]:[/\\\\]", value) | grepl("^/", value)
    url <- grepl("^[a-z][a-z0-9+.-]*://", value, ignore.case = TRUE)
    value[absolute & !url] <- basename(value[absolute & !url])
    return(value)
  }
  if (is.list(value)) {
    attributes_value <- attributes(value)
    out <- lapply(value, .viz_private_state)
    attributes(out) <- attributes_value
    return(out)
  }
  value
}

.viz_coordinate_metadata <- function(data, roles, units = list()) {
  coordinate_roles <- intersect(
    c("longitude", "latitude", "depth", "time", "distance"),
    names(roles)[!vapply(roles, is.null, logical(1))]
  )
  stats::setNames(lapply(coordinate_roles, function(role) {
    column <- roles[[role]]
    values <- data[[column]]
    numeric_values <- if (inherits(values, c("Date", "POSIXct", "POSIXt"))) {
      as.numeric(values)
    } else if (is.numeric(values)) {
      values
    } else {
      numeric()
    }
    finite <- numeric_values[is.finite(numeric_values)]
    order <- if (length(numeric_values) < 2L) {
      "SINGLETON"
    } else if (all(diff(numeric_values) >= 0)) {
      "NON_DECREASING"
    } else if (all(diff(numeric_values) <= 0)) {
      "NON_INCREASING"
    } else {
      "STORED"
    }
    list(
      column = column,
      units = if (!is.null(units[[role]])) as.character(units[[role]]) else NA_character_,
      order = order,
      range = if (length(finite)) range(finite) else c(NA_real_, NA_real_),
      semantics = role,
      n = nrow(data)
    )
  }), coordinate_roles)
}

.viz_time_metadata <- function(values = NULL, selection = NULL) {
  if (is.null(values)) {
    return(list(class = NA_character_, calendar = NA_character_, range = NULL,
                order = "NOT_APPLICABLE", selection = selection))
  }
  list(
    class = class(values)[[1L]],
    calendar = as.character(attr(values, "calendar", exact = TRUE) %||% NA_character_),
    range = if (length(values)) range(values) else values,
    order = if (length(values) < 2L || all(diff(as.numeric(values)) >= 0)) {
      "NON_DECREASING"
    } else {
      "STORED"
    },
    selection = selection
  )
}

.viz_depth_metadata <- function(values = NULL, display_reverse = FALSE,
                                units = NA_character_) {
  list(
    values = values,
    units = as.character(units),
    scientific_positive = if (is.null(values)) NA_character_ else "down",
    display_reverse = isTRUE(display_reverse),
    range = if (is.null(values) || !length(values) || all(is.na(values))) {
      c(NA_real_, NA_real_)
    } else {
      range(values, na.rm = TRUE)
    }
  )
}

.new_oceancube_viz_data <- function(kind, data, roles, variables, coordinates,
                                    selection, time, depth, source_semantics,
                                    geometry, projection, scale, support,
                                    provenance, qa, renderer_hints) {
  out <- structure(
    list(
      schema_name = .viz_data_schema_name,
      schema_version = .viz_data_schema_version,
      kind = kind,
      data = data,
      roles = roles,
      variables = variables,
      coordinates = coordinates,
      selection = selection,
      time = time,
      depth = depth,
      source_semantics = source_semantics,
      geometry = geometry,
      projection = projection,
      scale = scale,
      support = support,
      provenance = provenance,
      qa = qa,
      renderer_hints = renderer_hints
    ),
    class = c("oceancube_viz_data", "list")
  )
  .validate_oceancube_viz_data(out)
  out
}

.validate_oceancube_viz_data <- function(x) {
  required <- c(
    "schema_name", "schema_version", "kind", "data", "roles", "variables",
    "coordinates", "selection", "time", "depth", "source_semantics",
    "geometry", "projection", "scale", "support", "provenance", "qa",
    "renderer_hints"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    .viz_abort(
      paste0("Visualization data are missing required schema key(s): ",
             paste(missing, collapse = ", "), "."),
      "oceancube_viz_data_error"
    )
  }
  data <- .viz_prepared_table(x)
  if (!identical(x$schema_name, .viz_data_schema_name) ||
      !identical(x$schema_version, .viz_data_schema_version)) {
    .viz_abort("Unsupported oceancube visualization-data schema.",
               "oceancube_viz_data_error")
  }
  if (!is.character(x$kind) || length(x$kind) != 1L ||
      !x$kind %in% .viz_data_kinds) {
    .viz_abort("Invalid oceancube visualization-data kind.",
               "oceancube_viz_data_error")
  }
  if (!is.data.frame(data)) {
    .viz_abort("Prepared visualization `data` must be a data frame.",
               "oceancube_viz_data_error")
  }
  if (!is.list(x$roles) || !identical(names(x$roles), .viz_data_role_names)) {
    .viz_abort("Prepared visualization roles are incomplete or invalid.",
               "oceancube_viz_data_error")
  }
  used_roles <- x$roles[!vapply(x$roles, is.null, logical(1))]
  valid_role <- vapply(used_roles, function(column) {
    is.character(column) && length(column) == 1L && !is.na(column) &&
      column %in% names(data)
  }, logical(1))
  if (length(valid_role) && !all(valid_role)) {
    .viz_abort("A prepared visualization role refers to a missing data column.",
               "oceancube_viz_data_error")
  }
  if (!is.list(x$coordinates) || any(!vapply(x$coordinates, function(item) {
    is.list(item) && is.character(item$column) && length(item$column) == 1L &&
      item$column %in% names(data) && identical(item$n, nrow(data))
  }, logical(1)))) {
    .viz_abort("Coordinate metadata and prepared values are not aligned.",
               "oceancube_viz_data_error")
  }
  if (!is.list(x$depth) || !is.logical(x$depth$display_reverse) ||
      length(x$depth$display_reverse) != 1L || is.na(x$depth$display_reverse) ||
      (!is.null(x$depth$values) && !is.numeric(x$depth$values)) ||
      !is.numeric(x$depth$range) || length(x$depth$range) != 2L ||
      !(is.na(x$depth$scientific_positive) ||
        identical(x$depth$scientific_positive, "down"))) {
    .viz_abort("Invalid prepared depth metadata.", "oceancube_viz_data_error")
  }
  if (!is.list(x$time) || !is.character(x$time$class) ||
      length(x$time$class) != 1L || is.na(x$time$class) ||
      !x$time$class %in% c("Date", "POSIXct") ||
      !is.character(x$time$order) || length(x$time$order) != 1L ||
      is.na(x$time$order)) {
    .viz_abort("Invalid prepared time metadata.", "oceancube_viz_data_error")
  }
  rendered_from <- x$source_semantics$rendered_from
  allowed_source <- c("RAW_POINTS", "GRIDDED_FIELD", "MODEL_FIELD", "DERIVED_FIELD")
  if (!is.list(x$source_semantics) || length(rendered_from) != 1L ||
      (!is.na(rendered_from) && !rendered_from %in% allowed_source)) {
    .viz_abort("Invalid `rendered_from` source semantics.",
               "oceancube_viz_data_error")
  }
  if (!is.list(x$scale) ||
      !identical(x$scale$classification, "UNSPECIFIED_CONTINUOUS") ||
      (!is.null(x$scale$limits) &&
       (!is.numeric(x$scale$limits) || length(x$scale$limits) != 2L ||
        any(!is.finite(x$scale$limits)) || diff(x$scale$limits) <= 0))) {
    .viz_abort("Invalid prepared visualization scale classification.",
               "oceancube_viz_data_error")
  }
  projection_status <- c("UNKNOWN", "KNOWN", "CURRENT", "NOT_APPLICABLE")
  if (!is.list(x$projection) || !is.character(x$projection$status) ||
      length(x$projection$status) != 1L ||
      !x$projection$status %in% projection_status) {
    .viz_abort("Invalid prepared projection structure.",
               "oceancube_viz_data_error")
  }
  if ((!is.null(x$provenance) && !is.list(x$provenance)) ||
      (!is.null(x$qa) && !is.list(x$qa))) {
    .viz_abort("Prepared provenance and QA must be lists or NULL.",
               "oceancube_viz_data_error")
  }
  forbidden <- function(value) {
    is.environment(value) || typeof(value) == "externalptr" ||
      inherits(value, c("connection", "ggplot", "htmlwidget"))
  }
  walk <- function(value) {
    if (forbidden(value)) return(TRUE)
    if (is.list(value)) return(any(vapply(value, walk, logical(1))))
    FALSE
  }
  if (walk(unclass(x))) {
    .viz_abort("Prepared visualization data contain live or renderer-specific state.",
               "oceancube_viz_data_error")
  }
  invisible(TRUE)
}

.viz_attach_plot_attributes <- function(plot, attributes) {
  for (name in names(attributes)) {
    attr(plot, name) <- attributes[[name]]
  }
  plot
}

.viz_prepared_table <- function(x) {
  x[[match("data", names(x))]]
}

.viz_render_ggplot <- function(x) {
  .validate_oceancube_viz_data(x)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .viz_abort("Package `ggplot2` is required to render oceancube visualization data.")
  }
  plot <- switch(
    x$kind,
    MAP_LAYER = .viz_render_map_ggplot(x),
    PROFILE = .viz_render_profile_ggplot(x),
    SECTION = .viz_render_section_ggplot(x),
    TRANSECT_SECTION = .viz_render_transect_section_ggplot(x),
    TRANSECT_LINE = .viz_render_transect_line_ggplot(x),
    TIMESERIES = .viz_render_timeseries_ggplot(x),
    .viz_abort("No ggplot renderer is available for this visualization-data kind.")
  )
  plot <- .viz_attach_plot_attributes(plot, x$renderer_hints$plot_attributes)
  if (identical(x$kind, "TIMESERIES")) {
    attr(plot, "oceancube_provenance") <- x$provenance
  }
  plot
}

.viz_render_map_ggplot <- function(x) {
  hints <- x$renderer_hints
  data <- .viz_prepared_table(x)
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$longitude, y = .data$latitude, fill = .data$value)
  )
  plot <- if (isTRUE(x$geometry$regular_grid)) {
    plot + ggplot2::geom_raster(na.rm = hints$na.rm)
  } else {
    plot + ggplot2::geom_tile(na.rm = hints$na.rm)
  }
  plot <- plot +
    ggplot2::scale_fill_continuous(
      name = hints$value_label, limits = x$scale$limits, oob = .viz_squish
    ) +
    ggplot2::labs(
      title = hints$title, subtitle = hints$subtitle, caption = hints$caption,
      x = "Longitude", y = "Latitude"
    )
  coastline <- hints$coastline
  if (identical(hints$coastline_type, "sf")) {
    coastline <- if (inherits(coastline, "sfc")) sf::st_sf(geometry = coastline) else coastline
    plot <- plot + ggplot2::geom_sf(
      data = coastline, inherit.aes = FALSE, fill = NA, colour = "black"
    )
  } else if (identical(hints$coastline_type, "data.frame")) {
    plot <- plot + ggplot2::geom_path(
      data = coastline,
      mapping = ggplot2::aes(
        x = .data$longitude, y = .data$latitude, group = .data$group
      ),
      inherit.aes = FALSE, colour = "black"
    )
  }
  suppressMessages(plot + ggplot2::coord_equal(expand = FALSE))
}

.viz_render_profile_ggplot <- function(x) {
  hints <- x$renderer_hints
  data <- .viz_prepared_table(x)
  plot <- ggplot2::ggplot(
    data, ggplot2::aes(x = .data$value, y = .data$depth)
  ) + ggplot2::geom_line(na.rm = hints$na.rm, orientation = "y")
  if (isTRUE(hints$points)) plot <- plot + ggplot2::geom_point(na.rm = hints$na.rm)
  plot <- plot +
    ggplot2::scale_x_continuous(limits = x$scale$limits, oob = .viz_squish) +
    ggplot2::labs(
      title = hints$title, subtitle = hints$subtitle, caption = hints$caption,
      x = hints$value_label, y = hints$depth_label
    )
  if (isTRUE(x$depth$display_reverse)) plot <- plot + ggplot2::scale_y_reverse()
  plot
}

.viz_render_section_ggplot <- function(x) {
  hints <- x$renderer_hints
  horizontal <- x$geometry$horizontal
  data <- .viz_prepared_table(x)
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[horizontal]], y = .data$depth, fill = .data$value)
  )
  plot <- if (isTRUE(x$geometry$regular_grid)) {
    plot + ggplot2::geom_raster(na.rm = hints$na.rm)
  } else {
    plot + ggplot2::geom_tile(na.rm = hints$na.rm)
  }
  plot <- plot +
    ggplot2::scale_fill_continuous(
      name = hints$value_label, limits = x$scale$limits, oob = .viz_squish
    ) +
    ggplot2::labs(
      title = hints$title, subtitle = hints$subtitle, caption = hints$caption,
      x = if (identical(horizontal, "longitude")) "Longitude" else "Latitude",
      y = "Depth"
    )
  if (isTRUE(x$depth$display_reverse)) plot <- plot + ggplot2::scale_y_reverse()
  plot
}

.viz_render_transect_section_ggplot <- function(x) {
  hints <- x$renderer_hints
  distance_column <- x$geometry$distance_column
  data <- .viz_prepared_table(x)
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[distance_column]], y = .data$depth, fill = .data$value)
  )
  plot <- if (isTRUE(x$geometry$regular_grid)) {
    plot + ggplot2::geom_raster(na.rm = hints$na.rm)
  } else {
    plot + ggplot2::geom_tile(na.rm = hints$na.rm)
  }
  plot <- plot +
    ggplot2::scale_fill_continuous(
      name = hints$value_label, limits = x$scale$limits, oob = .viz_squish
    ) +
    ggplot2::labs(
      title = hints$title, subtitle = hints$subtitle, caption = hints$caption,
      x = hints$distance_label, y = hints$depth_label
    )
  if (isTRUE(x$depth$display_reverse)) plot <- plot + ggplot2::scale_y_reverse()
  plot
}

.viz_render_transect_line_ggplot <- function(x) {
  hints <- x$renderer_hints
  distance_column <- x$geometry$distance_column
  data <- .viz_prepared_table(x)
  plot <- ggplot2::ggplot(
    data, ggplot2::aes(x = .data[[distance_column]], y = .data$value)
  ) + ggplot2::geom_line(na.rm = hints$na.rm)
  if (isTRUE(hints$points)) plot <- plot + ggplot2::geom_point(na.rm = hints$na.rm)
  plot +
    ggplot2::scale_y_continuous(limits = x$scale$limits, oob = .viz_squish) +
    ggplot2::labs(
      title = hints$title, subtitle = hints$subtitle, caption = hints$caption,
      x = hints$distance_label, y = hints$value_label
    )
}

.viz_render_timeseries_ggplot <- function(x) {
  hints <- x$renderer_hints
  data <- .viz_prepared_table(x)
  plot <- ggplot2::ggplot(
    data, ggplot2::aes(x = .data$time, y = .data$value)
  ) + ggplot2::geom_line(na.rm = hints$na.rm)
  if (isTRUE(hints$points)) plot <- plot + ggplot2::geom_point(na.rm = hints$na.rm)
  plot +
    ggplot2::scale_y_continuous(limits = x$scale$limits, oob = .viz_squish) +
    ggplot2::labs(
      title = hints$title, subtitle = hints$subtitle, caption = hints$caption,
      x = "Time", y = hints$value_label
    )
}
