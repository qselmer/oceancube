# Internal renderer-neutral visualization data contract --------------------

.viz_data_schema_name <- "oceancube_viz_data"
.viz_data_schema_version <- "1.0.0"
.viz_data_kinds <- c(
  "MAP_LAYER", "PROFILE", "SECTION", "TRANSECT_SECTION",
  "TRANSECT_LINE", "TIMESERIES", "HOVMOLLER"
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

.viz_scale_classes <- c(
  "SEQUENTIAL", "DIVERGING", "CYCLIC", "CATEGORICAL",
  "UNSPECIFIED_CONTINUOUS"
)

.viz_support_geometry_classes <- c(
  "EXPLICIT_CELL_BOUNDS", "STORED_CENTRES"
)

.viz_display_unit <- function(unit) {
  if (is.null(unit) || length(unit) != 1L || is.na(unit) || !nzchar(unit)) {
    return(unit)
  }
  if (identical(unit, "degC")) "\u00B0C" else unit
}

.viz_display_footprint <- function(x, y) {
  spacing <- function(values, role) {
    numeric_values <- if (inherits(values, c("Date", "POSIXct", "POSIXt"))) {
      as.numeric(values)
    } else if (is.numeric(values)) {
      as.numeric(values)
    } else {
      numeric()
    }
    differences <- diff(sort(unique(numeric_values[is.finite(numeric_values)])))
    differences <- differences[is.finite(differences) & differences > 0]
    if (!length(differences)) {
      .viz_abort(
        paste0("A display footprint requires at least two finite `", role,
               "` centres."),
        "oceancube_viz_data_error"
      )
    }
    min(differences)
  }

  list(
    semantics = "DISPLAY_ONLY",
    source = "DERIVED_FROM_STORED_CENTRES",
    method = "MINIMUM_POSITIVE_CENTRE_SPACING",
    x_width = spacing(x, "time"),
    y_height = spacing(y, "coordinate"),
    scientific_cell_bounds = FALSE,
    enters_cf_metadata = FALSE
  )
}

.viz_scale_spec <- function(
    classification = "UNSPECIFIED_CONTINUOUS",
    limits = NULL,
    centre = NULL,
    palette = "viridis") {
  if (!is.character(classification) || length(classification) != 1L ||
      is.na(classification) || !classification %in% .viz_scale_classes) {
    .viz_abort(
      paste0(
        "`classification` must be one of ",
        paste0("`", .viz_scale_classes, "`", collapse = ", "), "."
      ),
      "oceancube_viz_scale_error"
    )
  }
  if (!is.null(limits) &&
      (!is.numeric(limits) || !is.null(dim(limits)) || length(limits) != 2L ||
       any(!is.finite(limits)) || limits[[1L]] >= limits[[2L]])) {
    .viz_abort(
      "`limits` must be NULL or two finite numeric values with min < max.",
      "oceancube_viz_scale_error"
    )
  }
  if (!is.null(centre) &&
      (!is.numeric(centre) || !is.null(dim(centre)) || length(centre) != 1L ||
       is.na(centre) || !is.finite(centre))) {
    .viz_abort(
      "`centre` must be NULL or one finite numeric value.",
      "oceancube_viz_scale_error"
    )
  }
  if (identical(classification, "DIVERGING") && is.null(centre)) {
    .viz_abort(
      "A DIVERGING scale requires an explicit scientifically meaningful `centre`.",
      "oceancube_viz_scale_error"
    )
  }
  if (!identical(classification, "DIVERGING") && !is.null(centre)) {
    .viz_abort(
      "`centre` is available only for a DIVERGING scale.",
      "oceancube_viz_scale_error"
    )
  }
  if (!is.character(palette) || length(palette) != 1L ||
      is.na(palette) || !nzchar(palette) || !identical(palette, "viridis")) {
    .viz_abort(
      "D2A supports only the deterministic internal `viridis` palette.",
      "oceancube_viz_scale_error"
    )
  }
  list(
    classification = classification,
    limits = limits,
    centre = centre,
    palette = palette
  )
}

.viz_palette_resolve <- function(scale) {
  if (!is.list(scale) || !identical(scale$palette, "viridis") ||
      !is.character(scale$classification) ||
      !scale$classification %in% .viz_scale_classes) {
    .viz_abort(
      "The prepared visualization scale has no supported deterministic palette.",
      "oceancube_viz_scale_error"
    )
  }
  list(
    engine = "ggplot2::scale_fill_viridis_c",
    family = "viridis",
    option = "D",
    direction = 1,
    na.value = "grey85"
  )
}

.viz_map_scale_spec <- function(classification, limits = NULL, centre = NULL,
                                effective_range) {
  allowed <- c("UNSPECIFIED_CONTINUOUS", "SEQUENTIAL", "DIVERGING")
  if (!is.character(classification) || length(classification) != 1L ||
      is.na(classification) || !classification %in% allowed) {
    .viz_abort(
      paste0("Map `scale_class` must resolve to one of ",
             paste0("`", allowed, "`", collapse = ", "), "."),
      "oceancube_viz_scale_error"
    )
  }
  if (identical(classification, "DIVERGING")) {
    if (!is.numeric(effective_range) || length(effective_range) != 2L ||
        any(!is.finite(effective_range)) || effective_range[[1L]] >=
        effective_range[[2L]]) {
      .viz_abort("The effective diverging map display range is invalid.",
                 "oceancube_viz_scale_error")
    }
    if (!is.numeric(centre) || !is.null(dim(centre)) || length(centre) != 1L ||
        is.na(centre) || !is.finite(centre)) {
      .viz_abort(
        "A DIVERGING map scale requires an explicit finite `center`.",
        "oceancube_viz_scale_error"
      )
    }
    if (centre <= effective_range[[1L]] || centre >= effective_range[[2L]]) {
      .viz_abort(
        "A DIVERGING map `center` must lie strictly inside the effective display range.",
        "oceancube_viz_scale_error"
      )
    }
  } else if (!is.null(centre)) {
    .viz_abort(
      "`center` is available only for `scale_class = \"diverging\"`.",
      "oceancube_viz_scale_error"
    )
  }
  palette <- switch(
    classification,
    UNSPECIFIED_CONTINUOUS = "ggplot2_default_continuous",
    SEQUENTIAL = "viridis_D",
    DIVERGING = "base_hcl_Blue-Red_3"
  )
  list(
    classification = classification,
    limits = limits,
    centre = centre,
    palette = palette
  )
}

.viz_map_contour_spec <- function(values, limits = NULL, breaks = NULL,
                                  required = FALSE) {
  if (!isTRUE(required)) {
    return(list(breaks = NULL, rule = "NOT_APPLICABLE"))
  }
  finite <- as.numeric(values[is.finite(values)])
  display_range <- if (is.null(limits)) range(finite) else limits
  if (length(finite) < 2L || diff(display_range) <= 0) {
    .viz_abort(
      "Contour rendering requires at least two distinct finite display values.",
      "oceancube_viz_style_error"
    )
  }
  if (is.null(breaks)) {
    breaks <- pretty(display_range, n = 7L)
    breaks <- breaks[breaks > display_range[[1L]] &
                       breaks < display_range[[2L]]]
    if (!length(breaks)) {
      breaks <- seq(display_range[[1L]], display_range[[2L]], length.out = 9L)
      breaks <- breaks[-c(1L, 9L)]
    }
    rule <- "PRETTY_FINITE_DISPLAY_RANGE_N7"
  } else {
    breaks <- as.numeric(breaks)
    if (!any(breaks > display_range[[1L]] & breaks < display_range[[2L]])) {
      .viz_abort(
        "At least one `contour_breaks` value must lie inside the effective display range.",
        "oceancube_viz_style_error"
      )
    }
    rule <- "EXPLICIT_USER_LEVELS"
  }
  list(breaks = breaks, rule = rule)
}

.viz_map_display_footprint <- function(longitude, latitude) {
  minimum_spacing <- function(values) {
    values <- sort(unique(as.numeric(values[is.finite(values)])))
    differences <- diff(values)
    differences <- differences[is.finite(differences) & differences > 0]
    if (length(differences)) min(differences) else NA_real_
  }
  list(
    semantics = "DISPLAY_ONLY",
    source = "DERIVED_FROM_STORED_CENTRES",
    method = "MINIMUM_POSITIVE_CENTRE_SPACING",
    x_width = minimum_spacing(longitude),
    y_height = minimum_spacing(latitude),
    scientific_cell_bounds = FALSE,
    enters_cf_metadata = FALSE
  )
}

.viz_map_wrap_longitude <- function(values, mode) {
  values <- as.numeric(values)
  if (identical(mode, "SOURCE")) return(values)
  wrapped <- if (identical(mode, "NEG180_180")) {
    ((values + 180) %% 360) - 180
  } else if (identical(mode, "ZERO_360")) {
    values %% 360
  } else {
    .viz_abort("Unsupported map longitude display mode.",
               "oceancube_viz_longitude_error")
  }
  order_source <- order(values)
  if (length(wrapped) > 1L &&
      any(abs(diff(wrapped[order_source])) > 180, na.rm = TRUE)) {
    .viz_abort(
      paste0(
        "Longitude display wrapping introduces an internal dateline ",
        "discontinuity; D2B does not connect or split that geometry."
      ),
      "oceancube_viz_longitude_error"
    )
  }
  wrapped
}

.viz_map_wrap_coastline <- function(coastline, type, mode) {
  if (identical(mode, "SOURCE") || is.null(coastline)) return(coastline)
  if (identical(type, "sf")) {
    .viz_abort(
      paste0(
        "A non-SOURCE longitude display currently requires a data-frame ",
        "coastline so dateline safety can be checked explicitly."
      ),
      "oceancube_viz_longitude_error"
    )
  }
  if (!identical(type, "data.frame")) return(coastline)
  out <- coastline
  split_rows <- split(seq_len(nrow(out)), out$group)
  for (rows in split_rows) {
    out$longitude[rows] <- .viz_map_wrap_longitude(
      out$longitude[rows], mode
    )
  }
  out
}

.viz_map_longitude_labels <- function(values, mode) {
  format_number <- function(value) {
    format(abs(value), trim = TRUE, scientific = FALSE)
  }
  if (identical(mode, "ZERO_360")) {
    return(ifelse(values == 0, "0\u00B0", paste0(format_number(values), "\u00B0E")))
  }
  ifelse(
    values < 0, paste0(format_number(values), "\u00B0W"),
    ifelse(values > 0, paste0(format_number(values), "\u00B0E"), "0\u00B0")
  )
}

.viz_map_contour_data <- function(data, breaks) {
  x <- sort(unique(data$longitude[is.finite(data$longitude)]))
  y <- sort(unique(data$latitude[is.finite(data$latitude)]))
  if (length(x) < 2L || length(y) < 2L) {
    .viz_abort(
      "Contour rendering requires at least two stored longitude and latitude centres.",
      "oceancube_viz_style_error"
    )
  }
  z <- matrix(NA_real_, nrow = length(x), ncol = length(y))
  ix <- match(data$longitude, x)
  iy <- match(data$latitude, y)
  valid <- !is.na(ix) & !is.na(iy)
  z[cbind(ix[valid], iy[valid])] <- data$value[valid]
  contours <- grDevices::contourLines(x = x, y = y, z = z, levels = breaks)
  if (!length(contours)) {
    .viz_abort(
      "The requested contour levels produced no display geometry.",
      "oceancube_viz_style_error"
    )
  }
  do.call(rbind, lapply(seq_along(contours), function(index) {
    item <- contours[[index]]
    data.frame(
      longitude = item$x,
      latitude = item$y,
      level = item$level,
      piece = index,
      stringsAsFactors = FALSE
    )
  }))
}

.viz_map_palette_resolve <- function(scale) {
  if (identical(scale$classification, "UNSPECIFIED_CONTINUOUS")) {
    return(list(engine = "ggplot2_default_continuous", colours = NULL))
  }
  if (identical(scale$classification, "SEQUENTIAL")) {
    return(list(engine = "ggplot2_viridis_D", colours = NULL))
  }
  if (identical(scale$classification, "DIVERGING")) {
    return(list(
      engine = "grDevices_hcl_Blue-Red_3",
      colours = grDevices::hcl.colors(3L, palette = "Blue-Red 3")
    ))
  }
  .viz_abort("Unsupported D2B map scale classification.",
             "oceancube_viz_scale_error")
}

.viz_map_fill_scale <- function(scale, name) {
  palette <- .viz_map_palette_resolve(scale)
  if (identical(scale$classification, "UNSPECIFIED_CONTINUOUS")) {
    return(ggplot2::scale_fill_continuous(
      name = name, limits = scale$limits, oob = .viz_squish
    ))
  }
  if (identical(scale$classification, "SEQUENTIAL")) {
    return(ggplot2::scale_fill_viridis_c(
      name = name, limits = scale$limits, oob = .viz_squish,
      option = "D", direction = 1, na.value = "grey85"
    ))
  }
  ggplot2::scale_fill_gradient2(
    name = name, limits = scale$limits, oob = .viz_squish,
    low = palette$colours[[1L]], mid = palette$colours[[2L]],
    high = palette$colours[[3L]], midpoint = scale$centre,
    na.value = "grey85"
  )
}

.viz_map_colour_scale <- function(scale, name) {
  palette <- .viz_map_palette_resolve(scale)
  if (identical(scale$classification, "UNSPECIFIED_CONTINUOUS")) {
    return(ggplot2::scale_colour_continuous(
      name = name, limits = scale$limits, oob = .viz_squish
    ))
  }
  if (identical(scale$classification, "SEQUENTIAL")) {
    return(ggplot2::scale_colour_viridis_c(
      name = name, limits = scale$limits, oob = .viz_squish,
      option = "D", direction = 1, na.value = "grey85"
    ))
  }
  ggplot2::scale_colour_gradient2(
    name = name, limits = scale$limits, oob = .viz_squish,
    low = palette$colours[[1L]], mid = palette$colours[[2L]],
    high = palette$colours[[3L]], midpoint = scale$centre,
    na.value = "grey85"
  )
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
  if (!is.list(x$scale) || !is.character(x$scale$classification) ||
      length(x$scale$classification) != 1L || is.na(x$scale$classification) ||
      !x$scale$classification %in% .viz_scale_classes ||
      (!is.null(x$scale$limits) &&
       (!is.numeric(x$scale$limits) || length(x$scale$limits) != 2L ||
        any(!is.finite(x$scale$limits)) || diff(x$scale$limits) <= 0)) ||
      (identical(x$scale$classification, "DIVERGING") &&
       (is.null(x$scale$centre) || !is.numeric(x$scale$centre) ||
        length(x$scale$centre) != 1L || !is.finite(x$scale$centre))) ||
      (!is.null(x$scale$centre) &&
       !identical(x$scale$classification, "DIVERGING")) ||
      (!is.null(x$scale$palette) &&
       (!is.character(x$scale$palette) || length(x$scale$palette) != 1L ||
        is.na(x$scale$palette) || !nzchar(x$scale$palette)))) {
    .viz_abort("Invalid prepared visualization scale classification.",
               "oceancube_viz_data_error")
  }
  if (identical(x$kind, "HOVMOLLER")) {
    footprint <- if (is.list(x$support)) x$support$display_footprint else NULL
    if (!is.list(x$support) ||
        !is.character(x$support$geometry) ||
        length(x$support$geometry) != 1L ||
        !x$support$geometry %in% .viz_support_geometry_classes ||
        !identical(x$support$geometry, "STORED_CENTRES") ||
        !is.list(footprint) ||
        !identical(footprint$semantics, "DISPLAY_ONLY") ||
        !identical(footprint$source, "DERIVED_FROM_STORED_CENTRES") ||
        !identical(footprint$method, "MINIMUM_POSITIVE_CENTRE_SPACING") ||
        !is.numeric(footprint$x_width) || length(footprint$x_width) != 1L ||
        !is.finite(footprint$x_width) || footprint$x_width <= 0 ||
        !is.numeric(footprint$y_height) || length(footprint$y_height) != 1L ||
        !is.finite(footprint$y_height) || footprint$y_height <= 0 ||
        !identical(footprint$scientific_cell_bounds, FALSE) ||
        !identical(footprint$enters_cf_metadata, FALSE) ||
        !is.null(x$support$scientific_bounds) ||
        !identical(
          x$support$explicit_cell_bounds_runtime,
          "DEFERRED_NOT_CERTIFIED_D2A"
        )) {
      .viz_abort(
        "Invalid HOVMOLLER support-geometry metadata.",
        "oceancube_viz_data_error"
      )
    }
  }
  if (identical(x$kind, "MAP_LAYER")) {
    footprint <- if (is.list(x$support)) x$support$display_footprint else NULL
    map_styles <- c("FIELD", "CONTOUR", "FIELD_CONTOUR")
    longitude_modes <- c("SOURCE", "NEG180_180", "ZERO_360")
    if (!is.list(x$support) ||
        !identical(x$support$geometry, "STORED_CENTRES") ||
        !is.null(x$support$scientific_bounds) ||
        !identical(
          x$support$explicit_cell_bounds_runtime,
          "DEFERRED_NOT_CERTIFIED_D2B"
        ) ||
        !is.list(footprint) ||
        !identical(footprint$semantics, "DISPLAY_ONLY") ||
        !identical(footprint$source, "DERIVED_FROM_STORED_CENTRES") ||
        !identical(footprint$method, "MINIMUM_POSITIVE_CENTRE_SPACING") ||
        !identical(footprint$scientific_cell_bounds, FALSE) ||
        !identical(footprint$enters_cf_metadata, FALSE) ||
        !is.character(x$renderer_hints$map_style) ||
        length(x$renderer_hints$map_style) != 1L ||
        !x$renderer_hints$map_style %in% map_styles ||
        !is.character(x$renderer_hints$longitude_display) ||
        length(x$renderer_hints$longitude_display) != 1L ||
        !x$renderer_hints$longitude_display %in% longitude_modes) {
      .viz_abort("Invalid MAP_LAYER D2B renderer/support metadata.",
                 "oceancube_viz_data_error")
    }
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
    HOVMOLLER = .viz_render_hovmoller_ggplot(x),
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
  source_longitude <- data$longitude
  data$longitude <- .viz_map_wrap_longitude(
    source_longitude, hints$longitude_display
  )
  coastline <- .viz_map_wrap_coastline(
    hints$coastline, hints$coastline_type, hints$longitude_display
  )
  style <- hints$map_style

  if (identical(style, "FIELD")) {
    plot <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data$longitude, y = .data$latitude, fill = .data$value
      )
    )
    plot <- if (isTRUE(x$geometry$regular_grid)) {
      plot + ggplot2::geom_raster(na.rm = hints$na.rm)
    } else {
      plot + ggplot2::geom_tile(na.rm = hints$na.rm)
    }
    plot <- plot + .viz_map_fill_scale(x$scale, hints$value_label)
  } else if (style %in% c("CONTOUR", "FIELD_CONTOUR")) {
    contour_data <- .viz_map_contour_data(data, hints$contour_breaks)
    if (identical(style, "CONTOUR")) {
      plot <- ggplot2::ggplot(data) +
        ggplot2::geom_path(
          data = contour_data,
          mapping = ggplot2::aes(
            x = .data$longitude, y = .data$latitude,
            group = .data$piece, colour = .data$level
          ),
          inherit.aes = FALSE, linewidth = 0.45, na.rm = TRUE
        ) +
        .viz_map_colour_scale(x$scale, hints$value_label) +
        ggplot2::expand_limits(
          x = range(data$longitude), y = range(data$latitude)
        )
    } else {
      plot <- ggplot2::ggplot(
        data,
        ggplot2::aes(
          x = .data$longitude, y = .data$latitude, fill = .data$value
        )
      )
      plot <- if (isTRUE(x$geometry$regular_grid)) {
        plot + ggplot2::geom_raster(na.rm = hints$na.rm)
      } else {
        plot + ggplot2::geom_tile(na.rm = hints$na.rm)
      }
      plot <- plot +
        ggplot2::geom_path(
          data = contour_data,
          mapping = ggplot2::aes(
            x = .data$longitude, y = .data$latitude, group = .data$piece
          ),
          inherit.aes = FALSE, colour = "black", linewidth = 0.35,
          na.rm = TRUE
        ) +
        .viz_map_fill_scale(x$scale, hints$value_label)
    }
  } else {
    .viz_abort("Unsupported D2B map style.", "oceancube_viz_style_error")
  }

  plot <- plot +
    ggplot2::labs(
      title = hints$title, subtitle = hints$subtitle, caption = hints$caption,
      x = "Longitude", y = "Latitude"
    )
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
  if (!identical(hints$longitude_display, "SOURCE")) {
    plot <- plot + ggplot2::scale_x_continuous(
      labels = function(values) {
        .viz_map_longitude_labels(values, hints$longitude_display)
      }
    )
  }
  plot <- suppressMessages(plot + ggplot2::coord_equal(expand = FALSE))
  attr(plot, "oceancube_source_longitude_range") <- range(source_longitude)
  attr(plot, "oceancube_display_longitude_range") <- range(data$longitude)
  attr(plot, "oceancube_contour_geometry") <- if (
    style %in% c("CONTOUR", "FIELD_CONTOUR")
  ) {
    "DISPLAY_CONTOUR_GEOMETRY"
  } else {
    "NONE"
  }
  plot
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

.viz_render_hovmoller_ggplot <- function(x) {
  hints <- x$renderer_hints
  data <- .viz_prepared_table(x)
  axis <- x$geometry$axis
  palette <- .viz_palette_resolve(x$scale)
  footprint <- x$support$display_footprint
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$time, y = .data[[axis]], fill = .data$value)
  ) +
    ggplot2::geom_tile(
      width = footprint$x_width,
      height = footprint$y_height,
      colour = "white",
      linewidth = 0.18,
      na.rm = hints$na.rm
    ) +
    ggplot2::scale_fill_viridis_c(
      name = hints$value_label,
      limits = x$scale$limits,
      oob = .viz_squish,
      option = palette$option,
      direction = palette$direction,
      na.value = palette$na.value
    ) +
    ggplot2::labs(
      title = hints$title,
      subtitle = hints$subtitle,
      caption = hints$caption,
      x = "Time",
      y = hints$axis_label
    ) +
    ggplot2::theme_minimal()
  if (identical(axis, "depth") && isTRUE(x$depth$display_reverse)) {
    plot <- plot + ggplot2::scale_y_reverse()
  }
  plot
}
