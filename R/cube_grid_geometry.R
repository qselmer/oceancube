#' Compute horizontal cell areas for an ocean cube
#'
#' `cube_cell_area()` derives rectangular cell bounds from explicit metadata or,
#' for axes with at least two coordinates, from centre midpoints. Areas are
#' computed geodesically with the `sf` s2 engine. No scientific cube values are
#' read.
#'
#' @param x An [ocean_cube()] with one-dimensional longitude and latitude axes.
#' @param unit Output unit, either square metres (`"m2"`) or square kilometres
#'   (`"km2"`).
#'
#' @return A longitude-by-latitude numeric matrix with geometry provenance
#'   attributes.
#' @export
cube_cell_area <- function(x, unit = c("m2", "km2")) {
  .check_cube(x)
  unit <- match.arg(unit)
  grid <- .cube_horizontal_geometry(x)
  area_m2 <- .with_s2_geometry(function() {
    as.numeric(sf::st_area(grid$geometry))
  })
  values <- if (identical(unit, "km2")) area_m2 / 1e6 else area_m2
  out <- matrix(
    values,
    nrow = length(x$lon),
    ncol = length(x$lat),
    dimnames = list(
      longitude = as.character(x$lon),
      latitude = as.character(x$lat)
    )
  )
  attr(out, "unit") <- unit
  attr(out, "method") <- "sf+s2 geodesic polygon area"
  attr(out, "bounds_source") <- grid$bounds_source
  attr(out, "crs") <- "EPSG:4326"
  out
}

#' Compute vertical layer thicknesses for an ocean cube
#'
#' Vertical bounds must be supplied explicitly or be present in cube metadata;
#' they are never inferred from depth centres. Supported representations are an
#' interface vector of length `n_depth + 1` or an `n_depth`-by-2 matrix. Layer
#' thickness is the positive distance between bounds, so axes declared positive
#' upward or positive downward retain cube order without producing negative
#' thicknesses.
#'
#' @param x An [ocean_cube()] with a finite depth axis.
#' @param depth_bounds Explicit vertical interfaces or paired bounds. If `NULL`,
#'   metadata attached to the cube is used.
#' @param unit Output unit: native depth units, metres, or kilometres.
#'
#' @return A numeric vector in cube depth order.
#' @export
cube_layer_thickness <- function(x, depth_bounds = NULL,
                                 unit = c("native", "m", "km")) {
  .check_cube(x)
  unit <- match.arg(unit)
  vertical <- .cube_vertical_geometry(x, depth_bounds)
  factor <- .depth_conversion_factor(vertical$unit, unit)
  out <- vertical$thickness_native * factor
  names(out) <- as.character(x$depth)
  attr(out, "unit") <- if (identical(unit, "native")) {
    vertical$unit %||% "native"
  } else {
    unit
  }
  attr(out, "bounds_source") <- vertical$source
  attr(out, "positive") <- vertical$positive
  out
}

#' Compute three-dimensional cell volumes for an ocean cube
#'
#' Volumes combine geodesic horizontal cell areas with explicit vertical layer
#' thicknesses. No scientific cube values are read or aggregated; the result is
#' grid geometry, not a spatial indicator.
#'
#' @param x An [ocean_cube()] with rectilinear horizontal coordinates and a
#'   finite depth axis.
#' @param depth_bounds Explicit vertical interfaces or paired bounds. If `NULL`,
#'   metadata attached to the cube is used.
#' @param unit Output unit, either cubic metres (`"m3"`) or cubic kilometres
#'   (`"km3"`).
#'
#' @return A longitude-by-latitude-by-depth numeric array.
#' @export
cube_cell_volume <- function(x, depth_bounds = NULL,
                             unit = c("m3", "km3")) {
  .check_cube(x)
  unit <- match.arg(unit)
  area <- cube_cell_area(x, unit = "m2")
  thickness <- cube_layer_thickness(x, depth_bounds, unit = "m")
  out <- array(
    as.numeric(area) %o% as.numeric(thickness),
    dim = c(length(x$lon), length(x$lat), length(x$depth)),
    dimnames = list(
      longitude = as.character(x$lon),
      latitude = as.character(x$lat),
      depth = as.character(x$depth)
    )
  )
  if (identical(unit, "km3")) {
    out <- out / 1e9
  }
  attr(out, "unit") <- unit
  attr(out, "area_method") <- attr(area, "method")
  attr(out, "horizontal_bounds_source") <- attr(area, "bounds_source")
  attr(out, "vertical_bounds_source") <- attr(thickness, "bounds_source")
  attr(out, "crs") <- "EPSG:4326"
  out
}

.with_s2_geometry <- function(code) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    rlang::abort(
      "Package `sf` is required for cube geometry operations.",
      class = "oceancube_geometry_missing_sf"
    )
  }
  previous <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(TRUE))
  on.exit(suppressMessages(sf::sf_use_s2(previous)), add = TRUE)
  code()
}

.cube_horizontal_geometry <- function(x) {
  lon <- .cube_axis_bounds(x, "longitude")
  lat <- .cube_axis_bounds(x, "latitude")
  cells <- expand.grid(
    longitude_index = seq_along(x$lon),
    latitude_index = seq_along(x$lat),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  cells$longitude <- x$lon[cells$longitude_index]
  cells$latitude <- x$lat[cells$latitude_index]
  cells$lon_min <- lon$lower[cells$longitude_index]
  cells$lon_max <- lon$upper[cells$longitude_index]
  cells$lat_min <- lat$lower[cells$latitude_index]
  cells$lat_max <- lat$upper[cells$latitude_index]
  polygons <- lapply(seq_len(nrow(cells)), function(i) {
    ring <- rbind(
      c(cells$lon_min[i], cells$lat_min[i]),
      c(cells$lon_max[i], cells$lat_min[i]),
      c(cells$lon_max[i], cells$lat_max[i]),
      c(cells$lon_min[i], cells$lat_max[i]),
      c(cells$lon_min[i], cells$lat_min[i])
    )
    sf::st_polygon(list(ring))
  })
  list(
    cells = cells,
    geometry = sf::st_sfc(polygons, crs = 4326),
    bounds_source = c(longitude = lon$source, latitude = lat$source)
  )
}

.cube_axis_bounds <- function(x, axis) {
  cube_name <- if (identical(axis, "longitude")) "lon" else "lat"
  coordinate <- x[[cube_name]]
  .validate_geometry_axis(coordinate, axis)
  explicit <- .axis_bounds_metadata(x, axis)
  source <- "inferred_from_centres"
  if (is.null(explicit)) {
    if (length(coordinate) == 1L) {
      .abort_badarg(
        cube_name,
        paste0(
          "is a singleton axis; explicit ", axis,
          " bounds are required for geometry."
        )
      )
    }
    interfaces <- c(
      coordinate[1L] - (coordinate[2L] - coordinate[1L]) / 2,
      (coordinate[-length(coordinate)] + coordinate[-1L]) / 2,
      coordinate[length(coordinate)] +
        (coordinate[length(coordinate)] -
           coordinate[length(coordinate) - 1L]) / 2
    )
    paired <- cbind(interfaces[-length(interfaces)], interfaces[-1L])
  } else {
    paired <- .bounds_to_pairs(explicit$value, length(coordinate), cube_name)
    source <- explicit$source
  }
  lower <- pmin(paired[, 1L], paired[, 2L])
  upper <- pmax(paired[, 1L], paired[, 2L])
  if (anyNA(paired) || any(!is.finite(paired)) || any(upper <= lower)) {
    .abort_badarg(
      paste0(cube_name, "_bounds"),
      "must contain finite pairs with strictly positive widths."
    )
  }
  tolerance <- 1e-10
  if (any(coordinate < lower - tolerance |
          coordinate > upper + tolerance)) {
    .abort_badarg(
      paste0(cube_name, "_bounds"),
      "must contain each coordinate centre in its corresponding cell."
    )
  }
  ordered <- order(lower, upper)
  if (length(ordered) > 1L &&
      any(lower[ordered][-1L] <
          upper[ordered][-length(ordered)] - tolerance)) {
    .abort_badarg(
      paste0(cube_name, "_bounds"),
      "contains overlapping horizontal cells."
    )
  }
  if (identical(axis, "latitude") &&
      (any(lower < -90 - tolerance) || any(upper > 90 + tolerance))) {
    .abort_badarg(
      "lat_bounds",
      "must remain within the geographic range [-90, 90]."
    )
  }
  if (identical(axis, "longitude")) {
    if (any(upper - lower >= 180 - tolerance)) {
      .abort_badarg(
        "lon_bounds",
        "must not span 180 degrees or cross the antimeridian."
      )
    }
    if (any(lower < -180 - tolerance) || any(upper > 360 + tolerance)) {
      .abort_badarg(
        "lon_bounds",
        "must use a conventional non-wrapping longitude domain."
      )
    }
  }
  list(lower = lower, upper = upper, source = source)
}

.validate_geometry_axis <- function(coordinate, axis) {
  name <- switch(
    axis,
    longitude = "lon",
    latitude = "lat",
    depth = "depth",
    axis
  )
  if (!is.numeric(coordinate) || !is.null(dim(coordinate)) ||
      !length(coordinate) || anyNA(coordinate) ||
      any(!is.finite(coordinate))) {
    .abort_badarg(
      name,
      "must be a finite one-dimensional numeric rectilinear axis."
    )
  }
  if (length(coordinate) > 1L) {
    delta <- diff(coordinate)
    if (any(delta == 0) || !(all(delta > 0) || all(delta < 0))) {
      .abort_badarg(
        name,
        "must be strictly monotonic without duplicate coordinates."
      )
    }
  }
  invisible(TRUE)
}

.axis_bounds_metadata <- function(x, axis) {
  short <- if (identical(axis, "longitude")) "lon" else "lat"
  coordinate <- x[[short]]
  attached <- attr(coordinate, "bounds", exact = TRUE)
  if (!is.null(attached)) {
    return(list(value = attached, source = paste0(short, " attribute")))
  }
  direct_name <- paste0(short, "_bounds")
  if (!is.null(x[[direct_name]])) {
    return(list(value = x[[direct_name]], source = direct_name))
  }
  geometry_name <- paste0(axis, "_bounds")
  if (!is.null(x$geometry) && !is.null(x$geometry[[geometry_name]])) {
    return(list(
      value = x$geometry[[geometry_name]],
      source = paste0("geometry$", geometry_name)
    ))
  }
  canonical <- x$storage$dimensions$canonical[[axis]]
  if (!is.null(canonical) && !is.null(canonical$bounds)) {
    return(list(
      value = canonical$bounds,
      source = paste0("NetCDF ", axis, " bounds")
    ))
  }
  NULL
}

.bounds_to_pairs <- function(bounds, n, name) {
  if (is.numeric(bounds) && is.null(dim(bounds))) {
    if (length(bounds) != n + 1L) {
      .abort_badarg(
        paste0(name, "_bounds"),
        paste0("must have length ", n + 1L, " for ", n, " cells.")
      )
    }
    return(cbind(bounds[-length(bounds)], bounds[-1L]))
  }
  if (is.matrix(bounds) && is.numeric(bounds) &&
      identical(dim(bounds), c(n, 2L))) {
    return(unname(bounds))
  }
  .abort_badarg(
    paste0(name, "_bounds"),
    paste0(
      "must be a numeric interface vector of length ", n + 1L,
      " or a ", n, "-by-2 matrix."
    )
  )
}

.cube_vertical_geometry <- function(x, depth_bounds) {
  depth <- x$depth
  if (!is.numeric(depth) || !is.null(dim(depth)) || !length(depth) ||
      anyNA(depth) || any(!is.finite(depth))) {
    .abort_badarg(
      "x",
      "must have a finite depth axis; surface cubes cannot define thickness or volume."
    )
  }
  .validate_geometry_axis(depth, "depth")
  metadata <- .vertical_bounds_metadata(x, depth_bounds)
  if (is.null(metadata$value)) {
    .abort_badarg(
      "depth_bounds",
      "must be supplied explicitly or stored unambiguously in cube metadata; depth bounds are never inferred from centres."
    )
  }
  paired <- .bounds_to_pairs(metadata$value, length(depth), "depth")
  depth_for_bounds <- depth
  if (!is.null(metadata$depth_unit) &&
      !identical(metadata$depth_unit, metadata$unit)) {
    depth_for_bounds <- depth *
      .depth_conversion_factor(metadata$depth_unit, metadata$unit)
  }
  lower <- pmin(paired[, 1L], paired[, 2L])
  upper <- pmax(paired[, 1L], paired[, 2L])
  if (anyNA(paired) || any(!is.finite(paired)) || any(upper <= lower)) {
    .abort_badarg(
      "depth_bounds",
      "must contain finite pairs with strictly positive thickness."
    )
  }
  tolerance <- 1e-10
  if (any(depth_for_bounds < lower - tolerance |
          depth_for_bounds > upper + tolerance)) {
    .abort_badarg(
      "depth_bounds",
      "must contain each depth centre in its corresponding layer."
    )
  }
  ordered <- order(lower, upper)
  if (length(ordered) > 1L &&
      any(lower[ordered][-1L] <
          upper[ordered][-length(ordered)] - tolerance)) {
    .abort_badarg("depth_bounds", "contains overlapping vertical layers.")
  }
  list(
    lower = lower,
    upper = upper,
    thickness_native = upper - lower,
    unit = metadata$unit,
    positive = metadata$positive,
    source = metadata$source
  )
}

.vertical_bounds_metadata <- function(x, depth_bounds) {
  attached <- attr(x$depth, "bounds", exact = TRUE)
  source <- "argument"
  value <- depth_bounds
  if (is.null(value) && !is.null(attached)) {
    value <- attached
    source <- "depth attribute"
  }
  if (is.null(value) && !is.null(x$depth_bounds)) {
    value <- x$depth_bounds
    source <- "depth_bounds"
  }
  if (is.null(value) && !is.null(x$geometry$depth_bounds)) {
    value <- x$geometry$depth_bounds
    source <- "geometry$depth_bounds"
  }
  canonical <- x$storage$dimensions$canonical$depth
  if (is.null(value) && !is.null(canonical$bounds)) {
    value <- canonical$bounds
    source <- "NetCDF depth bounds"
  }
  bound_unit <- .normalize_depth_unit(
    attr(value, "units", exact = TRUE) %||%
      attr(value, "unit", exact = TRUE)
  )
  depth_unit <- .normalize_depth_unit(
    attr(x$depth, "units", exact = TRUE) %||%
      attr(x$depth, "unit", exact = TRUE) %||%
      x$depth_units %||%
      x$geometry$depth_units %||%
      canonical$units
  )
  unit <- bound_unit %||% depth_unit
  positive <- attr(x$depth, "positive", exact = TRUE) %||%
    x$depth_positive %||%
    x$geometry$depth_positive %||%
    canonical$positive %||%
    "unspecified"
  list(
    value = value,
    source = source,
    unit = unit,
    depth_unit = depth_unit,
    positive = as.character(positive)[1L]
  )
}

.normalize_depth_unit <- function(unit) {
  if (is.null(unit) || !length(unit) || is.na(unit[1L]) ||
      !nzchar(trimws(as.character(unit[1L])))) {
    return(NULL)
  }
  value <- tolower(trimws(as.character(unit[1L])))
  if (value %in% c("m", "meter", "meters", "metre", "metres")) {
    return("m")
  }
  if (value %in% c("km", "kilometer", "kilometers",
                   "kilometre", "kilometres")) {
    return("km")
  }
  .abort_badarg(
    "depth_bounds",
    paste0(
      "uses unsupported depth unit `", value,
      "`; supported physical units are metres and kilometres."
    )
  )
}

.depth_conversion_factor <- function(source, target) {
  if (identical(target, "native")) {
    return(1)
  }
  if (is.null(source)) {
    .abort_badarg(
      "depth_bounds",
      paste0(
        "must declare depth units before conversion to `", target,
        "`; attach a `unit` or `units` attribute (`m` or `km`) to the bounds or depth axis."
      )
    )
  }
  if (identical(source, target)) {
    return(1)
  }
  if (identical(source, "km") && identical(target, "m")) {
    return(1000)
  }
  if (identical(source, "m") && identical(target, "km")) {
    return(1 / 1000)
  }
  rlang::abort("Internal unsupported depth-unit conversion.")
}
