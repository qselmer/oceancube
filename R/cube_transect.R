#' Extract an ordered ocean transect as a table
#'
#' `cube_transect()` is an experimental API being stabilized for 0.2.0. It
#' resolves the rows of `path` as ordered longitude-latitude pairs and extracts
#' one time, one or more depths, and selected variables. Unlike
#' [cube_extract()], spatial coordinates never form a Cartesian product.
#'
#' @param x A valid `<ocean_cube>` using the memory or NetCDF backend.
#' @param path A data frame (or matrix) with one row per ordered point. Values
#'   are interpreted as geographic longitude and latitude in degrees. Objects
#'   inheriting from `sf` or `sfc` are rejected explicitly; convert them to an
#'   ordinary longitude-latitude table in EPSG:4326 first.
#' @param lon_col,lat_col Column names containing longitude and latitude values,
#'   or one-based positions when `by = "index"`.
#' @param id_col Optional column name used as `point_id`. Factors are converted
#'   safely to their labels. Duplicated identifiers are preserved.
#' @param depth,time,variable Depth, time, and variable selectors. Exactly one
#'   time must resolve. `NULL` retains an entire axis, except that `time = NULL`
#'   is valid only for a singleton time axis.
#' @param by Whether selectors contain coordinate `"value"`s or one-based
#'   `"index"` positions.
#' @param match `"nearest"` selects the closest stored coordinate within each
#'   axis domain; `"exact"` requires a stored coordinate. Ties select the first
#'   stored position. This argument is not supplied with `by = "index"`. The
#'   historical default is nearest; omitting `match` emits a compatibility
#'   warning so new code makes the scientific choice explicit.
#' @param tolerance Optional fully named list of non-negative maximum distances
#'   for nearest matching. Spatial and depth tolerances are numeric; time uses
#'   `difftime`.
#' @param mode `"profile"` requires one point, `"horizontal"` at least two
#'   points and one depth, and `"section"` at least two points and at least two
#'   depths. `"auto"` infers these rules.
#' @param format `"long"` returns point-depth-variable rows. `"wide"` returns
#'   one point-depth-time row and one column per variable.
#' @param keep_index Include global one-based cube indices for longitude,
#'   latitude, depth, time, and variable. The default remains `FALSE` for
#'   backward-compatible schemas.
#'
#' @return A fully materialized base `data.frame`. Long output contains
#'   `point_id`, `point_order`, `longitude_requested`, `latitude_requested`,
#'   `longitude`, `latitude`, `match_distance_km`, `requested_distance_km`,
#'   `matched_distance_km`, `depth`, `time`, `variable`, `unit`, and `value`.
#'   When `keep_index = TRUE`, global `longitude_index`, `latitude_index`,
#'   `depth_index`, `time_index`, and `variable_index` columns are appended.
#'
#' @details
#' Row order in long format is point, then depth, then variable. Supplied
#' vertices are not densified. Point order, repeated vertices, zero-length
#' segments, selected depth order, and variable order are preserved without
#' aggregation. A path of two or more points whose total requested distance is
#' zero produces a warning; a single point remains valid as a profile. Wide
#' output rejects duplicated variables and keeps scientific names without
#' syntactic conversion.
#'
#' All distances use the spherical Haversine formula with mean Earth radius
#' 6371.0088 km. `requested_distance_km` is cumulative distance along the user
#' path, `matched_distance_km` is cumulative distance along matched grid cells,
#' and `match_distance_km` is the pointwise displacement from each requested
#' coordinate to its matched grid coordinate. This is not exact ellipsoidal
#' distance. Antimeridian crossings (a segment with an absolute longitude
#' change greater than 180 degrees, including 359 to 1) are rejected;
#' longitudes are not normalized. Vertical distance is not added.
#'
#' Nearest matching selects cells independently on the longitude and latitude
#' axes. It does not interpolate spatially, vertically, or temporally. Explicit
#' nearest matching without `tolerance` emits a scientific warning; supplied
#' per-axis tolerances retain their existing units and semantics. Exact matching
#' never falls back to nearest. With `by = "index"`, requested and matched paths
#' use the selected cube coordinates and `match_distance_km` is zero.
#'
#' Ordinary path tables must use one recognized longitude convention,
#' `[-180, 180]` or `[0, 360]`, compatible with the cube. Latitude must lie in
#' `[-90, 90]`. CRS-bearing objects are never interpreted silently.
#'
#' Duplicate stored depth or time coordinates and duplicate explicit depth,
#' time, or variable selectors are rejected as ambiguous. The approved
#' singleton `NA_real_` surface-depth representation remains valid. Exactly one
#' time must resolve; `time = NULL` is therefore valid only for singleton time.
#' Fully and partially outside paths error; boundary points remain valid.
#'
#' NetCDF reads use one connection per call, one physical block per unique
#' spatial pair and unique selected variable, and reconstruct repeated points.
#' Non-contiguous depths are subset from their smallest enclosing vertical
#' block. A diagonal path therefore avoids reading its spatial bounding box.
#'
#' Function comparison:
#'
#' | Function | Spatial semantics | Output |
#' | --- | --- | --- |
#' | `cube_extract()` | Cartesian product | table |
#' | `link_events()` | event rows | enriched table |
#' | `cube_transect()` | ordered pairs | distance-depth table |
#' | `cube_crop()` | rectangular subdomain | cube |
#'
#' @seealso [cube_extract()], [link_events()], [cube_crop()]
#' @export
#'
#' @examples
#' values <- array(seq_len(3 * 2 * 2 * 1 * 1), dim = c(3, 2, 2, 1, 1))
#' cube <- ocean_cube(
#'   lon = c(-80, -79, -78), lat = c(-12, -11), depth = c(0, 50),
#'   time = as.Date("2021-02-01"), vars = "temperature",
#'   units = c(temperature = "degC"), data = values
#' )
#' path <- data.frame(
#'   station = c("A", "B", "C"),
#'   longitude = c(-80, -79, -78),
#'   latitude = c(-12, -11, -12)
#' )
#' cube_transect(
#'   cube, path, id_col = "station", depth = c(0, 50),
#'   time = as.Date("2021-02-01"), match = "exact", mode = "section"
#' )
cube_transect <- function(x, path, lon_col = "longitude",
                          lat_col = "latitude", id_col = NULL,
                          depth = NULL, time = NULL, variable = NULL,
                          by = c("value", "index"),
                          match = c("nearest", "exact"),
                          tolerance = NULL,
                          mode = c("auto", "horizontal", "section", "profile"),
                          format = c("long", "wide"), keep_index = FALSE) {
  match_was_missing <- missing(match)
  cube_validate(x, strict = TRUE)
  .transect_validate_cube_axes(x)
  backend <- .cube_backend(x)
  by <- base::match.arg(by)
  method <- base::match.arg(match)
  mode <- base::match.arg(mode)
  format <- base::match.arg(format)
  .validate_extract_flag(keep_index, "keep_index")

  if (identical(by, "index")) {
    if (!isTRUE(match_was_missing) || !is.null(tolerance)) {
      .abort_badarg(
        "by",
        "`by = \"index\"` accepts positions only; do not supply `match` or `tolerance`."
      )
    }
    method <- "index"
  } else if (identical(method, "exact") && !is.null(tolerance)) {
    .abort_badarg(
      "tolerance",
      "is available only with `by = \"value\", match = \"nearest\"`."
    )
  }
  plan <- .plan_cube_transect(
    x = x, path = path, lon_col = lon_col, lat_col = lat_col,
    id_col = id_col, depth = depth, time = time, variable = variable,
    by = by, method = method, tolerance = tolerance, mode = mode,
    format = format
  )
  if (identical(by, "value")) {
    if (isTRUE(match_was_missing)) {
      rlang::warn(
        paste(
          "`cube_transect()` preserved its legacy implicit nearest matching.",
          "For 0.2.0, specify `match = \"exact\"` or `match = \"nearest\"` explicitly."
        ),
        class = "oceancube_transect_compat_warning"
      )
    } else if (identical(method, "nearest") && is.null(tolerance)) {
      rlang::warn(
        paste(
          "`match = \"nearest\"` has no explicit maximum tolerance.",
          "Supply per-axis `tolerance` values when unrestricted snapping is not scientifically appropriate."
        ),
        class = "oceancube_transect_matching_warning"
      )
    }
  }
  values <- .cube_read_spatial_pairs(
    x,
    plan$points$longitude_index,
    plan$points$latitude_index,
    plan$depth_index,
    plan$time_index,
    plan$variable_index
  )
  read_metrics <- attr(values, "oceancube_read_metrics", exact = TRUE)
  attr(values, "oceancube_read_metrics") <- NULL
  expected <- c(
    nrow(plan$points), length(plan$depth_index), 1L,
    length(plan$variable_index)
  )
  if (!is.array(values) || !identical(unname(dim(values)), as.integer(expected))) {
    rlang::abort(
      "The backend pair reader returned an invalid point-depth-time-variable shape.",
      class = "oceancube_transect_backend"
    )
  }

  units <- .extract_table_units(x, plan$variable_index)
  result <- if (identical(format, "long")) {
    .cube_transect_long(x, plan, values, units, keep_index)
  } else {
    .cube_transect_wide(x, plan, values, units, keep_index)
  }
  provenance <- list(
    operation = "cube_transect",
    backend = backend,
    mode = plan$mode,
    format = format,
    by = by,
    match = method,
    n_points = nrow(plan$points),
    n_unique_grid_cells = nrow(plan$unique_pairs),
    depth_indices = plan$depth_index,
    time_index = plan$time_index,
    variable_indices = plan$variable_index,
    distance_method = "spherical Haversine; mean Earth radius 6371.0088 km",
    distance_units = "km",
    requested_path_length_km =
      plan$requested_distance_km[[length(plan$requested_distance_km)]],
    matched_path_length_km =
      plan$matched_distance_km[[length(plan$matched_distance_km)]],
    maximum_match_distance_km = max(plan$match_distance_km),
    match_distance_method =
      "spherical Haversine; mean Earth radius 6371.0088 km",
    tolerance = tolerance,
    physical_reads = read_metrics,
    source = x$source,
    dataset_id = x$dataset_id
  )
  attr(result, "oceancube_mode") <- plan$mode
  attr(result, "oceancube_backend") <- backend
  attr(result, "oceancube_path") <- plan$points
  attr(result, "oceancube_provenance") <- provenance
  attr(result, "units") <- stats::setNames(
    units, x$vars[plan$variable_index]
  )
  result
}

.transect_scalar_column <- function(x, arg, allow_null = FALSE) {
  if (isTRUE(allow_null) && is.null(x)) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .abort_badarg(arg, "must be one non-empty, non-missing column name.")
  }
  x
}

.transect_point_label <- function(order, id) {
  paste0("path point_order ", order, " (point_id ", encodeString(id), ")")
}

.transect_validate_cube_axes <- function(x) {
  surface <- length(x$depth) == 1L && is.na(x$depth[[1L]]) &&
    !is.nan(x$depth[[1L]])
  if (!surface && anyDuplicated(x$depth)) {
    rlang::abort(
      paste(
        "`cube_transect()` requires unique stored depth coordinates;",
        "the cube contains duplicated depth values."
      ),
      class = c(
        "oceancube_transect_selection_error",
        "oceancube_transect_error"
      )
    )
  }
  if (anyDuplicated(x$time)) {
    rlang::abort(
      paste(
        "`cube_transect()` requires unique stored time coordinates;",
        "the cube contains duplicated time values."
      ),
      class = c(
        "oceancube_transect_selection_error",
        "oceancube_transect_error"
      )
    )
  }
  invisible(TRUE)
}

.transect_validate_selector_duplicates <- function(depth, time, variable) {
  selectors <- list(depth = depth, time = time, variable = variable)
  duplicated <- names(selectors)[vapply(
    selectors,
    function(value) !is.null(value) && anyDuplicated(value) > 0L,
    logical(1)
  )]
  if (length(duplicated)) {
    detail <- if (identical(duplicated[[1L]], "variable")) {
      "Duplicate selector: duplicate variables are ambiguous in `cube_transect()`; supply each requested variable once."
    } else {
      paste0(
        "Duplicate `", duplicated[[1L]], "` selectors are ambiguous in ",
        "`cube_transect()`; supply each requested position once."
      )
    }
    rlang::abort(
      detail,
      class = c(
        "oceancube_transect_selection_error",
        "oceancube_transect_error"
      )
    )
  }
  invisible(TRUE)
}

.transect_validate_longitude_convention <- function(requested, stored) {
  recognized <- all(requested >= -180 & requested <= 180) ||
    all(requested >= 0 & requested <= 360)
  if (!recognized) {
    rlang::abort(
      paste(
        "Path longitude values must use one recognized geographic convention:",
        "[-180, 180] or [0, 360]."
      ),
      class = c("oceancube_transect_crs_error", "oceancube_transect_error")
    )
  }
  incompatible <- (any(stored < 0) && any(requested > 180)) ||
    (any(stored > 180) && any(requested < 0))
  if (incompatible) {
    rlang::abort(
      paste(
        "Path and cube longitudes use incompatible geographic conventions.",
        "Convert the path explicitly before extraction."
      ),
      class = c("oceancube_transect_crs_error", "oceancube_transect_error")
    )
  }
  invisible(TRUE)
}

.transect_resolve_spatial <- function(axis_values, requested, axis, method,
                                      tolerance, point_id) {
  index <- integer(length(requested))
  matched <- numeric(length(requested))
  distance <- numeric(length(requested))
  for (i in seq_along(requested)) {
    resolved <- tryCatch(
      .resolve_axis_values(
        axis_values, requested[[i]], axis, method, tolerance
      ),
      error = function(e) {
        rlang::abort(
          paste0(
            "Invalid ", .transect_point_label(i, point_id[[i]]), ": ",
            conditionMessage(e)
          ),
          class = c("oceancube_transect_point", class(e)),
          parent = e
        )
      }
    )
    index[[i]] <- resolved$index
    matched[[i]] <- resolved$matched
    distance[[i]] <- resolved$distance
  }
  list(index = index, matched = matched, distance = distance)
}

.plan_cube_transect <- function(x, path, lon_col, lat_col, id_col,
                                depth, time, variable, by, method,
                                tolerance, mode, format) {
  if (inherits(path, "sf") || inherits(path, "sfc")) {
    rlang::abort(
      paste(
        "`cube_transect()` does not yet accept sf/sfc paths because their CRS",
        "must not be ignored. Transform to EPSG:4326 and convert explicitly",
        "to an ordinary data.frame with longitude/latitude columns in degrees."
      ),
      class = c("oceancube_transect_crs_error", "oceancube_transect_error")
    )
  }
  if (is.matrix(path)) {
    path <- as.data.frame(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
  if (!is.data.frame(path)) {
    .abort_badarg("path", "must be a data frame or matrix with ordered point rows.")
  }
  if (nrow(path) == 0L) {
    .abort_badarg("path", "must contain at least one point row.")
  }
  lon_col <- .transect_scalar_column(lon_col, "lon_col")
  lat_col <- .transect_scalar_column(lat_col, "lat_col")
  id_col <- .transect_scalar_column(id_col, "id_col", allow_null = TRUE)
  required <- c(lon_col, lat_col, id_col)
  missing_columns <- setdiff(required, names(path))
  if (length(missing_columns)) {
    .abort_badarg(
      "path",
      paste0(
        "missing column(s): ", paste(missing_columns, collapse = ", "),
        ". Supply existing `lon_col`, `lat_col`, and `id_col` names."
      )
    )
  }
  if (identical(lon_col, lat_col)) {
    .abort_badarg("path", "`lon_col` and `lat_col` must identify different columns.")
  }
  requested_lon <- path[[lon_col]]
  requested_lat <- path[[lat_col]]
  if (!is.numeric(requested_lon) || !is.null(dim(requested_lon)) ||
      !is.numeric(requested_lat) || !is.null(dim(requested_lat))) {
    .abort_badarg(
      "path",
      "longitude and latitude columns must be numeric vectors."
    )
  }
  point_id <- if (is.null(id_col)) seq_len(nrow(path)) else path[[id_col]]
  if (!is.atomic(point_id) || !is.null(dim(point_id))) {
    .abort_badarg("id_col", "must identify an atomic vector column.")
  }
  if (is.factor(point_id)) point_id <- as.character(point_id)

  .transect_validate_selector_duplicates(depth, time, variable)

  selectors <- list(
    longitude = requested_lon,
    latitude = requested_lat,
    depth = depth,
    time = time,
    variable = variable
  )
  validated_tolerance <- if (identical(method, "nearest")) {
    .validate_slice_tolerance(tolerance, selectors)
  } else {
    NULL
  }

  if (identical(by, "index")) {
    resolved <- .resolve_cube_slice(
      x, selectors, by = by, method = method, tolerance = NULL,
      allow_variable_duplicates = TRUE
    )
    longitude_index <- resolved$index$longitude
    latitude_index <- resolved$index$latitude
    requested_lon_geo <- x$lon[longitude_index]
    requested_lat_geo <- x$lat[latitude_index]
  } else {
    if (anyNA(requested_lon) || any(!is.finite(requested_lon)) ||
        anyNA(requested_lat) || any(!is.finite(requested_lat))) {
      bad <- which(
        is.na(requested_lon) | !is.finite(requested_lon) |
          is.na(requested_lat) | !is.finite(requested_lat)
      )[[1L]]
      rlang::abort(
        paste0(
          "Invalid ", .transect_point_label(bad, point_id[[bad]]),
          ": longitude and latitude must be finite and non-missing."
        ),
        class = "oceancube_transect_point"
      )
    }
    if (any(requested_lat < -90 | requested_lat > 90)) {
      bad <- which(requested_lat < -90 | requested_lat > 90)[[1L]]
      rlang::abort(
        paste0(
          "Invalid ", .transect_point_label(bad, point_id[[bad]]),
          ": latitude ", requested_lat[[bad]],
          " is outside [-90, 90]. Correct the geographic coordinate."
        ),
        class = "oceancube_transect_point"
      )
    }
    .transect_validate_longitude_convention(requested_lon, x$lon)
    lon_resolved <- .transect_resolve_spatial(
      x$lon, requested_lon, "longitude", method,
      validated_tolerance$longitude, point_id
    )
    lat_resolved <- .transect_resolve_spatial(
      x$lat, requested_lat, "latitude", method,
      validated_tolerance$latitude, point_id
    )
    longitude_index <- lon_resolved$index
    latitude_index <- lat_resolved$index
    requested_lon_geo <- requested_lon
    requested_lat_geo <- requested_lat
    remaining_tolerance <- validated_tolerance[
      intersect(names(validated_tolerance), c("depth", "time"))
    ]
    if (length(remaining_tolerance) == 0L) remaining_tolerance <- NULL
    resolved <- .resolve_cube_slice(
      x,
      selectors = list(
        longitude = NULL, latitude = NULL, depth = depth,
        time = time, variable = variable
      ),
      by = by, method = method, tolerance = remaining_tolerance,
      allow_variable_duplicates = TRUE
    )
  }

  if (length(resolved$index$time) != 1L) {
    .abort_badarg(
      "time",
      paste0(
        "must resolve exactly one position; resolved ",
        length(resolved$index$time),
        ". Supply one time for a multitemporal cube."
      )
    )
  }
  if (identical(format, "wide") &&
      anyDuplicated(resolved$index$variable)) {
    .abort_badarg(
      "format",
      "`format = \"wide\"` cannot represent duplicate variables; remove duplicates or use long format."
    )
  }
  matched_lon <- x$lon[longitude_index]
  matched_lat <- x$lat[latitude_index]
  .transect_validate_antimeridian(requested_lon_geo, "requested")
  .transect_validate_antimeridian(matched_lon, "matched")
  requested_distance <- .transect_cumulative_distance(
    requested_lon_geo, requested_lat_geo
  )
  matched_distance <- .transect_cumulative_distance(matched_lon, matched_lat)
  match_distance <- .transect_point_distance(
    requested_lon_geo, requested_lat_geo, matched_lon, matched_lat
  )
  if (nrow(path) > 1L && requested_distance[[length(requested_distance)]] == 0) {
    rlang::warn(
      paste(
        "The supplied transect has zero total requested distance.",
        "Repeated vertices are retained for backward compatibility."
      ),
      class = "oceancube_transect_zero_length_warning"
    )
  }
  inferred_mode <- .transect_mode(
    mode, nrow(path), length(resolved$index$depth)
  )
  pair_map <- .spatial_pair_map(longitude_index, latitude_index)
  points <- data.frame(
    point_id = point_id,
    point_order = seq_len(nrow(path)),
    longitude_requested = requested_lon_geo,
    latitude_requested = requested_lat_geo,
    longitude_index = longitude_index,
    latitude_index = latitude_index,
    longitude = matched_lon,
    latitude = matched_lat,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  list(
    points = points,
    unique_pairs = pair_map$unique_pairs,
    point_to_pair = pair_map$point_to_pair,
    depth_index = resolved$index$depth,
    time_index = resolved$index$time,
    variable_index = resolved$index$variable,
    requested_distance_km = requested_distance,
    matched_distance_km = matched_distance,
    match_distance_km = match_distance,
    mode = inferred_mode,
    output_shape = c(
      point = nrow(path), depth = length(resolved$index$depth),
      time = 1L, variable = length(resolved$index$variable)
    )
  )
}

.transect_mode <- function(mode, n_points, n_depth) {
  inferred <- if (n_points == 1L) {
    "profile"
  } else if (n_depth == 1L) {
    "horizontal"
  } else {
    "section"
  }
  selected <- if (identical(mode, "auto")) inferred else mode
  valid <- switch(
    selected,
    profile = n_points == 1L,
    horizontal = n_points >= 2L && n_depth == 1L,
    section = n_points >= 2L && n_depth >= 2L,
    FALSE
  )
  if (!valid) {
    rlang::abort(
      paste0(
        "Mode `", selected, "` is incompatible with ", n_points,
        " point(s) and ", n_depth, " depth position(s). Use `", inferred,
        "` or adjust the selectors."
      ),
      class = "oceancube_transect_mode"
    )
  }
  selected
}

.transect_validate_antimeridian <- function(longitude, label) {
  if (length(longitude) > 1L) {
    crossing <- which(abs(diff(longitude)) > 180)
    if (length(crossing)) {
      rlang::abort(
        paste0(
          "The ", label, " path segment ", crossing[[1L]], "->",
          crossing[[1L]] + 1L,
          " crosses the antimeridian (longitude change > 180 degrees). ",
          "This version does not normalize or support antimeridian crossings."
        ),
        class = "oceancube_transect_antimeridian"
      )
    }
  }
  invisible(TRUE)
}

.transect_cumulative_distance <- function(longitude, latitude) {
  if (length(longitude) == 1L) return(0)
  segment <- .transect_point_distance(
    longitude[-length(longitude)], latitude[-length(latitude)],
    longitude[-1L], latitude[-1L]
  )
  c(0, cumsum(segment))
}

.transect_point_distance <- function(longitude1, latitude1,
                                     longitude2, latitude2) {
  radians <- pi / 180
  lon1 <- longitude1 * radians
  lon2 <- longitude2 * radians
  lat1 <- latitude1 * radians
  lat2 <- latitude2 * radians
  a <- sin((lat2 - lat1) / 2)^2 +
    cos(lat1) * cos(lat2) * sin((lon2 - lon1) / 2)^2
  a <- pmin(1, pmax(0, a))
  2 * 6371.0088 * asin(sqrt(a))
}

.cube_transect_long <- function(x, plan, values, units, keep_index) {
  n_point <- nrow(plan$points)
  n_depth <- length(plan$depth_index)
  n_variable <- length(plan$variable_index)
  point_position <- rep(seq_len(n_point), each = n_depth * n_variable)
  depth_position <- rep(
    rep(seq_len(n_depth), each = n_variable),
    times = n_point
  )
  variable_position <- rep(seq_len(n_variable), times = n_point * n_depth)
  value <- values[cbind(
    point_position, depth_position, 1L, variable_position
  )]
  points <- plan$points[point_position, , drop = FALSE]
  out <- data.frame(
    point_id = points$point_id,
    point_order = points$point_order,
    longitude_requested = points$longitude_requested,
    latitude_requested = points$latitude_requested,
    longitude = points$longitude,
    latitude = points$latitude,
    match_distance_km = plan$match_distance_km[point_position],
    requested_distance_km =
      plan$requested_distance_km[point_position],
    matched_distance_km = plan$matched_distance_km[point_position],
    depth = x$depth[plan$depth_index][depth_position],
    time = x$time[plan$time_index],
    variable = x$vars[plan$variable_index][variable_position],
    unit = units[variable_position],
    value = value,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (isTRUE(keep_index)) {
    out$longitude_index <- points$longitude_index
    out$latitude_index <- points$latitude_index
    out$depth_index <- plan$depth_index[depth_position]
    out$time_index <- plan$time_index[[1L]]
    out$variable_index <- plan$variable_index[variable_position]
  }
  rownames(out) <- NULL
  out
}

.cube_transect_wide <- function(x, plan, values, units, keep_index) {
  n_point <- nrow(plan$points)
  n_depth <- length(plan$depth_index)
  point_position <- rep(seq_len(n_point), each = n_depth)
  depth_position <- rep(seq_len(n_depth), times = n_point)
  points <- plan$points[point_position, , drop = FALSE]
  out <- data.frame(
    point_id = points$point_id,
    point_order = points$point_order,
    longitude_requested = points$longitude_requested,
    latitude_requested = points$latitude_requested,
    longitude = points$longitude,
    latitude = points$latitude,
    match_distance_km = plan$match_distance_km[point_position],
    requested_distance_km =
      plan$requested_distance_km[point_position],
    matched_distance_km = plan$matched_distance_km[point_position],
    depth = x$depth[plan$depth_index][depth_position],
    time = x$time[plan$time_index],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (i in seq_along(plan$variable_index)) {
    out[[x$vars[plan$variable_index[[i]]]]] <-
      values[cbind(point_position, depth_position, 1L, i)]
  }
  if (isTRUE(keep_index)) {
    out$longitude_index <- points$longitude_index
    out$latitude_index <- points$latitude_index
    out$depth_index <- plan$depth_index[depth_position]
    out$time_index <- plan$time_index[[1L]]
  }
  rownames(out) <- NULL
  out
}
