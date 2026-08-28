#' Mask an ocean cube with polygon cell-centre coverage
#'
#' `cube_mask()` classifies the longitude-latitude cell centres of an
#' `<ocean_cube>` against polygon geometry. It preserves the complete five
#' dimensional cube and replaces values in cells not retained by the mask with
#' `NA`.
#'
#' @param x A valid `<ocean_cube>` using the memory or NetCDF backend.
#' @param polygons An `sf`, `sfc`, or polygonal `sfg` object containing only
#'   `POLYGON` or `MULTIPOLYGON` geometries.
#' @param crs Optional CRS used only to assign a CRS to geometry that has none.
#'   Geometry with an existing CRS is never reinterpreted.
#' @param keep Whether to retain centres `"inside"` or `"outside"` the polygon
#'   union.
#' @param boundary Whether cell centres on polygon boundaries are included or
#'   excluded.
#'
#' @return A memory-backed `<ocean_cube>` with the same coordinates, units,
#'   dimensions, and extents as `x`. Its `<ocean_mask>` records coverage and
#'   lightweight geometry metadata.
#'
#' @details
#' Coverage is determined only from cell centres. `fraction_cells_kept` is a
#' fraction of cell centers, not a fraction of geographic area. No cell areas,
#' partial overlaps, interpolation, resampling, or area weighting are computed.
#'
#' Several polygon features are combined as one geometric union; overlaps are
#' counted once and polygon holes are respected. `boundary = "include"` uses a
#' covered-by relation, while `"exclude"` uses strict within. Exact boundary
#' classification is subject to the numerical behaviour of the spatial engine.
#'
#' Cubes currently have no explicit CRS field, so longitude and latitude are
#' interpreted as geographic EPSG:4326 coordinates. Polygon geometry must
#' already use a compatible geographic CRS and the same longitude convention
#' as `x$lon`. Geometry is not transformed. Dateline-crossing polygons are not
#' supported in this phase.
#'
#' An existing compatible 3D `<ocean_mask>` is combined using logical AND and
#' never overwritten or reactivated. Distance-to-coast metadata is preserved.
#' Climatology, anomaly, and dimensional QA components are discarded because
#' they can become stale.
#'
#' A NetCDF input is read only after centre classification. For `keep =
#' "inside"`, only the smallest longitude-latitude bounding rectangle containing
#' effective retained centres is read. `keep = "outside"` may require a full
#' spatial read in the initial implementation. Output is always fully
#' materialized in memory and independent of the source file.
#'
#' | Function | Geometry | Action | Output |
#' | --- | --- | --- | --- |
#' | `cube_crop()` | rectangular range | reduces axes | smaller cube |
#' | `cube_mask()` | polygon | replaces outside with `NA` | same-shape cube |
#' | future `cube_polygon_summary()` | polygon | summarizes values | table |
#'
#' @seealso [cube_crop()], [stock_mask()], [crop_stock()]
#' @export
#'
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   values <- array(seq_len(3 * 2), dim = c(3, 2, 1, 1, 1))
#'   cube <- ocean_cube(
#'     lon = c(-80, -79, -78), lat = c(-12, -11), depth = 0,
#'     time = as.Date("2021-01-01"), vars = "temperature", data = values
#'   )
#'   polygon <- sf::st_sfc(sf::st_polygon(list(matrix(
#'     c(-80.5, -12.5, -78.5, -12.5, -78.5, -10.5,
#'       -80.5, -10.5, -80.5, -12.5),
#'     ncol = 2, byrow = TRUE
#'   ))), crs = 4326)
#'   cube_mask(cube, polygon)
#' }
cube_mask <- function(x, polygons, crs = NULL,
                      keep = c("inside", "outside"),
                      boundary = c("include", "exclude")) {
  .check_cube(x)
  backend <- .cube_backend(x)
  if (!backend %in% c("memory", "netcdf")) {
    rlang::abort(
      paste0("Unsupported ocean_cube backend: '", backend, "'."),
      class = "oceancube_unsupported_backend"
    )
  }
  keep <- base::match.arg(keep)
  boundary <- base::match.arg(boundary)

  geometry <- .normalize_polygon_geometry(polygons, crs)
  centers <- .cube_cell_centers(x)
  inside <- .classify_cell_centers(
    centers$points, geometry$union, boundary
  )
  inside <- array(
    inside,
    dim = c(longitude = length(x$lon), latitude = length(x$lat)),
    dimnames = list(
      longitude = as.character(x$lon),
      latitude = as.character(x$lat)
    )
  )
  if (identical(keep, "inside") && !any(inside)) {
    rlang::abort(
      paste(
        "The polygon geometry contains no cube cell centers.",
        "Use a geometry covering at least one stored centre, or",
        "`keep = \"outside\"`."
      ),
      class = "oceancube_mask_no_centers"
    )
  }

  polygon_keep <- if (identical(keep, "inside")) inside else !inside
  polygon_keep_3d <- array(
    rep(polygon_keep, times = length(x$depth)),
    dim = c(length(x$lon), length(x$lat), length(x$depth)),
    dimnames = list(
      longitude = as.character(x$lon),
      latitude = as.character(x$lat),
      depth = as.character(x$depth)
    )
  )
  combined <- .combine_cube_masks(x, polygon_keep_3d)
  effective_spatial <- apply(combined$keep, c(1L, 2L), any)
  read_plan <- .plan_masked_cube_read(
    x, backend, keep, effective_spatial
  )
  values <- .read_masked_cube_values(x, read_plan)
  dimnames(values) <- .canonical_cube_dimnames(x)
  values <- .apply_cube_mask_5d(values, combined$keep)
  coverage <- .mask_coverage(
    x, inside, polygon_keep, combined$keep,
    geometry$n_features
  )

  mask <- .new_polygon_ocean_mask(
    x = x,
    keep = combined$keep,
    polygon_keep = polygon_keep_3d,
    coverage = coverage,
    boundary = boundary,
    keep_mode = keep,
    geometry = geometry
  )
  discarded <- c(
    if (!is.null(x$climatology)) "climatology",
    if (!is.null(x$anomaly)) "anomaly"
  )
  qa <- x$qa
  if (!is.null(qa) && .selection_contains_dimensional_data(qa)) {
    qa <- NULL
    discarded <- c(discarded, "qa")
  }
  if (is.null(qa)) qa <- list()
  if (!is.list(qa)) qa <- list(previous = qa)
  qa$mask <- list(
    coverage = coverage,
    bounding_rectangle_read = read_plan$metrics,
    discarded_components = discarded
  )
  provenance_context <- .provenance_cube_context(
    source = x$source,
    dataset_id = x$dataset_id,
    time = x$time,
    shape = .cube_shape(x),
    variables = x$vars,
    backend = "memory",
    provenance = x$provenance
  )
  provenance <- .provenance_append(
    x$provenance,
    operation = "cube_mask",
    parameters = list(
      requested = list(
        keep = keep,
        boundary = boundary,
        geometry = list(
          crs = geometry$crs,
          bbox = geometry$bbox,
          n_features = as.integer(geometry$n_features),
          geometry_type = geometry$types
        )
      ),
      resolved = list(
        mask_semantics = "cell_center",
        n_spatial_cells_total = coverage$n_spatial_cells_total,
        n_spatial_cells_kept = coverage$n_spatial_cells_kept,
        fraction_cells_kept = coverage$fraction_cells_kept,
        combined_with_existing_mask = combined$combined
      )
    ),
    output = .provenance_summary(provenance_context),
    scientific_method = .provenance_method("cube_mask", list()),
    context = provenance_context
  )
  output_template <- x
  output_template$qa <- qa
  output_template$metadata <- .cf_metadata_for_transform(
    x$metadata %||% NULL,
    "cube_mask"
  )

  out <- .new_collected_memory_cube(output_template, values, provenance)
  out$mask <- mask
  out$climatology <- NULL
  out$anomaly <- NULL
  out$qa <- qa
  .check_cube(out)
  out
}

.normalize_polygon_geometry <- function(polygons, crs,
                                        operation = "cube_mask",
                                        union_features = TRUE) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    rlang::abort(
      paste0(
        "Package `sf` is required for `", operation,
        "()`; install it before processing polygons."
      ),
      class = "oceancube_mask_missing_sf"
    )
  }
  if (missing(polygons) || is.null(polygons)) {
    .abort_badarg(
      "polygons",
      "must be an sf, sfc, or polygonal sfg object."
    )
  }
  original_class <- class(polygons)
  geometry <- if (inherits(polygons, "sf")) {
    sf::st_geometry(polygons)
  } else if (inherits(polygons, "sfc")) {
    polygons
  } else if (inherits(polygons, "sfg")) {
    sf::st_sfc(polygons)
  } else {
    .abort_badarg(
      "polygons",
      paste0(
        "must inherit from sf, sfc, or sfg; received class ",
        paste(original_class, collapse = "/"),
        ". Convert the object explicitly with `sf` first."
      )
    )
  }
  if (length(geometry) == 0L) {
    .abort_badarg("polygons", "must contain at least one polygon feature.")
  }

  polygon_crs <- sf::st_crs(geometry)
  supplied_crs <- if (is.null(crs)) NULL else sf::st_crs(crs)
  if (!is.null(crs) && is.na(supplied_crs)) {
    .abort_badarg("crs", "must identify a valid coordinate reference system.")
  }
  if (is.na(polygon_crs)) {
    if (is.null(supplied_crs)) {
      .abort_badarg(
        "polygons",
        "has no CRS; supply `crs` for unreferenced geometry."
      )
    }
    sf::st_crs(geometry) <- supplied_crs
    polygon_crs <- supplied_crs
  } else if (!is.null(supplied_crs) &&
             !isTRUE(polygon_crs == supplied_crs)) {
    .abort_badarg(
      "crs",
      paste0(
        "cannot reinterpret geometry that already has a different CRS; ",
        "transform it explicitly before calling `", operation, "()`."
      )
    )
  }
  if (!isTRUE(sf::st_is_longlat(geometry))) {
    .abort_badarg(
      "polygons",
      "must use a geographic longitude-latitude CRS compatible with EPSG:4326; transform projected geometry explicitly."
    )
  }

  types <- as.character(sf::st_geometry_type(geometry, by_geometry = TRUE))
  invalid_types <- setdiff(unique(types), c("POLYGON", "MULTIPOLYGON"))
  if (length(invalid_types)) {
    .abort_badarg(
      "polygons",
      paste0(
        "contains unsupported geometry type(s): ",
        paste(invalid_types, collapse = ", "),
        ". Only POLYGON and MULTIPOLYGON are allowed; convert geometry explicitly."
      )
    )
  }
  if (any(sf::st_is_empty(geometry))) {
    .abort_badarg(
      "polygons",
      "contains empty geometry; remove or replace empty features."
    )
  }
  valid <- sf::st_is_valid(geometry)
  if (anyNA(valid) || any(!valid)) {
    .abort_badarg(
      "polygons",
      paste0(
        "contains invalid geometry; validate or repair it explicitly before ",
        "calling `", operation, "()`."
      )
    )
  }
  coordinates <- sf::st_coordinates(geometry)
  if (length(coordinates) == 0L ||
      anyNA(coordinates[, c("X", "Y"), drop = FALSE]) ||
      any(!is.finite(coordinates[, c("X", "Y"), drop = FALSE]))) {
    .abort_badarg(
      "polygons",
      "must contain finite, non-missing coordinates."
    )
  }
  if (.polygon_crosses_antimeridian(geometry)) {
    .abort_badarg(
      "polygons",
      "appears to cross the antimeridian; dateline-crossing polygons are not supported in this phase."
    )
  }
  union <- if (isTRUE(union_features)) {
    tryCatch(
      sf::st_union(geometry),
      error = function(e) {
        rlang::abort(
          paste0(
            "Failed to union polygon features: ", conditionMessage(e),
            ". Validate the geometry explicitly."
          ),
          class = "oceancube_mask_union",
          parent = e
        )
      }
    )
  } else {
    NULL
  }
  bbox <- sf::st_bbox(if (is.null(union)) geometry else union)
  list(
    geometry = geometry,
    union = union,
    n_features = length(geometry),
    types = unique(types),
    crs = polygon_crs$input %||% polygon_crs$wkt,
    bbox = stats::setNames(as.numeric(bbox), names(bbox))
  )
}

.polygon_crosses_antimeridian <- function(geometry) {
  ring_crosses <- function(x) {
    if (is.matrix(x)) {
      return(nrow(x) > 1L && any(abs(diff(x[, 1L])) > 180))
    }
    if (is.list(x)) {
      return(any(vapply(x, ring_crosses, logical(1))))
    }
    FALSE
  }
  any(vapply(unclass(geometry), ring_crosses, logical(1)))
}

.cube_cell_centers <- function(x) {
  grid <- expand.grid(
    longitude = x$lon,
    latitude = x$lat,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  points <- sf::st_as_sf(
    grid, coords = c("longitude", "latitude"),
    crs = 4326, remove = FALSE
  )
  list(grid = grid, points = points)
}

.classify_cell_centers <- function(points, polygon_union, boundary) {
  previous_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(previous_s2)), add = TRUE)
  relation <- tryCatch(
    suppressMessages(if (identical(boundary, "include")) {
      sf::st_covered_by(points, polygon_union)
    } else {
      sf::st_within(points, polygon_union)
    }),
    error = function(e) {
      rlang::abort(
        paste0(
          "Failed to classify cube cell centers against polygons: ",
          conditionMessage(e), ". Check geometry validity and CRS."
        ),
        class = "oceancube_mask_relation",
        parent = e
      )
    }
  )
  lengths(relation) > 0L
}

.combine_cube_masks <- function(x, polygon_keep_3d) {
  existing <- x$mask
  if (is.null(existing)) {
    return(list(keep = polygon_keep_3d, combined = FALSE))
  }
  if (!inherits(existing, "ocean_mask") ||
      !is.array(existing$mask) ||
      !is.logical(existing$mask) ||
      !identical(
        unname(dim(existing$mask)),
        unname(dim(polygon_keep_3d))
      ) ||
      anyNA(existing$mask) ||
      !identical(existing$lon, x$lon) ||
      !identical(existing$lat, x$lat) ||
      !identical(existing$depth, x$depth)) {
    rlang::abort(
      paste(
        "Existing `x$mask` is incompatible with the cube.",
        "It must be a complete non-missing logical <ocean_mask> aligned",
        "as longitude-latitude-depth."
      ),
      class = "oceancube_mask_existing"
    )
  }
  list(keep = existing$mask & polygon_keep_3d, combined = TRUE)
}

.typed_missing <- function(values) {
  switch(
    typeof(values),
    integer = NA_integer_,
    double = NA_real_,
    logical = NA,
    rlang::abort(
      "Masked cube values must be integer, double, or logical.",
      class = "oceancube_mask_type"
    )
  )
}

.canonical_cube_dimnames <- function(x) {
  list(
    lon = as.character(x$lon),
    lat = as.character(x$lat),
    depth = as.character(x$depth),
    time = as.character(x$time),
    var = x$vars
  )
}

.apply_cube_mask_5d <- function(values, keep_3d) {
  if (!is.array(values) || length(dim(values)) != 5L) {
    .abort_badarg(
      "values",
      "must be a five-dimensional longitude-latitude-depth-time-variable array."
    )
  }
  if (!is.array(keep_3d) || !is.logical(keep_3d) ||
      anyNA(keep_3d) ||
      !identical(unname(dim(keep_3d)), unname(dim(values)[1:3]))) {
    .abort_badarg(
      "spatial_keep",
      "must be a non-missing logical array matching longitude, latitude, and depth."
    )
  }
  output <- values
  missing <- .typed_missing(output)
  for (variable in seq_len(dim(output)[[5L]])) {
    for (time in seq_len(dim(output)[[4L]])) {
      block <- output[, , , time, variable, drop = FALSE]
      dim(block) <- dim(output)[1:3]
      block[!keep_3d] <- missing
      output[, , , time, variable] <- block
    }
  }
  dimnames(output) <- dimnames(values)
  output
}

.plan_masked_cube_read <- function(x, backend, keep_mode,
                                   effective_spatial) {
  shape <- .cube_shape(x)
  full_index <- stats::setNames(
    lapply(shape, seq_len), .cube_axis_names()
  )
  kept_positions <- which(effective_spatial, arr.ind = TRUE)
  read_full <- identical(backend, "memory") ||
    identical(keep_mode, "outside")
  if (nrow(kept_positions) == 0L) {
    longitude_index <- latitude_index <- integer()
    index <- NULL
  } else if (read_full) {
    longitude_index <- seq_len(shape[["longitude"]])
    latitude_index <- seq_len(shape[["latitude"]])
    index <- full_index
  } else {
    longitude_index <- seq.int(
      min(kept_positions[, 1L]), max(kept_positions[, 1L])
    )
    latitude_index <- seq.int(
      min(kept_positions[, 2L]), max(kept_positions[, 2L])
    )
    index <- full_index
    index$longitude <- longitude_index
    index$latitude <- latitude_index
  }
  n_bbox <- length(longitude_index) * length(latitude_index)
  n_kept <- sum(effective_spatial)
  list(
    index = index,
    longitude_index = longitude_index,
    latitude_index = latitude_index,
    full = read_full,
    backend = backend,
    metrics = list(
      longitude_start =
        if (length(longitude_index)) min(longitude_index) else NA_integer_,
      longitude_count = length(longitude_index),
      latitude_start =
        if (length(latitude_index)) min(latitude_index) else NA_integer_,
      latitude_count = length(latitude_index),
      spatial_cells_in_bbox = n_bbox,
      spatial_cells_kept = n_kept,
      read_amplification = if (n_kept > 0L) n_bbox / n_kept else NA_real_,
      n_open = if (identical(backend, "netcdf") &&
                    !is.null(index)) 1L else 0L,
      n_ncvar_get = if (identical(backend, "netcdf") &&
                         !is.null(index)) length(x$vars) else 0L,
      n_values_read = n_bbox * shape[["depth"]] *
        shape[["time"]] * shape[["variable"]],
      spatial_read = if (is.null(index)) {
        "none"
      } else if (read_full) {
        "full"
      } else {
        "bounding_rectangle"
      }
    )
  )
}

.read_masked_cube_values <- function(x, plan) {
  shape <- unname(.cube_shape(x))
  if (is.null(plan$index)) {
    return(array(
      NA_real_, dim = shape, dimnames = .canonical_cube_dimnames(x)
    ))
  }
  block <- .cube_read(x, index = plan$index)
  if (isTRUE(plan$full)) return(block)
  output <- array(
    .typed_missing(block),
    dim = shape,
    dimnames = .canonical_cube_dimnames(x)
  )
  output[
    plan$longitude_index,
    plan$latitude_index,
    seq_len(shape[[3L]]),
    seq_len(shape[[4L]]),
    seq_len(shape[[5L]])
  ] <- block
  output
}

.mask_coverage <- function(x, inside, polygon_keep, final_keep_3d,
                           n_features) {
  total <- length(inside)
  n_inside <- sum(inside)
  n_kept <- sum(polygon_keep)
  multiplier <- length(x$depth) * length(x$time) * length(x$vars)
  list(
    semantics = "cell_center",
    n_polygon_features = n_features,
    n_spatial_cells_total = total,
    n_centers_inside_polygon = n_inside,
    n_centers_outside_polygon = total - n_inside,
    n_spatial_cells_kept = n_kept,
    n_spatial_cells_masked = total - n_kept,
    fraction_centers_inside = n_inside / total,
    fraction_cells_kept = n_kept / total,
    fraction_cells_masked = (total - n_kept) / total,
    n_logical_values_total = total * multiplier,
    n_logical_values_kept_by_geometry = n_kept * multiplier,
    n_logical_values_masked_by_geometry = (total - n_kept) * multiplier,
    n_effective_cells_3d = sum(final_keep_3d),
    fraction_effective_cells_3d = mean(final_keep_3d)
  )
}

.new_polygon_ocean_mask <- function(x, keep, polygon_keep, coverage,
                                    boundary, keep_mode, geometry) {
  out <- list(
    stock = if (!is.null(x$mask)) x$mask$stock else NULL,
    mask = keep,
    keep = keep,
    polygon_keep = polygon_keep,
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    source = "polygon",
    semantics = "cell_center",
    boundary = boundary,
    keep_mode = keep_mode,
    coverage = coverage,
    geometry = list(
      crs = geometry$crs,
      bbox = geometry$bbox,
      n_features = geometry$n_features,
      geometry_type = geometry$types
    )
  )
  class(out) <- c("ocean_mask", "list")
  out
}
