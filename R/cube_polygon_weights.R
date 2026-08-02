#' Build polygon-to-cube geometric weights
#'
#' `cube_polygon_weights()` computes feature-specific geodesic intersections
#' against the rectilinear cells of an [ocean_cube()]. The result is a
#' self-contained table of merge keys and geometric weights intended for
#' downstream consumers such as `spatind`. It does not read cube values,
#' aggregate variables, or calculate spatial indicators.
#'
#' Polygon features are processed independently. Holes and multipart geometry
#' are preserved, and overlapping features are not unioned or normalised against
#' one another. Unlike [cube_mask()], this function measures cell intersection
#' instead of classifying cell centres and does not modify cube values. It
#' supplies geometric inputs for downstream analysis; indicator calculation and
#' inference remain the responsibility of packages such as `spatind`.
#'
#' @param x An [ocean_cube()] with one-dimensional longitude and latitude axes.
#' @param polygons An `sf`, `sfc`, or polygonal `sfg` object in a geographic CRS.
#' @param id_col Optional column name containing feature identifiers. It is
#'   available only when `polygons` is an `sf` object. Values and duplicates are
#'   preserved.
#' @param crs CRS to assign only when `polygons` has no CRS. Existing geometry is
#'   never reinterpreted.
#' @param dimension Generate horizontal (`"2d"`) or volumetric (`"3d"`) weights.
#' @param depth_bounds Explicit vertical interfaces or paired bounds for 3-D
#'   weights. If `NULL`, unambiguous cube metadata is used.
#' @param include_zero Include all feature-cell combinations, including zero
#'   intersections.
#'
#' @return A data frame with stable feature/cell keys, bounds, areas, fractions,
#'   coverage diagnostics, and—when `dimension = "3d"`—depth and volume fields.
#' @export
cube_polygon_weights <- function(x, polygons, id_col = NULL, crs = NULL,
                                 dimension = c("2d", "3d"),
                                 depth_bounds = NULL,
                                 include_zero = FALSE) {
  .check_cube(x)
  dimension <- match.arg(dimension)
  if (!is.logical(include_zero) || length(include_zero) != 1L ||
      is.na(include_zero)) {
    .abort_badarg("include_zero", "must be one non-missing logical value.")
  }
  polygon_input <- polygons
  normalized <- .normalize_polygon_geometry(
    polygons,
    crs,
    operation = "cube_polygon_weights",
    union_features = FALSE
  )
  if (!isTRUE(sf::st_crs(normalized$geometry) == sf::st_crs(4326))) {
    .abort_badarg(
      "polygons",
      "must use EPSG:4326; transform geographic geometry explicitly before calculating weights."
    )
  }
  ids <- .polygon_feature_ids(polygon_input, id_col, normalized$n_features)
  grid <- .cube_horizontal_geometry(x)
  calculated <- .with_s2_geometry(function() {
    .polygon_cell_intersections(normalized$geometry, grid)
  })
  base <- .polygon_weight_table(
    ids = ids,
    cells = grid$cells,
    cell_area = calculated$cell_area,
    overlap = calculated$overlap,
    include_zero = include_zero
  )
  coverage <- .polygon_feature_coverage(
    ids,
    calculated$polygon_area,
    calculated$overlap
  )
  base <- .add_polygon_coverage_columns(base, coverage)
  vertical_source <- NULL
  if (identical(dimension, "3d")) {
    vertical <- .cube_vertical_geometry(x, depth_bounds)
    thickness_m <- vertical$thickness_native *
      .depth_conversion_factor(vertical$unit, "m")
    base <- .expand_polygon_weights_depth(
      base, x$depth, vertical, thickness_m
    )
    vertical_source <- vertical$source
  }
  attr(base, "oceancube_geometry") <- "polygon-cell intersection weights"
  attr(base, "dimension") <- dimension
  attr(base, "area_method") <- "sf+s2 geodesic polygon area/intersection"
  attr(base, "area_unit") <- "m2"
  attr(base, "volume_unit") <- if (identical(dimension, "3d")) "m3" else NULL
  attr(base, "horizontal_bounds_source") <- grid$bounds_source
  attr(base, "vertical_bounds_source") <- vertical_source
  attr(base, "polygon_crs") <- normalized$crs
  attr(base, "n_features") <- normalized$n_features
  attr(base, "feature_coverage") <- coverage
  attr(base, "provenance") <- list(
    operation = "cube_polygon_weights",
    package = "oceancube",
    role = "geometric weights only; no indicator calculation",
    dimension = dimension,
    area_method = "sf+s2",
    crs = "EPSG:4326",
    bounds_source = list(
      horizontal = grid$bounds_source,
      vertical = vertical_source
    ),
    n_features = normalized$n_features,
    n_cells = nrow(grid$cells),
    n_feature_cell_pairs = normalized$n_features * nrow(grid$cells),
    n_candidates = calculated$n_candidates,
    n_intersections = nrow(calculated$overlap),
    include_zero = include_zero,
    depth_source = vertical_source,
    intended_consumer = "spatind or another explicit downstream package"
  )
  base
}

.polygon_feature_ids <- function(polygons, id_col, n_features) {
  if (is.null(id_col)) {
    return(seq_len(n_features))
  }
  if (!is.character(id_col) || length(id_col) != 1L ||
      is.na(id_col) || !nzchar(id_col)) {
    .abort_badarg("id_col", "must be NULL or one non-empty column name.")
  }
  if (!inherits(polygons, "sf")) {
    .abort_badarg(
      "id_col",
      "can be used only when `polygons` is an sf object."
    )
  }
  if (!id_col %in% names(polygons)) {
    .abort_badarg(
      "id_col",
      paste0("column `", id_col, "` is not present in `polygons`.")
    )
  }
  ids <- polygons[[id_col]]
  if (length(ids) != n_features) {
    rlang::abort("Internal polygon identifier length mismatch.")
  }
  ids
}

.polygon_cell_intersections <- function(polygons, grid) {
  cell_area <- as.numeric(sf::st_area(grid$geometry))
  polygon_area <- as.numeric(sf::st_area(polygons))
  overlap <- vector("list", length(polygons))
  n_candidates <- 0L
  tolerance <- 1e-10
  for (feature_index in seq_along(polygons)) {
    bbox <- sf::st_bbox(polygons[feature_index])
    candidate <- which(
      grid$cells$lon_max >= unname(bbox["xmin"]) &
        grid$cells$lon_min <= unname(bbox["xmax"]) &
        grid$cells$lat_max >= unname(bbox["ymin"]) &
        grid$cells$lat_min <= unname(bbox["ymax"])
    )
    n_candidates <- n_candidates + length(candidate)
    feature_overlap <- vector("list", length(candidate))
    for (candidate_index in seq_along(candidate)) {
      cell_index <- candidate[candidate_index]
      intersection <- suppressWarnings(
        sf::st_intersection(
          grid$geometry[cell_index],
          polygons[feature_index]
        )
      )
      if (length(intersection)) {
        value <- sum(as.numeric(sf::st_area(intersection)))
        fraction <- value / cell_area[cell_index]
        if (fraction < 0 && fraction >= -tolerance) {
          value <- 0
          fraction <- 0
        }
        if (fraction > 1 && fraction <= 1 + tolerance) {
          value <- cell_area[cell_index]
          fraction <- 1
        }
        if (!is.finite(fraction) || fraction < 0 || fraction > 1) {
          rlang::abort(
            "Polygon intersection fractions fell outside [0, 1] beyond numerical tolerance.",
            class = "oceancube_geometry_precision"
          )
        }
        if (value > 0) {
          feature_overlap[[candidate_index]] <- data.frame(
            feature_order = as.integer(feature_index),
            cell_index = as.integer(cell_index),
            overlap_area_m2 = value
          )
        }
      }
    }
    overlap[[feature_index]] <- feature_overlap
  }
  overlap <- Filter(Negate(is.null), unlist(overlap, recursive = FALSE))
  if (length(overlap)) {
    overlap <- do.call(rbind, overlap)
    rownames(overlap) <- NULL
  } else {
    overlap <- data.frame(
      feature_order = integer(),
      cell_index = integer(),
      overlap_area_m2 = numeric()
    )
  }
  list(
    cell_area = cell_area,
    polygon_area = polygon_area,
    overlap = overlap,
    n_candidates = n_candidates
  )
}

.polygon_weight_table <- function(ids, cells, cell_area, overlap,
                                  include_zero) {
  if (include_zero) {
    index <- expand.grid(
      feature_order = seq_along(ids),
      cell_index = seq_len(nrow(cells)),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    index <- index[order(index$feature_order, index$cell_index), , drop = FALSE]
    key <- paste(index$feature_order, index$cell_index, sep = ":")
    overlap_key <- paste(
      overlap$feature_order, overlap$cell_index, sep = ":"
    )
    matched <- match(key, overlap_key)
    values <- overlap$overlap_area_m2[matched]
    values[is.na(values)] <- 0
  } else {
    index <- overlap[c("feature_order", "cell_index")]
    values <- overlap$overlap_area_m2
  }
  cell_index <- as.integer(index$cell_index)
  feature_order <- as.integer(index$feature_order)
  cell_values <- cell_area[cell_index]
  fraction <- if (length(values)) values / cell_values else numeric()
  out <- data.frame(
    feature_id = ids[feature_order],
    feature_order = feature_order,
    longitude_index = as.integer(cells$longitude_index[cell_index]),
    latitude_index = as.integer(cells$latitude_index[cell_index]),
    cell_index = cell_index,
    longitude = cells$longitude[cell_index],
    latitude = cells$latitude[cell_index],
    lon_min = cells$lon_min[cell_index],
    lon_max = cells$lon_max[cell_index],
    lat_min = cells$lat_min[cell_index],
    lat_max = cells$lat_max[cell_index],
    cell_area_m2 = cell_values,
    overlap_area_m2 = values,
    fraction_cell_covered = fraction,
    effective_area_m2 = values,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rownames(out) <- NULL
  out
}

.polygon_feature_coverage <- function(ids, polygon_area, overlap) {
  intersected <- numeric(length(ids))
  if (nrow(overlap)) {
    sums <- tapply(
      overlap$overlap_area_m2,
      overlap$feature_order,
      sum
    )
    intersected[as.integer(names(sums))] <- as.numeric(sums)
  }
  fraction <- ifelse(
    polygon_area > 0,
    intersected / polygon_area,
    0
  )
  tolerance <- 1e-10
  fraction[fraction < 0 & fraction >= -tolerance] <- 0
  fraction[fraction > 1 & fraction <= 1 + tolerance] <- 1
  data.frame(
    feature_id = ids,
    feature_order = as.integer(seq_along(ids)),
    polygon_area_m2 = polygon_area,
    intersected_grid_area_m2 = intersected,
    fraction_polygon_covered_by_grid = fraction,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.add_polygon_coverage_columns <- function(weights, coverage) {
  order <- weights$feature_order
  weights$polygon_area_m2 <- coverage$polygon_area_m2[order]
  weights$intersected_grid_area_m2 <-
    coverage$intersected_grid_area_m2[order]
  weights$fraction_polygon_covered_by_grid <-
    coverage$fraction_polygon_covered_by_grid[order]
  weights
}

.expand_polygon_weights_depth <- function(weights, depth, vertical,
                                          thickness_m) {
  n_depth <- length(depth)
  row_index <- rep(seq_len(nrow(weights)), each = n_depth)
  depth_index <- rep(seq_len(n_depth), times = nrow(weights))
  out <- weights[row_index, , drop = FALSE]
  out$depth_index <- as.integer(depth_index)
  out$depth <- depth[depth_index]
  out$depth_min <- vertical$lower[depth_index]
  out$depth_max <- vertical$upper[depth_index]
  out$layer_thickness_m <- thickness_m[depth_index]
  out$cell_volume_m3 <- out$cell_area_m2 * out$layer_thickness_m
  out$effective_volume_m3 <-
    out$cell_volume_m3 * out$fraction_cell_covered
  rownames(out) <- NULL
  out
}
