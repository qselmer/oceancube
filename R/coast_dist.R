#' Attach distance to coast to an ocean cube
#'
#' Distances are computed on geographic coordinates with the `sf`/S2 spherical
#' geometry engine. `coast_dist()` enables S2 only for the duration of the
#' geometry calculation and restores the caller's existing [sf::sf_use_s2()]
#' setting afterward. For line geometries the result is the shortest distance
#' to the line; for polygon geometries points inside or on the boundary have
#' zero distance.
#'
#' @param x An `<ocean_cube>` object.
#' @param coast Either an `sf`/`sfc` object or a path readable by `sf::st_read()`.
#'
#' @return An `<ocean_cube>` with `x$dc` as a `[lon, lat]` matrix in nautical miles.
#' @export
coast_dist <- function(x, coast) {
  .check_cube(x)
  cube_shape <- .cube_shape(x)
  if (!requireNamespace("sf", quietly = TRUE)) {
    rlang::abort("Package `sf` is required for `coast_dist()`.")
  }

  coast_input_type <- if (inherits(coast, c("sf", "sfc"))) {
    "geometry"
  } else {
    "file"
  }
  coast_sf <- if (inherits(coast, c("sf", "sfc"))) {
    coast
  } else if (is.character(coast) && length(coast) == 1L && file.exists(coast)) {
    sf::st_read(coast, quiet = TRUE)
  } else {
    .abort_badarg("coast", "must be an sf/sfc object or an existing spatial file path.")
  }

  coast_sf <- sf::st_transform(sf::st_as_sf(coast_sf), 4326)
  grid <- expand.grid(lon = x$lon, lat = x$lat)
  pts <- sf::st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)
  dc_m <- .with_s2_geometry(function() {
    as.numeric(sf::st_distance(pts, sf::st_union(coast_sf)))
  })
  dc_nm <- dc_m * 0.000539957
  dc_mat <- matrix(
    dc_nm,
    nrow = cube_shape[["longitude"]],
    ncol = cube_shape[["latitude"]],
    byrow = FALSE
  )

  x$dc <- dc_mat
  bbox <- suppressWarnings(as.numeric(sf::st_bbox(coast_sf)))
  if (length(bbox) != 4L || any(!is.finite(bbox))) bbox <- NULL
  if (!is.null(bbox)) names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
  provenance_context <- .provenance_cube_context(
    source = x$source,
    dataset_id = x$dataset_id,
    time = x$time,
    shape = cube_shape,
    variables = x$vars,
    backend = .cube_backend(x),
    provenance = x$provenance
  )
  x$provenance <- .provenance_append(
    x$provenance,
    operation = "coast_dist",
    parameters = list(
      requested = list(coast_input_type = coast_input_type),
      resolved = list(
        crs = "EPSG:4326",
        n_features = as.integer(nrow(coast_sf)),
        geometry_types = unique(as.character(sf::st_geometry_type(coast_sf))),
        bbox = bbox,
        input_distance_unit = "m",
        output_distance_unit = "nautical_mile",
        distance_engine = "s2",
        earth_model = "sphere",
        distance_type = "shortest geographic distance"
      )
    ),
    output = .provenance_summary(provenance_context),
    scientific_method = .provenance_method("coast_dist", list()),
    context = provenance_context
  )
  x
}
