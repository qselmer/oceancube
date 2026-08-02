#' Attach distance to coast to an ocean cube
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
  dc_m <- as.numeric(sf::st_distance(pts, sf::st_union(coast_sf)))
  dc_nm <- dc_m * 0.000539957
  dc_mat <- matrix(
    dc_nm,
    nrow = cube_shape[["longitude"]],
    ncol = cube_shape[["latitude"]],
    byrow = FALSE
  )

  x$dc <- dc_mat
  x$provenance <- .make_provenance("coast_dist", args = list(), extra = list(parent = x$provenance))
  x
}
