#' Average an ocean cube over vertical layers
#'
#' @param x An `<ocean_cube>` object with at least two depth levels.
#' @param depth Numeric vector. Either `c(zmin, zmax)` for one layer or a sequence
#'   of breaks, e.g. `c(0, 10, 25, 50, 100)`.
#'
#' @return An `<ocean_cube>` with the depth dimension replaced by vertical layers.
#' @export
layer_mean <- function(x, depth) {
  .check_cube(x)
  .check_numeric_vector(depth, "depth")

  if (length(x$depth) < 2L || all(is.na(x$depth))) {
    rlang::abort("`x` must contain at least two valid depth levels.")
  }

  breaks <- if (length(depth) == 2L) depth else depth
  if (length(breaks) < 2L) .abort_badarg("depth", "must contain at least two values.")
  if (is.unsorted(breaks)) .abort_badarg("depth", "must be sorted increasingly.")

  bins <- lapply(seq_len(length(breaks) - 1L), function(i) c(breaks[i], breaks[i + 1L]))
  z_edge <- .depth_edges(x$depth)
  layer_depth <- vapply(bins, mean, numeric(1))

  d <- unname(.cube_shape(x))
  out <- array(NA_real_, dim = c(d[1], d[2], length(bins), d[4], d[5]))

  for (b in seq_along(bins)) {
    zmin <- bins[[b]][1]
    zmax <- bins[[b]][2]
    w <- .depth_weights(zmin, zmax, z_edge)
    idx <- which(w > 0)
    if (length(idx) == 0L) next
    w_use <- w[idx]
    layer_values <- .cube_read(x, index = list(depth = idx))

    for (k in seq_len(d[5])) {
      sub <- layer_values[, , , , k, drop = FALSE]
      sub <- array(sub, dim = dim(sub)[1:4])
      out[, , b, , k] <- apply(sub, c(1, 2, 4), function(z) .weighted_mean(z, w_use))
    }
  }

  ocean_cube(
    lon = x$lon,
    lat = x$lat,
    depth = layer_depth,
    time = x$time,
    vars = x$vars,
    data = out,
    units = x$units,
    source = x$source,
    dataset_id = x[["dataset_id"]],
    spatial_extent = x$spatial_extent,
    temporal_extent = x$temporal_extent,
    depth_extent = range(breaks),
    mask = x$mask,
    dc = x$dc,
    provenance = .make_provenance("layer_mean", args = list(depth = depth), extra = list(parent = x$provenance))
  )
}
