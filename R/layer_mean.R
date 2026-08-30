#' Average an ocean cube over vertical layers
#'
#' @param x An `<ocean_cube>` object with at least two depth levels.
#' @param depth Numeric vector. Either `c(zmin, zmax)` for one layer or a sequence
#'   of breaks, e.g. `c(0, 10, 25, 50, 100)`.
#'
#' @return An `<ocean_cube>` with the depth dimension replaced by vertical layers.
#'
#' @details For certified CF metric-depth cubes, weights are exact overlaps
#'   between requested intervals and explicit cell bounds. Every requested
#'   interval must have full geometric coverage; partial coverage (including
#'   internal gaps) errors before payload data are read, while zero coverage
#'   returns an all-missing layer. Missing scientific values are renormalized
#'   over finite contributing cells and do not change geometric coverage.
#'
#'   Cubes without certified metric bounds retain the historical
#'   centre-derived `.depth_edges()` / `.depth_weights()` calculation for
#'   backward compatibility. That path is explicitly uncertified. Certified
#'   results carry requested output bounds and a derived current CF vertical
#'   descriptor; the immutable CF source record is preserved.
#' @export
layer_mean <- function(x, depth) {
  .check_cube(x)
  .check_numeric_vector(depth, "depth")

  if (length(x$depth) < 2L || all(is.na(x$depth))) {
    rlang::abort("`x` must contain at least two valid depth levels.")
  }

  breaks <- if (length(depth) == 2L) depth else depth
  if (length(breaks) < 2L) .abort_badarg("depth", "must contain at least two values.")
  if (any(!is.finite(breaks)) || any(diff(breaks) <= 0)) {
    .abort_badarg(
      "depth",
      "must be sorted increasingly and contain only finite, distinct values."
    )
  }

  bins <- lapply(seq_len(length(breaks) - 1L), function(i) c(breaks[i], breaks[i + 1L]))
  layer_depth <- vapply(bins, mean, numeric(1))
  support <- .vertical_support_engine(x, mode = "layer_mean")
  resolved <- .vertical_resolve_bins(support, bins)

  d <- unname(.cube_shape(x))
  out <- array(NA_real_, dim = c(d[1], d[2], length(bins), d[4], d[5]))

  contributing <- lapply(resolved$weights, function(weight) which(weight > 0))
  read_index <- sort(unique(unlist(contributing, use.names = FALSE)))
  if (length(read_index)) {
    layer_values <- .cube_read(x, index = list(depth = read_index))
    for (b in seq_along(bins)) {
      idx <- contributing[[b]]
      if (!length(idx)) next
      local_idx <- match(idx, read_index)
      w_use <- resolved$weights[[b]][idx]
      for (k in seq_len(d[5])) {
        sub <- layer_values[, , local_idx, , k, drop = FALSE]
        sub <- array(sub, dim = dim(sub)[1:4])
        out[, , b, , k] <- apply(
          sub, c(1, 2, 4), function(z) .weighted_mean(z, w_use)
        )
      }
    }
  }

  output_shape <- stats::setNames(
    as.integer(c(d[1], d[2], length(bins), d[4], d[5])),
    .cube_axis_names()
  )
  provenance_context <- .provenance_cube_context(
    source = x$source,
    dataset_id = x$dataset_id,
    time = x$time,
    shape = output_shape,
    variables = x$vars,
    backend = "memory",
    provenance = x$provenance
  )
  provenance <- .provenance_append(
    x$provenance,
    operation = "layer_mean",
    parameters = list(
      requested = list(depth = depth),
      resolved = list(
        layer_ranges = bins,
        n_layers = as.integer(length(bins)),
        layer_centers = layer_depth,
        depth_representation = "arithmetic midpoint of requested layer bounds",
        support_basis = support$support_basis,
        weight_basis = if (identical(support$mode, "explicit_bounds")) {
          "exact interval overlap against explicit cell bounds"
        } else {
          "legacy centre-derived edges via .depth_edges/.depth_weights"
        },
        source_bounds = if (identical(support$mode, "explicit_bounds")) {
          lapply(seq_len(nrow(support$bounds)), function(i) {
            as.numeric(support$bounds[i, ])
          })
        } else {
          list()
        },
        bounds_source = support$bounds_source,
        overlap_weights = lapply(resolved$weights, as.numeric),
        coverage_fraction = resolved$coverage_fraction,
        coverage_status = as.character(resolved$coverage_status),
        coverage_tolerance = resolved$tolerance,
        coverage_policy = if (identical(support$mode, "explicit_bounds")) {
          "full coverage required; zero coverage returns NA"
        } else {
          "legacy coverage not certified"
        },
        vertical_unit = support$depth_unit %||% support$unit %||% NA_character_,
        contributing_depth_cells = lapply(contributing, as.integer),
        source_depth_centers = as.numeric(x$depth),
        vertical_certification = support$certification_status,
        output_shape = output_shape
      )
    ),
    output = .provenance_summary(provenance_context),
    scientific_method = .provenance_method("layer_mean", list()),
    context = provenance_context
  )

  result <- ocean_cube(
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
    provenance = provenance
  )
  if (identical(support$mode, "explicit_bounds")) {
    output_bounds <- do.call(rbind, bins)
    attr(output_bounds, "units") <- support$depth_unit %||% support$unit
    attr(output_bounds, "positive") <- support$positive
    attr(result$depth, "bounds") <- output_bounds
    attr(result$depth, "units") <- support$depth_unit %||% support$unit
    attr(result$depth, "positive") <- support$positive
  }
  .attach_cube_metadata(
    result,
    .cf_metadata_for_layer_mean(
      x$metadata %||% NULL,
      bins = bins,
      centers = layer_depth,
      certified = identical(support$certification_status, "CERTIFIED")
    )
  )
}
