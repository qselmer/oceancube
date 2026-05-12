#' Create a stock-oriented spatial mask
#'
#' @param x An `<ocean_cube>` object.
#' @param stock Optional stock name.
#' @param lat Optional latitude range `c(min, max)`.
#' @param dc Optional distance-to-coast range in nautical miles `c(min, max)`.
#'   Requires `x$dc` to be available.
#' @param depth Optional depth range `c(min, max)`.
#'
#' @return An `<ocean_mask>` object.
#' @export
stock_mask <- function(x, stock = NULL, lat = NULL, dc = NULL, depth = NULL) {
  .check_cube(x)
  if (!is.null(lat)) .check_range(lat, "lat")
  if (!is.null(dc)) .check_range(dc, "dc")
  if (!is.null(depth)) .check_range(depth, "depth")

  nlon <- length(x$lon)
  nlat <- length(x$lat)
  nz <- length(x$depth)

  lat_mat <- matrix(rep(x$lat, each = nlon), nrow = nlon, ncol = nlat)
  mask2 <- matrix(TRUE, nrow = nlon, ncol = nlat)

  if (!is.null(lat)) {
    mask2 <- mask2 & lat_mat >= lat[1] & lat_mat <= lat[2]
  }

  if (!is.null(dc)) {
    if (is.null(x$dc)) {
      rlang::abort("`x$dc` is NULL. Run `coast_dist()` or attach a distance-to-coast matrix first.")
    }
    mask2 <- mask2 & x$dc >= dc[1] & x$dc <= dc[2]
  }

  mask3 <- array(rep(mask2, times = nz), dim = c(nlon, nlat, nz))

  if (!is.null(depth)) {
    depth_sel <- x$depth >= depth[1] & x$depth <= depth[2]
    for (z in seq_len(nz)) {
      if (!isTRUE(depth_sel[z])) mask3[, , z] <- FALSE
    }
  }

  out <- list(
    stock = stock,
    mask = mask3,
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    lat_range = lat,
    dc_range = dc,
    depth_range = depth
  )
  class(out) <- c("ocean_mask", "list")
  out
}

#' @export
print.ocean_mask <- function(x, ...) {
  cat("<ocean_mask>\n")
  cat("  stock      : ", x$stock %||% "not specified", "\n", sep = "")
  cat("  dimensions : ", paste(dim(x$mask), collapse = " x "), " [lon x lat x depth]\n", sep = "")
  cat("  kept cells : ", sum(x$mask, na.rm = TRUE), " / ", length(x$mask), "\n", sep = "")
  invisible(x)
}

#' Apply a stock mask to an ocean cube
#'
#' @param x An `<ocean_cube>` object.
#' @param mask An `<ocean_mask>` object.
#'
#' @return A masked `<ocean_cube>` object.
#' @export
crop_stock <- function(x, mask) {
  .check_cube(x)
  if (!inherits(mask, "ocean_mask")) {
    rlang::abort("`mask` must be an <ocean_mask> object.")
  }
  if (!identical(dim(mask$mask), dim(x$data)[1:3])) {
    rlang::abort("Mask dimensions must match cube [lon, lat, depth] dimensions.")
  }

  out_data <- x$data
  d <- dim(out_data)
  for (k in seq_len(d[5])) {
    for (t in seq_len(d[4])) {
      slice <- out_data[, , , t, k]
      slice[!mask$mask] <- NA_real_
      out_data[, , , t, k] <- slice
    }
  }

  ans <- ocean_cube(
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    time = x$time,
    vars = x$vars,
    data = out_data,
    units = x$units,
    source = x$source,
    dataset_id = x$dataset_id,
    spatial_extent = x$spatial_extent,
    temporal_extent = x$temporal_extent,
    depth_extent = x$depth_extent,
    mask = mask,
    dc = x$dc,
    climatology = x$climatology,
    anomaly = x$anomaly,
    provenance = .make_provenance("crop_stock", args = list(stock = mask$stock), extra = list(parent = x$provenance))
  )
  class(ans) <- c("stock_cube", class(ans))
  ans
}
