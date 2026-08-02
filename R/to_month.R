#' Aggregate an ocean cube to monthly resolution
#'
#' @param x An `<ocean_cube>` object.
#' @param fun Aggregation function. Defaults to mean.
#'
#' @return An `<ocean_cube>` aggregated to one value per month.
#' @export
to_month <- function(x, fun = mean) {
  .check_cube(x)

  ym <- format(x$time, "%Y-%m")
  months <- unique(ym)
  month_dates <- as.Date(paste0(months, "-01"))

  d <- unname(.cube_shape(x))
  values <- .cube_read(x)
  out <- array(NA_real_, dim = c(d[1], d[2], d[3], length(months), d[5]))

  for (i in seq_along(months)) {
    idx <- which(ym == months[i])
    sub <- values[, , , idx, , drop = FALSE]
    out[, , , i, ] <- apply(sub, c(1, 2, 3, 5), function(z) fun(z, na.rm = TRUE))
  }

  ocean_cube(
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    time = month_dates,
    vars = x$vars,
    data = out,
    units = x$units,
    source = x$source,
    dataset_id = x[["dataset_id"]],
    spatial_extent = x$spatial_extent,
    temporal_extent = range(month_dates),
    depth_extent = x$depth_extent,
    mask = x$mask,
    dc = x$dc,
    provenance = .make_provenance("to_month", args = list(), extra = list(parent = x$provenance))
  )
}
