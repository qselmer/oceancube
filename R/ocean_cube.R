#' Construct an oceanographic data cube
#'
#' `ocean_cube()` stores gridded oceanographic data in a standard 5D array:
#' `[longitude, latitude, depth, time, variable]`. Surface fields can be stored
#' with a single depth level set to `NA_real_`.
#'
#' @param lon Numeric vector of longitudes.
#' @param lat Numeric vector of latitudes.
#' @param time Date vector.
#' @param data Numeric array. Preferred shape is 5D: `[lon, lat, depth, time, var]`.
#'   A 4D array `[lon, lat, time, var]` is accepted and internally promoted to
#'   a single-depth cube.
#' @param depth Numeric vector of depth levels. If `NULL`, a single surface/no-depth
#'   level is used.
#' @param vars Character vector of variable names.
#' @param units Optional named list or character vector of units.
#' @param source Optional data source label.
#' @param dataset_id Optional dataset identifier.
#' @param spatial_extent Optional spatial extent.
#' @param temporal_extent Optional temporal extent.
#' @param depth_extent Optional depth extent.
#' @param mask Optional mask object.
#' @param dc Optional distance-to-coast matrix in nautical miles.
#' @param climatology Optional climatology object.
#' @param anomaly Optional anomaly metadata.
#' @param provenance Optional provenance metadata.
#' @param qa Optional QA/QC metadata.
#'
#' @return An object of class `<ocean_cube>`.
#' @export
#'
#' @examples
#' lon <- seq(-82, -80, length.out = 3)
#' lat <- seq(-12, -10, length.out = 4)
#' time <- as.Date("2000-01-01") + 0:2
#' data <- array(rnorm(3 * 4 * 1 * 3 * 1), dim = c(3, 4, 1, 3, 1))
#' cube <- ocean_cube(lon = lon, lat = lat, depth = 0, time = time, data = data, vars = "thetao")
#' cube
#' summary(cube)
ocean_cube <- function(lon, lat, time, data, depth = NULL, vars = NULL, units = NULL,
                       source = NULL, dataset_id = NULL, spatial_extent = NULL,
                       temporal_extent = NULL, depth_extent = NULL, mask = NULL,
                       dc = NULL, climatology = NULL, anomaly = NULL,
                       provenance = NULL, qa = NULL) {
  .check_numeric_vector(lon, "lon")
  .check_numeric_vector(lat, "lat")

  time <- as.Date(time)
  if (anyNA(time)) {
    .abort_badarg("time", "must be coercible to Date without NA values.")
  }

  if (!is.array(data) || !is.numeric(data)) {
    .abort_badarg("data", "must be a numeric array.")
  }

  data_dim <- dim(data)

  if (length(data_dim) == 4L) {
    # Input convention: [lon, lat, time, var]
    data <- array(
      data,
      dim = c(data_dim[1], data_dim[2], 1L, data_dim[3], data_dim[4])
    )
    depth <- depth %||% NA_real_
    data_dim <- dim(data)
  }

  if (length(data_dim) != 5L) {
    .abort_badarg("data", "must have 5 dimensions [lon, lat, depth, time, var], or 4 dimensions [lon, lat, time, var].")
  }

  if (is.null(depth)) {
    depth <- if (data_dim[3] == 1L) NA_real_ else seq_len(data_dim[3])
  }

  .check_numeric_vector(depth, "depth", allow_na = TRUE)

  if (length(lon) != data_dim[1]) .abort_badarg("lon", "length must match dim(data)[1].")
  if (length(lat) != data_dim[2]) .abort_badarg("lat", "length must match dim(data)[2].")
  if (length(depth) != data_dim[3]) .abort_badarg("depth", "length must match dim(data)[3].")
  if (length(time) != data_dim[4]) .abort_badarg("time", "length must match dim(data)[4].")

  if (is.null(vars)) {
    vars <- paste0("var", seq_len(data_dim[5]))
  }
  if (!is.character(vars) || length(vars) != data_dim[5]) {
    .abort_badarg("vars", "must be a character vector with length dim(data)[5].")
  }

  dimnames(data) <- list(
    lon = as.character(lon),
    lat = as.character(lat),
    depth = as.character(depth),
    time = as.character(time),
    var = vars
  )

  spatial_extent <- spatial_extent %||% c(
    lon_min = min(lon, na.rm = TRUE),
    lon_max = max(lon, na.rm = TRUE),
    lat_min = min(lat, na.rm = TRUE),
    lat_max = max(lat, na.rm = TRUE)
  )

  temporal_extent <- temporal_extent %||% range(time, na.rm = TRUE)
  depth_extent <- depth_extent %||% range(depth, na.rm = TRUE)

  out <- list(
    lon = lon,
    lat = lat,
    depth = depth,
    time = time,
    vars = vars,
    data = data,
    units = units,
    source = source,
    dataset_id = dataset_id,
    spatial_extent = spatial_extent,
    temporal_extent = temporal_extent,
    depth_extent = depth_extent,
    mask = mask,
    dc = dc,
    climatology = climatology,
    anomaly = anomaly,
    provenance = provenance,
    qa = qa
  )

  class(out) <- c("ocean_cube", "list")
  .check_cube(out)
  out
}

#' @export
print.ocean_cube <- function(x, ...) {
  .check_cube(x)
  cat("<ocean_cube>\n")
  cat("  dimensions : ", paste(dim(x$data), collapse = " x "), " [lon x lat x depth x time x var]\n", sep = "")
  cat("  lon        : ", min(x$lon), " to ", max(x$lon), " (n = ", length(x$lon), ")\n", sep = "")
  cat("  lat        : ", min(x$lat), " to ", max(x$lat), " (n = ", length(x$lat), ")\n", sep = "")
  cat("  depth      : ", paste(range(x$depth, na.rm = TRUE), collapse = " to "), " (n = ", length(x$depth), ")\n", sep = "")
  cat("  time       : ", paste(range(x$time), collapse = " to "), " (n = ", length(x$time), ")\n", sep = "")
  cat("  variables  : ", paste(x$vars, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @export
summary.ocean_cube <- function(object, ...) {
  .check_cube(object)
  out <- data.frame(
    field = c("longitude", "latitude", "depth", "time", "variable"),
    n = c(length(object$lon), length(object$lat), length(object$depth), length(object$time), length(object$vars)),
    min = c(min(object$lon), min(object$lat), min(object$depth, na.rm = TRUE), as.character(min(object$time)), NA_character_),
    max = c(max(object$lon), max(object$lat), max(object$depth, na.rm = TRUE), as.character(max(object$time)), NA_character_),
    stringsAsFactors = FALSE
  )
  out
}
