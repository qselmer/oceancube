#' Read NetCDF variables as an ocean cube
#'
#' Reads selected NetCDF variables and standardizes them as a 5D
#' `[lon, lat, depth, time, var]` array.
#'
#' @param file NetCDF file path.
#' @param vars Character vector of variable names. If `NULL`, all non-coordinate
#'   variables are read.
#' @param lon_name Optional longitude dimension name.
#' @param lat_name Optional latitude dimension name.
#' @param depth_name Optional depth dimension name.
#' @param time_name Optional time dimension name.
#' @param source Optional source label.
#' @param dataset_id Optional dataset identifier.
#'
#' @return An `<ocean_cube>` object. CF time is decoded as UTC `POSIXct`
#'   without truncating sub-day or fractional-second precision.
#'
#' @details CF units in seconds, minutes, hours, or days since an offset-aware
#'   origin are supported. `standard` and `gregorian` are accepted on or after
#'   1582-10-15, and `proleptic_gregorian` is supported explicitly. Missing
#'   calendar metadata defaults to `standard`. Other calendars error rather
#'   than being reinterpreted as Gregorian.
#' @export
read_nc <- function(file, vars = NULL, lon_name = NULL, lat_name = NULL,
                    depth_name = NULL, time_name = NULL, source = "netcdf",
                    dataset_id = NULL) {
  requested <- list(
    vars = vars,
    lon_name = lon_name,
    lat_name = lat_name,
    depth_name = depth_name,
    time_name = time_name
  )
  if (!file.exists(file)) {
    .abort_badarg("file", "file does not exist.")
  }

  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  dim_names <- names(nc$dim)
  lon_name <- lon_name %||% .guess_dim(dim_names, c("longitude", "lon", "x"), arg = "longitude")
  lat_name <- lat_name %||% .guess_dim(dim_names, c("latitude", "lat", "y"), arg = "latitude")
  time_name <- time_name %||% .guess_dim(dim_names, c("time", "date"), arg = "time")
  depth_name <- depth_name %||% .guess_dim(dim_names, c("depth", "deptht", "lev", "z"), required = FALSE)

  lon <- as.vector(ncdf4::ncvar_get(nc, lon_name))
  lat <- as.vector(ncdf4::ncvar_get(nc, lat_name))
  time_raw <- as.vector(ncdf4::ncvar_get(nc, time_name))
  time_units <- ncdf4::ncatt_get(nc, time_name, "units")$value
  time_calendar <- tryCatch({
    calendar_attr <- ncdf4::ncatt_get(nc, time_name, "calendar")
    if (isTRUE(calendar_attr$hasatt)) calendar_attr$value else NA_character_
  }, error = function(e) NA_character_)
  time_descriptor <- .decode_cf_time(time_raw, time_units, calendar = time_calendar)
  time <- time_descriptor$decoded_values

  all_vars <- names(nc$var)
  coord_vars <- c(lon_name, lat_name, depth_name, time_name)
  coord_vars <- coord_vars[!is.na(coord_vars)]

  vars <- vars %||% setdiff(all_vars, coord_vars)
  vars <- as.character(vars)

  missing_vars <- setdiff(vars, all_vars)
  if (length(missing_vars) > 0L) {
    rlang::abort(paste0("Variables not found in NetCDF: ", paste(missing_vars, collapse = ", ")))
  }

  has_depth <- !is.null(depth_name) && any(vapply(vars, function(v) {
    depth_name %in% vapply(nc$var[[v]]$dim, function(d) d$name, character(1))
  }, logical(1)))

  depth <- if (isTRUE(has_depth)) {
    as.vector(ncdf4::ncvar_get(nc, depth_name))
  } else {
    NA_real_
  }

  data <- array(
    NA_real_,
    dim = c(length(lon), length(lat), length(depth), length(time), length(vars))
  )

  units <- stats::setNames(vector("list", length(vars)), vars)

  for (k in seq_along(vars)) {
    v <- vars[k]
    var_obj <- nc$var[[v]]
    v_dims <- vapply(var_obj$dim, function(d) d$name, character(1))
    arr <- ncdf4::ncvar_get(nc, v, collapse_degen = FALSE)

    if (has_depth && depth_name %in% v_dims) {
      target <- c(lon_name, lat_name, depth_name, time_name)
      perm <- match(target, v_dims)
      if (anyNA(perm)) {
        rlang::abort(paste0("Variable `", v, "` does not contain expected dimensions."))
      }
      arr <- aperm(arr, perm)
      data[, , , , k] <- arr
    } else {
      target <- c(lon_name, lat_name, time_name)
      perm <- match(target, v_dims)
      if (anyNA(perm)) {
        rlang::abort(paste0("Variable `", v, "` does not contain expected dimensions."))
      }
      arr <- aperm(arr, perm)
      data[, , 1, , k] <- arr
    }

    units[[v]] <- var_obj$units %||% NA_character_
  }

  provenance_context <- .provenance_cube_context(
    source = source,
    dataset_id = dataset_id,
    time = time,
    shape = stats::setNames(as.integer(dim(data)), .cube_axis_names()),
    variables = vars,
    backend = "memory"
  )
  provenance_context$calendar <- time_descriptor$calendar
  provenance <- .provenance_empty(provenance_context)
  provenance$source$locator <- list(
    type = "file",
    value = normalizePath(file, winslash = "/", mustWork = TRUE),
    basename = basename(file),
    portable = FALSE
  )
  provenance$time["source"] <- list(.provenance_time_source(list(
    time = .cf_time_provenance(time_descriptor)
  )))
  provenance <- .provenance_append(
    provenance,
    operation = "read_nc",
    parameters = list(
      requested = requested,
      resolved = list(
        variables = vars,
        dimension_mapping = list(
          longitude = lon_name,
          latitude = lat_name,
          depth = depth_name,
          time = time_name
        )
      )
    ),
    output = .provenance_summary(provenance_context),
    scientific_method = .provenance_method("read_nc", list()),
    context = provenance_context
  )

  ocean_cube(
    lon = lon,
    lat = lat,
    depth = depth,
    time = time,
    vars = vars,
    data = data,
    units = units,
    source = source,
    dataset_id = dataset_id,
    provenance = provenance
  )
}
