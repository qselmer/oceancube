`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.abort_badarg <- function(arg, message) {
  rlang::abort(
    message = paste0("Invalid `", arg, "`: ", message),
    class = "oceancube_bad_argument"
  )
}

.message_info <- function(...) {
  cli::cli_inform(paste0(...))
}

.message_done <- function(...) {
  cli::cli_inform(c("v" = paste0(...)))
}

.check_numeric_vector <- function(x, arg, allow_na = FALSE) {
  if (!is.numeric(x)) {
    .abort_badarg(arg, "must be numeric.")
  }
  if (!allow_na && anyNA(x)) {
    .abort_badarg(arg, "must not contain NA values.")
  }
  invisible(TRUE)
}

.check_range <- function(x, arg) {
  .check_numeric_vector(x, arg)
  if (length(x) != 2L) {
    .abort_badarg(arg, "must have length 2.")
  }
  if (x[1] > x[2]) {
    .abort_badarg(arg, "must be ordered as c(min, max).")
  }
  invisible(TRUE)
}

.check_cube <- function(x) {
  if (!inherits(x, "ocean_cube")) {
    rlang::abort("`x` must be an <ocean_cube> object.", class = "oceancube_bad_cube")
  }
  required <- c("lon", "lat", "depth", "time", "vars", "data")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    rlang::abort(
      paste0("<ocean_cube> is missing fields: ", paste(missing, collapse = ", ")),
      class = "oceancube_bad_cube"
    )
  }
  if (length(dim(x$data)) != 5L) {
    rlang::abort("`x$data` must be a 5D array [lon, lat, depth, time, var].")
  }
  invisible(TRUE)
}

.safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  mean(x)
}

.safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1L) return(NA_real_)
  stats::sd(x)
}

.weighted_mean <- function(x, w) {
  sel <- is.finite(x) & is.finite(w) & w > 0
  if (!any(sel)) return(NA_real_)
  sum(x[sel] * w[sel]) / sum(w[sel])
}

.depth_edges <- function(depth) {
  .check_numeric_vector(depth, "depth")
  if (length(depth) < 2L) {
    .abort_badarg("depth", "must contain at least two vertical levels.")
  }
  mid <- (depth[-1] + depth[-length(depth)]) / 2
  first <- max(0, depth[1] - (mid[1] - depth[1]))
  last <- depth[length(depth)] + (depth[length(depth)] - mid[length(mid)])
  c(first, mid, last)
}

.depth_weights <- function(zmin, zmax, z_edge) {
  pmax(0, pmin(z_edge[-1], zmax) - pmax(z_edge[-length(z_edge)], zmin))
}

.guess_dim <- function(dim_names, candidates, required = TRUE, arg = "dimension") {
  hit <- candidates[candidates %in% dim_names]
  if (length(hit) > 0L) return(hit[1])
  if (isTRUE(required)) {
    rlang::abort(
      paste0("Could not identify ", arg, ". Available dimensions: ", paste(dim_names, collapse = ", "))
    )
  }
  NULL
}

.read_cf_time <- function(time_raw, units, calendar = "gregorian") {
  if (is.null(units) || !grepl("since", units, fixed = TRUE)) {
    rlang::abort("NetCDF time units must follow a '<unit> since <origin>' pattern.")
  }

  origin_txt <- sub(".*since\\s+", "", units)
  origin <- as.POSIXct(origin_txt, tz = "UTC")

  if (is.na(origin)) {
    rlang::abort(paste0("Could not parse NetCDF time origin: ", origin_txt))
  }

  multiplier <- if (grepl("seconds since", units, ignore.case = TRUE)) {
    1
  } else if (grepl("minutes since", units, ignore.case = TRUE)) {
    60
  } else if (grepl("hours since", units, ignore.case = TRUE)) {
    3600
  } else if (grepl("days since", units, ignore.case = TRUE)) {
    86400
  } else {
    rlang::abort(paste0("Unsupported NetCDF time units: ", units))
  }

  if (!tolower(calendar) %in% c("gregorian", "standard", "proleptic_gregorian")) {
    cli::cli_warn("Calendar {.val {calendar}} is not fully supported; treating it as Gregorian.")
  }

  as.Date(origin + time_raw * multiplier)
}

.make_filename <- function(dataset_id, vars, lon = NULL, lat = NULL, time = NULL,
                           depth = NULL, ext = "nc") {
  clean <- function(x) gsub("[^A-Za-z0-9._-]+", "-", x)

  v_str <- paste(vars, collapse = "-")
  lon_str <- if (is.null(lon)) "fullLon" else sprintf("lon%.2f_%.2f", lon[1], lon[2])
  lat_str <- if (is.null(lat)) "fullLat" else sprintf("lat%.2f_%.2f", lat[1], lat[2])
  time_str <- if (is.null(time)) {
    "fullTime"
  } else {
    paste(format(as.Date(time[1]), "%Y%m%d"), format(as.Date(time[2]), "%Y%m%d"), sep = "_")
  }
  depth_str <- if (is.null(depth)) "fullDepth" else sprintf("z%.0f_%.0fm", depth[1], depth[2])

  paste0(clean(paste(dataset_id, v_str, lon_str, lat_str, depth_str, time_str, sep = "_")), ".", ext)
}

.make_provenance <- function(fun, args = list(), extra = list()) {
  list(
    package = "oceancube",
    package_version = tryCatch(as.character(utils::packageVersion("oceancube")), error = function(e) NA_character_),
    r_version = R.version.string,
    platform = R.version$platform,
    system = as.list(Sys.info()),
    date = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    function_name = fun,
    arguments = args,
    extra = extra
  )
}
