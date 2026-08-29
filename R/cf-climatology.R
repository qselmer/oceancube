.cf_nc_attribute <- function(nc, variable, name, default = NULL) {
  attribute <- tryCatch(
    ncdf4::ncatt_get(nc, variable, name),
    error = function(e) list(hasatt = FALSE)
  )
  if (!isTRUE(attribute$hasatt)) default else attribute$value
}

.cf_time_text <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"))
  }
  format(x)
}

.cf_decode_climatology_bounds <- function(raw, time_descriptor) {
  values <- sort(unique(as.numeric(raw)))
  decoded <- .decode_cf_time(
    values,
    time_descriptor$units,
    time_descriptor$calendar
  )$decoded_values
  decoded[match(as.numeric(raw), values)]
}

.cf_parse_coverage_time <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    return(NULL)
  }
  text <- trimws(value)
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", text)) {
    text <- paste0(text, "T00:00:00Z")
  }
  parsed <- suppressWarnings(as.POSIXct(text, tz = "UTC"))
  if (is.na(parsed)) NULL else parsed
}

.cf_climatology_cell_methods <- function(nc, variables) {
  raw <- stats::setNames(lapply(variables, function(variable) {
    value <- .cf_nc_attribute(nc, variable, "cell_methods", NA_character_)
    if (length(value) != 1L) NA_character_ else as.character(value)
  }), variables)
  recognized <- vapply(raw, function(value) {
    !is.na(value) &&
      grepl("time[[:space:]]*:[[:space:]]*[[:alpha:]_]+[[:space:]]+within[[:space:]]+years", value, ignore.case = TRUE) &&
      grepl("time[[:space:]]*:[[:space:]]*[[:alpha:]_]+[[:space:]]+over[[:space:]]+years", value, ignore.case = TRUE)
  }, logical(1L))
  list(raw = raw, recognized = recognized)
}

.cf_climatology_time_descriptor <- function(nc, time_name, variables,
                                             time_descriptor) {
  target <- .cf_nc_attribute(nc, time_name, "climatology", NULL)
  if (is.null(target)) return(NULL)
  target <- as.character(target)
  if (length(target) != 1L || is.na(target) || !nzchar(target)) {
    rlang::abort(
      "CF climatology target must be one non-empty variable name.",
      class = c("oceancube_climatology_semantics_error", "oceancube_netcdf_schema_error")
    )
  }
  if (!is.null(.cf_nc_attribute(nc, time_name, "bounds", NULL))) {
    rlang::abort(
      "A time coordinate cannot use ordinary bounds and climatology bounds simultaneously.",
      class = c("oceancube_climatology_semantics_error", "oceancube_netcdf_schema_error")
    )
  }
  if (!target %in% names(nc$var)) {
    rlang::abort(
      paste0("CF climatology target `", target, "` is missing."),
      class = c("oceancube_climatology_semantics_error", "oceancube_netcdf_schema_error")
    )
  }
  variable <- nc$var[[target]]
  if (!is.null(.cf_nc_attribute(nc, target, "scale_factor", NULL)) ||
      !is.null(.cf_nc_attribute(nc, target, "add_offset", NULL))) {
    rlang::abort(
      "Packed climatology bounds are not in the B6 runtime-supported subset.",
      class = c("oceancube_climatology_semantics_error", "oceancube_netcdf_schema_error")
    )
  }
  dimension_names <- vapply(variable$dim, `[[`, character(1L), "name")
  dimension_lengths <- as.integer(vapply(variable$dim, `[[`, numeric(1L), "len"))
  time_position <- match(time_name, dimension_names)
  bounds_position <- which(
    dimension_lengths == 2L & seq_along(dimension_lengths) != time_position
  )
  if (length(dimension_names) != 2L || is.na(time_position) ||
      length(bounds_position) != 1L ||
      dimension_lengths[[time_position]] != length(time_descriptor$raw_values)) {
    rlang::abort(
      "CF climatology bounds must have the time dimension and exactly one size-two bounds dimension.",
      class = c("oceancube_climatology_semantics_error", "oceancube_netcdf_schema_error")
    )
  }
  raw <- ncdf4::ncvar_get(
    nc, target, collapse_degen = FALSE, raw_datavals = TRUE
  )
  raw <- aperm(raw, c(time_position, bounds_position))
  dim(raw) <- c(length(time_descriptor$raw_values), 2L)
  if (!is.numeric(raw) || anyNA(raw) || any(!is.finite(raw)) ||
      any(raw[, 1L] >= raw[, 2L])) {
    rlang::abort(
      "CF climatology bounds must contain finite start/end pairs with start before end.",
      class = c("oceancube_climatology_semantics_error", "oceancube_netcdf_schema_error")
    )
  }
  decoded <- .cf_decode_climatology_bounds(raw, time_descriptor)
  n_time <- nrow(raw)
  decoded_key <- matrix(.time_key(decoded), nrow = n_time, ncol = 2L)
  representative_key <- .time_key(time_descriptor$decoded_values)
  support_start_key <- decoded_key[, 1L]
  support_end_key <- decoded_key[, 2L]
  if (any(representative_key < support_start_key |
      representative_key > support_end_key)) {
    rlang::abort(
      "Each climatological representative coordinate must lie within its decoded support envelope.",
      class = c("oceancube_climatology_semantics_error", "oceancube_netcdf_schema_error")
    )
  }
  methods <- .cf_climatology_cell_methods(nc, variables)
  if (!length(methods$recognized) || any(!methods$recognized)) {
    rlang::abort(
      paste0(
        "CF climatological time requires the recognized `time: ... within years ",
        "time: ... over years` cell_methods pattern for every selected variable."
      ),
      class = c("oceancube_climatology_semantics_error", "oceancube_netcdf_schema_error")
    )
  }
  coverage_start_raw <- .cf_nc_attribute(
    nc, 0, "time_coverage_start", NULL
  )
  coverage_start <- .cf_parse_coverage_time(coverage_start_raw)
  decoded_start <- decoded[[which.min(support_start_key)]]
  if (!is.null(coverage_start) && inherits(decoded, "POSIXct") &&
      abs(as.numeric(decoded_start) - as.numeric(coverage_start)) > 1) {
    rlang::abort(
      paste0(
        "CF climatology support decoded from `", target, "` begins at ",
        .cf_time_text(decoded_start),
        ", but global `time_coverage_start` declares ",
        as.character(coverage_start_raw),
        ". Core will not infer a provider-specific offset correction."
      ),
      class = c("oceancube_climatology_semantics_error", "oceancube_netcdf_schema_error")
    )
  }
  list(
    chronology_kind = "climatological",
    climatology_target = target,
    representative_time = as.list(.cf_time_text(time_descriptor$decoded_values)),
    support_start = as.list(.cf_time_text(decoded[seq_len(n_time)])),
    support_end = as.list(.cf_time_text(decoded[n_time + seq_len(n_time)])),
    raw_bounds = list(
      start = as.list(raw[, 1L]),
      end = as.list(raw[, 2L])
    ),
    cell_methods_raw = methods$raw,
    structural_status = "CLIMATOLOGY_STRUCTURALLY_VALID",
    semantic_status = "CLIMATOLOGY_SEMANTICALLY_INTERPRETABLE",
    runtime_status = "CLIMATOLOGY_RUNTIME_SUPPORTED"
  )
}

.cf_attach_climatology_current <- function(cf, descriptor) {
  if (is.null(descriptor)) return(cf)
  cf$current$chronology <- descriptor
  cf
}

.cf_climatology_for_selection <- function(descriptor, time_index, variables) {
  if (is.null(descriptor)) return(NULL)
  out <- descriptor
  out$representative_time <- out$representative_time[time_index]
  out$support_start <- out$support_start[time_index]
  out$support_end <- out$support_end[time_index]
  out$raw_bounds$start <- out$raw_bounds$start[time_index]
  out$raw_bounds$end <- out$raw_bounds$end[time_index]
  out$cell_methods_raw <- out$cell_methods_raw[variables]
  out
}

.cube_chronology_kind <- function(x) {
  x$metadata$cf$current$chronology$chronology_kind %||% "ordinary"
}

.require_ordinary_chronology <- function(x, operation) {
  if (identical(.cube_chronology_kind(x), "climatological")) {
    rlang::abort(
      paste0(
        "`", operation, "()` requires ordinary chronology; this cube already ",
        "represents a CF climatological statistic."
      ),
      class = "oceancube_climatological_operation_error"
    )
  }
  invisible(TRUE)
}
