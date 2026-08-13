#' Inspect an ocean cube
#'
#' `cube_inspect()` returns a compact structural and diagnostic
#' summary. It complements [cube_validate()] by describing dimensions, ranges,
#' resolution, estimated size, missing values, and safe provenance metadata.
#'
#' @param x A valid `<ocean_cube>` using the memory or NetCDF backend.
#' @param missing Missing-value policy. `"auto"` computes metrics for memory
#'   cubes but not NetCDF cubes; `"none"` never computes them; and `"full"`
#'   computes them for either backend. For NetCDF, `"full"` warns before
#'   materializing the complete cube.
#'
#' @return A list with class `ocean_cube_inspection` containing structural,
#'   coordinate, storage, missing-value, validation, and provenance summaries.
#'   `time_summary` reports class, count, range, timezone, calendar, duplicate
#'   and ordering flags, regularity, and minimum/median/maximum positive
#'   intervals. Legacy duplicate or unsorted axes can be diagnosed without
#'   being reordered or repaired.
#' @export
#' @seealso [cube_validate()], [ocean_cube()]
#'
#' @examples
#' values <- array(c(1, NA, 3, 4), dim = c(2, 2, 1, 1, 1))
#' cube <- ocean_cube(
#'   lon = c(-80, -79), lat = c(-12, -11), depth = 0,
#'   time = as.Date("2020-01-01"), data = values, vars = "temperature"
#' )
#' cube_inspect(cube)
cube_inspect <- function(x, missing = c("auto", "none", "full")) {
  missing <- match.arg(missing)
  validation <- cube_validate(x, strict = FALSE)
  temporal_diagnostics <- c(
    "time_unique", "time_strictly_increasing", "time_timezone",
    "time_calendar", "time_provenance"
  )
  blocking_failures <- validation$status == "FAIL" &
    !validation$check %in% temporal_diagnostics
  if (any(blocking_failures)) {
    rlang::abort(
      paste0(
        "Ocean cube validation failed ", sum(blocking_failures),
        " non-temporal structural check(s)."
      ),
      class = "oceancube_validation_error",
      report = validation
    )
  }
  legacy_temporal_axis <- any(
    validation$status == "FAIL" & validation$check %in%
      c("time_unique", "time_strictly_increasing", "time_timezone")
  )
  backend <- .cube_backend(x)
  axes <- c("longitude", "latitude", "depth", "time", "variable")
  dimensions <- stats::setNames(
    as.integer(c(length(x$lon), length(x$lat), length(x$depth), length(x$time), length(x$vars))),
    axes
  )
  storage_dimensions <- .cube_storage_shape(x)

  axis_resolution <- function(values, time_axis = FALSE, surface = FALSE) {
    if (surface) {
      return(list(differences = NA_real_, regular = NA, resolution = NA_real_, unit = "surface"))
    }
    numeric_values <- if (time_axis) as.numeric(values) else as.numeric(values)
    differences <- diff(numeric_values)
    positive <- differences[is.finite(differences) & differences > 0]
    increasing <- length(differences) == 0L || all(differences > 0)
    regular <- increasing && (length(differences) <= 1L || isTRUE(all.equal(
      differences, rep(differences[[1L]], length(differences)),
      tolerance = sqrt(.Machine$double.eps), check.attributes = FALSE
    )))
    resolution <- if (length(differences) == 0L) {
      NA_real_
    } else if (regular) {
      differences[[1L]]
    } else {
      NA_real_
    }
    list(
      differences = differences,
      regular = regular,
      resolution = resolution,
      minimum_positive = if (length(positive)) min(positive) else NA_real_,
      median_positive = if (length(positive)) stats::median(positive) else NA_real_,
      maximum_positive = if (length(positive)) max(positive) else NA_real_,
      unit = if (time_axis) {
        if (inherits(values, "Date")) "days" else "seconds"
      } else {
        "coordinate units"
      }
    )
  }

  surface <- length(x$depth) == 1L && is.na(x$depth) && !is.nan(x$depth)
  coordinate_ranges <- list(
    longitude = range(x$lon),
    latitude = range(x$lat),
    depth = if (surface) c(NA_real_, NA_real_) else range(x$depth),
    time = range(x$time)
  )
  coordinate_resolution <- list(
    longitude = axis_resolution(x$lon),
    latitude = axis_resolution(x$lat),
    depth = axis_resolution(x$depth, surface = surface),
    time = axis_resolution(x$time, time_axis = TRUE)
  )
  time_provenance <- .find_time_provenance(x$provenance)
  time_numeric <- as.numeric(x$time)
  time_summary <- list(
    class = if (inherits(x$time, "Date")) "Date" else "POSIXct",
    n = length(x$time),
    range = range(x$time),
    timezone = if (inherits(x$time, "POSIXct")) .time_timezone(x$time) else NA_character_,
    calendar = if (is.list(time_provenance)) time_provenance$calendar %||% NA_character_ else NA_character_,
    duplicates = anyDuplicated(time_numeric) > 0L,
    strictly_increasing = length(time_numeric) <= 1L || all(diff(time_numeric) > 0),
    regular = coordinate_resolution$time$regular,
    minimum_positive_interval = coordinate_resolution$time$minimum_positive,
    median_positive_interval = coordinate_resolution$time$median_positive,
    maximum_positive_interval = coordinate_resolution$time$maximum_positive,
    interval_unit = coordinate_resolution$time$unit
  )

  units <- x$units
  if (is.null(units)) {
    units <- stats::setNames(rep(NA_character_, length(x$vars)), x$vars)
  } else if (is.null(names(units))) {
    names(units) <- x$vars
  } else {
    units <- units[x$vars]
  }

  estimated_bytes <- .cube_product_as_double(
    dimensions,
    "logical cube elements"
  ) * 8

  total_by_variable <- rep(prod(as.double(dimensions[seq_len(4L)])), length(x$vars))
  missing_values <- NULL
  compute_missing <- identical(backend, "memory") &&
    missing %in% c("auto", "full") && !legacy_temporal_axis
  if (identical(backend, "netcdf") && identical(missing, "full")) {
    warning(
      "`missing = \"full\"` materializes the complete NetCDF cube in memory.",
      call. = FALSE
    )
    missing_values <- .cube_read(x)
    compute_missing <- TRUE
  } else if (identical(backend, "memory") && compute_missing) {
    missing_values <- .cube_read(x)
  }

  if (compute_missing) {
    missing_by_variable <- vapply(
      seq_along(x$vars),
      function(i) sum(is.na(missing_values[, , , , i, drop = FALSE])),
      numeric(1)
    )
    missing_summary <- list(
      status = "computed",
      computed = TRUE,
      total = sum(total_by_variable),
      missing = sum(missing_by_variable),
      fraction = sum(missing_by_variable) / sum(total_by_variable),
      by_variable = data.frame(
        variable = x$vars,
        total = total_by_variable,
        missing = missing_by_variable,
        fraction = missing_by_variable / total_by_variable,
        stringsAsFactors = FALSE
      )
    )
  } else {
    missing_summary <- list(
      status = if (identical(missing, "none")) {
        "not requested"
      } else if (legacy_temporal_axis) {
        "not materialized: legacy temporal axis"
      } else {
        "not materialized"
      },
      computed = FALSE,
      total = sum(total_by_variable),
      missing = NA_real_,
      fraction = NA_real_,
      by_variable = data.frame(
        variable = x$vars,
        total = total_by_variable,
        missing = rep(NA_real_, length(x$vars)),
        fraction = rep(NA_real_, length(x$vars)),
        stringsAsFactors = FALSE
      )
    )
  }

  provenance <- x$provenance
  provenance_summary <- list(
    available = !is.null(provenance),
    source = if (is.character(x$source) && length(x$source) == 1L) x$source else NA_character_,
    dataset_id = if (is.character(x$dataset_id) && length(x$dataset_id) == 1L) x$dataset_id else NA_character_,
    fields = if (is.list(provenance)) names(provenance) else character()
  )

  out <- list(
    source = x$source,
    dataset_id = x$dataset_id,
    backend = backend,
    dimensions = dimensions,
    storage_dimensions = storage_dimensions,
    coordinate_ranges = coordinate_ranges,
    coordinate_resolution = coordinate_resolution,
    variables = x$vars,
    units = units,
    extents = list(
      spatial = x$spatial_extent,
      temporal = x$temporal_extent,
      depth = x$depth_extent
    ),
    time_resolution = coordinate_resolution$time,
    time_summary = time_summary,
    depth_resolution = coordinate_resolution$depth,
    estimated_bytes = estimated_bytes,
    missing = missing_summary,
    validation = validation,
    provenance_summary = provenance_summary
  )
  class(out) <- c("ocean_cube_inspection", "list")
  out
}

#' Print an ocean cube inspection
#'
#' @param x An `ocean_cube_inspection` object.
#' @param ... Additional arguments, currently unused.
#'
#' @return `x`, invisibly.
#' @export
print.ocean_cube_inspection <- function(x, ...) {
  range_text <- function(values) paste(format(values), collapse = " to ")
  resolution_text <- function(value) {
    if (is.na(value$regular)) return("surface")
    if (!isTRUE(value$regular)) return("irregular")
    if (is.na(value$resolution)) return("single value")
    paste0(format(value$resolution), " ", value$unit)
  }
  counts <- table(factor(x$validation$status, levels = c("PASS", "WARN", "FAIL")))
  unit_values <- vapply(x$units, function(value) {
    if (length(value) == 0L || all(is.na(value))) "NA" else paste(value, collapse = "/")
  }, character(1))

  cat("<ocean_cube_inspection>\n")
  cat("  backend     : ", x$backend, "\n", sep = "")
  cat("  dimensions  : ", paste(x$dimensions, collapse = " x "),
      " [lon x lat x depth x time x var]\n", sep = "")
  cat("  longitude   : ", range_text(x$coordinate_ranges$longitude), "\n", sep = "")
  cat("  latitude    : ", range_text(x$coordinate_ranges$latitude), "\n", sep = "")
  cat("  variables   : ", paste0(x$variables, " [", unit_values, "]", collapse = ", "), "\n", sep = "")
  cat("  time        : ", range_text(x$coordinate_ranges$time),
      " (", resolution_text(x$time_resolution), ")\n", sep = "")
  cat("  time class  : ", x$time_summary$class,
      if (!is.na(x$time_summary$timezone)) paste0(" [", x$time_summary$timezone, "]") else "",
      "; calendar=", x$time_summary$calendar,
      "; increasing=", x$time_summary$strictly_increasing,
      "; duplicates=", x$time_summary$duplicates, "\n", sep = "")
  cat("  depth       : ", range_text(x$coordinate_ranges$depth),
      " (", resolution_text(x$depth_resolution), ")\n", sep = "")
  cat("  est. bytes  : ", format(x$estimated_bytes, scientific = FALSE), "\n", sep = "")
  cat("  missing     : ", x$missing$status,
      if (isTRUE(x$missing$computed)) paste0(" (", x$missing$missing, "/", x$missing$total, ")") else "",
      "\n", sep = "")
  cat("  validation  : PASS ", counts[["PASS"]], ", WARN ", counts[["WARN"]],
      ", FAIL ", counts[["FAIL"]], "\n", sep = "")
  invisible(x)
}
