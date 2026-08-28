#' Compute pointwise anomalies from a canonical climatology
#'
#' Compares each source observation with the mean or mean/SD pair for the same
#' recurrent day, month, or meteorological season. Matching uses the canonical
#' group key rather than climatological pseudo-time.
#'
#' @param x A valid memory or lazy NetCDF ocean cube.
#' @param climatology An intact in-memory result from [cube_climatology()].
#' @param type Either "difference" or "z".
#'
#' @return An in-memory ocean cube with source dimensions and timestamps.
#'
#' @details `climatology` must be the intact canonical memory cube returned by
#' [cube_climatology()], including its aligned sample SD and complete recurring
#' group key. A climatology whose scientific metadata were discarded by a
#' selection or crop is rejected. Its reference period identifies the baseline
#' but does not restrict source timestamps, so `x` may fall inside or outside
#' that period.
#'
#' Longitude, latitude, depth, variables and variable order must be exactly
#' identical. Defined unit strings and canonical calendars must match exactly;
#' if both sides lack units the computation continues with a warning and an
#' unverified-units QA record. One-sided missing units or different strings are
#' errors, and no unit conversion is attempted. Date sources require a Date
#' baseline and POSIXct sources require the baseline's recorded POSIXct input
#' class; POSIXct instants remain UTC with sub-day precision. There is no
#' interpolation, tolerance, subsetting, conversion, or automatic reordering.
#'
#' Matching uses recurrent keys rather than climatological pseudo-time:
#' `MM-DD` for daily, `01` through `12` for monthly, and DJF/MAM/JJA/SON for
#' seasonal climatology. Daily `leap = "keep"` matches February 29 separately,
#' `"drop"` retains its source timestamp but returns NA, and `"feb28"` maps it
#' to the February-28-equivalent group without shifting March 1. Subdaily
#' observations remain pointwise and use their UTC calendar day/month/season.
#'
#' Difference anomalies are `source - climatological mean` and preserve source
#' units. Z anomalies divide that difference by the climatological sample SD
#' and use unit "1" for every variable. Only finite source, mean, and (for z)
#' SD values are computed. Zero or non-finite SD produces NA, any negative
#' finite SD is a global climatology error, and every positive finite SD,
#' however small, is valid. Canonical output never retains Inf or NaN.
#'
#' Memory and lazy NetCDF sources share the same calculation. Lazy sources are
#' read through bounded indexed blocks rather than as one complete source cube;
#' the final anomaly cube is materialized in memory with source dimensions and
#' timestamps plus compact QA and provenance.
#'
#' @examples
#' x <- ocean_cube(
#'   lon = -80, lat = -12, depth = 0,
#'   time = as.Date(c("2020-01-01", "2021-01-01")),
#'   data = array(c(12, 8), c(1, 1, 1, 2, 1)),
#'   vars = "temperature", units = "degC"
#' )
#' baseline <- suppressWarnings(cube_climatology(x, "month"))
#' cube_anomaly(x, baseline, "difference")
#' cube_anomaly(x, baseline, "z")
#'
#' daily <- ocean_cube(
#'   lon = -80, lat = -12, depth = 0,
#'   time = as.Date(c("2020-02-28", "2020-02-29", "2021-02-28")),
#'   data = array(c(10, 14, 12), c(1, 1, 1, 3, 1)),
#'   vars = "temperature", units = "degC"
#' )
#' daily_clim <- suppressWarnings(cube_climatology(daily, "day", leap = "feb28"))
#' cube_anomaly(daily, daily_clim)
#'
#' seasonal <- ocean_cube(
#'   lon = -80, lat = -12, depth = 0,
#'   time = as.Date(c("2025-12-15", "2026-01-15", "2026-02-15")),
#'   data = array(c(3, 6, 9), c(1, 1, 1, 3, 1)),
#'   vars = "temperature", units = "degC"
#' )
#' seasonal_clim <- suppressWarnings(cube_climatology(seasonal, "season"))
#' cube_anomaly(seasonal, seasonal_clim)
#'
#' @seealso [cube_climatology()], [cube_aggregate_time()], [anom_diff()],
#'   [anom_z()]
#' @export
cube_anomaly <- function(
    x,
    climatology,
    type = c("difference", "z")) {
  cube_validate(x, strict = TRUE)
  source_backend <- .cube_backend(x)
  if (!source_backend %in% c("memory", "netcdf")) {
    rlang::abort(
      paste0("Unsupported ocean_cube backend: '", source_backend, "'."),
      class = "oceancube_unsupported_backend"
    )
  }
  if (!missing(type) &&
      (!is.character(type) || length(type) != 1L || is.na(type))) {
    .anomaly_abort("type must be exactly one of difference or z.")
  }
  type <- match.arg(type)
  scientific <- .anomaly_validate_climatology(climatology)
  alignment <- .anomaly_validate_alignment(x, climatology, scientific)
  group_plan <- .anomaly_group_plan(x$time, scientific)
  shape <- unname(.cube_shape(x))
  output_shape <- shape
  if (identical(source_backend, "memory")) {
    names(output_shape) <- .cube_memory_dimension_names(x)
  }
  output <- array(NA_real_, dim = output_shape)
  source_values <- .cube_product_as_double(
    shape, "anomaly source values"
  )
  time_blocks <- .anomaly_time_blocks(shape, source_backend)
  spatial_blocks <- .temporal_spatial_blocks(
    shape, n_time = max(lengths(time_blocks))
  )
  counts <- list(
    source_values = source_values,
    finite_source = 0,
    missing_mean = 0,
    invalid_sd = 0,
    zero_sd = 0,
    expected_unmatched = 0,
    output_finite = 0
  )
  metrics <- list(
    source_backend = source_backend,
    source_shape = shape,
    output_shape = shape,
    read_count = 0L,
    logical_values = 0,
    physical_values = 0,
    max_block = 0,
    full_source_materialized = FALSE,
    final_output_materialized = TRUE
  )

  for (time_index in time_blocks) {
    for (block in spatial_blocks) {
      index <- list(
        longitude = block$longitude,
        latitude = block$latitude,
        depth = block$depth,
        time = time_index,
        variable = block$variable
      )
      if (identical(source_backend, "netcdf")) {
        read_plan <- .plan_cube_index_read(x, index)
        logical_values <- read_plan$values_requested
        physical_values <- read_plan$values_in_envelope
      } else {
        logical_values <- .cube_product_as_double(
          lengths(index), "anomaly memory block values"
        )
        physical_values <- logical_values
      }
      values <- .cube_read(x, index = index, drop = FALSE)
      metrics$read_count <- metrics$read_count + 1L
      metrics$logical_values <- metrics$logical_values + logical_values
      metrics$physical_values <- metrics$physical_values + physical_values
      metrics$max_block <- max(metrics$max_block, physical_values)
      if (physical_values >= source_values) {
        metrics$full_source_materialized <- TRUE
      }
      block_shape <- c(
        length(block$longitude), length(block$latitude),
        length(block$depth), length(block$variable)
      )

      for (local_time in seq_along(time_index)) {
        source_time <- time_index[[local_time]]
        observation <- array(
          values[, , , local_time, , drop = FALSE],
          dim = block_shape
        )
        finite_source <- is.finite(observation)
        counts$finite_source <- counts$finite_source + sum(finite_source)
        climatology_time <- group_plan$index[[source_time]]
        if (is.na(climatology_time)) {
          counts$expected_unmatched <-
            counts$expected_unmatched + length(observation)
          next
        }
        mean_value <- array(
          climatology$data[
            block$longitude, block$latitude, block$depth,
            climatology_time, block$variable, drop = FALSE
          ],
          dim = block_shape
        )
        finite_mean <- is.finite(mean_value)
        counts$missing_mean <- counts$missing_mean +
          sum(finite_source & !finite_mean)
        result <- array(NA_real_, dim = block_shape)
        if (identical(type, "difference")) {
          valid <- finite_source & finite_mean
          result[valid] <- observation[valid] - mean_value[valid]
        } else {
          sd_value <- array(
            scientific$sd[
              block$longitude, block$latitude, block$depth,
              climatology_time, block$variable, drop = FALSE
            ],
            dim = block_shape
          )
          zero_sd <- is.finite(sd_value) & sd_value == 0
          invalid_sd <- !is.finite(sd_value) | zero_sd
          eligible <- finite_source & finite_mean
          counts$zero_sd <- counts$zero_sd + sum(eligible & zero_sd)
          counts$invalid_sd <- counts$invalid_sd + sum(eligible & invalid_sd)
          valid <- eligible & is.finite(sd_value) & sd_value > 0
          result[valid] <-
            (observation[valid] - mean_value[valid]) / sd_value[valid]
        }
        result[!is.finite(result)] <- NA_real_
        output[
          block$longitude, block$latitude, block$depth,
          source_time, block$variable
        ] <- result
      }
    }
  }
  counts$output_finite <- sum(is.finite(output))
  anomaly_metadata <- list(
    type = type,
    climatology_by = scientific$by,
    leap = scientific$leap,
    baseline_requested = scientific$requested_period,
    baseline_effective = scientific$effective_period,
    recurring_key = group_plan$description,
    zero_sd_policy = "NA",
    near_zero_sd_policy = "strict_positive",
    units = if (identical(type, "difference")) "source_units" else "1"
  )
  qa <- list(anomaly = list(
    type = type,
    climatology_by = scientific$by,
    leap = scientific$leap,
    baseline_requested = scientific$requested_period,
    baseline_effective = scientific$effective_period,
    alignment = alignment,
    counts = counts,
    backend = metrics
  ))
  record <- list(
    operation = "anomaly",
    type = type,
    climatology_by = scientific$by,
    leap = scientific$leap,
    baseline_requested = scientific$requested_period,
    baseline_effective = scientific$effective_period,
    inner_weighting = scientific$inner_weighting,
    outer_weighting = scientific$outer_weighting,
    center = scientific$center,
    sd_method = scientific$sd_method,
    zero_sd_policy = "NA",
    near_zero_sd_policy = "strict_positive",
    alignment = "exact_identity",
    units_rule = alignment$units,
    calendar_rule = "exact_identity",
    time_class_rule = "exact_identity",
    recurring_key = group_plan$description
  )
  output_shape <- stats::setNames(as.integer(dim(output)), .cube_axis_names())
  provenance_context <- .provenance_cube_context(
    source = x$source,
    dataset_id = x$dataset_id,
    time = x$time,
    shape = output_shape,
    variables = x$vars,
    backend = "memory",
    provenance = x$provenance
  )
  provenance_context$time_kind <- "historical"
  source_context <- .provenance_cube_context(
    source = x$source,
    dataset_id = x$dataset_id,
    time = x$time,
    shape = .cube_shape(x),
    variables = x$vars,
    backend = .cube_backend(x),
    provenance = x$provenance
  )
  source_context$time_kind <- "historical"
  merged <- .provenance_merge_lineages(
    x$provenance,
    climatology$provenance,
    roles = "climatology"
  )
  inputs <- c(
    list(list(
      role = "source",
      lineage_ref = "primary",
      entity_ref = .provenance_current_entity(merged$provenance),
      summary = .provenance_summary(source_context)
    )),
    merged$refs
  )
  provenance <- .provenance_append(
    merged$provenance,
    operation = "cube_anomaly",
    parameters = list(
      requested = list(type = type),
      resolved = list(
        climatology_by = scientific$by,
        leap = scientific$leap,
        baseline_requested = scientific$requested_period,
        baseline_effective = scientific$effective_period,
        inner_weighting = scientific$inner_weighting,
        outer_weighting = scientific$outer_weighting,
        center = scientific$center,
        sd_method = scientific$sd_method,
        zero_sd_policy = "NA",
        near_zero_sd_policy = "strict_positive",
        alignment_policy = "exact_identity",
        units_rule = alignment$units,
        calendar_rule = "exact_identity",
        time_class_rule = "exact_identity",
        recurring_key_semantics = group_plan$description
      )
    ),
    inputs = inputs,
    output = .provenance_summary(provenance_context),
    scientific_method = .provenance_method("cube_anomaly", record),
    context = provenance_context
  )
  output_units <- if (identical(type, "difference")) {
    x$units
  } else {
    stats::setNames(rep("1", length(x$vars)), x$vars)
  }
  result <- ocean_cube(
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    time = x$time,
    vars = x$vars,
    data = output,
    units = output_units,
    source = x$source,
    dataset_id = x$dataset_id,
    spatial_extent = x$spatial_extent,
    temporal_extent = x$temporal_extent,
    depth_extent = x$depth_extent,
    mask = x$mask,
    dc = x$dc,
    anomaly = anomaly_metadata,
    provenance = provenance,
    qa = qa
  )
  result <- .attach_cube_metadata(
    result,
    .cf_metadata_for_transform(x$metadata %||% NULL, "cube_anomaly")
  )
  cube_validate(result, strict = TRUE)
  result
}

.anomaly_abort <- function(message, class = "oceancube_anomaly_error") {
  rlang::abort(message, class = class)
}

.anomaly_validate_climatology <- function(climatology) {
  if (!inherits(climatology, "ocean_cube")) {
    .anomaly_abort(
      "climatology must be an intact canonical ocean_cube produced by cube_climatology()."
    )
  }
  cube_validate(climatology, strict = TRUE)
  if (!identical(.cube_backend(climatology), "memory")) {
    .anomaly_abort("climatology must use the memory backend.")
  }
  metadata <- climatology$climatology
  incomplete <- paste(
    "Climatology metadata are incomplete.",
    "Use an intact cube_climatology() result.",
    "Select or crop the source before computing climatology."
  )
  if (!is.list(metadata) ||
      !identical(metadata$type, "recurrent_climatology")) {
    .anomaly_abort(incomplete)
  }
  required <- c(
    "sd", "group_key", "by", "leap", "requested_period",
    "effective_period", "center", "sd_method", "inner_weighting",
    "outer_weighting", "calendar", "input_time_class", "output_time_class",
    "cycle_anchor"
  )
  if (length(setdiff(required, names(metadata))) > 0L) {
    .anomaly_abort(incomplete)
  }
  sd <- metadata$sd
  if (!is.numeric(sd) || !is.array(sd) || length(dim(sd)) != 5L ||
      !identical(unname(dim(sd)), unname(dim(climatology$data)))) {
    .anomaly_abort(
      "Canonical climatology SD must be a numeric 5-D array with dimensions identical to climatology data."
    )
  }
  if (any(is.finite(sd) & sd < 0)) {
    .anomaly_abort(
      "Canonical climatology SD contains a negative finite value.",
      class = "oceancube_anomaly_negative_sd"
    )
  }
  by <- metadata$by
  if (!is.character(by) || length(by) != 1L || is.na(by) ||
      !by %in% c("day", "month", "season")) {
    .anomaly_abort("Canonical climatology by metadata are invalid.")
  }
  leap <- metadata$leap
  if (identical(by, "day")) {
    if (!is.character(leap) || length(leap) != 1L || is.na(leap) ||
        !leap %in% c("drop", "keep", "feb28")) {
      .anomaly_abort("Daily climatology leap metadata are invalid.")
    }
  } else if (length(leap) != 1L || !is.na(leap)) {
    .anomaly_abort("Non-daily climatology leap metadata must be NA.")
  }
  group_key <- metadata$group_key
  expected_key <- .anomaly_expected_group_key(by, leap)
  if (!is.character(group_key) || anyNA(group_key) || any(!nzchar(group_key)) ||
      anyDuplicated(group_key) ||
      length(group_key) != length(climatology$time)) {
    .anomaly_abort(
      "Canonical climatology group keys must be unique, complete, and aligned with its time axis."
    )
  }
  if (!identical(group_key, expected_key)) {
    .anomaly_abort(
      "Canonical climatology does not contain the exact ordered full recurring cycle."
    )
  }
  if (!identical(metadata$center, "arithmetic_mean") ||
      !identical(metadata$sd_method, "sample_standard_deviation") ||
      !identical(metadata$inner_weighting, "equal_observation") ||
      !identical(metadata$outer_weighting, "equal_period_replicate")) {
    .anomaly_abort("Canonical climatology scientific method metadata are invalid.")
  }
  if (!is.character(metadata$calendar) || length(metadata$calendar) != 1L ||
      is.na(metadata$calendar) || !nzchar(metadata$calendar)) {
    .anomaly_abort("Canonical climatology calendar metadata are missing or invalid.")
  }
  if (!is.character(metadata$input_time_class) ||
      length(metadata$input_time_class) != 1L ||
      !metadata$input_time_class %in% c("Date", "POSIXct")) {
    .anomaly_abort(
      "Canonical climatology source time-class metadata are missing or invalid."
    )
  }
  actual_output_class <- if (inherits(climatology$time, "Date")) {
    "Date"
  } else {
    "POSIXct"
  }
  if (!is.character(metadata$output_time_class) ||
      length(metadata$output_time_class) != 1L ||
      !identical(metadata$output_time_class, actual_output_class) ||
      !identical(metadata$output_time_class, metadata$input_time_class)) {
    .anomaly_abort(
      "Canonical climatology output time-class metadata are missing or inconsistent."
    )
  }
  climatology_time_provenance <- .find_time_provenance(climatology$provenance)
  if (!is.list(climatology_time_provenance) ||
      !identical(climatology_time_provenance$calendar, metadata$calendar)) {
    .anomaly_abort(
      "Canonical climatology calendar metadata are internally inconsistent."
    )
  }
  if (!is.list(metadata$cycle_anchor) || length(metadata$cycle_anchor) == 0L) {
    .anomaly_abort("Canonical climatology cycle-anchor metadata are missing or invalid.")
  }
  for (field in c("requested_period", "effective_period")) {
    value <- metadata[[field]]
    if (length(value) != 2L || anyNA(value) ||
        !inherits(value, metadata$input_time_class) ||
        any(!is.finite(as.numeric(value))) || value[[1L]] > value[[2L]]) {
      .anomaly_abort(
        "Canonical climatology reference-period metadata are missing or invalid."
      )
    }
  }
  metadata
}

.anomaly_expected_group_key <- function(by, leap) {
  switch(
    by,
    day = .daily_keys(leap),
    month = sprintf("%02d", 1:12),
    season = c("DJF", "MAM", "JJA", "SON")
  )
}

.anomaly_validate_alignment <- function(x, climatology, scientific) {
  for (axis in c("lon", "lat", "depth")) {
    if (!identical(x[[axis]], climatology[[axis]])) {
      .anomaly_abort(paste0(
        "Source and climatology ", axis,
        " coordinates must be exactly identical and in the same order."
      ))
    }
  }
  if (!identical(x$vars, climatology$vars)) {
    .anomaly_abort(
      "Source and climatology variables must have identical names and order."
    )
  }
  unit_result <- .anomaly_validate_units(
    x$units, climatology$units, x$vars
  )
  time_provenance <- .find_time_provenance(x$provenance)
  source_calendar <- if (is.list(time_provenance)) {
    time_provenance$calendar
  } else {
    NULL
  }
  if (!identical(source_calendar, scientific$calendar)) {
    .anomaly_abort(
      "Source and climatology calendars must be exactly identical."
    )
  }
  source_time_class <- if (inherits(x$time, "Date")) "Date" else "POSIXct"
  if (!identical(source_time_class, scientific$input_time_class)) {
    .anomaly_abort(
      "Source time class must exactly match the climatology source time class."
    )
  }
  list(
    longitude = "exact",
    latitude = "exact",
    depth = "exact",
    variables = "exact",
    units = unit_result$status,
    calendar = "exact",
    time_class = "exact"
  )
}

.anomaly_normalize_units <- function(units, vars) {
  if (is.null(units)) {
    return(stats::setNames(rep(NA_character_, length(vars)), vars))
  }
  if (!is.null(names(units))) units <- units[vars]
  normalized <- vapply(units, function(value) {
    if (is.null(value) || length(value) == 0L || all(is.na(value))) {
      return(NA_character_)
    }
    if (length(value) != 1L || !is.character(value) || is.na(value) ||
        !nzchar(value)) {
      return(NA_character_)
    }
    value
  }, character(1L))
  stats::setNames(unname(normalized), vars)
}

.anomaly_validate_units <- function(source, climatology, vars) {
  source_units <- .anomaly_normalize_units(source, vars)
  climatology_units <- .anomaly_normalize_units(climatology, vars)
  source_missing <- is.na(source_units)
  climatology_missing <- is.na(climatology_units)
  if (any(xor(source_missing, climatology_missing))) {
    .anomaly_abort(
      "Source and climatology units must both be defined or both be missing for every variable."
    )
  }
  defined <- !source_missing
  if (any(source_units[defined] != climatology_units[defined])) {
    .anomaly_abort(
      "Defined source and climatology unit strings must match exactly by variable."
    )
  }
  if (any(source_missing & climatology_missing)) {
    warning(
      "Source and climatology units are both missing; unit compatibility is unverified.",
      call. = FALSE
    )
    return(list(status = "unverified_missing"))
  }
  list(status = "exact")
}

.anomaly_group_plan <- function(time, scientific) {
  dates <- if (inherits(time, "POSIXct")) {
    as.Date(time, tz = "UTC")
  } else {
    time
  }
  by <- scientific$by
  if (identical(by, "day")) {
    key <- format(dates, "%m-%d")
    if (identical(scientific$leap, "drop")) {
      key[key == "02-29"] <- NA_character_
    }
    if (identical(scientific$leap, "feb28")) {
      key[key == "02-29"] <- "02-28"
    }
    description <- paste0("MM-DD; leap=", scientific$leap)
  } else if (identical(by, "month")) {
    key <- format(dates, "%m")
    description <- "calendar_month"
  } else {
    month <- as.integer(format(dates, "%m"))
    key <- c("DJF", "MAM", "JJA", "SON")[
      c(1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 1L)[month]
    ]
    description <- "meteorological_season"
  }
  index <- match(key, scientific$group_key)
  structural_unmatched <- is.na(index) & !is.na(key)
  if (any(structural_unmatched)) {
    .anomaly_abort(
      "A source recurring group is missing from the canonical climatology."
    )
  }
  list(key = key, index = index, description = description)
}

.anomaly_time_blocks <- function(shape, backend, target_values = 250000L) {
  cells_per_time <- max(
    1,
    prod(as.double(shape[c(1L, 2L, 3L, 5L)]))
  )
  block_size <- max(1L, min(
    shape[[4L]],
    as.integer(floor(target_values / cells_per_time))
  ))
  if (identical(backend, "netcdf") && shape[[4L]] > 1L) {
    block_size <- min(
      block_size,
      max(1L, as.integer(ceiling(shape[[4L]] / 2)))
    )
  }
  split(
    seq_len(shape[[4L]]),
    ceiling(seq_len(shape[[4L]]) / block_size)
  )
}

.anomaly_adapt_ocean_clim <- function(clim) {
  if (!inherits(clim, "ocean_clim")) {
    .anomaly_abort("clim must be an ocean_clim object.")
  }
  core <- attr(clim, "oceancube_climatology", exact = TRUE)
  if (is.null(core)) {
    core <- tryCatch(
      clim$provenance$extra$core,
      error = function(error) NULL
    )
  }
  required <- c(
    "type", "by", "group_key", "requested_period", "effective_period",
    "leap", "center", "sd_method", "inner_weighting", "outer_weighting",
    "cycle_anchor", "calendar", "input_time_class", "output_time_class"
  )
  if (!is.list(core) || length(setdiff(required, names(core))) > 0L ||
      is.null(clim$mean) || is.null(clim$sd)) {
    .anomaly_abort(paste(
      "Historical ocean_clim metadata are incomplete and cannot be aligned safely.",
      "Recompute with clim_day(), clim_month(), or cube_climatology()."
    ))
  }
  source_time <- if (identical(core$input_time_class, "Date")) {
    as.Date("2001-01-01")
  } else {
    as.POSIXct("2001-01-01", tz = "UTC")
  }
  cycle <- .climatology_cycle_axis(source_time, core$by, core$leap)
  metadata <- core
  metadata$sd <- clim$sd
  ocean_cube(
    lon = clim$lon,
    lat = clim$lat,
    depth = clim$depth,
    time = cycle$time,
    vars = clim$vars,
    data = clim$mean,
    units = clim$units,
    source = clim$source,
    dataset_id = clim$dataset_id,
    climatology = metadata,
    provenance = clim$provenance
  )
}
