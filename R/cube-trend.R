#' Compute descriptive per-cell linear temporal trends
#'
#' `cube_trend()` estimates an ordinary least-squares slope independently for
#' every longitude, latitude, depth, and variable cell of a historical ocean
#' cube. The predictor is actual elapsed time, never observation position.
#'
#' @param x A valid `<ocean_cube>` with historical `Date` or UTC `POSIXct`
#'   time semantics. Memory and lazy NetCDF backends are supported. Recurrent
#'   climatology pseudo-time is rejected.
#' @param method The trend method. In 0.2.0 this must be exactly `"linear"`.
#' @param period `NULL` for the complete source range, or two ordered bounds
#'   defining a closed interval. Bounds must use the same `Date` or `POSIXct`
#'   semantics as `x$time`. Partially overlapping bounds are clipped with one
#'   warning, and requested and effective periods are retained.
#' @param time_unit Output slope time unit, exactly one of `"year"`, `"day"`,
#'   `"hour"`, or `"second"`. A year is exactly 365.2425 days, or 31556952
#'   seconds.
#' @param min_n A finite integer-like scalar of at least two giving the minimum
#'   number of finite observations required per cell. The default is three.
#' @param diagnostics A single non-missing logical. If `TRUE`, aligned
#'   `n_valid`, `time_span`, `intercept`, `r2`, and `residual_sd` arrays are
#'   retained in `trend$diagnostics`.
#'
#' @return An in-memory `<ocean_cube>` with longitude, latitude, depth, and
#'   variable axes preserved and time collapsed to one representational
#'   midpoint. `data` contains the slope in source-unit per selected time unit.
#'
#' @details
#' The descriptive linear slope is
#' \deqn{b = \frac{\sum (t_i - \bar{t})(y_i - \bar{y})}
#'                  {\sum (t_i - \bar{t})^2}.}
#' `Date` differences use exact civil days; `POSIXct` differences use exact UTC
#' instants and preserve subdaily and fractional-second spacing. Internally,
#' elapsed SI seconds are centered at the output anchor before conversion to
#' the requested public time unit.
#'
#' Only finite values are fitted. Every finite stored timestamp has one equal
#' observation weight. Irregular spacing is accepted because actual elapsed
#' time is used, but unequal observation density remains scientifically
#' meaningful. Use [cube_aggregate_time()] first when equal day, month, season,
#' or year weighting is intended.
#'
#' The singleton output time is the midpoint of the selected stored timestamp
#' range. For `Date`, half-day ties select the earlier civil day. For `POSIXct`,
#' the exact arithmetic midpoint instant is retained in UTC. This timestamp is
#' structural; it is not a baseline, breakpoint, or time when the trend
#' occurred.
#'
#' The core is descriptive only. It does not compute Sen slopes,
#' Mann--Kendall tests, standard errors, confidence intervals, p-values,
#' change metrics, or breakpoints. Lazy NetCDF input is processed in bounded
#' spatial and temporal blocks with one scientific pass; the full source cube
#' is not materialized and the final trend cube is materialized in memory.
#'
#' @examples
#' regular_time <- as.Date(c("2000-01-01", "2001-01-01", "2002-01-01"))
#' regular_years <- (as.numeric(regular_time) - as.numeric(regular_time[1])) / 365.2425
#' regular <- ocean_cube(
#'   lon = -80, lat = -12, depth = 0, time = regular_time,
#'   data = array(4 + regular_years, c(1, 1, 1, 3, 1)),
#'   vars = "temperature", units = "degC"
#' )
#' cube_trend(regular)
#'
#' time <- as.Date(c("2000-01-01", "2001-01-01", "2004-01-01"))
#' elapsed_years <- (as.numeric(time) - as.numeric(time[1])) / 365.2425
#' x <- ocean_cube(
#'   lon = -80, lat = -12, depth = 0, time = time,
#'   data = array(5 + 2 * elapsed_years, c(1, 1, 1, 3, 1)),
#'   vars = "temperature", units = "degC"
#' )
#' cube_trend(x)
#'
#' annual <- suppressWarnings(cube_aggregate_time(x, by = "year"))
#' cube_trend(annual)
#'
#' baseline <- suppressWarnings(cube_climatology(x, by = "month"))
#' anomaly <- cube_anomaly(x, baseline, type = "difference")
#' cube_trend(anomaly)
#'
#' @seealso [cube_aggregate_time()], [cube_climatology()], [cube_anomaly()],
#'   [signal_noise()], [viz.map()]
#' @export
cube_trend <- function(
    x,
    method = "linear",
    period = NULL,
    time_unit = "year",
    min_n = 3L,
    diagnostics = FALSE) {
  cube_validate(x, strict = TRUE)
  .trend_validate_arguments(method, time_unit, min_n, diagnostics)
  .trend_validate_historical_time(x)

  source_backend <- .cube_backend(x)
  if (!source_backend %in% c("memory", "netcdf")) {
    rlang::abort(
      paste0("Unsupported ocean_cube backend: '", source_backend, "'."),
      class = "oceancube_unsupported_backend"
    )
  }

  reference <- .climatology_reference_period(x$time, period)
  time_index <- reference$source_index
  if (length(time_index) == 0L) {
    rlang::abort(
      "The requested trend period contains no stored source timestamps.",
      class = "oceancube_trend_no_timestamps"
    )
  }
  selected_time <- x$time[time_index]
  anchor <- .trend_time_anchor(selected_time)
  seconds_per_unit <- .trend_seconds_per_unit(time_unit)
  elapsed_seconds <- .trend_elapsed_seconds(selected_time, anchor)
  scaled_time <- elapsed_seconds / seconds_per_unit
  time_spacing_regular <- .trend_time_spacing_regular(elapsed_seconds)

  shape <- unname(.cube_shape(x))
  output_shape <- c(shape[1:3], 1L, shape[5])
  output <- array(NA_real_, dim = output_shape)
  diagnostic_arrays <- .trend_diagnostic_arrays(output_shape, diagnostics)
  source_values <- .cube_product_as_double(shape, "trend source values")
  blocks <- .temporal_spatial_blocks(shape, n_time = 1L)
  metrics <- list(
    source_backend = source_backend,
    input_shape = shape,
    output_shape = output_shape,
    backend_read_count = 0L,
    logical_values = 0,
    physical_values = 0,
    maximum_block_values = 0,
    full_source_materialized = FALSE,
    final_output_materialized = TRUE,
    scientific_passes = 1L
  )
  cells_total <- .cube_product_as_double(shape[c(1L, 2L, 3L, 5L)], "trend cells")
  cells_fitted <- 0
  cells_insufficient_n <- 0
  cells_all_invalid <- 0

  for (block in blocks) {
    block_cells <- .cube_product_as_double(
      c(length(block$longitude), length(block$latitude),
        length(block$depth), length(block$variable)),
      "trend block cells"
    )
    state <- .trend_accumulator(block_cells)
    time_blocks <- .trend_time_blocks(time_index, block_cells)

    for (selected_positions in time_blocks) {
      source_time_index <- time_index[selected_positions]
      index <- list(
        longitude = block$longitude,
        latitude = block$latitude,
        depth = block$depth,
        time = source_time_index,
        variable = block$variable
      )
      if (identical(source_backend, "netcdf")) {
        read_plan <- .plan_cube_index_read(x, index)
        logical_values <- read_plan$values_requested
        physical_values <- read_plan$values_in_envelope
      } else {
        logical_values <- .cube_product_as_double(
          lengths(index), "trend memory block values"
        )
        physical_values <- logical_values
      }
      values <- .cube_read(x, index = index, drop = FALSE)
      metrics$backend_read_count <- metrics$backend_read_count + 1L
      metrics$logical_values <- metrics$logical_values + logical_values
      metrics$physical_values <- metrics$physical_values + physical_values
      metrics$maximum_block_values <- max(
        metrics$maximum_block_values, physical_values
      )
      if (physical_values >= source_values) {
        metrics$full_source_materialized <- TRUE
      }
      state <- .trend_accumulate_block(
        state,
        values,
        scaled_time[selected_positions]
      )
    }

    fitted <- .trend_finalize_block(state, min_n = as.integer(min_n))
    target <- list(
      block$longitude, block$latitude, block$depth, 1L, block$variable
    )
    block_shape <- c(
      length(block$longitude), length(block$latitude),
      length(block$depth), 1L, length(block$variable)
    )
    output[target[[1L]], target[[2L]], target[[3L]], 1L, target[[5L]]] <-
      array(fitted$slope, dim = block_shape)
    if (diagnostics) {
      diagnostic_arrays$n_valid[
        target[[1L]], target[[2L]], target[[3L]], 1L, target[[5L]]
      ] <- array(fitted$n_valid, dim = block_shape)
      diagnostic_arrays$time_span[
        target[[1L]], target[[2L]], target[[3L]], 1L, target[[5L]]
      ] <- array(fitted$time_span, dim = block_shape)
      diagnostic_arrays$intercept[
        target[[1L]], target[[2L]], target[[3L]], 1L, target[[5L]]
      ] <- array(fitted$intercept, dim = block_shape)
      diagnostic_arrays$r2[
        target[[1L]], target[[2L]], target[[3L]], 1L, target[[5L]]
      ] <- array(fitted$r2, dim = block_shape)
      diagnostic_arrays$residual_sd[
        target[[1L]], target[[2L]], target[[3L]], 1L, target[[5L]]
      ] <- array(fitted$residual_sd, dim = block_shape)
    }
    cells_fitted <- cells_fitted + sum(is.finite(fitted$slope))
    cells_insufficient_n <- cells_insufficient_n + sum(
      fitted$n_valid > 0L & fitted$n_valid < min_n
    )
    cells_all_invalid <- cells_all_invalid + sum(fitted$n_valid == 0L)
  }

  output[!is.finite(output)] <- NA_real_
  output_units <- .trend_output_units(x$units, x$vars, time_unit)
  trend_units_unverified <- .trend_units_unverified(x$units, x$vars)
  output_dimnames <- list(
    lon = as.character(x$lon),
    lat = as.character(x$lat),
    depth = as.character(x$depth),
    time = as.character(anchor),
    var = x$vars
  )
  dimnames(output) <- output_dimnames
  if (diagnostics) {
    diagnostic_arrays <- lapply(diagnostic_arrays, function(value) {
      dimnames(value) <- output_dimnames
      value
    })
  }

  selected_range <- range(selected_time)
  time_anchor_semantics <- "midpoint_of_selected_timestamp_range"
  trend_record <- list(
    operation = "trend",
    method = method,
    time_basis = "actual_elapsed_time",
    internal_time_unit = "second",
    output_time_unit = time_unit,
    seconds_per_output_unit = seconds_per_unit,
    period_requested = reference$requested,
    period_effective = reference$effective,
    selected_timestamp_range = selected_range,
    time_anchor = anchor,
    time_anchor_semantics = time_anchor_semantics,
    min_n = as.integer(min_n),
    finite_value_policy = "is.finite",
    observation_weighting = "equal_observation",
    time_spacing_regular = time_spacing_regular,
    trend_units_unverified = trend_units_unverified,
    inference = FALSE
  )
  trend_metadata <- c(
    trend_record[names(trend_record) != "operation"],
    list(
      metric = "slope",
      diagnostics_enabled = diagnostics,
      diagnostics = if (diagnostics) diagnostic_arrays else NULL
    )
  )
  qa_record <- list(
    method = method,
    time_basis = "elapsed_seconds",
    time_unit = time_unit,
    seconds_per_time_unit = seconds_per_unit,
    period_requested = reference$requested,
    period_effective = reference$effective,
    selected_time_start = selected_range[[1L]],
    selected_time_end = selected_range[[2L]],
    time_anchor = anchor,
    time_anchor_semantics = time_anchor_semantics,
    min_n = as.integer(min_n),
    observation_weighting = "equal_observation",
    time_spacing_regular = time_spacing_regular,
    trend_units_unverified = trend_units_unverified,
    input_shape = shape,
    output_shape = output_shape,
    cells_total = cells_total,
    cells_fitted = cells_fitted,
    cells_insufficient_n = cells_insufficient_n,
    cells_all_invalid = cells_all_invalid,
    backend = metrics
  )
  qa <- list(trend = qa_record)
  if (!is.null(x$qa)) qa$parent <- x$qa
  provenance_context <- .provenance_cube_context(
    source = x$source,
    dataset_id = x$dataset_id,
    time = anchor,
    shape = stats::setNames(as.integer(output_shape), .cube_axis_names()),
    variables = x$vars,
    backend = "memory",
    provenance = x$provenance
  )
  provenance_context$time_kind <- "trend_anchor"
  provenance <- .provenance_append(
    x$provenance,
    operation = "cube_trend",
    parameters = list(
      requested = list(
        method = method,
        period = reference$requested,
        time_unit = time_unit,
        min_n = as.integer(min_n),
        diagnostics = diagnostics
      ),
      resolved = list(
        period_effective = reference$effective,
        time_basis = "elapsed",
        internal_time_unit = "second",
        output_time_unit = time_unit,
        seconds_per_output_unit = seconds_per_unit,
        finite_value_policy = "is.finite",
        observation_weighting = "equal_observation",
        time_spacing = if (time_spacing_regular) "regular" else "irregular",
        inference = "none",
        time_anchor = anchor,
        time_anchor_semantics = time_anchor_semantics
      )
    ),
    output = .provenance_summary(provenance_context),
    scientific_method = .provenance_method("cube_trend", trend_record),
    context = provenance_context
  )

  result <- ocean_cube(
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    time = anchor,
    vars = x$vars,
    data = output,
    units = output_units,
    source = x$source,
    dataset_id = x$dataset_id,
    spatial_extent = x$spatial_extent,
    temporal_extent = range(anchor),
    depth_extent = x$depth_extent,
    mask = x$mask,
    dc = x$dc,
    provenance = provenance,
    qa = qa
  )
  result$trend <- trend_metadata
  dimnames(result$data) <- output_dimnames
  cube_validate(result, strict = TRUE)
  result
}

.trend_validate_arguments <- function(method, time_unit, min_n, diagnostics) {
  if (!is.character(method) || length(method) != 1L || is.na(method) ||
      !identical(method, "linear")) {
    .abort_badarg("method", "must be exactly `linear`; Sen and other methods are not supported in 0.2.0.")
  }
  allowed_units <- c("year", "day", "hour", "second")
  if (!is.character(time_unit) || length(time_unit) != 1L ||
      is.na(time_unit) || !time_unit %in% allowed_units) {
    .abort_badarg(
      "time_unit",
      "must be exactly one of `year`, `day`, `hour`, or `second`."
    )
  }
  if (!is.numeric(min_n) || length(min_n) != 1L || is.na(min_n) ||
      !is.finite(min_n) || min_n < 2 || min_n != floor(min_n)) {
    .abort_badarg("min_n", "must be a finite integer-like scalar of at least 2.")
  }
  if (!is.logical(diagnostics) || length(diagnostics) != 1L ||
      is.na(diagnostics)) {
    .abort_badarg("diagnostics", "must be a single non-missing logical value.")
  }
  invisible(TRUE)
}

.trend_validate_historical_time <- function(x) {
  recurrent <- is.list(x$climatology) &&
    identical(x$climatology$type, "recurrent_climatology")
  semantics <- .trend_provenance_time_semantics(x$provenance)
  if (recurrent || semantics %in% c(
    "recurrent_climatology", "recurring_climatology", "trend_anchor"
  )) {
    rlang::abort(
      paste(
        "`cube_trend()` requires historical time.",
        "Climatology recurrence and trend-anchor axes are not historical time series."
      ),
      class = "oceancube_trend_nonhistorical_time"
    )
  }
  invisible(TRUE)
}

.trend_provenance_time_semantics <- function(provenance) {
  if (!is.list(provenance)) return("historical")
  if (!is.null(provenance$schema_version) &&
      is.list(provenance$time$current) &&
      .provenance_scalar_character(provenance$time$current$kind)) {
    return(provenance$time$current$kind)
  }
  if (is.list(provenance$cube_trend)) return("trend_anchor")
  if (is.list(provenance$signal_noise) || is.list(provenance$cube_anomaly)) {
    return("historical")
  }
  if (is.list(provenance$cube_climatology)) {
    return("recurrent_climatology")
  }
  preserving <- c(
    "cube_slice", "cube_crop", "cube_aggregate_time", "cube_mask"
  )
  if (any(vapply(preserving, function(name) is.list(provenance[[name]]), logical(1L))) &&
      is.list(provenance$parent)) {
    return(.trend_provenance_time_semantics(provenance$parent))
  }
  "historical"
}

.trend_seconds_per_unit <- function(time_unit) {
  switch(
    time_unit,
    year = 31556952,
    day = 86400,
    hour = 3600,
    second = 1
  )
}

.trend_time_anchor <- function(time) {
  start <- time[[1L]]
  end <- time[[length(time)]]
  if (inherits(time, "Date")) {
    return(start + floor(as.numeric(end - start) / 2))
  }
  as.POSIXct(
    as.numeric(start) + (as.numeric(end) - as.numeric(start)) / 2,
    origin = "1970-01-01",
    tz = "UTC"
  )
}

.trend_elapsed_seconds <- function(time, anchor) {
  if (inherits(time, "Date")) {
    return((as.numeric(time) - as.numeric(anchor)) * 86400)
  }
  as.numeric(time) - as.numeric(anchor)
}

.trend_time_spacing_regular <- function(elapsed_seconds) {
  intervals <- diff(elapsed_seconds)
  length(intervals) <= 1L || isTRUE(all.equal(
    intervals,
    rep(intervals[[1L]], length(intervals)),
    tolerance = sqrt(.Machine$double.eps),
    check.attributes = FALSE
  ))
}

.trend_time_blocks <- function(time_index, block_cells, target_values = 250000L) {
  maximum <- max(1L, as.integer(floor(target_values / max(1, block_cells))))
  if (length(time_index) > 1L) {
    maximum <- min(maximum, ceiling(length(time_index) / 2))
  }
  split(seq_along(time_index), ceiling(seq_along(time_index) / maximum))
}

.trend_accumulator <- function(cells) {
  list(
    n = integer(cells),
    mean_t = numeric(cells),
    mean_y = numeric(cells),
    c_tt = numeric(cells),
    c_ty = numeric(cells),
    m2_y = numeric(cells),
    first_t = rep(NA_real_, cells),
    last_t = rep(NA_real_, cells)
  )
}

.trend_accumulate_block <- function(state, values, time_values) {
  cells <- length(state$n)
  matrix_values <- matrix(
    aperm(values, c(1L, 2L, 3L, 5L, 4L)),
    nrow = cells,
    ncol = length(time_values)
  )
  for (time_position in seq_along(time_values)) {
    observation <- matrix_values[, time_position]
    valid <- is.finite(observation)
    if (!any(valid)) next
    previous_n <- state$n[valid]
    updated_n <- previous_n + 1L
    time_value <- time_values[[time_position]]
    delta_t <- time_value - state$mean_t[valid]
    delta_y <- observation[valid] - state$mean_y[valid]
    updated_mean_t <- state$mean_t[valid] + delta_t / updated_n
    updated_mean_y <- state$mean_y[valid] + delta_y / updated_n
    state$c_tt[valid] <- state$c_tt[valid] +
      delta_t * (time_value - updated_mean_t)
    state$c_ty[valid] <- state$c_ty[valid] +
      delta_t * (observation[valid] - updated_mean_y)
    state$m2_y[valid] <- state$m2_y[valid] +
      delta_y * (observation[valid] - updated_mean_y)
    first <- valid & state$n == 0L
    state$first_t[first] <- time_value
    state$last_t[valid] <- time_value
    state$mean_t[valid] <- updated_mean_t
    state$mean_y[valid] <- updated_mean_y
    state$n[valid] <- updated_n
  }
  state
}

.trend_finalize_block <- function(state, min_n) {
  cells <- length(state$n)
  slope <- intercept <- r2 <- residual_sd <- rep(NA_real_, cells)
  time_span <- rep(NA_real_, cells)
  time_span[state$n == 1L] <- 0
  multiple <- state$n >= 2L
  time_span[multiple] <- state$last_t[multiple] - state$first_t[multiple]
  eligible <- state$n >= min_n & is.finite(state$c_tt) & state$c_tt > 0
  slope[eligible] <- state$c_ty[eligible] / state$c_tt[eligible]
  intercept[eligible] <- state$mean_y[eligible] -
    slope[eligible] * state$mean_t[eligible]
  sse <- rep(NA_real_, cells)
  sse[eligible] <- pmax(
    0,
    state$m2_y[eligible] - slope[eligible] * state$c_ty[eligible]
  )
  variable_y <- eligible & state$m2_y > 0
  r2[variable_y] <- pmin(
    1,
    pmax(0, 1 - sse[variable_y] / state$m2_y[variable_y])
  )
  residual <- eligible & state$n > 2L
  residual_sd[residual] <- sqrt(sse[residual] / (state$n[residual] - 2L))
  for (value in c("slope", "intercept", "r2", "residual_sd", "time_span")) {
    current <- get(value)
    current[!is.finite(current)] <- NA_real_
    assign(value, current)
  }
  list(
    slope = slope,
    n_valid = state$n,
    time_span = time_span,
    intercept = intercept,
    r2 = r2,
    residual_sd = residual_sd
  )
}

.trend_diagnostic_arrays <- function(shape, diagnostics) {
  if (!diagnostics) return(NULL)
  list(
    n_valid = array(0L, dim = shape),
    time_span = array(NA_real_, dim = shape),
    intercept = array(NA_real_, dim = shape),
    r2 = array(NA_real_, dim = shape),
    residual_sd = array(NA_real_, dim = shape)
  )
}

.trend_output_units <- function(units, variables, time_unit) {
  if (is.null(units)) return(NULL)
  source <- if (is.null(names(units))) units else units[variables]
  suffix <- paste0(time_unit, "-1")
  output <- vapply(source, function(unit) {
    if (length(unit) != 1L || is.na(unit) || !nzchar(unit)) return(NA_character_)
    if (identical(unit, "1")) suffix else paste(unit, suffix)
  }, character(1L))
  names(output) <- variables
  output
}

.trend_units_unverified <- function(units, variables) {
  if (is.null(units)) return(TRUE)
  source <- if (is.null(names(units))) units else units[variables]
  any(vapply(source, function(unit) {
    length(unit) != 1L || is.na(unit) || !nzchar(unit)
  }, logical(1L)))
}
