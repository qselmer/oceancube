#' Compute a recurrent climatology from an ocean cube
#'
#' `cube_climatology()` computes a recurrent daily, monthly, or meteorological
#' seasonal climatology without pooling raw observations across years. Raw
#' finite observations are first averaged within each day-year, month-year, or
#' season-year replicate. The climatological mean and sample standard deviation
#' are then calculated across those replicates with equal replicate weights.
#'
#' @param x A valid `<ocean_cube>` using the memory or NetCDF backend.
#' @param by A character scalar selecting `"day"`, `"month"`, or `"season"`.
#' @param period `NULL` for the full source range, or two ordered bounds defining
#'   a closed reference interval. Bounds must use the same Date or POSIXct
#'   semantics as `x$time`; POSIXct bounds are exact UTC instants. Intersecting
#'   bounds outside the source range are clipped with one warning and both the
#'   requested and effective periods are retained.
#' @param leap Daily leap-day policy: `"drop"` removes February 29, `"keep"`
#'   retains it as an independent group, and `"feb28"` first combines February
#'   28 and 29 within a leap year into one equally weighted February-28-equivalent
#'   replicate. `leap` is not applicable to monthly or seasonal climatologies.
#' @param min_n A finite positive integer-like scalar giving the minimum number
#'   of valid period-year replicates required per cell and recurrent group.
#' @param diagnostics A non-missing logical scalar. If `TRUE`, aligned
#'   `n_clim_valid` and replicate-coverage arrays are retained in
#'   `qa$climatology`. Sample SD is always retained independently of this flag.
#'
#' @return An in-memory `<ocean_cube>`. `data` contains the climatological mean.
#'   The aligned sample SD is in `climatology$sd`, and recurrent keys are in
#'   `climatology$group_key`.
#'
#' @details
#' Inner period means use equal finite-observation weighting; duration weighting
#' is not performed. Across years or season-years, every valid replicate has one
#' equal weight. Irregular or gapped sampling emits at most one warning per call
#' and is recorded in provenance. `n_clim_total` counts eligible calendar-period
#' opportunities, `n_clim_valid` counts finite period-year replicates, and
#' `coverage_fraction = n_clim_valid / n_clim_total` when the denominator is
#' positive.
#'
#' The output always contains a complete recurrent cycle. Daily keys are stable
#' `MM-DD` values: `drop` and `feb28` use the 365-day year 2001, while `keep`
#' uses leap year 2000. Monthly timestamps are the first days of 2001. Seasonal
#' timestamps are DJF `2000-12-01`, MAM `2001-03-01`, JJA `2001-06-01`, and SON
#' `2001-09-01`. These timestamps are a strictly increasing climatological
#' pseudo-time representation, not the historical reference period. Date input
#' produces Date output; POSIXct input produces UTC POSIXct midnight output.
#'
#' Partial first and last periods are retained. Lazy NetCDF input is processed
#' with bounded source-period and spatial/depth/variable reads; neither the full
#' source cube nor a full chronological intermediate cube is materialized.
#' `cube_slice()` and `cube_crop()` deliberately discard dimensional scientific
#' metadata and record that action in provenance, so select or crop the source
#' before computing a climatology when the SD must remain attached.
#'
#' @examples
#' monthly <- ocean_cube(
#'   lon = -80, lat = -12, depth = 0,
#'   time = as.Date(c("2020-01-01", "2021-01-01")),
#'   data = array(c(10, 20), c(1, 1, 1, 2, 1)), vars = "temperature"
#' )
#' suppressWarnings(cube_climatology(monthly, by = "month"))
#'
#' daily <- ocean_cube(
#'   lon = -80, lat = -12, depth = 0,
#'   time = as.Date(c("2020-02-28", "2020-02-29")),
#'   data = array(c(20, 40), c(1, 1, 1, 2, 1)), vars = "temperature"
#' )
#' cube_climatology(daily, by = "day", leap = "feb28")
#'
#' seasonal <- ocean_cube(
#'   lon = -80, lat = -12, depth = 0,
#'   time = as.Date(c("2025-12-15", "2026-01-15", "2026-02-15")),
#'   data = array(c(3, 6, 9), c(1, 1, 1, 3, 1)), vars = "temperature"
#' )
#' cube_climatology(seasonal, by = "season")
#' suppressWarnings(cube_climatology(monthly, by = "month", diagnostics = TRUE))
#'
#' @seealso [cube_aggregate_time()], [clim_day()], [clim_month()], [cube_extract()]
#' @export
cube_climatology <- function(
    x,
    by,
    period = NULL,
    leap = c("feb28", "drop", "keep"),
    min_n = 1L,
    diagnostics = FALSE) {
  leap_missing <- missing(leap)
  cube_validate(x, strict = TRUE)
  .calendar_operation_unsupported(x, "cube_climatology")

  if (missing(by) || !is.character(by) || length(by) != 1L ||
      is.na(by) || !by %in% c("day", "month", "season")) {
    .abort_badarg("by", "must be exactly one of `day`, `month`, or `season`.")
  }
  if (!identical(by, "day") && !leap_missing) {
    .abort_badarg("leap", "is only applicable when `by = \"day\"`.")
  }
  leap <- if (identical(by, "day")) match.arg(leap) else NA_character_
  if (!is.numeric(min_n) || length(min_n) != 1L || is.na(min_n) ||
      !is.finite(min_n) || min_n < 1 || min_n != floor(min_n)) {
    .abort_badarg("min_n", "must be a finite positive integer-like scalar.")
  }
  min_n <- as.integer(min_n)
  if (!is.logical(diagnostics) || length(diagnostics) != 1L ||
      is.na(diagnostics)) {
    .abort_badarg("diagnostics", "must be a single non-missing logical value.")
  }

  reference <- .climatology_reference_period(x$time, period)
  inspection <- cube_inspect(x, missing = "none")
  irregular_sampling <- isFALSE(inspection$time_summary$regular)
  if (irregular_sampling) {
    warning(
      paste(
        "Climatology inner-period means use equal observation weighting;",
        "irregular or gapped sampling can differ from duration-weighted means."
      ),
      call. = FALSE
    )
  }

  plan <- .climatology_replicate_plan(
    x$time,
    reference$effective,
    by = by,
    leap = leap
  )
  cycle <- .climatology_cycle_axis(x$time, by = by, leap = leap)
  shape <- unname(.cube_shape(x))
  output_shape <- c(shape[1:3], length(cycle$group_key), shape[5])
  accumulator_n <- array(0L, dim = output_shape)
  accumulator_mean <- array(0, dim = output_shape)
  accumulator_m2 <- array(0, dim = output_shape)

  read_metrics <- list(
    backend = .cube_backend(x),
    input_shape = shape,
    reference_time_count = length(reference$source_index),
    output_shape = output_shape,
    logical_source_values = 0,
    physical_source_values = 0,
    backend_read_count = 0L,
    maximum_physical_block_values = 0,
    full_source_cube_materialized = FALSE,
    full_chronological_intermediate_materialized = FALSE
  )

  source_ids <- .climatology_time_ids(x$time, by = by, leap = leap)
  in_reference <- seq_along(x$time) %in% reference$source_index
  for (replicate in plan$replicates) {
    group_index <- match(replicate$recurring_group, cycle$group_key)
    component_indices <- lapply(
      replicate$component_id,
      function(component) which(in_reference & source_ids$component_id == component)
    )
    if (!any(lengths(component_indices) > 0L)) next

    maximum_component_time <- max(1L, lengths(component_indices))
    blocks <- .temporal_spatial_blocks(shape, maximum_component_time)
    for (block in blocks) {
      component_means <- list()
      for (time_index in component_indices) {
        if (length(time_index) == 0L) next
        index <- list(
          longitude = block$longitude,
          latitude = block$latitude,
          depth = block$depth,
          time = time_index,
          variable = block$variable
        )
        if (identical(read_metrics$backend, "netcdf")) {
          read_plan <- .plan_cube_index_read(x, index)
          logical_values <- read_plan$values_requested
          physical_values <- read_plan$values_in_envelope
        } else {
          logical_values <- prod(as.double(lengths(index)))
          physical_values <- logical_values
        }
        values <- .cube_read(x, index = index, drop = FALSE)
        component_means[[length(component_means) + 1L]] <-
          .temporal_reduce_block(
            values,
            method = "mean",
            na.rm = TRUE,
            min_n = 1L
          )$value
        read_metrics$logical_source_values <-
          read_metrics$logical_source_values + logical_values
        read_metrics$physical_source_values <-
          read_metrics$physical_source_values + physical_values
        read_metrics$backend_read_count <- read_metrics$backend_read_count + 1L
        read_metrics$maximum_physical_block_values <- max(
          read_metrics$maximum_physical_block_values,
          physical_values
        )
      }
      replicate_value <- .climatology_combine_components(component_means)
      block_dim <- c(
        length(block$longitude), length(block$latitude),
        length(block$depth), length(block$variable)
      )
      old_n <- array(
        accumulator_n[
          block$longitude, block$latitude, block$depth,
          group_index, block$variable, drop = FALSE
        ],
        dim = block_dim
      )
      old_mean <- array(
        accumulator_mean[
          block$longitude, block$latitude, block$depth,
          group_index, block$variable, drop = FALSE
        ],
        dim = block_dim
      )
      old_m2 <- array(
        accumulator_m2[
          block$longitude, block$latitude, block$depth,
          group_index, block$variable, drop = FALSE
        ],
        dim = block_dim
      )
      updated <- .climatology_welford_update(
        old_n, old_mean, old_m2, replicate_value
      )
      accumulator_n[
        block$longitude, block$latitude, block$depth,
        group_index, block$variable
      ] <- updated$n
      accumulator_mean[
        block$longitude, block$latitude, block$depth,
        group_index, block$variable
      ] <- updated$mean
      accumulator_m2[
        block$longitude, block$latitude, block$depth,
        group_index, block$variable
      ] <- updated$m2
    }
  }

  climatological_mean <- accumulator_mean
  climatological_mean[accumulator_n < min_n] <- NA_real_
  climatological_sd <- array(NA_real_, dim = output_shape)
  sd_valid <- accumulator_n >= max(2L, min_n)
  climatological_sd[sd_valid] <- sqrt(
    pmax(0, accumulator_m2[sd_valid]) / (accumulator_n[sd_valid] - 1L)
  )
  coverage <- array(NA_real_, dim = output_shape)
  for (group_index in seq_along(cycle$group_key)) {
    denominator <- plan$n_clim_total[[group_index]]
    if (denominator > 0L) {
      coverage[, , , group_index, ] <-
        accumulator_n[, , , group_index, ] / denominator
    }
  }

  output_dimnames <- list(
    lon = as.character(x$lon),
    lat = as.character(x$lat),
    depth = as.character(x$depth),
    time = as.character(cycle$time),
    var = x$vars
  )
  dimnames(climatological_mean) <- output_dimnames
  dimnames(climatological_sd) <- output_dimnames
  dimnames(accumulator_n) <- output_dimnames
  dimnames(coverage) <- output_dimnames

  time_provenance <- .find_time_provenance(x$provenance)
  calendar <- time_provenance$calendar %||% "proleptic_gregorian"
  metadata <- list(
    type = "recurrent_climatology",
    by = by,
    sd = climatological_sd,
    group_key = cycle$group_key,
    requested_period = reference$requested,
    effective_period = reference$effective,
    period_clipped = reference$clipped,
    leap = if (identical(by, "day")) leap else NA_character_,
    min_n = min_n,
    n_clim_total = plan$n_clim_total,
    center = "arithmetic_mean",
    sd_method = "sample_standard_deviation",
    inner_weighting = "equal_observation",
    outer_weighting = "equal_period_replicate",
    cycle_anchor = cycle$anchor,
    calendar = calendar,
    input_time_class = if (inherits(x$time, "Date")) "Date" else "POSIXct",
    output_time_class = if (inherits(cycle$time, "Date")) "Date" else "POSIXct"
  )
  record <- metadata[names(metadata) != "sd"]
  record$irregular_sampling <- irregular_sampling
  record$eligible_years <- plan$eligible_years
  record$eligible_season_years <- plan$eligible_season_years
  record$partial_edge_periods <- plan$period_summary$source_period_start[
    plan$period_summary$partial & plan$period_summary$included
  ]
  provenance_context <- .provenance_cube_context(
    source = x$source,
    dataset_id = x$dataset_id,
    time = cycle$time,
    shape = stats::setNames(as.integer(output_shape), .cube_axis_names()),
    variables = x$vars,
    backend = "memory",
    provenance = x$provenance
  )
  provenance_context$time_kind <- "recurring_climatology"
  provenance <- .provenance_append(
    x$provenance,
    operation = "cube_climatology",
    parameters = list(
      requested = list(
        by = by,
        period = reference$requested,
        leap = if (identical(by, "day")) leap else NA_character_,
        min_n = min_n,
        diagnostics = diagnostics
      ),
      resolved = list(
        period_effective = reference$effective,
        period_clipped = reference$clipped,
        center = metadata$center,
        sd_method = metadata$sd_method,
        inner_weighting = metadata$inner_weighting,
        outer_weighting = metadata$outer_weighting,
        cycle = by,
        cycle_anchor = cycle$anchor,
        irregular_sampling = irregular_sampling,
        calendar = calendar,
        input_time_class = metadata$input_time_class,
        output_time_class = metadata$output_time_class,
        output_shape = stats::setNames(as.integer(output_shape), .cube_axis_names())
      )
    ),
    output = .provenance_summary(provenance_context),
    scientific_method = .provenance_method("cube_climatology", record),
    context = provenance_context
  )
  qa_record <- list(
    diagnostics = diagnostics,
    n_clim_total = plan$n_clim_total,
    period_summary = plan$period_summary,
    read_metrics = read_metrics
  )
  if (diagnostics) {
    qa_record$n_clim_valid <- accumulator_n
    qa_record$coverage_fraction <- coverage
  }
  qa <- list(climatology = qa_record)
  if (!is.null(x$qa)) qa$parent <- x$qa

  result <- ocean_cube(
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    time = cycle$time,
    vars = x$vars,
    data = climatological_mean,
    units = x$units,
    source = x$source,
    dataset_id = x$dataset_id,
    spatial_extent = x$spatial_extent,
    temporal_extent = range(cycle$time),
    depth_extent = x$depth_extent,
    mask = x$mask,
    dc = x$dc,
    climatology = metadata,
    provenance = provenance,
    qa = qa
  )
  dimnames(result$data) <- output_dimnames
  result <- .attach_cube_metadata(
    result,
    .cf_metadata_for_transform(x$metadata %||% NULL, "cube_climatology")
  )
  cube_validate(result, strict = TRUE)
  result
}

.climatology_reference_period <- function(time, period) {
  source_range <- range(time)
  if (is.null(period)) {
    requested <- source_range
  } else {
    if (length(period) != 2L || !is.null(dim(period))) {
      .abort_badarg("period", "must contain exactly two ordered bounds.")
    }
    requested <- .canonicalize_time(
      period,
      arg = "period",
      validate_axis = FALSE
    )$values
    expected_class <- if (inherits(time, "Date")) "Date" else "POSIXct"
    if (!inherits(requested, expected_class)) {
      .abort_badarg(
        "period",
        paste0("must use the same ", expected_class, " semantics as `x$time`.")
      )
    }
    if (as.numeric(requested[[1L]]) > as.numeric(requested[[2L]])) {
      .abort_badarg("period", "bounds must be ordered as `c(start, end)`.")
    }
  }
  requested_number <- as.numeric(requested)
  source_number <- as.numeric(source_range)
  effective_number <- c(
    max(requested_number[[1L]], source_number[[1L]]),
    min(requested_number[[2L]], source_number[[2L]])
  )
  if (effective_number[[1L]] > effective_number[[2L]]) {
    rlang::abort(
      "The requested reference period does not overlap the source time range.",
      class = "oceancube_climatology_no_overlap"
    )
  }
  effective <- if (inherits(time, "Date")) {
    as.Date(effective_number, origin = "1970-01-01")
  } else {
    as.POSIXct(effective_number, origin = "1970-01-01", tz = "UTC")
  }
  clipped <- !identical(requested_number, effective_number)
  if (clipped) {
    warning(
      "The requested reference period was clipped to the overlapping source range.",
      call. = FALSE
    )
  }
  source_index <- which(
    as.numeric(time) >= effective_number[[1L]] &
      as.numeric(time) <= effective_number[[2L]]
  )
  list(
    requested = requested,
    effective = effective,
    clipped = clipped,
    source_index = source_index
  )
}

.climatology_replicate_plan <- function(time, effective, by, leap) {
  dates <- if (inherits(time, "POSIXct")) as.Date(time, tz = "UTC") else time
  effective_dates <- if (inherits(effective, "POSIXct")) {
    as.Date(effective, tz = "UTC")
  } else {
    effective
  }
  component_dates <- switch(
    by,
    day = seq.Date(effective_dates[[1L]], effective_dates[[2L]], by = "day"),
    month = {
      endpoints <- .temporal_date_components(effective_dates)
      first <- .temporal_make_date(endpoints$year[[1L]], endpoints$month[[1L]])
      last <- .temporal_make_date(endpoints$year[[2L]], endpoints$month[[2L]])
      seq.Date(first, last, by = "month")
    },
    season = {
      first <- .climatology_season_start(effective_dates[[1L]])
      last <- .climatology_season_start(effective_dates[[2L]])
      seq.Date(first, last, by = "3 months")
    }
  )
  ids <- .climatology_time_ids(component_dates, by = by, leap = leap)
  included <- !is.na(ids$replicate_id)
  included_ids <- unique(ids$replicate_id[included])
  replicates <- lapply(included_ids, function(replicate_id) {
    selected <- which(ids$replicate_id == replicate_id & included)
    start_date <- min(component_dates[selected])
    next_dates <- vapply(
      component_dates[selected],
      function(value) as.numeric(.climatology_next_period(as.Date(value, origin = "1970-01-01"), by)),
      numeric(1L)
    )
    end_date <- as.Date(max(next_dates), origin = "1970-01-01") - 1
    list(
      replicate_id = replicate_id,
      component_id = ids$component_id[selected],
      recurring_group = ids$group_key[selected[[1L]]],
      replicate_year = ids$replicate_year[selected[[1L]]],
      season_year = ids$season_year[selected[[1L]]],
      start_date = start_date,
      end_date = end_date
    )
  })
  group_key <- .climatology_cycle_axis(time, by = by, leap = leap)$group_key
  n_clim_total <- tabulate(
    match(vapply(replicates, `[[`, character(1L), "recurring_group"), group_key),
    nbins = length(group_key)
  )
  names(n_clim_total) <- group_key

  source_ids <- .climatology_time_ids(dates, by = by, leap = leap)
  period_rows <- lapply(replicates, function(replicate) {
    stored <- sum(
      as.numeric(time) >= as.numeric(effective[[1L]]) &
        as.numeric(time) <= as.numeric(effective[[2L]]) &
        source_ids$replicate_id == replicate$replicate_id,
      na.rm = TRUE
    )
    start <- .climatology_date_as_time(replicate$start_date, time)
    end <- .climatology_date_as_period_end(replicate$end_date, time)
    data.frame(
      source_period_start = start,
      source_period_end = end,
      recurring_group = replicate$recurring_group,
      replicate_year = as.integer(replicate$replicate_year),
      season_year = as.integer(replicate$season_year),
      partial = as.numeric(effective[[1L]]) > as.numeric(start) ||
        as.numeric(effective[[2L]]) < as.numeric(end),
      included = TRUE,
      raw_timestamp_count = as.integer(stored),
      stringsAsFactors = FALSE
    )
  })
  if (identical(by, "day") && identical(leap, "drop")) {
    dropped <- which(!included)
    for (index in dropped) {
      start <- .climatology_date_as_time(component_dates[[index]], time)
      end <- .climatology_date_as_period_end(component_dates[[index]], time)
      period_rows[[length(period_rows) + 1L]] <- data.frame(
        source_period_start = start,
        source_period_end = end,
        recurring_group = "02-29",
        replicate_year = ids$replicate_year[[index]],
        season_year = NA_integer_,
        partial = FALSE,
        included = FALSE,
        raw_timestamp_count = as.integer(sum(dates == component_dates[[index]])),
        stringsAsFactors = FALSE
      )
    }
  }
  period_summary <- do.call(rbind, period_rows)
  period_summary <- period_summary[
    order(period_summary$source_period_start, period_summary$included),
    , drop = FALSE
  ]
  rownames(period_summary) <- NULL
  n_clim_total <- as.integer(n_clim_total)
  names(n_clim_total) <- group_key
  list(
    replicates = replicates,
    n_clim_total = n_clim_total,
    period_summary = period_summary,
    eligible_years = sort(unique(period_summary$replicate_year[period_summary$included])),
    eligible_season_years = sort(unique(
      period_summary$season_year[period_summary$included & !is.na(period_summary$season_year)]
    ))
  )
}

.climatology_time_ids <- function(time, by, leap) {
  dates <- if (inherits(time, "POSIXct")) as.Date(time, tz = "UTC") else as.Date(time)
  components <- .temporal_date_components(dates)
  if (identical(by, "day")) {
    raw_key <- format(dates, "%m-%d")
    group_key <- raw_key
    if (identical(leap, "feb28")) group_key[raw_key == "02-29"] <- "02-28"
    replicate_id <- paste(components$year, group_key, sep = "-")
    if (identical(leap, "drop")) replicate_id[raw_key == "02-29"] <- NA_character_
    return(list(
      component_id = as.character(dates),
      replicate_id = replicate_id,
      group_key = group_key,
      replicate_year = components$year,
      season_year = rep(NA_integer_, length(dates))
    ))
  }
  if (identical(by, "month")) {
    group_key <- sprintf("%02d", components$month)
    replicate_id <- sprintf("%04d-%s", components$year, group_key)
    return(list(
      component_id = replicate_id,
      replicate_id = replicate_id,
      group_key = group_key,
      replicate_year = components$year,
      season_year = rep(NA_integer_, length(dates))
    ))
  }
  starts <- .climatology_season_start(dates)
  start_components <- .temporal_date_components(starts)
  group_key <- c(`12` = "DJF", `3` = "MAM", `6` = "JJA", `9` = "SON")[
    as.character(start_components$month)
  ]
  group_key <- unname(group_key)
  season_year <- start_components$year + as.integer(start_components$month == 12L)
  replicate_id <- paste(season_year, group_key, sep = "-")
  list(
    component_id = replicate_id,
    replicate_id = replicate_id,
    group_key = group_key,
    replicate_year = season_year,
    season_year = season_year
  )
}

.climatology_season_start <- function(date) {
  components <- .temporal_date_components(as.Date(date))
  start_month <- c(12L, 12L, 3L, 3L, 3L, 6L, 6L, 6L, 9L, 9L, 9L, 12L)[
    components$month
  ]
  start_year <- components$year - as.integer(components$month %in% c(1L, 2L))
  .temporal_make_date(start_year, start_month)
}

.climatology_next_period <- function(start, by) {
  switch(
    by,
    day = start + 1,
    month = seq.Date(start, by = "month", length.out = 2L)[[2L]],
    season = seq.Date(start, by = "3 months", length.out = 2L)[[2L]]
  )
}

.climatology_date_as_time <- function(date, source_time) {
  if (inherits(source_time, "POSIXct")) as.POSIXct(date, tz = "UTC") else date
}

.climatology_date_as_period_end <- function(date, source_time) {
  if (inherits(source_time, "POSIXct")) {
    as.POSIXct(date + 1, tz = "UTC") - 1
  } else {
    date
  }
}

.climatology_cycle_axis <- function(time, by, leap) {
  if (identical(by, "day")) {
    anchor_year <- if (identical(leap, "keep")) 2000L else 2001L
    dates <- seq.Date(
      as.Date(sprintf("%d-01-01", anchor_year)),
      as.Date(sprintf("%d-12-31", anchor_year)),
      by = "day"
    )
    group_key <- format(dates, "%m-%d")
    anchor <- list(year = anchor_year, policy = leap)
  } else if (identical(by, "month")) {
    dates <- seq.Date(as.Date("2001-01-01"), as.Date("2001-12-01"), by = "month")
    group_key <- sprintf("%02d", 1:12)
    anchor <- list(year = 2001L)
  } else {
    dates <- as.Date(c("2000-12-01", "2001-03-01", "2001-06-01", "2001-09-01"))
    group_key <- c("DJF", "MAM", "JJA", "SON")
    anchor <- list(season_year = 2001L, starts = dates)
  }
  output_time <- if (inherits(time, "POSIXct")) {
    .as_utc_posixct(as.POSIXct(dates, tz = "UTC"))
  } else {
    dates
  }
  list(time = output_time, group_key = group_key, anchor = anchor)
}

.climatology_combine_components <- function(component_means) {
  if (length(component_means) == 1L) return(component_means[[1L]])
  template <- component_means[[1L]]
  total <- array(0, dim = dim(template))
  count <- array(0L, dim = dim(template))
  for (value in component_means) {
    finite <- is.finite(value)
    total[finite] <- total[finite] + value[finite]
    count[finite] <- count[finite] + 1L
  }
  output <- array(NA_real_, dim = dim(template))
  output[count > 0L] <- total[count > 0L] / count[count > 0L]
  output
}

.climatology_welford_update <- function(n, mean, m2, value) {
  finite <- is.finite(value)
  new_n <- n
  new_mean <- mean
  new_m2 <- m2
  new_n[finite] <- n[finite] + 1L
  delta <- value[finite] - mean[finite]
  new_mean[finite] <- mean[finite] + delta / new_n[finite]
  delta_two <- value[finite] - new_mean[finite]
  new_m2[finite] <- m2[finite] + delta * delta_two
  list(n = new_n, mean = new_mean, m2 = new_m2)
}
