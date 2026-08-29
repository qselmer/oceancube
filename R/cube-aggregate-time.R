#' Aggregate an ocean cube along the time dimension
#'
#' `cube_aggregate_time()` groups the canonical time axis into calendar periods
#' and applies one controlled reducer to every longitude, latitude, depth, and
#' variable cell. It changes only the time dimension and always returns an
#' in-memory `<ocean_cube>`.
#'
#' @param x A valid `<ocean_cube>` using the memory or NetCDF backend.
#' @param by A character scalar selecting `"day"`, `"week"`, `"month"`,
#'   `"season"`, or `"year"`.
#' @param method A character scalar selecting `"mean"`, `"sum"`, `"min"`,
#'   `"max"`, or `"median"`. Arbitrary reducer functions are not supported.
#' @param na.rm A non-missing logical scalar. If `TRUE`, only finite values are
#'   reduced. If `FALSE`, any non-finite value makes that cell-period missing.
#' @param min_n A positive integer-like scalar giving the minimum number of
#'   finite values required per cell, depth, variable, and period.
#' @param diagnostics A non-missing logical scalar. If `TRUE`, aligned
#'   `n_valid` and observation-coverage arrays are retained in
#'   `qa$temporal_aggregation`; lightweight period metadata are always retained.
#'
#' @return An in-memory `<ocean_cube>` with unchanged longitude, latitude,
#'   depth, variable, and unit coordinates, and a regularized aggregated time
#'   axis.
#'
#' @details
#' Date input produces Date period starts. POSIXct input produces UTC POSIXct
#' period starts, and all POSIXct boundaries are evaluated in UTC. Weeks use
#' ISO-8601 Monday--Sunday groups: week 1 contains January 4 and the ISO year is
#' the year containing the week's Thursday. Seasons are DJF, MAM, JJA, and SON;
#' DJF 2026 contains December 2025 through February 2026 and is timestamped
#' 2025-12-01. All frequencies use canonical period-start timestamps.
#'
#' Every canonical period between the first and last represented periods is
#' returned. Partial boundary periods remain present, and internal periods with
#' no timestamps contain missing values. A finite value is one for which
#' `is.finite()` is true, so NA, NaN, Inf, and -Inf are invalid. `n_total` is the
#' number of stored timestamps in a period, `n_valid` is cell-specific, and
#' `coverage_fraction = n_valid / n_total` is observation coverage, not duration
#' coverage. Empty periods have zero counts and undefined coverage.
#'
#' All methods use equal observation weighting; duration weighting is not
#' performed. Irregular or gapped input emits a warning because an
#' observation-weighted summary can differ from a duration-weighted temporal
#' summary. `method = "sum"` means a sum of sampled finite values, retains the
#' input unit string, and does not perform integration or unit conversion.
#'
#' Lazy NetCDF cubes are read through bounded indexed backend reads, one period
#' and spatial/depth/variable block at a time; the complete multi-period cube is
#' not materialized. Exact median retains the complete temporal period only for
#' the current bounded block and can therefore require more memory than the
#' other reducers.
#'
#' @examples
#' monthly <- ocean_cube(
#'   lon = -80, lat = -12, depth = 0,
#'   time = as.Date(c("2020-01-01", "2020-01-15", "2020-02-01")),
#'   data = array(c(1, 3, 10), c(1, 1, 1, 3, 1)), vars = "temperature"
#' )
#' cube_aggregate_time(monthly, by = "month")
#'
#' hourly <- ocean_cube(
#'   lon = -80, lat = -12, depth = 0,
#'   time = as.POSIXct(
#'     c("2020-01-01 00:00:00", "2020-01-01 12:00:00"), tz = "UTC"
#'   ),
#'   data = array(c(1, 3), c(1, 1, 1, 2, 1)), vars = "temperature"
#' )
#' cube_aggregate_time(hourly, by = "day")
#'
#' djf <- ocean_cube(
#'   lon = -80, lat = -12, depth = 0,
#'   time = as.Date(c("2025-12-15", "2026-01-15", "2026-02-15")),
#'   data = array(c(3, 6, 9), c(1, 1, 1, 3, 1)), vars = "temperature"
#' )
#' cube_aggregate_time(djf, by = "season", diagnostics = TRUE)
#'
#' @export
#' @seealso [to_month()], [cube_extract()], [cube_collect()], [cube_inspect()]
cube_aggregate_time <- function(
    x,
    by,
    method = c("mean", "sum", "min", "max", "median"),
    na.rm = TRUE,
    min_n = 1L,
    diagnostics = FALSE) {
  cube_validate(x, strict = TRUE)
  .require_ordinary_chronology(x, "cube_aggregate_time")
  .calendar_operation_unsupported(x, "cube_aggregate_time")

  if (missing(by) || !is.character(by) || length(by) != 1L ||
      is.na(by) || !by %in% c("day", "week", "month", "season", "year")) {
    .abort_badarg(
      "by",
      "must be exactly one of `day`, `week`, `month`, `season`, or `year`."
    )
  }
  if (missing(method)) {
    method <- "mean"
  } else if (!is.character(method) || length(method) != 1L ||
             is.na(method) ||
             !method %in% c("mean", "sum", "min", "max", "median")) {
    .abort_badarg(
      "method",
      "must be exactly one of `mean`, `sum`, `min`, `max`, or `median`; custom reducers are not supported."
    )
  }
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    .abort_badarg("na.rm", "must be a single non-missing logical value.")
  }
  if (!is.numeric(min_n) || length(min_n) != 1L || is.na(min_n) ||
      !is.finite(min_n) || min_n < 1 || min_n != floor(min_n)) {
    .abort_badarg("min_n", "must be a finite positive integer-like scalar.")
  }
  min_n <- as.integer(min_n)
  if (!is.logical(diagnostics) || length(diagnostics) != 1L ||
      is.na(diagnostics)) {
    .abort_badarg("diagnostics", "must be a single non-missing logical value.")
  }

  inspection <- cube_inspect(x, missing = "none")
  irregular_sampling <- isFALSE(inspection$time_summary$regular)
  if (irregular_sampling) {
    warning(
      paste(
        "Temporal aggregation uses equal observation weighting; irregular or",
        "gapped sampling can make observation-weighted summaries differ from",
        "duration-weighted temporal summaries."
      ),
      call. = FALSE
    )
  }
  if (identical(method, "sum")) {
    warning(
      paste(
        "`method = \"sum\"` computes a sum of sampled finite values; it does",
        "not perform temporal integration, duration weighting, or automatic",
        "unit conversion."
      ),
      call. = FALSE
    )
  }

  plan <- .temporal_period_plan(x$time, by)
  shape <- unname(.cube_shape(x))
  output_shape <- c(shape[1:3], nrow(plan$periods), shape[5])
  output <- array(NA_real_, dim = output_shape)
  n_valid_output <- if (diagnostics) {
    array(0L, dim = output_shape)
  } else {
    NULL
  }
  coverage_output <- if (diagnostics) {
    array(NA_real_, dim = output_shape)
  } else {
    NULL
  }

  read_metrics <- list(
    backend = .cube_backend(x),
    input_shape = shape,
    output_shape = output_shape,
    logical_values_selected = 0,
    physical_values_read = 0,
    backend_read_count = 0L,
    maximum_logical_block_values = 0,
    full_cube_materialized = FALSE
  )

  for (period_index in seq_len(nrow(plan$periods))) {
    time_index <- which(plan$assignment == period_index)
    if (length(time_index) == 0L) next
    blocks <- .temporal_spatial_blocks(shape, length(time_index))
    for (block in blocks) {
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
      reduced <- .temporal_reduce_block(
        values,
        method = method,
        na.rm = na.rm,
        min_n = min_n
      )

      output[
        block$longitude,
        block$latitude,
        block$depth,
        period_index,
        block$variable
      ] <- reduced$value
      if (diagnostics) {
        n_valid_output[
          block$longitude,
          block$latitude,
          block$depth,
          period_index,
          block$variable
        ] <- reduced$n_valid
        coverage_output[
          block$longitude,
          block$latitude,
          block$depth,
          period_index,
          block$variable
        ] <- reduced$n_valid / length(time_index)
      }

      read_metrics$logical_values_selected <-
        read_metrics$logical_values_selected + logical_values
      read_metrics$physical_values_read <-
        read_metrics$physical_values_read + physical_values
      read_metrics$backend_read_count <- read_metrics$backend_read_count + 1L
      read_metrics$maximum_logical_block_values <- max(
        read_metrics$maximum_logical_block_values,
        logical_values
      )
    }
  }

  output_dimnames <- list(
    lon = as.character(x$lon),
    lat = as.character(x$lat),
    depth = as.character(x$depth),
    time = as.character(plan$periods$time),
    var = x$vars
  )
  dimnames(output) <- output_dimnames
  if (diagnostics) {
    dimnames(n_valid_output) <- output_dimnames
    dimnames(coverage_output) <- output_dimnames
  }

  time_provenance <- .find_time_provenance(x$provenance)
  calendar <- time_provenance$calendar %||% "proleptic_gregorian"
  record <- list(
    operation = "temporal_aggregation",
    by = by,
    method = method,
    na.rm = na.rm,
    min_n = min_n,
    weighting = "equal",
    diagnostics = diagnostics,
    input_time_class = if (inherits(x$time, "Date")) "Date" else "POSIXct",
    output_time_class = if (inherits(plan$periods$time, "Date")) "Date" else "POSIXct",
    timezone = if (inherits(plan$periods$time, "POSIXct")) "UTC" else NA_character_,
    calendar = calendar,
    period_definition = .temporal_period_definition(by),
    period_start_convention = "canonical period start",
    irregular_sampling = irregular_sampling,
    unit_semantics = if (identical(method, "sum")) {
      "sampled_value_sum"
    } else {
      "input_units_retained"
    }
  )
  provenance_context <- .provenance_cube_context(
    source = x$source,
    dataset_id = x$dataset_id,
    time = plan$periods$time,
    shape = stats::setNames(as.integer(output_shape), .cube_axis_names()),
    variables = x$vars,
    backend = "memory",
    provenance = x$provenance
  )
  provenance <- .provenance_append(
    x$provenance,
    operation = "cube_aggregate_time",
    parameters = list(
      requested = list(
        by = by,
        method = method,
        na.rm = na.rm,
        min_n = min_n,
        diagnostics = diagnostics
      ),
      resolved = list(
        weighting = "equal_observation",
        period_definition = record$period_definition,
        period_start_convention = record$period_start_convention,
        irregular_sampling = irregular_sampling,
        unit_semantics = record$unit_semantics,
        input_time_class = record$input_time_class,
        output_time_class = record$output_time_class,
        calendar = calendar,
        output_shape = stats::setNames(as.integer(output_shape), .cube_axis_names())
      )
    ),
    output = .provenance_summary(provenance_context),
    scientific_method = .provenance_method("cube_aggregate_time", record),
    context = provenance_context
  )
  qa_record <- list(
    by = by,
    method = method,
    na.rm = na.rm,
    min_n = min_n,
    weighting = "equal",
    diagnostics = diagnostics,
    irregular_sampling = irregular_sampling,
    periods = plan$periods,
    read_metrics = read_metrics
  )
  if (diagnostics) {
    qa_record$n_valid <- n_valid_output
    qa_record$coverage_fraction <- coverage_output
  }
  qa <- list(temporal_aggregation = qa_record)
  if (!is.null(x$qa)) qa$parent <- x$qa

  result <- ocean_cube(
    lon = x$lon,
    lat = x$lat,
    depth = x$depth,
    time = plan$periods$time,
    vars = x$vars,
    data = output,
    units = x$units,
    source = x$source,
    dataset_id = x$dataset_id,
    spatial_extent = x$spatial_extent,
    temporal_extent = range(plan$periods$time),
    depth_extent = x$depth_extent,
    mask = x$mask,
    dc = x$dc,
    provenance = provenance,
    qa = qa
  )
  dimnames(result$data) <- output_dimnames
  result <- .attach_cube_metadata(
    result,
    .cf_metadata_for_transform(x$metadata %||% NULL, "cube_aggregate_time")
  )
  .check_cube(result)
  result
}

.temporal_period_plan <- function(time, by) {
  input_posixct <- inherits(time, "POSIXct")
  dates <- if (input_posixct) as.Date(time, tz = "UTC") else time
  components <- .temporal_date_components(dates)

  observed_start <- switch(
    by,
    day = dates,
    week = dates - .temporal_monday_offset(dates),
    month = .temporal_make_date(components$year, components$month),
    season = {
      season_month <- c(12L, 12L, 3L, 3L, 3L, 6L, 6L, 6L, 9L, 9L, 9L, 12L)[components$month]
      season_year <- components$year + as.integer(components$month == 12L)
      start_year <- season_year - as.integer(season_month == 12L)
      .temporal_make_date(start_year, season_month)
    },
    year = .temporal_make_date(components$year, 1L)
  )

  sequence_by <- switch(
    by,
    day = "day",
    week = "7 days",
    month = "month",
    season = "3 months",
    year = "year"
  )
  period_dates <- seq.Date(min(observed_start), max(observed_start), by = sequence_by)
  assignment <- match(as.numeric(observed_start), as.numeric(period_dates))
  n_total <- tabulate(assignment, nbins = length(period_dates))
  output_time <- if (input_posixct) {
    as.POSIXct(period_dates, tz = "UTC")
  } else {
    period_dates
  }
  if (input_posixct) output_time <- .as_utc_posixct(output_time)

  period_components <- .temporal_date_components(period_dates)
  periods <- data.frame(time = output_time, n_total = as.integer(n_total))
  if (identical(by, "day")) {
    periods$date <- period_dates
  } else if (identical(by, "week")) {
    iso <- .temporal_iso_fields(period_dates)
    periods$iso_year <- iso$year
    periods$iso_week <- iso$week
  } else if (identical(by, "month")) {
    periods$year <- period_components$year
    periods$month <- period_components$month
  } else if (identical(by, "season")) {
    periods$season <- c(
      `12` = "DJF", `3` = "MAM", `6` = "JJA", `9` = "SON"
    )[as.character(period_components$month)]
    periods$season <- unname(periods$season)
    periods$season_year <- period_components$year +
      as.integer(period_components$month == 12L)
  } else {
    periods$year <- period_components$year
  }

  list(periods = periods, assignment = assignment)
}

.temporal_date_components <- function(x) {
  value <- as.POSIXlt(as.POSIXct(x, tz = "UTC"), tz = "UTC")
  list(
    year = as.integer(value$year + 1900L),
    month = as.integer(value$mon + 1L),
    day = as.integer(value$mday)
  )
}

.temporal_make_date <- function(year, month, day = 1L) {
  as.Date(sprintf("%04d-%02d-%02d", year, month, day))
}

.temporal_monday_offset <- function(x) {
  as.integer((as.numeric(x) + 3) %% 7)
}

.temporal_iso_fields <- function(monday) {
  thursday <- monday + 3L
  iso_year <- .temporal_date_components(thursday)$year
  january_4 <- .temporal_make_date(iso_year, 1L, 4L)
  week_one_monday <- january_4 - .temporal_monday_offset(january_4)
  iso_week <- as.integer((as.numeric(monday - week_one_monday) %/% 7) + 1L)
  list(year = iso_year, week = iso_week)
}

.temporal_spatial_blocks <- function(shape, n_time, target_values = 250000L) {
  latitude_block <- min(
    shape[2],
    max(1L, as.integer(floor(sqrt(target_values / max(1L, n_time)))))
  )
  longitude_block <- min(
    shape[1],
    max(1L, as.integer(floor(
      target_values / max(1L, n_time * latitude_block)
    )))
  )
  split_axis <- function(size, block) {
    split(seq_len(size), ceiling(seq_len(size) / block))
  }
  longitude <- split_axis(shape[1], longitude_block)
  latitude <- split_axis(shape[2], latitude_block)
  blocks <- vector("list", length(longitude) * length(latitude) * shape[3] * shape[5])
  position <- 0L
  for (variable in seq_len(shape[5])) {
    for (depth in seq_len(shape[3])) {
      for (latitude_index in latitude) {
        for (longitude_index in longitude) {
          position <- position + 1L
          blocks[[position]] <- list(
            longitude = longitude_index,
            latitude = latitude_index,
            depth = depth,
            variable = variable
          )
        }
      }
    }
  }
  blocks
}

.temporal_reduce_block <- function(values, method, na.rm, min_n) {
  cells <- prod(dim(values)[c(1L, 2L, 3L, 5L)])
  time_n <- dim(values)[[4L]]
  matrix_values <- matrix(
    aperm(values, c(1L, 2L, 3L, 5L, 4L)),
    nrow = cells,
    ncol = time_n
  )
  finite <- is.finite(matrix_values)
  n_valid <- rowSums(finite)
  reduce_one <- function(value) {
    selected <- value[is.finite(value)]
    if (length(selected) < min_n || (!na.rm && length(selected) < length(value))) {
      return(NA_real_)
    }
    answer <- switch(
      method,
      mean = mean(selected),
      sum = sum(selected),
      min = min(selected),
      max = max(selected),
      median = stats::median(selected)
    )
    if (length(answer) != 1L || !is.finite(answer)) NA_real_ else as.numeric(answer)
  }
  reduced <- apply(matrix_values, 1L, reduce_one)
  output_dim <- dim(values)[c(1L, 2L, 3L, 5L)]
  list(
    value = array(reduced, dim = output_dim),
    n_valid = array(as.integer(n_valid), dim = output_dim)
  )
}

.temporal_period_definition <- function(by) {
  switch(
    by,
    day = "UTC civil day for POSIXct; civil Date for Date input",
    week = "ISO-8601 Monday-Sunday; ISO year contains the week's Thursday",
    month = "calendar year and month",
    season = "DJF/MAM/JJA/SON; DJF season year is the January/February year",
    year = "calendar year"
  )
}
