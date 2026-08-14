#' Aggregate an ocean cube to monthly resolution
#'
#' `to_month()` is the compatibility wrapper for monthly temporal aggregation.
#' The built-in `mean`, `sum`, `min`, `max`, and `median` functions delegate to
#' [cube_aggregate_time()]. Arbitrary functions remain temporarily available
#' through the deprecated 0.1.0 materializing path.
#'
#' @param x An `<ocean_cube>` object.
#' @param fun Aggregation function. Defaults to `mean`. The five supported
#'   built-ins delegate to [cube_aggregate_time()]; other functions are
#'   deprecated and may materialize the complete input cube.
#'
#' @return An `<ocean_cube>` aggregated to one value per month.
#'
#' @details For compatibility, `to_month()` returns first-day Date timestamps
#' even for POSIXct input and warns when that legacy demotion occurs. Use
#' [cube_aggregate_time()] to preserve POSIXct input as UTC POSIXct output.
#' Custom `fun` values use the legacy full-read implementation and emit a
#' deprecation warning.
#' @export
#' @seealso [cube_aggregate_time()]
to_month <- function(x, fun = mean) {
  .check_cube(x)

  method <- .to_month_builtin_method(fun)
  if (!is.null(method)) {
    aggregated <- withCallingHandlers(
      cube_aggregate_time(
        x,
        by = "month",
        method = method,
        na.rm = TRUE,
        min_n = 1L,
        diagnostics = FALSE
      ),
      warning = function(condition) {
        if (grepl(
          "Temporal aggregation uses equal observation weighting",
          conditionMessage(condition),
          fixed = TRUE
        )) {
          invokeRestart("muffleWarning")
        }
      }
    )
    return(.to_month_compatibility_result(x, aggregated, method))
  }

  warning(
    paste(
      "Arbitrary functions in `to_month()` are deprecated; use",
      "`cube_aggregate_time()` with a supported method. The legacy custom",
      "path materializes the complete cube."
    ),
    call. = FALSE
  )
  .to_month_legacy_custom(x, fun)
}

.to_month_builtin_method <- function(fun) {
  candidates <- list(
    mean = base::mean,
    sum = base::sum,
    min = base::min,
    max = base::max,
    median = stats::median
  )
  matches <- vapply(candidates, identical, logical(1), y = fun)
  if (any(matches)) names(candidates)[which(matches)[[1L]]] else NULL
}

.to_month_compatibility_result <- function(x, aggregated, method) {
  legacy_demotion <- inherits(x$time, "POSIXct")
  output_time <- aggregated$time
  if (legacy_demotion) {
    warning(
      paste(
        "`to_month()` preserves its legacy POSIXct-to-Date output behavior;",
        "use `cube_aggregate_time(x, by = \"month\")` to retain UTC POSIXct."
      ),
      call. = FALSE
    )
    output_time <- as.Date(output_time, tz = "UTC")
  }
  compatibility <- list(
    operation = "to_month_compatibility_wrapper",
    core_method = method,
    core_delegated = TRUE,
    legacy_posixct_date_demotion = legacy_demotion
  )
  provenance <- .make_provenance(
    "to_month",
    args = list(fun = method),
    extra = list(
      parent = x$provenance,
      core = aggregated$provenance,
      compatibility = compatibility
    )
  )
  if (legacy_demotion) {
    provenance$time <- .to_month_legacy_time_provenance(x)
  }
  qa <- aggregated$qa
  qa$to_month <- compatibility

  ocean_cube(
    lon = aggregated$lon,
    lat = aggregated$lat,
    depth = aggregated$depth,
    time = output_time,
    vars = aggregated$vars,
    data = aggregated$data,
    units = aggregated$units,
    source = aggregated$source,
    dataset_id = aggregated$dataset_id,
    spatial_extent = aggregated$spatial_extent,
    temporal_extent = range(output_time),
    depth_extent = aggregated$depth_extent,
    mask = aggregated$mask,
    dc = aggregated$dc,
    provenance = provenance,
    qa = qa
  )
}

.to_month_legacy_custom <- function(x, fun) {
  if (!is.function(fun)) {
    .abort_badarg("fun", "must be a function.")
  }
  legacy_demotion <- inherits(x$time, "POSIXct")
  if (legacy_demotion) {
    warning(
      paste(
        "`to_month()` preserves its legacy POSIXct-to-Date output behavior;",
        "use `cube_aggregate_time(x, by = \"month\")` to retain UTC POSIXct."
      ),
      call. = FALSE
    )
  }

  date <- if (legacy_demotion) as.Date(x$time, tz = "UTC") else x$time
  ym <- format(date, "%Y-%m")
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
    provenance = {
      provenance <- .make_provenance(
        "to_month",
        args = list(fun = "custom"),
        extra = list(
          parent = x$provenance,
          compatibility = list(
            operation = "to_month_legacy_custom",
            core_delegated = FALSE,
            full_cube_materialized = TRUE,
            legacy_posixct_date_demotion = legacy_demotion,
            deprecated = TRUE
          )
        )
      )
      if (legacy_demotion) {
        provenance$time <- .to_month_legacy_time_provenance(x)
      }
      provenance
    },
    qa = list(
      parent = x$qa,
      to_month = list(
        path = "legacy_custom",
        full_cube_materialized = TRUE,
        deprecated = TRUE
      )
    )
  )
}

.to_month_legacy_time_provenance <- function(x) {
  source <- .find_time_provenance(x$provenance)
  list(
    canonical_class = "Date",
    canonical_timezone = NA_character_,
    source_class = "POSIXct",
    source_timezone = .time_timezone(x$time),
    source_offset = unique(format(x$time, "%z", tz = "UTC")),
    calendar = source$calendar %||% "proleptic_gregorian",
    source_calendar = source$calendar %||% "proleptic_gregorian",
    calendar_defaulted = source$calendar_defaulted %||% FALSE,
    decoder = "oceancube::to_month",
    decode_status = "decoded",
    normalization = "legacy monthly POSIXct period starts demoted to Date"
  )
}
