# OCEANCUBE 0.3.0-B4 architecture-only prototype.
#
# This file is evidence, not package runtime code. It deliberately keeps the
# canonical state as plain R and reconstructs CFtime only as a transient engine.

b4_calendar_alias <- function(calendar) {
  key <- tolower(trimws(calendar))
  switch(key,
    gregorian = "standard",
    noleap = "365_day",
    all_leap = "366_day",
    key
  )
}

b4_plain_state <- function(definition, calendar, offsets,
                           chronology_kind = "historical",
                           custom = NULL, precision = "source-double") {
  stopifnot(
    is.character(definition), length(definition) == 1L,
    is.character(calendar), length(calendar) == 1L,
    is.numeric(offsets), all(is.finite(offsets)),
    chronology_kind %in% c("historical", "climatological", "perpetual")
  )
  structure(list(
    schema_name = "oceancube_cf_time_state",
    schema_version = "1.0.0-prototype",
    calendar = b4_calendar_alias(calendar),
    calendar_raw = calendar,
    definition_raw = definition,
    numeric_offsets = as.numeric(offsets),
    precision = precision,
    chronology_kind = chronology_kind,
    custom_calendar = custom,
    validity = "prototype-valid"
  ), class = "list")
}

b4_reconstruct_cftime <- function(state) {
  stopifnot(is.list(state), identical(state$schema_name, "oceancube_cf_time_state"))
  if (!requireNamespace("CFtime", quietly = TRUE)) {
    stop("CFtime is required only for this isolated B4 probe.")
  }
  if (!is.null(state$custom_calendar)) {
    stop("CFtime 1.7.3 cannot reconstruct explicitly defined custom calendars.")
  }
  CFtime::CFtime(
    state$definition_raw,
    state$calendar,
    state$numeric_offsets
  )
}

b4_same_chronology <- function(x, y) {
  identical(x$calendar, y$calendar) &&
    identical(x$definition_raw, y$definition_raw) &&
    identical(x$chronology_kind, y$chronology_kind)
}

b4_compare <- function(x, y, op = c("<", "<=", "==", ">=", ">")) {
  op <- match.arg(op)
  if (!b4_same_chronology(x, y)) {
    stop("Calendar-aware comparison requires the same calendar, origin and chronology kind.")
  }
  if (length(x$numeric_offsets) != length(y$numeric_offsets)) {
    stop("Prototype vector comparison requires equal lengths.")
  }
  switch(op,
    `<` = x$numeric_offsets < y$numeric_offsets,
    `<=` = x$numeric_offsets <= y$numeric_offsets,
    `==` = x$numeric_offsets == y$numeric_offsets,
    `>=` = x$numeric_offsets >= y$numeric_offsets,
    `>` = x$numeric_offsets > y$numeric_offsets
  )
}

b4_custom_next_day <- function(year, month, day, month_lengths,
                               leap_year = NULL, leap_month = 2L) {
  stopifnot(length(month_lengths) == 12L, all(month_lengths > 0L))
  lengths <- as.integer(month_lengths)
  is_leap <- !is.null(leap_year) && ((year - leap_year) %% 4L == 0L)
  if (is_leap) lengths[[as.integer(leap_month)]] <- lengths[[as.integer(leap_month)]] + 1L
  day <- day + 1L
  if (day > lengths[[month]]) {
    day <- 1L
    month <- month + 1L
    if (month > 12L) {
      month <- 1L
      year <- year + 1L
    }
  }
  sprintf("%04d-%02d-%02d", year, month, day)
}

b4_run_probe <- function() {
  stopifnot(requireNamespace("CFtime", quietly = TRUE))
  cases <- list(
    `360_day impossible Gregorian date` = b4_plain_state(
      "days since 2001-01-01", "360_day", c(0, 59)
    ),
    `365_day no leap insertion` = b4_plain_state(
      "days since 2000-02-28", "noleap", c(0, 1)
    ),
    `366_day leap day every year` = b4_plain_state(
      "days since 2001-02-28", "all_leap", c(0, 1, 2)
    ),
    `Julian leap rule` = b4_plain_state(
      "days since 1900-02-28", "julian", c(0, 1, 2)
    ),
    `standard reform transition` = b4_plain_state(
      "days since 1582-10-04", "standard", c(0, 1, 2)
    ),
    `negative and fractional offsets` = b4_plain_state(
      "hours since 2001-01-01", "360_day", c(-1, 0, 0.5, 1)
    ),
    `UTC leap second` = b4_plain_state(
      "seconds since 2016-12-31 23:59:58", "utc", 0:4
    ),
    `TAI continuous seconds` = b4_plain_state(
      "seconds since 1972-01-01 00:00:10", "tai", 0:4
    )
  )

  out <- lapply(names(cases), function(label) {
    state <- cases[[label]]
    round_trip <- unserialize(serialize(state, NULL))
    engine <- b4_reconstruct_cftime(round_trip)
    data.frame(
      case = label,
      calendar_raw = state$calendar_raw,
      calendar_canonical = state$calendar,
      offsets = paste(state$numeric_offsets, collapse = ";"),
      timestamps = paste(CFtime::as_timestamp(engine), collapse = ";"),
      plain_round_trip_identical = identical(state, round_trip),
      engine_reconstructed = TRUE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

if (identical(Sys.getenv("OCEANCUBE_RUN_B4_PROBE"), "true")) {
  result <- b4_run_probe()
  print(result, row.names = FALSE)
  stopifnot(
    grepl("2001-02-30", result$timestamps[result$case == "360_day impossible Gregorian date"], fixed = TRUE),
    grepl("2000-03-01", result$timestamps[result$case == "365_day no leap insertion"], fixed = TRUE),
    grepl("2001-02-29", result$timestamps[result$case == "366_day leap day every year"], fixed = TRUE),
    grepl("1582-10-15", result$timestamps[result$case == "standard reform transition"], fixed = TRUE),
    grepl("23:59:60", result$timestamps[result$case == "UTC leap second"], fixed = TRUE),
    all(result$plain_round_trip_identical),
    all(result$engine_reconstructed),
    identical(
      b4_custom_next_day(2001L, 2L, 30L, rep(30L, 12L)),
      "2001-03-01"
    )
  )
  cat("B4_PROTOTYPE: PASS\n")
}
