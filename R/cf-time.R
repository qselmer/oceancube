.CF_TIME_SCHEMA_VERSION <- "1.0.0"
.CF_TIME_MIN_YEAR <- 1L
.CF_TIME_MAX_YEAR <- 9999L
.CF_TIME_SECOND_TOLERANCE <- 1e-4

.cf_calendar <- function(calendar, arg = "calendar") {
  if (!is.character(calendar) || length(calendar) != 1L || is.na(calendar) ||
      !nzchar(trimws(calendar))) {
    .abort_badarg(arg, "must be one supported CF calendar name.")
  }
  raw <- tolower(trimws(calendar))
  canonical <- switch(
    raw,
    gregorian = "standard",
    noleap = "365_day",
    all_leap = "366_day",
    raw
  )
  supported <- c(
    "standard", "proleptic_gregorian", "julian",
    "365_day", "366_day", "360_day"
  )
  if (!canonical %in% supported) {
    rlang::abort(
      paste0(
        "Calendar `", raw, "` is not supported by the oceancube CF time engine. ",
        "Supported historical calendars are standard/gregorian, ",
        "proleptic_gregorian, julian, 365_day/noleap, ",
        "366_day/all_leap, and 360_day."
      ),
      class = "oceancube_netcdf_schema_error"
    )
  }
  canonical
}

.cf_calendar_family <- function(calendar) {
  switch(
    .cf_calendar(calendar),
    standard = "mixed_julian_gregorian",
    proleptic_gregorian = "proleptic_gregorian",
    julian = "proleptic_julian",
    `365_day` = "idealized_365_day",
    `366_day` = "idealized_366_day",
    `360_day` = "idealized_360_day"
  )
}

.cf_is_leap_year <- function(year, calendar) {
  calendar <- .cf_calendar(calendar)
  switch(
    calendar,
    `360_day` = rep(FALSE, length(year)),
    `365_day` = rep(FALSE, length(year)),
    `366_day` = rep(TRUE, length(year)),
    julian = year %% 4L == 0L,
    proleptic_gregorian = year %% 4L == 0L &
      (year %% 100L != 0L | year %% 400L == 0L),
    standard = {
      julian_side <- year < 1582L
      gregorian_side <- year > 1582L
      out <- logical(length(year))
      out[julian_side] <- year[julian_side] %% 4L == 0L
      out[gregorian_side] <- year[gregorian_side] %% 4L == 0L &
        (year[gregorian_side] %% 100L != 0L | year[gregorian_side] %% 400L == 0L)
      out[!julian_side & !gregorian_side] <- FALSE
      out
    }
  )
}

.cf_days_in_month <- function(year, month, calendar) {
  calendar <- .cf_calendar(calendar)
  if (calendar == "360_day") return(rep.int(30L, length(year)))
  days <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[month]
  leap_february <- month == 2L & .cf_is_leap_year(year, calendar)
  days[leap_february] <- 29L
  days
}

.cf_validate_components <- function(year, month, day, hour, minute, second,
                                    calendar, arg) {
  calendar <- .cf_calendar(calendar)
  if (any(year < .CF_TIME_MIN_YEAR | year > .CF_TIME_MAX_YEAR)) {
    .abort_badarg(
      arg,
      paste0(
        "must stay inside the supported year envelope ",
        .CF_TIME_MIN_YEAR, "-", .CF_TIME_MAX_YEAR, "."
      )
    )
  }
  if (any(month < 1L | month > 12L)) {
    .abort_badarg(arg, "contains a month outside 01-12.")
  }
  maximum <- .cf_days_in_month(year, month, calendar)
  if (any(day < 1L | day > maximum)) {
    .abort_badarg(arg, paste0("contains a date invalid in calendar `", calendar, "`."))
  }
  if (calendar == "standard" &&
      any(year == 1582L & month == 10L & day >= 5L & day <= 14L)) {
    .abort_badarg(
      arg,
      "contains a date in the standard-calendar reform gap 1582-10-05 through 1582-10-14."
    )
  }
  if (any(hour < 0L | hour > 23L) || any(minute < 0L | minute > 59L) ||
      any(!is.finite(second)) || any(second < 0 | second >= 60)) {
    .abort_badarg(arg, "contains an invalid clock time; leap-second calendars are out of scope.")
  }
  invisible(TRUE)
}

.cf_offset_seconds <- function(offset, arg) {
  if (is.na(offset) || identical(offset, "") || identical(offset, "Z")) return(0)
  digits <- gsub(":", "", substring(offset, 2L), fixed = TRUE)
  hours <- suppressWarnings(as.integer(substr(digits, 1L, 2L)))
  minutes <- suppressWarnings(as.integer(substr(digits, 3L, 4L)))
  if (nchar(digits) != 4L || is.na(hours) || is.na(minutes) ||
      hours > 23L || minutes > 59L) {
    .abort_badarg(arg, paste0("contains an invalid UTC offset `", offset, "`."))
  }
  sign <- if (substr(offset, 1L, 1L) == "+") 1 else -1
  sign * (hours * 3600 + minutes * 60)
}

.cf_parse_components <- function(x, calendar, arg = "time") {
  calendar <- .cf_calendar(calendar)
  if (!is.character(x) || anyNA(x) || length(x) == 0L) {
    .abort_badarg(arg, "must contain non-missing calendar date strings.")
  }
  pattern <- paste0(
    "^([0-9]{4})-([0-9]{2})-([0-9]{2})",
    "(?:[T ]([0-9]{2}):([0-9]{2})(?::([0-9]{2}(?:\\.[0-9]+)?))?)?",
    "(?:[ ]*(Z|[+-][0-9]{2}:?[0-9]{2}))?",
    "(?:[ ]*\\[([A-Za-z0-9_]+)\\])?$"
  )
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    groups <- regmatches(x[[i]], regexec(pattern, trimws(x[[i]]), perl = TRUE))[[1L]]
    if (length(groups) == 0L) {
      .abort_badarg(
        arg,
        paste0(
          "cannot parse calendar datetime `", x[[i]],
          "`; use YYYY-MM-DD or YYYY-MM-DDTHH:MM:SS with an optional Z/UTC offset."
        )
      )
    }
    suffix <- groups[[9L]]
    if (nzchar(suffix) && !identical(.cf_calendar(suffix, arg = arg), calendar)) {
      .abort_badarg(arg, paste0("calendar suffix `", suffix, "` is incompatible with `", calendar, "`."))
    }
    values <- c(
      year = as.integer(groups[[2L]]),
      month = as.integer(groups[[3L]]),
      day = as.integer(groups[[4L]]),
      hour = if (nzchar(groups[[5L]])) as.integer(groups[[5L]]) else 0L,
      minute = if (nzchar(groups[[6L]])) as.integer(groups[[6L]]) else 0L,
      second = if (nzchar(groups[[7L]])) as.numeric(groups[[7L]]) else 0
    )
    .cf_validate_components(
      values[["year"]], values[["month"]], values[["day"]],
      values[["hour"]], values[["minute"]], values[["second"]],
      calendar, arg
    )
    offset <- if (nzchar(groups[[8L]])) groups[[8L]] else "Z"
    out[[i]] <- c(as.list(values), list(offset = offset))
  }
  out
}

.cf_gregorian_jdn <- function(year, month, day) {
  a <- floor((14 - month) / 12)
  y <- year + 4800 - a
  m <- month + 12 * a - 3
  day + floor((153 * m + 2) / 5) + 365 * y + floor(y / 4) -
    floor(y / 100) + floor(y / 400) - 32045
}

.cf_julian_jdn <- function(year, month, day) {
  a <- floor((14 - month) / 12)
  y <- year + 4800 - a
  m <- month + 12 * a - 3
  day + floor((153 * m + 2) / 5) + 365 * y + floor(y / 4) - 32083
}

.cf_components_to_key <- function(components, calendar, arg = "time") {
  calendar <- .cf_calendar(calendar)
  year <- vapply(components, `[[`, numeric(1), "year")
  month <- vapply(components, `[[`, numeric(1), "month")
  day <- vapply(components, `[[`, numeric(1), "day")
  hour <- vapply(components, `[[`, numeric(1), "hour")
  minute <- vapply(components, `[[`, numeric(1), "minute")
  second <- vapply(components, `[[`, numeric(1), "second")
  .cf_validate_components(year, month, day, hour, minute, second, calendar, arg)
  ordinal <- switch(
    calendar,
    proleptic_gregorian = .cf_gregorian_jdn(year, month, day),
    julian = .cf_julian_jdn(year, month, day),
    standard = {
      gregorian <- year > 1582L |
        (year == 1582L & (month > 10L | (month == 10L & day >= 15L)))
      ifelse(
        gregorian,
        .cf_gregorian_jdn(year, month, day),
        .cf_julian_jdn(year, month, day)
      )
    },
    `365_day` = {
      starts <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
      (year - 1) * 365 + starts[month] + day - 1
    },
    `366_day` = {
      starts <- c(0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
      (year - 1) * 366 + starts[month] + day - 1
    },
    `360_day` = (year - 1) * 360 + (month - 1) * 30 + day - 1
  )
  offset <- vapply(components, function(value) {
    .cf_offset_seconds(value$offset %||% "Z", arg)
  }, numeric(1))
  ordinal * 86400 + hour * 3600 + minute * 60 + second - offset
}

.cf_inverse_gregorian <- function(jdn) {
  a <- jdn + 32044
  b <- floor((4 * a + 3) / 146097)
  c_value <- a - floor(146097 * b / 4)
  d <- floor((4 * c_value + 3) / 1461)
  e <- c_value - floor(1461 * d / 4)
  m <- floor((5 * e + 2) / 153)
  list(
    year = 100 * b + d - 4800 + floor(m / 10),
    month = m + 3 - 12 * floor(m / 10),
    day = e - floor((153 * m + 2) / 5) + 1
  )
}

.cf_inverse_julian <- function(jdn) {
  c_value <- jdn + 32082
  d <- floor((4 * c_value + 3) / 1461)
  e <- c_value - floor(1461 * d / 4)
  m <- floor((5 * e + 2) / 153)
  list(
    year = d - 4800 + floor(m / 10),
    month = m + 3 - 12 * floor(m / 10),
    day = e - floor((153 * m + 2) / 5) + 1
  )
}

.cf_idealized_date <- function(ordinal, calendar) {
  year_length <- switch(calendar, `365_day` = 365L, `366_day` = 366L, `360_day` = 360L)
  year <- floor(ordinal / year_length) + 1
  day_of_year <- ordinal %% year_length
  if (calendar == "360_day") {
    return(list(
      year = year,
      month = floor(day_of_year / 30) + 1,
      day = day_of_year %% 30 + 1
    ))
  }
  starts <- if (calendar == "365_day") {
    c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
  } else {
    c(0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  }
  month <- findInterval(day_of_year, starts, rightmost.closed = TRUE)
  list(year = year, month = month, day = day_of_year - starts[month] + 1)
}

.cf_components_from_key <- function(key, calendar, arg = "time") {
  calendar <- .cf_calendar(calendar)
  if (!is.numeric(key) || anyNA(key) || any(!is.finite(key))) {
    .abort_badarg(arg, "contains missing or non-finite calendar keys.")
  }
  ordinal <- floor(key / 86400)
  clock <- key - ordinal * 86400
  near_day <- clock >= 86400 - .CF_TIME_SECOND_TOLERANCE
  if (any(near_day)) {
    ordinal[near_day] <- ordinal[near_day] + 1
    clock[near_day] <- 0
  }
  date <- switch(
    calendar,
    proleptic_gregorian = .cf_inverse_gregorian(ordinal),
    julian = .cf_inverse_julian(ordinal),
    standard = {
      cutoff <- .cf_julian_jdn(1582, 10, 4)
      julian <- ordinal <= cutoff
      gregorian_values <- .cf_inverse_gregorian(ordinal)
      julian_values <- .cf_inverse_julian(ordinal)
      list(
        year = ifelse(julian, julian_values$year, gregorian_values$year),
        month = ifelse(julian, julian_values$month, gregorian_values$month),
        day = ifelse(julian, julian_values$day, gregorian_values$day)
      )
    },
    `365_day` = .cf_idealized_date(ordinal, calendar),
    `366_day` = .cf_idealized_date(ordinal, calendar),
    `360_day` = .cf_idealized_date(ordinal, calendar)
  )
  hour <- floor(clock / 3600)
  clock <- clock - hour * 3600
  minute <- floor(clock / 60)
  second <- clock - minute * 60
  .cf_validate_components(
    date$year, date$month, date$day, hour, minute, second,
    calendar, arg
  )
  data.frame(
    year = as.integer(date$year), month = as.integer(date$month),
    day = as.integer(date$day), hour = as.integer(hour),
    minute = as.integer(minute), second = as.numeric(second),
    stringsAsFactors = FALSE
  )
}

.cf_time_key <- function(x) {
  value <- unclass(x)
  attributes(value) <- NULL
  as.numeric(value)
}

.new_cf_time <- function(key, calendar, calendar_raw = calendar,
                         source_unit = NULL, source_units = NULL,
                         source_origin = NULL, origin_descriptor = NULL) {
  calendar <- .cf_calendar(calendar)
  .cf_components_from_key(key, calendar, arg = "CF time coordinate")
  structure(
    as.numeric(key),
    class = "oceancube_cf_time",
    schema_name = "oceancube_cf_time",
    schema_version = .CF_TIME_SCHEMA_VERSION,
    calendar = calendar,
    calendar_raw = calendar_raw,
    calendar_family = .cf_calendar_family(calendar),
    chronology_kind = "historical",
    precision = "double_seconds",
    source_unit = source_unit,
    source_units = source_units,
    source_origin = source_origin,
    origin_descriptor = origin_descriptor
  )
}

.cf_time_restore <- function(key, template) {
  attributes(key) <- attributes(template)
  key
}

.cf_time_compatible <- function(x, y) {
  inherits(x, "oceancube_cf_time") && inherits(y, "oceancube_cf_time") &&
    identical(attr(x, "schema_version", exact = TRUE), attr(y, "schema_version", exact = TRUE)) &&
    identical(attr(x, "calendar", exact = TRUE), attr(y, "calendar", exact = TRUE)) &&
    identical(attr(x, "chronology_kind", exact = TRUE), attr(y, "chronology_kind", exact = TRUE))
}

.cf_time_parse <- function(x, template, arg = "time") {
  calendar <- attr(template, "calendar", exact = TRUE)
  components <- .cf_parse_components(x, calendar, arg = arg)
  key <- .cf_components_to_key(components, calendar, arg = arg)
  .new_cf_time(
    key,
    calendar = calendar,
    calendar_raw = attr(template, "calendar_raw", exact = TRUE),
    source_unit = attr(template, "source_unit", exact = TRUE),
    source_units = attr(template, "source_units", exact = TRUE),
    source_origin = attr(template, "source_origin", exact = TRUE),
    origin_descriptor = attr(template, "origin_descriptor", exact = TRUE)
  )
}

.cf_time_format_seconds <- function(x) {
  x <- round(x, 6L)
  integer <- abs(x - round(x)) < 1e-6
  out <- character(length(x))
  out[integer] <- sprintf("%02d", as.integer(round(x[integer])))
  out[!integer] <- sub("0+$", "", sprintf("%09.6f", x[!integer]))
  out
}

format.oceancube_cf_time <- function(x, ...) {
  calendar <- attr(x, "calendar", exact = TRUE)
  values <- .cf_components_from_key(.cf_time_key(x), calendar)
  paste0(
    sprintf("%04d-%02d-%02dT%02d:%02d:", values$year, values$month,
            values$day, values$hour, values$minute),
    .cf_time_format_seconds(values$second), "Z [", calendar, "]"
  )
}

as.character.oceancube_cf_time <- function(x, ...) format(x, ...)

print.oceancube_cf_time <- function(x, ...) {
  cat("<oceancube_cf_time[", length(x), "] calendar=",
      attr(x, "calendar", exact = TRUE), ">\n", sep = "")
  if (length(x) > 0L) print(format(x, ...), quote = FALSE)
  invisible(x)
}

`[.oceancube_cf_time` <- function(x, i, ...) {
  .cf_time_restore(.cf_time_key(x)[i], x)
}

`[[.oceancube_cf_time` <- function(x, i, ...) {
  .cf_time_restore(.cf_time_key(x)[[i]], x)
}

`[<-.oceancube_cf_time` <- function(x, i, value) {
  if (!.cf_time_compatible(x, value)) {
    .abort_badarg("time", "replacement values must use identical calendar semantics.")
  }
  key <- .cf_time_key(x)
  key[i] <- .cf_time_key(value)
  .cf_time_restore(key, x)
}

rep.oceancube_cf_time <- function(x, ...) {
  .cf_time_restore(rep(.cf_time_key(x), ...), x)
}

c.oceancube_cf_time <- function(..., recursive = FALSE) {
  values <- list(...)
  template <- values[[1L]]
  compatible <- vapply(values, function(value) .cf_time_compatible(template, value), logical(1))
  if (!all(compatible)) {
    .abort_badarg("time", "cannot combine calendar-aware values with different calendar semantics.")
  }
  .cf_time_restore(do.call(c, lapply(values, .cf_time_key)), template)
}

Ops.oceancube_cf_time <- function(e1, e2) {
  allowed <- c("==", "!=", "<", "<=", ">", ">=")
  if (!.Generic %in% allowed || missing(e2) || !.cf_time_compatible(e1, e2)) {
    rlang::abort(
      "Calendar-aware time arithmetic is undefined; compare only compatible oceancube_cf_time values or use internal elapsed-second helpers.",
      class = "oceancube_cf_time_operation"
    )
  }
  do.call(.Generic, list(.cf_time_key(e1), .cf_time_key(e2)))
}

Summary.oceancube_cf_time <- function(..., na.rm = FALSE) {
  values <- list(...)
  template <- values[[1L]]
  if (!.Generic %in% c("min", "max", "range") ||
      !all(vapply(values, function(value) .cf_time_compatible(template, value), logical(1)))) {
    rlang::abort(
      "Only min(), max(), and range() are defined for compatible oceancube_cf_time values.",
      class = "oceancube_cf_time_operation"
    )
  }
  keys <- unlist(lapply(values, .cf_time_key), use.names = FALSE)
  .cf_time_restore(do.call(.Generic, list(keys, na.rm = na.rm)), template)
}

xtfrm.oceancube_cf_time <- function(x) .cf_time_key(x)

.time_class <- function(x) {
  if (inherits(x, "oceancube_cf_time")) return("oceancube_cf_time")
  if (inherits(x, "Date")) return("Date")
  if (inherits(x, "POSIXct")) return("POSIXct")
  NA_character_
}

.time_key <- function(x) {
  if (inherits(x, "oceancube_cf_time")) return(.cf_time_key(x))
  if (inherits(x, "Date")) return(as.numeric(x) * 86400)
  if (inherits(x, "POSIXct")) return(as.numeric(x))
  .abort_badarg("time", "must use Date, POSIXct, or oceancube_cf_time semantics.")
}

.time_format <- function(x) {
  if (inherits(x, "oceancube_cf_time")) return(format(x))
  format(x, trim = TRUE, usetz = TRUE)
}

.time_subset <- function(x, i) x[i]

.time_compatible <- function(x, y) {
  if (inherits(x, "oceancube_cf_time") || inherits(y, "oceancube_cf_time")) {
    return(.cf_time_compatible(x, y))
  }
  identical(.time_class(x), .time_class(y))
}

.time_distance_seconds <- function(x, y) {
  if (!.time_compatible(x, y)) {
    .abort_badarg("time", "cannot measure distance between incompatible temporal representations.")
  }
  abs(.time_key(x) - .time_key(y))
}

.time_parse_selector <- function(selector, axis, arg = "time") {
  if (inherits(axis, "oceancube_cf_time")) {
    if (is.character(selector)) return(.cf_time_parse(selector, axis, arg = arg))
    if (!.cf_time_compatible(selector, axis)) {
      .abort_badarg(
        arg,
        paste0(
          "must be compatible oceancube_cf_time values or calendar-valid character dates for `",
          attr(axis, "calendar", exact = TRUE), "`."
        )
      )
    }
    return(selector)
  }
  selector
}

.calendar_operation_unsupported <- function(x, operation) {
  if (inherits(x$time, "oceancube_cf_time")) {
    rlang::abort(
      paste0(
        "`", operation, "()` is not implemented for calendar-aware `",
        attr(x$time, "calendar", exact = TRUE),
        "` time. Select, crop, collect, extract, or transect the cube without ",
        "Gregorian temporal aggregation, or explicitly convert the calendar outside oceancube."
      ),
      class = c("oceancube_cf_time_unsupported_operation", "oceancube_unsupported_calendar")
    )
  }
  invisible(x)
}
