.PROVENANCE_SCHEMA_VERSION <- "1.0.0"
.PROVENANCE_TOP_LEVEL_FIELDS <- c(
  "schema_version", "source", "time", "history", "lineages", "extensions"
)
.PROVENANCE_TIME_KINDS <- c(
  "historical", "recurring_climatology", "trend_anchor", "static"
)

.provenance_abort <- function(message, class) {
  rlang::abort(
    message,
    class = c(class, "oceancube_provenance_error")
  )
}

.provenance_scalar_character <- function(x, allow_null = FALSE) {
  if (isTRUE(allow_null) && is.null(x)) return(TRUE)
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

.provenance_version_major <- function(x) {
  if (!.provenance_scalar_character(x) ||
      !grepl("^[0-9]+\\.[0-9]+\\.[0-9]+(?:\\.[0-9]+)?$", x)) {
    return(NA_integer_)
  }
  as.integer(strsplit(x, ".", fixed = TRUE)[[1L]][[1L]])
}

.provenance_secret_paths <- function(x, path = "provenance") {
  found <- character()
  if (is.list(x)) {
    item_names <- names(x)
    if (!is.null(item_names)) {
      secret_name <- grepl(
        "(^|[._-])(token|password|secret|credential|api_key|authorization|bearer)([._-]|$)",
        item_names,
        ignore.case = TRUE,
        perl = TRUE
      )
      if (any(secret_name)) {
        found <- c(found, paste0(path, "$", item_names[secret_name]))
      }
    }
    for (i in seq_along(x)) {
      label <- if (!is.null(item_names) && nzchar(item_names[[i]])) {
        paste0(path, "$", item_names[[i]])
      } else {
        paste0(path, "[[", i, "]]")
      }
      found <- c(found, .provenance_secret_paths(x[[i]], label))
    }
  }
  unique(found)
}

.provenance_url_has_auth <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(FALSE)
  grepl(
    paste0(
      "([?&](access_token|token|sig|signature|x-amz-signature|authorization)=)",
      "|(^|[?&])authorization($|[=&])",
      "|://[^/@:]+:[^/@]*@"
    ),
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
}

.provenance_value_problems <- function(x, path = "provenance") {
  if (is.null(x)) return(character())
  if (is.environment(x) || is.function(x) || inherits(x, "connection") ||
      inherits(x, "formula") || typeof(x) %in% c(
        "externalptr", "language", "symbol", "expression", "promise", "weakref"
      )) {
    return(paste0("unsafe value at ", path))
  }
  if (inherits(x, "POSIXct")) {
    zone <- .time_timezone(x)
    problems <- character()
    if (!identical(zone, "UTC")) {
      problems <- c(problems, paste0("non-UTC POSIXct at ", path))
    }
    if (any(is.infinite(as.numeric(x))) || any(is.nan(as.numeric(x)))) {
      problems <- c(problems, paste0("non-finite POSIXct at ", path))
    }
    return(problems)
  }
  if (inherits(x, "Date")) {
    if (any(is.infinite(as.numeric(x))) || any(is.nan(as.numeric(x)))) {
      return(paste0("non-finite Date at ", path))
    }
    return(character())
  }
  if (is.data.frame(x)) {
    if (!identical(class(x), "data.frame")) {
      return(paste0("non-ordinary data.frame at ", path))
    }
    problems <- character()
    for (name in names(x)) {
      problems <- c(
        problems,
        .provenance_value_problems(x[[name]], paste0(path, "$", name))
      )
    }
    return(problems)
  }
  if (is.list(x)) {
    if (is.object(x)) return(paste0("non-plain list at ", path))
    problems <- character()
    item_names <- names(x)
    for (i in seq_along(x)) {
      label <- if (!is.null(item_names) && nzchar(item_names[[i]])) {
        paste0(path, "$", item_names[[i]])
      } else {
        paste0(path, "[[", i, "]]")
      }
      problems <- c(problems, .provenance_value_problems(x[[i]], label))
    }
    return(problems)
  }
  if (is.logical(x) || is.integer(x) || is.character(x)) return(character())
  if (is.double(x)) {
    if (any(is.infinite(x)) || any(is.nan(x))) {
      return(paste0("non-finite double at ", path))
    }
    return(character())
  }
  paste0("unsupported value type at ", path)
}

.provenance_security_problems <- function(x) {
  paths <- .provenance_secret_paths(x)
  if (length(paths) == 0L) return(character())
  paste0("secret-named field at ", paths)
}

.provenance_empty <- function(context = NULL) {
  identity <- list()
  if (is.list(context)) {
    if (.provenance_scalar_character(context$source, allow_null = TRUE) &&
        !is.null(context$source)) {
      identity$label <- context$source
    }
    if (.provenance_scalar_character(context$dataset_id, allow_null = TRUE) &&
        !is.null(context$dataset_id)) {
      identity$dataset_id <- context$dataset_id
    }
  }
  list(
    schema_version = .PROVENANCE_SCHEMA_VERSION,
    source = list(identity = identity, locator = NULL, metadata = NULL),
    time = list(source = NULL, current = .provenance_context_time(context)),
    history = list(),
    lineages = list(),
    extensions = list()
  )
}

.provenance_context_time <- function(context) {
  if (!is.list(context) || is.null(context$time)) return(NULL)
  time <- context$time
  if (!inherits(time, c("Date", "POSIXct"))) return(NULL)
  kind <- context$time_kind %||% "historical"
  list(
    kind = kind,
    class = if (inherits(time, "Date")) "Date" else "POSIXct",
    timezone = if (inherits(time, "POSIXct")) .time_timezone(time) else NULL,
    calendar = context$calendar %||% "proleptic_gregorian",
    count = as.integer(length(time)),
    start = if (length(time) > 0L) time[[1L]] else NULL,
    end = if (length(time) > 0L) time[[length(time)]] else NULL
  )
}

.provenance_cube_context <- function(source = NULL, dataset_id = NULL, time,
                                     shape, variables, backend,
                                     provenance = NULL) {
  classification <- .provenance_validate(provenance)
  current <- if (identical(classification$kind, "v1")) {
    provenance$time$current
  } else {
    NULL
  }
  source_time <- .provenance_time_source(provenance)
  list(
    source = source,
    dataset_id = dataset_id,
    time = time,
    time_kind = current$kind %||% .provenance_legacy_time_kind(provenance),
    calendar = current$calendar %||% source_time$calendar %||%
      "proleptic_gregorian",
    shape = stats::setNames(as.integer(shape), names(shape)),
    variables = as.character(variables),
    backend = backend
  )
}

.provenance_legacy_time_kind <- function(provenance) {
  if (!is.list(provenance)) return("historical")
  if (is.list(provenance$cube_trend)) return("trend_anchor")
  if (is.list(provenance$signal_noise) || is.list(provenance$cube_anomaly)) {
    return("historical")
  }
  is_climatology_wrapper <-
    .provenance_scalar_character(provenance$function_name, allow_null = TRUE) &&
    !is.null(provenance$function_name) &&
    provenance$function_name %in% c("clim_month", "clim_day")
  if (is.list(provenance$cube_climatology) || is_climatology_wrapper) {
    return("recurring_climatology")
  }
  parent <- provenance$parent %||% provenance$extra$parent
  .provenance_legacy_time_kind(parent)
}

.provenance_refresh_current <- function(provenance, context) {
  if (!identical(.provenance_validate(provenance)$kind, "v1")) {
    return(provenance)
  }
  provenance$time["current"] <- list(.provenance_context_time(context))
  .provenance_validate(provenance, strict = TRUE)
  provenance
}

.provenance_deferred_legacy <- function(provenance) {
  FALSE
}

.provenance_validate_source <- function(source, prefix = "source") {
  problems <- character()
  if (!is.list(source) || is.object(source)) {
    return(paste0(prefix, " must be a plain list"))
  }
  allowed <- c("identity", "locator", "metadata")
  if (!identical(sort(names(source)), sort(allowed))) {
    problems <- c(problems, paste0(prefix, " must contain identity, locator, metadata only"))
  }
  if (!is.list(source$identity) || is.object(source$identity)) {
    problems <- c(problems, paste0(prefix, "$identity must be a plain list"))
  } else {
    allowed_identity <- c(
      "label", "dataset_id", "provider", "product", "version", "doi",
      "fixture_id", "checksum"
    )
    unexpected <- setdiff(names(source$identity), allowed_identity)
    if (length(unexpected) > 0L) {
      problems <- c(problems, paste0(prefix, "$identity has unsupported fields"))
    }
    scalar_fields <- intersect(names(source$identity), setdiff(allowed_identity, "checksum"))
    for (field in scalar_fields) {
      if (!.provenance_scalar_character(source$identity[[field]])) {
        problems <- c(problems, paste0(prefix, "$identity$", field, " must be one string"))
      }
    }
    checksum <- source$identity$checksum
    if (!is.null(checksum)) {
      if (!is.list(checksum) ||
          !identical(sort(names(checksum)), c("algorithm", "value")) ||
          !.provenance_scalar_character(checksum$algorithm) ||
          !.provenance_scalar_character(checksum$value)) {
        problems <- c(problems, paste0(prefix, "$identity$checksum is malformed"))
      }
    }
  }
  locator <- source$locator
  if (!is.null(locator)) {
    allowed_locator <- c("type", "value", "basename", "portable")
    if (!is.list(locator) || is.object(locator) ||
        length(setdiff(names(locator), allowed_locator)) > 0L ||
        !all(c("type", "value", "portable") %in% names(locator))) {
      problems <- c(problems, paste0(prefix, "$locator is malformed"))
    } else {
      if (!.provenance_scalar_character(locator$type) ||
          !.provenance_scalar_character(locator$value) ||
          !is.logical(locator$portable) || length(locator$portable) != 1L ||
          is.na(locator$portable)) {
        problems <- c(problems, paste0(prefix, "$locator fields are malformed"))
      }
      if (!is.null(locator$basename) &&
          !.provenance_scalar_character(locator$basename)) {
        problems <- c(problems, paste0(prefix, "$locator$basename is malformed"))
      }
      if (.provenance_url_has_auth(locator$value)) {
        problems <- c(problems, paste0(prefix, "$locator contains authentication material"))
      }
    }
  }
  if (!is.null(source$metadata) &&
      (!is.list(source$metadata) || is.object(source$metadata))) {
    problems <- c(problems, paste0(prefix, "$metadata must be NULL or a plain list"))
  }
  problems
}

.provenance_validate_time <- function(time, prefix = "time") {
  problems <- character()
  if (!is.list(time) || is.object(time) ||
      !identical(sort(names(time)), c("current", "source"))) {
    return(paste0(prefix, " must contain source and current only"))
  }
  if (!is.null(time$source) &&
      (!is.list(time$source) || is.object(time$source))) {
    problems <- c(problems, paste0(prefix, "$source must be NULL or a plain list"))
  } else if (!is.null(time$source)) {
    allowed_source <- c(
      "source_class", "source_timezone", "source_offset", "calendar",
      "calendar_defaulted", "cf_units", "cf_origin", "decoder",
      "decode_status", "normalization"
    )
    if (length(setdiff(names(time$source), allowed_source)) > 0L) {
      problems <- c(problems, paste0(prefix, "$source has unsupported fields"))
    }
    character_fields <- setdiff(allowed_source, "calendar_defaulted")
    for (field in intersect(names(time$source), character_fields)) {
      if (!.provenance_scalar_character(time$source[[field]])) {
        problems <- c(problems, paste0(prefix, "$source$", field, " is malformed"))
      }
    }
    defaulted <- time$source$calendar_defaulted
    if (!is.null(defaulted) &&
        (!is.logical(defaulted) || length(defaulted) != 1L || is.na(defaulted))) {
      problems <- c(problems, paste0(prefix, "$source$calendar_defaulted is malformed"))
    }
  }
  current <- time$current
  if (!is.null(current)) {
    allowed <- c("kind", "class", "timezone", "calendar", "count", "start", "end")
    if (!is.list(current) || is.object(current) ||
        length(setdiff(names(current), allowed)) > 0L ||
        !"kind" %in% names(current) ||
        !.provenance_scalar_character(current$kind) ||
        !current$kind %in% .PROVENANCE_TIME_KINDS) {
      problems <- c(problems, paste0(prefix, "$current is malformed"))
    } else {
      for (field in intersect(c("class", "timezone", "calendar"), names(current))) {
        if (!is.null(current[[field]]) &&
            !.provenance_scalar_character(current[[field]])) {
          problems <- c(problems, paste0(prefix, "$current$", field, " is malformed"))
        }
      }
      if (!is.null(current$count) &&
          (!is.integer(current$count) || length(current$count) != 1L ||
           is.na(current$count) || current$count < 0L)) {
        problems <- c(problems, paste0(prefix, "$current$count is malformed"))
      }
      if (!is.null(current$class) && !current$class %in% c("Date", "POSIXct")) {
        problems <- c(problems, paste0(prefix, "$current$class is unsupported"))
      }
      for (field in intersect(c("start", "end"), names(current))) {
        value <- current[[field]]
        if (!is.null(value) &&
            (length(value) != 1L ||
             (!inherits(value, "Date") && !inherits(value, "POSIXct")))) {
          problems <- c(problems, paste0(prefix, "$current$", field, " is malformed"))
        } else if (!is.null(value) && !is.null(current$class) &&
                   !inherits(value, current$class)) {
          problems <- c(problems, paste0(prefix, "$current$", field,
                                         " does not match current class"))
        }
      }
    }
  }
  problems
}

.provenance_validate_summary <- function(summary, prefix) {
  if (!is.list(summary) || is.object(summary)) {
    return(paste0(prefix, " must be a plain list"))
  }
  allowed <- c("backend", "shape", "variables", "time_kind")
  problems <- character()
  if (length(setdiff(names(summary), allowed)) > 0L) {
    problems <- c(problems, paste0(prefix, " has unsupported fields"))
  }
  if (!is.null(summary$backend) &&
      !.provenance_scalar_character(summary$backend)) {
    problems <- c(problems, paste0(prefix, "$backend is malformed"))
  }
  if (!is.null(summary$shape) &&
      (!is.integer(summary$shape) || anyNA(summary$shape) ||
       any(summary$shape < 0L))) {
    problems <- c(problems, paste0(prefix, "$shape must be non-negative integer"))
  }
  if (!is.null(summary$variables) &&
      (!is.character(summary$variables) || anyNA(summary$variables))) {
    problems <- c(problems, paste0(prefix, "$variables is malformed"))
  }
  if (!is.null(summary$time_kind) &&
      (!.provenance_scalar_character(summary$time_kind) ||
       !summary$time_kind %in% .PROVENANCE_TIME_KINDS)) {
    problems <- c(problems, paste0(prefix, "$time_kind is malformed"))
  }
  problems
}

.provenance_validate_history <- function(history, lineages, prefix = "history") {
  if (!is.list(history) || is.object(history)) {
    return(paste0(prefix, " must be a plain list"))
  }
  problems <- character()
  ids <- vapply(history, function(record) {
    if (is.list(record) && .provenance_scalar_character(record$id)) record$id else ""
  }, character(1))
  expected_ids <- if (length(history) == 0L) character() else {
    sprintf("op_%03d", seq_along(history))
  }
  if (!identical(ids, expected_ids)) {
    problems <- c(problems, paste0(prefix, " operation IDs must be sequential and deterministic"))
  }
  if (anyDuplicated(ids)) {
    problems <- c(problems, paste0(prefix, " operation IDs must be unique"))
  }
  lineage_names <- names(lineages) %||% character()
  for (i in seq_along(history)) {
    record <- history[[i]]
    record_prefix <- paste0(prefix, "[[", i, "]]")
    required <- c("id", "operation", "parameters", "inputs", "output", "software")
    allowed <- c(required, "scientific_method", "execution")
    if (!is.list(record) || is.object(record)) {
      problems <- c(problems, paste0(record_prefix, " must be a plain list"))
      next
    }
    if (!all(required %in% names(record)) ||
        length(setdiff(names(record), allowed)) > 0L) {
      problems <- c(problems, paste0(record_prefix, " fields are malformed"))
      next
    }
    if (!.provenance_scalar_character(record$operation)) {
      problems <- c(problems, paste0(record_prefix, "$operation is malformed"))
    }
    parameters <- record$parameters
    if (!is.list(parameters) || is.object(parameters) ||
        !identical(sort(names(parameters)), c("requested", "resolved")) ||
        !is.list(parameters$requested) || !is.list(parameters$resolved)) {
      problems <- c(problems, paste0(record_prefix, "$parameters is malformed"))
    }
    if (!is.list(record$inputs) || is.object(record$inputs)) {
      problems <- c(problems, paste0(record_prefix, "$inputs must be a list"))
    } else {
      for (j in seq_along(record$inputs)) {
        input <- record$inputs[[j]]
        input_prefix <- paste0(record_prefix, "$inputs[[", j, "]]")
        required_input <- c("role", "lineage_ref", "entity_ref", "summary")
        if (!is.list(input) || is.object(input) ||
            !identical(sort(names(input)), sort(required_input)) ||
            !.provenance_scalar_character(input$role) ||
            !.provenance_scalar_character(input$lineage_ref) ||
            !.provenance_scalar_character(input$entity_ref)) {
          problems <- c(problems, paste0(input_prefix, " is malformed"))
          next
        }
        problems <- c(
          problems,
          .provenance_validate_summary(input$summary, paste0(input_prefix, "$summary"))
        )
        if (!identical(input$lineage_ref, "primary") &&
            !input$lineage_ref %in% lineage_names) {
          problems <- c(problems, paste0(input_prefix, " has invalid lineage reference"))
        } else if (identical(input$lineage_ref, "primary")) {
          valid_entities <- c("source", paste0(ids[seq_len(max(0L, i - 1L))], ":output"))
          if (!input$entity_ref %in% valid_entities) {
            problems <- c(problems, paste0(input_prefix, " has invalid primary entity reference"))
          }
        } else {
          lineage_history <- lineages[[input$lineage_ref]]$history %||% list()
          lineage_ids <- vapply(lineage_history, `[[`, character(1), "id")
          valid_entities <- c("source", paste0(lineage_ids, ":output"))
          if (!input$entity_ref %in% valid_entities) {
            problems <- c(problems, paste0(input_prefix, " has invalid secondary entity reference"))
          }
        }
      }
    }
    output <- record$output
    if (!is.list(output) || is.object(output) ||
        !"entity_ref" %in% names(output) ||
        length(setdiff(names(output), c(
          "entity_ref", "backend", "shape", "variables", "time_kind"
        ))) > 0L ||
        !identical(output$entity_ref, paste0(record$id, ":output"))) {
      problems <- c(problems, paste0(record_prefix, "$output is malformed"))
    } else {
      problems <- c(
        problems,
        .provenance_validate_summary(
          output[names(output) != "entity_ref"],
          paste0(record_prefix, "$output")
        )
      )
    }
    method <- record$scientific_method
    if (!is.null(method) &&
        (!is.list(method) ||
         !identical(sort(names(method)), c("id", "version")) ||
         !.provenance_scalar_character(method$id) ||
         !grepl("^[A-Za-z][A-Za-z0-9._-]*:[A-Za-z0-9._-]+$", method$id) ||
         !.provenance_scalar_character(method$version))) {
      problems <- c(problems, paste0(record_prefix, "$scientific_method is malformed"))
    }
    software <- record$software
    if (!is.list(software) ||
        !identical(sort(names(software)), c("package", "version")) ||
        !.provenance_scalar_character(software$package) ||
        !.provenance_scalar_character(software$version)) {
      problems <- c(problems, paste0(record_prefix, "$software is malformed"))
    }
    execution <- record$execution
    if (!is.null(execution)) {
      if (!is.list(execution) ||
          length(setdiff(names(execution), "recorded_at")) > 0L) {
        problems <- c(problems, paste0(record_prefix, "$execution is malformed"))
      } else if (!is.null(execution$recorded_at) &&
                 (!inherits(execution$recorded_at, "POSIXct") ||
                  !identical(.time_timezone(execution$recorded_at), "UTC"))) {
        problems <- c(problems, paste0(record_prefix, "$execution$recorded_at must be UTC POSIXct"))
      }
    }
  }
  problems
}

.provenance_validate_v1 <- function(x) {
  problems <- character()
  if (!is.list(x) || is.object(x)) return("V1 provenance must be a plain list")
  if (!all(.PROVENANCE_TOP_LEVEL_FIELDS %in% names(x)) ||
      length(setdiff(names(x), .PROVENANCE_TOP_LEVEL_FIELDS)) > 0L) {
    problems <- c(problems, "V1 top-level fields are malformed")
  }
  if (!.provenance_scalar_character(x$schema_version) ||
      is.na(.provenance_version_major(x$schema_version))) {
    problems <- c(problems, "schema_version is malformed")
  }
  problems <- c(problems, .provenance_validate_source(x$source))
  problems <- c(problems, .provenance_validate_time(x$time))
  if (!is.list(x$lineages) || is.object(x$lineages)) {
    problems <- c(problems, "lineages must be a plain list")
  } else {
    lineage_names <- names(x$lineages) %||% character()
    expected <- if (length(x$lineages) == 0L) character() else {
      sprintf("lineage_%03d", seq_along(x$lineages))
    }
    if (!identical(lineage_names, expected)) {
      problems <- c(problems, "lineage IDs must be sequential and deterministic")
    }
    for (i in seq_along(x$lineages)) {
      lineage <- x$lineages[[i]]
      prefix <- paste0("lineages$", lineage_names[[i]])
      if (!is.list(lineage) || is.object(lineage) ||
          !identical(sort(names(lineage)), c("history", "source", "time"))) {
        problems <- c(problems, paste0(prefix, " must contain source, time, history only"))
        next
      }
      problems <- c(problems, .provenance_validate_source(lineage$source, paste0(prefix, "$source")))
      problems <- c(problems, .provenance_validate_time(lineage$time, paste0(prefix, "$time")))
      problems <- c(
        problems,
        .provenance_validate_history(
          lineage$history,
          x$lineages,
          paste0(prefix, "$history")
        )
      )
    }
  }
  problems <- c(problems, .provenance_validate_history(x$history, x$lineages))
  if (!is.list(x$extensions) || is.object(x$extensions)) {
    problems <- c(problems, "extensions must be a plain list")
  }
  problems <- c(problems, .provenance_value_problems(x))
  problems <- c(problems, .provenance_security_problems(x))
  unique(problems)
}

.provenance_is_legacy <- function(x) {
  if (!is.list(x)) return(FALSE)
  known <- c(
    "parent", "source_identity", "time", "cube_slice", "cube_crop",
    "cube_extract", "cube_collect", "cube_aggregate_time",
    "cube_climatology", "cube_anomaly", "signal_noise", "cube_trend",
    "cube_mask", "netcdf", "netcdf_read", "source_provenance",
    "function_name", "package", "package_version", "arguments", "extra",
    "operation", "backend", "file"
  )
  any(names(x) %in% known)
}

.provenance_validate <- function(x, strict = FALSE) {
  schema <- if (is.list(x)) x$schema_version else NULL
  if (is.null(x)) {
    result <- list(valid = FALSE, kind = "null", schema_version = NULL, problems = character())
  } else if (!is.null(schema)) {
    major <- .provenance_version_major(schema)
    if (is.na(major)) {
      result <- list(
        valid = FALSE, kind = "malformed_v1", schema_version = schema,
        problems = "schema_version is malformed"
      )
    } else if (major != 1L) {
      result <- list(
        valid = FALSE, kind = "future_schema", schema_version = schema,
        problems = paste0("unsupported provenance schema major ", major)
      )
    } else {
      problems <- .provenance_validate_v1(x)
      result <- list(
        valid = length(problems) == 0L,
        kind = if (length(problems) == 0L) "v1" else "malformed_v1",
        schema_version = schema,
        problems = problems
      )
    }
  } else if (.provenance_is_legacy(x)) {
    result <- list(
      valid = FALSE, kind = "legacy", schema_version = NULL, problems = character()
    )
  } else {
    result <- list(
      valid = FALSE, kind = "opaque_user", schema_version = NULL, problems = character()
    )
  }
  if (isTRUE(strict) && !isTRUE(result$valid)) {
    if (identical(result$kind, "future_schema")) {
      .provenance_abort(
        paste0("Unsupported provenance schema `", result$schema_version, "`."),
        "oceancube_provenance_future_schema"
      )
    }
    if (identical(result$kind, "malformed_v1")) {
      unsafe <- any(grepl("unsafe|secret|authentication|non-UTC|non-finite", result$problems))
      lineage <- any(grepl("lineage reference|entity reference", result$problems))
      class <- if (unsafe) {
        "oceancube_provenance_unsafe"
      } else if (lineage) {
        "oceancube_provenance_lineage_error"
      } else {
        "oceancube_provenance_malformed"
      }
      .provenance_abort(
        paste(
          "Malformed provenance V1:",
          paste(result$problems, collapse = "; ")
        ),
        class
      )
    }
  }
  result
}

.provenance_software_version <- function() {
  version <- tryCatch(
    as.character(utils::packageDescription("oceancube", fields = "Version")),
    error = function(error) NA_character_
  )
  if (.provenance_scalar_character(version) &&
      grepl("^[0-9]+\\.[0-9]+\\.[0-9]+(?:\\.[0-9]+)?$", version)) {
    return(version)
  }
  namespace_path <- tryCatch(
    getNamespaceInfo(asNamespace("oceancube"), "path"),
    error = function(error) ""
  )
  description <- file.path(namespace_path, "DESCRIPTION")
  if (nzchar(namespace_path) && file.exists(description)) {
    version <- tryCatch(
      as.character(read.dcf(description, fields = "Version")[[1L]]),
      error = function(error) NA_character_
    )
  }
  if (!.provenance_scalar_character(version) ||
      !grepl("^[0-9]+\\.[0-9]+\\.[0-9]+(?:\\.[0-9]+)?$", version)) {
    .provenance_abort(
      "Could not resolve the loaded oceancube package version.",
      "oceancube_provenance_error"
    )
  }
  version
}

.provenance_compact <- function(x, depth = 0L) {
  if (is.null(x) || depth > 5L) return(NULL)
  if (inherits(x, "POSIXct")) {
    out <- as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC")
    names(out) <- names(x)
    return(out)
  }
  if (inherits(x, "Date")) return(x)
  if (is.atomic(x)) {
    if (length(x) <= 24L) return(x)
    return(list(count = as.integer(length(x))))
  }
  if (!is.list(x) || is.object(x)) return(NULL)
  drop <- c(
    "parent", "source_provenance", "core", "system", "platform", "r_version",
    "resolved_indices", "indices_resolved", "selected_coordinates",
    "matched_coordinates", "distances", "netcdf_read", "backend", "qa",
    "diagnostics", "eligible_years", "eligible_season_years",
    "partial_edge_periods", "source_file", "file", "file_size_bytes",
    "file_modified_utc"
  )
  kept <- setdiff(names(x) %||% character(), drop)
  kept <- kept[!grepl("(_utc$|^date$)", kept)]
  out <- lapply(x[kept], .provenance_compact, depth = depth + 1L)
  out[!vapply(out, is.null, logical(1))]
}

.provenance_time_source <- function(x) {
  if (!is.list(x)) return(NULL)
  candidate <- x$time
  if (is.list(candidate)) {
    allowed <- c(
      "source_class", "source_timezone", "source_offset", "calendar",
      "calendar_defaulted", "cf_units", "cf_origin", "decoder",
      "decode_status", "normalization"
    )
    out <- candidate[intersect(names(candidate), allowed)]
    character_fields <- setdiff(names(out), "calendar_defaulted")
    for (field in character_fields) {
      if (!.provenance_scalar_character(out[[field]])) out[[field]] <- NULL
    }
    if (!is.null(out$calendar_defaulted) &&
        (!is.logical(out$calendar_defaulted) ||
         length(out$calendar_defaulted) != 1L || is.na(out$calendar_defaulted))) {
      out$calendar_defaulted <- NULL
    }
    if (length(out) > 0L) return(out)
  }
  for (value in x) {
    found <- .provenance_time_source(value)
    if (!is.null(found)) return(found)
  }
  NULL
}

.provenance_source_from_legacy <- function(x, context = NULL) {
  out <- .provenance_empty(context)$source
  find_value <- function(value, field) {
    if (!is.list(value)) return(NULL)
    if (.provenance_scalar_character(value[[field]], allow_null = TRUE) &&
        !is.null(value[[field]])) return(value[[field]])
    for (child in value) {
      found <- find_value(child, field)
      if (!is.null(found)) return(found)
    }
    NULL
  }
  label <- find_value(x, "source")
  dataset_id <- find_value(x, "dataset_id")
  if (!is.null(label)) out$identity$label <- label
  if (!is.null(dataset_id)) out$identity$dataset_id <- dataset_id
  find_node <- function(value, field) {
    if (!is.list(value)) return(NULL)
    if (is.list(value[[field]])) return(value[[field]])
    for (child in value) {
      found <- find_node(child, field)
      if (!is.null(found)) return(found)
    }
    NULL
  }
  identity_source <- find_node(x, "source_identity") %||% list()
  for (field in c("provider", "product", "doi", "fixture_id")) {
    value <- context[[field]] %||% identity_source[[field]]
    if (.provenance_scalar_character(value, allow_null = TRUE) && !is.null(value)) {
      out$identity[[field]] <- value
    }
  }
  product_version <- context$product_version %||%
    identity_source$product_version %||% identity_source$version
  if (.provenance_scalar_character(product_version, allow_null = TRUE) &&
      !is.null(product_version)) out$identity$version <- product_version
  checksum <- context$checksum %||% identity_source$checksum
  if (is.list(checksum) &&
      identical(sort(names(checksum)), c("algorithm", "value"))) {
    out$identity$checksum <- checksum
  }
  file <- find_value(x, "file")
  if (!is.null(file) && !.provenance_url_has_auth(file)) {
    base <- basename(file)
    out$locator <- list(
      type = if (grepl("^https?://", file, ignore.case = TRUE)) "url" else "file",
      value = if (grepl("^https?://", file, ignore.case = TRUE)) file else base,
      basename = base,
      portable = grepl("^https?://", file, ignore.case = TRUE)
    )
  }
  out
}

.provenance_summary <- function(context = NULL, record = NULL) {
  out <- list()
  first <- function(...) {
    values <- list(...)
    for (value in values) if (!is.null(value)) return(value)
    NULL
  }
  backend <- first(context$backend, record$backend_to, record$target_backend,
                   record$backend)
  shape <- first(context$shape, record$output_shape, record$shape_selected,
                 record$shape)
  variables <- first(context$variables, record$selected_variables,
                     record$variables)
  time_kind <- first(context$time_kind, record$time_kind)
  if (.provenance_scalar_character(backend, allow_null = TRUE) && !is.null(backend)) {
    out$backend <- backend
  }
  if (!is.null(shape) && is.numeric(shape) && all(is.finite(shape)) &&
      all(shape >= 0) && length(shape) <= 16L) {
    out$shape <- stats::setNames(as.integer(shape), names(shape))
  }
  if (!is.null(variables) && is.character(variables) && !anyNA(variables)) {
    out$variables <- variables
  }
  if (.provenance_scalar_character(time_kind, allow_null = TRUE) &&
      !is.null(time_kind) && time_kind %in% .PROVENANCE_TIME_KINDS) {
    out$time_kind <- time_kind
  }
  out
}

.provenance_current_entity <- function(provenance) {
  if (length(provenance$history) == 0L) return("source")
  paste0(provenance$history[[length(provenance$history)]]$id, ":output")
}

.provenance_method <- function(operation, record) {
  id <- switch(
    operation,
    read_nc = "oceancube:cf_netcdf_ingestion",
    cube_slice = "oceancube:coordinate_selection",
    cube_crop = "oceancube:coordinate_range_crop",
    cube_extract = "oceancube:coordinate_extraction",
    cube_collect = "oceancube:backend_materialization",
    cube_aggregate_time = paste0(
      "oceancube:equal_observation_weighted_",
      gsub("[^A-Za-z0-9._-]", "_", record$method %||% "mean")
    ),
    cube_climatology = "oceancube:two_stage_equal_year_weighting",
    cube_anomaly = if (identical(record$type, "z")) {
      "oceancube:standardized_z"
    } else {
      "oceancube:difference"
    },
    signal_noise = if (isTRUE(record$signed)) {
      "oceancube:standardized_z"
    } else {
      "oceancube:absolute_standardized_z"
    },
    cube_trend = "oceancube:ols_elapsed_time_linear",
    to_month = "oceancube:legacy_custom_month",
    cube_mask = "oceancube:cell_center_polygon_mask",
    cube_transect = "oceancube:haversine_transect",
    cube_polygon_weights = "oceancube:s2_polygon_cell_intersection",
    coast_dist = NULL,
    layer_mean = "oceancube:depth_layer_mean",
    crop_stock = "oceancube:stock_mask",
    NULL
  )
  if (is.null(id)) NULL else list(id = id, version = "1")
}

.provenance_execution <- function(record) {
  if (!is.list(record)) return(NULL)
  candidates <- record[grepl("(^date$|_utc$)", names(record) %||% character())]
  for (value in candidates) {
    if (!.provenance_scalar_character(value)) next
    normalized <- sub("[[:space:]]+(UTC|GMT)$", "", value, ignore.case = TRUE)
    parsed <- suppressWarnings(tryCatch(
      as.POSIXct(
        normalized, tz = "UTC",
        tryFormats = c(
          "%Y-%m-%dT%H:%M:%OSZ",
          "%Y-%m-%d %H:%M:%OS %z",
          "%Y-%m-%d %H:%M:%OS"
        )
      ),
      error = function(error) as.POSIXct(NA, tz = "UTC")
    ))
    if (length(parsed) == 1L && !is.na(parsed)) {
      attr(parsed, "tzone") <- "UTC"
      return(list(recorded_at = parsed))
    }
  }
  NULL
}

.provenance_time_after <- function(provenance, operation, context = NULL) {
  current <- .provenance_context_time(context)
  if (is.null(current)) current <- provenance$time$current
  if (is.null(current)) return(NULL)
  if (identical(operation, "cube_climatology")) current$kind <- "recurring_climatology"
  if (operation %in% c("clim_month", "clim_day")) current$kind <- "recurring_climatology"
  if (identical(operation, "cube_anomaly")) current$kind <- "historical"
  if (identical(operation, "cube_trend")) current$kind <- "trend_anchor"
  current
}

.provenance_append <- function(
    provenance, operation,
    parameters = list(requested = list(), resolved = list()),
    inputs = NULL, output = list(), scientific_method = NULL,
    execution = NULL, context = NULL) {
  if (!.provenance_scalar_character(operation)) {
    .provenance_abort("`operation` must be one non-empty string.",
                      "oceancube_provenance_malformed")
  }
  out <- .provenance_normalize(provenance, context = context, allow_future = FALSE)
  if (!is.list(parameters) || is.object(parameters)) {
    .provenance_abort("`parameters` must be a plain list.",
                      "oceancube_provenance_malformed")
  }
  if (is.null(parameters$requested) && !is.null(parameters$request)) {
    parameters$requested <- parameters$request
  }
  parameters <- list(
    requested = parameters$requested %||% list(),
    resolved = parameters$resolved %||% list()
  )
  if (!is.list(parameters$requested) || !is.list(parameters$resolved)) {
    .provenance_abort("Requested and resolved parameters must be lists.",
                      "oceancube_provenance_malformed")
  }
  if (is.null(inputs)) {
    inputs <- list(list(
      role = "source",
      lineage_ref = "primary",
      entity_ref = .provenance_current_entity(out),
      summary = .provenance_summary(context)
    ))
  }
  id <- sprintf("op_%03d", length(out$history) + 1L)
  output$entity_ref <- paste0(id, ":output")
  record <- list(
    id = id,
    operation = operation,
    parameters = parameters,
    inputs = inputs,
    output = output,
    scientific_method = scientific_method,
    software = list(package = "oceancube", version = .provenance_software_version()),
    execution = execution
  )
  out$history[[length(out$history) + 1L]] <- record
  out$time["current"] <- list(.provenance_time_after(out, operation, context))
  .provenance_validate(out, strict = TRUE)
  out
}

.provenance_operation_name <- function(name, record = NULL) {
  operation <- record$operation %||% name
  switch(
    operation,
    temporal_aggregation = "cube_aggregate_time",
    trend = "cube_trend",
    anomaly = "cube_anomaly",
    to_month_compatibility_wrapper = "to_month",
    operation
  )
}

.provenance_record_parameters <- function(operation, record) {
  requested_names <- switch(
    operation,
    read_nc = c("vars"),
    cube_slice = c("by", "match", "requested", "tolerance"),
    cube_crop = c("bbox_requested", "ranges_requested", "outside"),
    cube_extract = c("by", "match", "format", "selectors_requested", "keep_index", "keep_distance"),
    cube_aggregate_time = c("by", "method", "na.rm", "min_n"),
    cube_climatology = c("by", "requested_period", "leap", "min_n"),
    cube_anomaly = c("type"),
    signal_noise = c("signed"),
    cube_trend = c("method", "period_requested", "output_time_unit", "min_n"),
    to_month = c("fun"),
    names(record)
  )
  requested <- .provenance_compact(record[intersect(names(record), requested_names)])
  resolved <- .provenance_compact(record[setdiff(
    names(record), c(requested_names, "operation", "date", "time")
  )])
  list(requested = requested %||% list(), resolved = resolved %||% list())
}

.provenance_legacy_steps <- function(x) {
  if (!is.list(x)) return(list())
  if (!is.null(x$schema_version)) return(list())
  if (!is.null(x$source_provenance)) {
    base <- .provenance_legacy_steps(x$source_provenance)
  } else if (is.list(x$parent) && !is.null(x$parent$source) &&
             !is.null(x$parent$climatology)) {
    base <- .provenance_legacy_steps(x$parent$source)
  } else if (x$function_name %in% c("clim_month", "clim_day") &&
             is.list(x$extra$core)) {
    base <- .provenance_legacy_steps(x$extra$parent)
    base[[length(base) + 1L]] <- list(
      name = "cube_climatology", record = x$extra$core
    )
  } else if (!is.null(x$extra$core)) {
    base <- .provenance_legacy_steps(x$extra$core)
  } else {
    parent <- x$parent %||% x$extra$parent
    base <- .provenance_legacy_steps(parent)
  }
  step_names <- c(
    "netcdf", "netcdf_read", "cube_slice", "cube_crop", "cube_extract",
    "cube_collect", "cube_aggregate_time", "cube_climatology", "cube_anomaly",
    "signal_noise", "cube_trend", "cube_mask", "cube_transect"
  )
  for (name in intersect(step_names, names(x))) {
    record <- x[[name]]
    if (is.list(record)) base[[length(base) + 1L]] <- list(name = name, record = record)
  }
  if (.provenance_scalar_character(x$function_name, allow_null = TRUE) &&
      !is.null(x$function_name)) {
    record <- c(x$arguments %||% list(), x$extra %||% list())
    if (!is.null(x$date)) record$date <- x$date
    base[[length(base) + 1L]] <- list(name = x$function_name, record = record)
  } else if (.provenance_scalar_character(x$operation, allow_null = TRUE) &&
             !is.null(x$operation) && length(intersect(step_names, names(x))) == 0L) {
    base[[length(base) + 1L]] <- list(name = x$operation, record = x)
  } else if (!is.null(x$backend) && !is.null(x$file) &&
             length(intersect(step_names, names(x))) == 0L) {
    base[[length(base) + 1L]] <- list(name = "read_nc", record = x)
  }
  base
}

.provenance_primary_v1 <- function(x) {
  if (!is.list(x)) return(NULL)
  if (identical(.provenance_validate(x)$kind, "v1")) return(x)
  candidates <- list()
  if (!is.null(x$source_provenance)) {
    candidates <- c(candidates, list(x$source_provenance))
  } else if (is.list(x$parent) && !is.null(x$parent$source) &&
             !is.null(x$parent$climatology)) {
    candidates <- c(candidates, list(x$parent$source))
  } else if (x$function_name %in% c("clim_month", "clim_day") &&
             is.list(x$extra$core)) {
    candidates <- c(candidates, list(x$extra$parent))
  } else if (!is.null(x$extra$core)) {
    candidates <- c(candidates, list(x$extra$core))
  } else {
    candidates <- c(candidates, list(x$parent, x$extra$parent))
  }
  for (candidate in candidates) {
    found <- .provenance_primary_v1(candidate)
    if (!is.null(found)) return(found)
  }
  NULL
}

.provenance_migrate_legacy <- function(x, context = NULL) {
  seed <- .provenance_primary_v1(x)
  out <- seed %||% .provenance_empty(context)
  if (is.null(seed)) {
    out$source <- .provenance_source_from_legacy(x, context)
    out$time["source"] <- list(.provenance_time_source(x))
  }
  steps <- .provenance_legacy_steps(x)
  for (step in steps) {
    operation <- .provenance_operation_name(step$name, step$record)
    if (!.provenance_scalar_character(operation)) next
    if (length(out$history) > 0L &&
        identical(out$history[[length(out$history)]]$operation, operation) &&
        identical(out$history[[length(out$history)]]$parameters,
                  .provenance_record_parameters(operation, step$record))) next
    out <- .provenance_append(
      out,
      operation = operation,
      parameters = .provenance_record_parameters(operation, step$record),
      output = .provenance_summary(context, step$record),
      scientific_method = .provenance_method(operation, step$record),
      execution = .provenance_execution(step$record),
      context = context
    )
  }
  if (is.list(x$parent) && !is.null(x$parent$climatology) &&
      any(vapply(steps, function(step) identical(
        .provenance_operation_name(step$name, step$record), "cube_anomaly"
      ), logical(1)))) {
    secondary <- .provenance_normalize(x$parent$climatology, context = NULL)
    anomaly_index <- which(vapply(out$history, function(record) {
      identical(record$operation, "cube_anomaly")
    }, logical(1)))[1L]
    if (!is.na(anomaly_index)) {
      anomaly <- out$history[[anomaly_index]]
      trailing <- if (anomaly_index < length(out$history)) {
        out$history[(anomaly_index + 1L):length(out$history)]
      } else {
        list()
      }
      out$history <- if (anomaly_index > 1L) {
        out$history[seq_len(anomaly_index - 1L)]
      } else {
        list()
      }
      merged <- .provenance_merge_lineages(out, secondary, roles = "climatology")
      out <- merged$provenance
      inputs <- c(list(list(
        role = "source", lineage_ref = "primary",
        entity_ref = .provenance_current_entity(out),
        summary = .provenance_summary(context)
      )), merged$refs)
      out <- .provenance_append(
        out, "cube_anomaly", anomaly$parameters, inputs = inputs,
        output = anomaly$output[names(anomaly$output) != "entity_ref"],
        scientific_method = anomaly$scientific_method, context = context
      )
      for (record in trailing) {
        out <- .provenance_append(
          out, record$operation, record$parameters,
          output = record$output[names(record$output) != "entity_ref"],
          scientific_method = record$scientific_method,
          execution = record$execution, context = context
        )
      }
    }
  }
  structural <- c(
    "parent", "source_identity", "time", "cube_slice", "cube_crop",
    "cube_extract", "cube_collect", "cube_aggregate_time",
    "cube_climatology", "cube_anomaly", "signal_noise", "cube_trend",
    "cube_mask", "cube_transect", "netcdf", "netcdf_read",
    "source_provenance", "function_name", "package", "package_version",
    "arguments", "extra", "operation", "backend", "file", "date",
    "system", "platform", "r_version"
  )
  legacy_fields <- x[setdiff(names(x), structural)]
  if (!is.null(x$source_identity) && !is.list(x$source_identity)) {
    legacy_fields$source_identity <- x$source_identity
  }
  legacy <- .provenance_compact(legacy_fields)
  if (length(legacy) > 0L) out$extensions$legacy <- legacy
  .provenance_validate(out, strict = TRUE)
  out
}

.provenance_normalize <- function(provenance = NULL, context = NULL,
                                  allow_future = TRUE) {
  classification <- .provenance_validate(provenance)
  if (identical(classification$kind, "v1")) return(provenance)
  if (identical(classification$kind, "future_schema")) {
    if (isTRUE(allow_future)) return(provenance)
    .provenance_validate(provenance, strict = TRUE)
  }
  if (identical(classification$kind, "malformed_v1")) {
    .provenance_validate(provenance, strict = TRUE)
  }
  if (classification$kind %in% c("null")) return(.provenance_empty(context))
  if (identical(classification$kind, "legacy")) {
    return(.provenance_migrate_legacy(provenance, context))
  }
  problems <- c(.provenance_value_problems(provenance),
                .provenance_security_problems(provenance))
  if (length(problems) > 0L) {
    .provenance_abort("Unsafe opaque user provenance cannot be preserved.",
                      "oceancube_provenance_unsafe")
  }
  out <- .provenance_empty(context)
  out$extensions$user <- provenance
  .provenance_validate(out, strict = TRUE)
  out
}

.provenance_lineage_fragment <- function(x) {
  list(source = x$source, time = x$time, history = x$history)
}

.provenance_semantic_record <- function(record) {
  semantic_summary <- function(summary) summary[setdiff(names(summary), "backend")]
  inputs <- lapply(record$inputs, function(input) {
    input$summary <- semantic_summary(input$summary)
    input
  })
  output <- semantic_summary(record$output)
  list(
    id = record$id,
    operation = record$operation,
    parameters = record$parameters,
    inputs = inputs,
    output = output,
    scientific_method = record$scientific_method,
    software = record$software
  )
}

.provenance_semantic <- function(provenance) {
  x <- .provenance_normalize(provenance, allow_future = FALSE)
  list(
    schema_version = x$schema_version,
    source = list(identity = x$source$identity),
    time = x$time,
    history = lapply(x$history, .provenance_semantic_record),
    lineages = lapply(x$lineages, function(lineage) list(
      source = list(identity = lineage$source$identity),
      time = lineage$time,
      history = lapply(lineage$history, .provenance_semantic_record)
    ))
  )
}

.provenance_rewrite_lineage_refs <- function(fragment, mapping) {
  fragment$history <- lapply(fragment$history, function(record) {
    record$inputs <- lapply(record$inputs, function(input) {
      if (!identical(input$lineage_ref, "primary") &&
          input$lineage_ref %in% names(mapping)) {
        input$lineage_ref <- unname(mapping[[input$lineage_ref]])
      }
      input
    })
    record
  })
  fragment
}

.provenance_lineage_graph <- function(fragment, registry) {
  mapping <- character()
  collected <- list()
  visit <- function(value) {
    value$history <- lapply(value$history, function(record) {
      record$inputs <- lapply(record$inputs, function(input) {
        old <- input$lineage_ref
        if (!identical(old, "primary") && old %in% names(registry)) {
          if (!old %in% names(mapping)) {
            canonical <- sprintf("lineage_%03d", length(mapping) + 1L)
            mapping[[old]] <<- canonical
            collected[[canonical]] <<- visit(registry[[old]])
          }
          input$lineage_ref <- unname(mapping[[old]])
        }
        input
      })
      .provenance_semantic_record(record)
    })
    list(
      source = list(identity = value$source$identity),
      time = value$time,
      history = value$history
    )
  }
  root <- visit(fragment)
  list(root = root, lineages = collected)
}

.provenance_merge_lineages <- function(provenance, secondary, roles = NULL) {
  out <- .provenance_normalize(provenance, allow_future = FALSE)
  secondaries <- if (is.list(secondary) && !is.null(secondary$schema_version)) {
    list(secondary)
  } else {
    secondary
  }
  if (!is.list(secondaries) || length(secondaries) == 0L) {
    .provenance_abort("`secondary` must contain at least one provenance object.",
                      "oceancube_provenance_lineage_error")
  }
  roles <- roles %||% names(secondaries)
  if (is.null(roles) || any(!nzchar(roles))) roles <- rep("secondary", length(secondaries))
  if (length(roles) != length(secondaries)) {
    .provenance_abort("`roles` must match the number of secondary lineages.",
                      "oceancube_provenance_lineage_error")
  }
  refs <- list()
  for (i in seq_along(secondaries)) {
    value <- .provenance_normalize(secondaries[[i]], allow_future = FALSE)
    root_fragment <- .provenance_lineage_fragment(value)
    target_graph <- .provenance_lineage_graph(root_fragment, value$lineages)
    existing <- which(vapply(out$lineages, function(candidate) {
      identical(
        .provenance_lineage_graph(candidate, out$lineages),
        target_graph
      )
    }, logical(1)))
    if (length(existing) > 0L) {
      root_id <- names(out$lineages)[existing[[1L]]]
    } else {
      imported <- c(list(root_fragment), value$lineages)
      new_ids <- sprintf(
        "lineage_%03d",
        length(out$lineages) + seq_along(imported)
      )
      mapping <- stats::setNames(new_ids[-1L], names(value$lineages))
      rewritten <- lapply(imported, .provenance_rewrite_lineage_refs,
                          mapping = mapping)
      for (j in seq_along(rewritten)) out$lineages[[new_ids[[j]]]] <- rewritten[[j]]
      root_id <- new_ids[[1L]]
    }
    root <- out$lineages[[root_id]]
    entity <- if (length(root$history) == 0L) "source" else {
      paste0(root$history[[length(root$history)]]$id, ":output")
    }
    summary <- if (length(root$history) == 0L) list() else {
      root$history[[length(root$history)]]$output
    }
    summary$entity_ref <- NULL
    refs[[i]] <- list(
      role = roles[[i]], lineage_ref = root_id, entity_ref = entity,
      summary = summary
    )
  }
  .provenance_validate(out, strict = TRUE)
  list(provenance = out, refs = refs)
}
