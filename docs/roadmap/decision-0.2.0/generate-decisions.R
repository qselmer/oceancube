#!/usr/bin/env Rscript

# Reproducible, static decision analysis for oceancube 0.2.0.
# This script reads repository sources and the prior audit, and writes only to
# docs/roadmap/decision-0.2.0/. It never loads or executes package code.

options(stringsAsFactors = FALSE, warn = 1)

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(root, "docs", "roadmap", "decision-0.2.0")
audit_dir <- file.path(root, "docs", "roadmap", "audit-0.2.0")
roadmap_path <- file.path(root, "docs", "roadmap", "oceancube-v0.2.0-scope.Rmd")

stopifnot(
  basename(root) == "oceancube",
  dir.exists(out_dir),
  dir.exists(audit_dir),
  file.exists(roadmap_path)
)

expected_branch <- "dev-0.2.0"
expected_head <- "c8d018195dd40b91e76309fb27ceb48df9bf2fb7"
expected_protected <- "93d2a79b11a6ae7622443ae068e6e2a2709c9324"

git_lines <- function(args) {
  value <- system2("git", args, stdout = TRUE, stderr = TRUE)
  status <- attr(value, "status")
  if (!is.null(status) && status != 0L) {
    stop("git failed: git ", paste(args, collapse = " "), "\n", paste(value, collapse = "\n"))
  }
  enc2utf8(value)
}

stopifnot(
  identical(git_lines(c("branch", "--show-current")), expected_branch),
  identical(git_lines(c("rev-parse", "HEAD")), expected_head),
  identical(git_lines(c("rev-parse", "main")), expected_protected),
  identical(git_lines(c("rev-list", "-n", "1", "v0.1.0")), expected_protected)
)

rel_path <- function(path) {
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  ifelse(startsWith(normalized, prefix), substring(normalized, nchar(prefix) + 1L), normalized)
}

read_text <- function(path) {
  tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
}

write_csv <- function(value, filename) {
  destination <- file.path(out_dir, filename)
  normalized_parent <- normalizePath(dirname(destination), winslash = "/", mustWork = TRUE)
  normalized_output <- normalizePath(out_dir, winslash = "/", mustWork = TRUE)
  if (!identical(normalized_parent, normalized_output)) stop("Refusing write outside decision directory")
  utils::write.csv(value, destination, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

write_utf8 <- function(lines, filename) {
  destination <- file.path(out_dir, filename)
  normalized_parent <- normalizePath(dirname(destination), winslash = "/", mustWork = TRUE)
  normalized_output <- normalizePath(out_dir, winslash = "/", mustWork = TRUE)
  if (!identical(normalized_parent, normalized_output)) stop("Refusing write outside decision directory")
  writeLines(enc2utf8(lines), destination, useBytes = TRUE)
}

collapse_values <- function(value, sep = "; ") {
  value <- unique(value[!is.na(value) & nzchar(value)])
  if (length(value)) paste(value, collapse = sep) else ""
}

escape_regex <- function(value) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", value)
call_pattern <- function(name) paste0("(?<![[:alnum:]_.])", escape_regex(name), "[[:space:]]*\\(")

namespace <- read_text(file.path(root, "NAMESPACE"))
exports <- sort(sub("^export\\(([^)]+)\\)$", "\\1", grep("^export\\(", namespace, value = TRUE)))
s3_entries <- grep("^S3method\\(", namespace, value = TRUE)
s3_methods <- sort(sub("^S3method\\(([^,]+),([^)]+)\\)$", "\\1.\\2", s3_entries))
import_from_entries <- grep("^importFrom\\(", namespace, value = TRUE)
import_entries <- grep("^import\\(", namespace, value = TRUE)
imported_function_count <- if (length(import_from_entries)) {
  sum(vapply(strsplit(sub("^importFrom\\(|\\)$", "", import_from_entries), ","), function(x) max(length(x) - 1L, 0L), integer(1)))
} else 0L

parse_definitions <- function(path) {
  expressions <- parse(path, keep.source = TRUE)
  refs <- attr(expressions, "srcref")
  rows <- list()
  for (i in seq_along(expressions)) {
    expression <- expressions[[i]]
    if (!is.call(expression) || !as.character(expression[[1]]) %in% c("<-", "=")) next
    lhs <- expression[[2]]
    rhs <- expression[[3]]
    if (!is.symbol(lhs) || !is.call(rhs) || !identical(rhs[[1]], as.name("function"))) next
    sr <- if (!is.null(refs) && length(refs) >= i) refs[[i]] else attr(expression, "srcref")
    rows[[length(rows) + 1L]] <- list(
      name = as.character(lhs),
      file = rel_path(path),
      line_start = if (!is.null(sr)) as.integer(sr)[1] else NA_integer_,
      line_end = if (!is.null(sr)) as.integer(sr)[3] else NA_integer_,
      expression = rhs,
      signature = paste(names(formals(eval(rhs, envir = baseenv()))), collapse = ", ")
    )
  }
  rows
}

find_local_definitions <- function(node, owner, source_file, rows = list()) {
  if (!is.call(node)) return(rows)
  assignment_call <- identical(node[[1]], as.name("<-")) || identical(node[[1]], as.name("="))
  if (assignment_call && length(node) >= 3L &&
      is.symbol(node[[2]]) && is.call(node[[3]]) && identical(node[[3]][[1]], as.name("function"))) {
    sr <- attr(node, "srcref")
    rows[[length(rows) + 1L]] <- list(
      name = as.character(node[[2]]),
      owner = owner,
      file = source_file,
      line_start = if (!is.null(sr)) as.integer(sr)[1] else NA_integer_,
      line_end = if (!is.null(sr)) as.integer(sr)[3] else NA_integer_
    )
  }
  for (i in seq_along(node)[-1L]) {
    if (i <= length(node)) rows <- find_local_definitions(node[[i]], owner, source_file, rows)
  }
  rows
}

r_files <- sort(list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE))
definitions <- unlist(lapply(r_files, parse_definitions), recursive = FALSE)
definition_names <- vapply(definitions, function(x) x$name, character(1))
defined_names <- sort(unique(definition_names))
definition_counts <- table(definition_names)
definition_by_name <- setNames(definitions, definition_names)

local_definitions <- unlist(lapply(definitions, function(definition) {
  find_local_definitions(definition$expression[[3]], definition$name, definition$file)
}), recursive = FALSE)

assignment_range <- function(source_file, function_name, owner) {
  parsed <- parse(file.path(root, source_file), keep.source = TRUE)
  data <- getParseData(parsed)
  symbols <- data[data$token == "SYMBOL" & data$text == function_name, , drop = FALSE]
  owner_definition <- definition_by_name[[owner]]
  for (i in seq_len(nrow(symbols))) {
    lhs <- data[data$id == symbols$parent[[i]], , drop = FALSE]
    if (!nrow(lhs)) next
    assignment <- data[data$id == lhs$parent[[1]], , drop = FALSE]
    if (!nrow(assignment)) next
    children <- data[data$parent == assignment$id[[1]], , drop = FALSE]
    if (!any(children$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"))) next
    if (assignment$line1[[1]] < owner_definition$line_start || assignment$line2[[1]] > owner_definition$line_end) next
    return(c(as.integer(assignment$line1[[1]]), as.integer(assignment$line2[[1]])))
  }
  c(NA_integer_, NA_integer_)
}

if (length(local_definitions)) {
  for (i in seq_along(local_definitions)) {
    range <- assignment_range(local_definitions[[i]]$file, local_definitions[[i]]$name, local_definitions[[i]]$owner)
    local_definitions[[i]]$line_start <- range[[1]]
    local_definitions[[i]]$line_end <- range[[2]]
  }
}

static_callees <- lapply(definitions, function(definition) {
  calls <- all.names(definition$expression[[3]], functions = TRUE, unique = TRUE)
  sort(setdiff(intersect(calls, defined_names), definition$name))
})
names(static_callees) <- definition_names

static_callers <- setNames(lapply(defined_names, function(name) {
  sort(names(static_callees)[vapply(static_callees, function(values) name %in% values, logical(1))])
}), defined_names)

test_files <- sort(list.files(file.path(root, "tests"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE))
doc_files <- sort(c(
  list.files(file.path(root, "man"), pattern = "[.]Rd$", full.names = TRUE),
  list.files(file.path(root, "vignettes"), recursive = TRUE, full.names = TRUE),
  list.files(file.path(root, "handbook"), recursive = TRUE, full.names = TRUE),
  list.files(file.path(root, "inst", "examples"), recursive = TRUE, full.names = TRUE),
  file.path(root, c("README.md", "README.Rmd", "NEWS.md")),
  roadmap_path
))
doc_files <- doc_files[file.exists(doc_files) & !file.info(doc_files)$isdir]

reference_files <- unique(c(test_files, doc_files))
reference_text <- setNames(lapply(reference_files, read_text), normalizePath(reference_files, winslash = "/", mustWork = FALSE))

files_with_reference <- function(files, function_name) {
  pattern <- call_pattern(function_name)
  normalized <- normalizePath(files, winslash = "/", mustWork = FALSE)
  hits <- files[vapply(normalized, function(path) any(grepl(pattern, reference_text[[path]], perl = TRUE)), logical(1))]
  sort(rel_path(hits))
}

previous_functions <- read.csv(file.path(audit_dir, "02-function-inventory.csv"), check.names = FALSE)
previous_public <- read.csv(file.path(audit_dir, "03-public-api-current.csv"), check.names = FALSE)
previous_proposed <- read.csv(file.path(audit_dir, "04-public-api-proposed.csv"), check.names = FALSE)
previous_files <- read.csv(file.path(audit_dir, "01-file-inventory.csv"), check.names = FALSE)

top_rows <- lapply(definitions, function(definition) {
  previous <- previous_functions[previous_functions$function_name == definition$name &
                                   previous_functions$source_file == definition$file, , drop = FALSE]
  exported <- definition$name %in% exports
  s3 <- definition$name %in% s3_methods
  classification <- if (exported) "exported-top-level" else if (s3) "s3-method-top-level" else "internal-top-level"
  data.frame(
    function_name = definition$name,
    source_file = definition$file,
    line_start = definition$line_start,
    line_end = definition$line_end,
    top_level = TRUE,
    exported = exported,
    s3_method = s3,
    local_function = FALSE,
    duplicate_definition = unname(definition_counts[[definition$name]]) > 1L,
    verified_source_defined = TRUE,
    previous_audit_classification = if (nrow(previous)) previous$visibility[[1]] else "not-in-previous-audit",
    corrected_classification = classification,
    evidence = paste0(definition$file, ":", definition$line_start, " top-level assignment to function; ",
                      if (exported) "registered by export() in NAMESPACE" else if (s3) "registered by S3method() in NAMESPACE" else "not exported in NAMESPACE"),
    notes = if (unname(definition_counts[[definition$name]]) > 1L) "Duplicate top-level name requires human review." else "",
    check.names = FALSE
  )
})

local_rows <- lapply(local_definitions, function(definition) {
  data.frame(
    function_name = definition$name,
    source_file = definition$file,
    line_start = definition$line_start,
    line_end = definition$line_end,
    top_level = FALSE,
    exported = FALSE,
    s3_method = FALSE,
    local_function = TRUE,
    duplicate_definition = FALSE,
    verified_source_defined = FALSE,
    previous_audit_classification = "not-counted-separately",
    corrected_classification = "local-function-excluded",
    evidence = paste0("Named function assignment nested inside top-level function ", definition$owner, "; excluded from package function count."),
    notes = "Not an oceancube top-level function; anonymous callbacks are not counted.",
    check.names = FALSE
  )
})

source_verification <- do.call(rbind, c(top_rows, local_rows))
source_verification <- source_verification[order(source_verification$source_file, source_verification$line_start, source_verification$function_name), ]
write_csv(source_verification, "01-source-function-verification.csv")

verified_top_count <- length(definitions)
verified_export_count <- sum(definition_names %in% exports)
verified_internal_count <- verified_top_count - verified_export_count
verified_local_count <- length(local_definitions)
verified_s3_count <- sum(definition_names %in% s3_methods)
namespace_function_count <- length(unique(c(exports, s3_methods)))

count_reconciliation <- data.frame(
  metric = c(
    "source-defined-top-level-functions", "exported-functions", "internal-top-level-functions",
    "local-functions", "S3-methods", "namespace-functions", "imported-functions"
  ),
  previous_count = c(182L, 27L, 155L, NA_integer_, 4L, 31L, 0L),
  verified_count = c(verified_top_count, verified_export_count, verified_internal_count,
                     verified_local_count, verified_s3_count, namespace_function_count, imported_function_count),
  difference = c(verified_top_count - 182L, verified_export_count - 27L, verified_internal_count - 155L,
                 NA_integer_, verified_s3_count - 4L, namespace_function_count - 31L, imported_function_count),
  explanation = c(
    "Parsed only top-level name <- function or name = function expressions in R/*.R; imports, tests, examples, anonymous callbacks and nested definitions are excluded.",
    "Every export() entry in NAMESPACE resolves to one verified top-level definition in R/*.R.",
    "Computed as verified top-level definitions minus the 27 exports; this includes four registered but unexported S3 methods.",
    "Named nested function assignments found recursively inside top-level function bodies; reported separately and excluded from the 182.",
    "Four top-level definitions match the four S3method() registrations in NAMESPACE.",
    "Unique callable registrations in NAMESPACE: 27 export() names plus four S3 methods; this is not a count of all namespace objects.",
    paste0("Explicit imported functions from importFrom(): ", imported_function_count, "; import() directives: ", length(import_entries), ". Calls to other packages are not oceancube definitions.")
  ),
  check.names = FALSE
)
write_csv(count_reconciliation, "02-count-reconciliation.csv")

stopifnot(
  verified_top_count == 182L,
  verified_export_count == 27L,
  verified_internal_count == 155L,
  verified_s3_count == 4L,
  setequal(exports, definition_names[definition_names %in% exports])
)

review_exports <- c("annual_index", "coast_dist", "crop_stock", "stock_mask")
rename_map <- c(download_nc = "download_copernicus", layer_mean = "cube_vertical_mean", link_events = "cube_join")
expand_exports <- c("ocean_cube", "read_nc", "to_month")
current_core_exports <- c(
  "anom_diff", "anom_z", "clim_day", "clim_month", "cm_connect", "cm_setup", "cube_collect",
  "cube_crop", "cube_extract", "cube_slice", "cube_transect", "download_nc", "layer_mean",
  "link_events", "ocean_cube", "read_nc", "signal_noise", "to_month"
)

workflow_for <- function(name) {
  if (name %in% c("cm_setup", "cm_connect", "download_nc", "read_nc")) return("Acquire or open ocean data before cube analysis")
  if (name %in% c("ocean_cube", "cube_collect")) return("Construct or materialize the canonical cube")
  if (grepl("crop|slice|mask", name)) return("Select a spatial, temporal, vertical or stock subset")
  if (grepl("extract|transect|link", name)) return("Match cube values to analytical tables or paths")
  if (grepl("clim|anom|signal", name)) return("Compute climatology or anomaly products")
  if (grepl("cell|layer|polygon|coast", name)) return("Derive geometry or spatial metrics")
  if (grepl("month|annual", name)) return("Aggregate or summarize time series")
  "Existing documented public workflow"
}

roadmap_role_for_export <- function(name) {
  proposed <- if (name %in% names(rename_map)) unname(rename_map[[name]]) else name
  row <- previous_proposed[previous_proposed$function_name == proposed, , drop = FALSE]
  if (nrow(row)) paste0(row$module[[1]], " / ", row$priority[[1]])
  else if (name %in% review_exports) "Existing specialized API; outside mandatory core until roadmap approval"
  else "Existing public API"
}

export_decisions <- lapply(exports, function(name) {
  source <- definition_by_name[[name]]
  prior <- previous_public[previous_public$function_name == name, , drop = FALSE]
  decision <- if (name %in% names(rename_map)) "rename-with-alias" else if (name %in% expand_exports) "retain-and-expand" else "retain"
  proposed_name <- if (name %in% names(rename_map)) unname(rename_map[[name]]) else name
  test_hits <- files_with_reference(test_files, name)
  documentation_hits <- files_with_reference(doc_files, name)
  risk <- if (decision == "rename-with-alias") "medium: wrapper parity and downstream name migration" else if (name %in% review_exports) "medium: specialized user workflows may depend on current semantics" else if (decision == "retain-and-expand") "medium: preserve backward-compatible defaults while extending contract" else "low: retain current public contract"
  data.frame(
    current_function = name,
    source_file = source$file,
    current_purpose = if (nrow(prior)) prior$purpose_current[[1]] else "Verified from source and Rd",
    current_input = if (nrow(prior)) prior$input[[1]] else source$signature,
    current_output = if (nrow(prior)) trimws(prior$output[[1]]) else "See current function documentation",
    current_users_workflow = workflow_for(name),
    roadmap_role = roadmap_role_for_export(name),
    decision = decision,
    proposed_name = proposed_name,
    compatibility_alias = decision == "rename-with-alias",
    deprecation_start = if (decision == "rename-with-alias") "0.2.x, after alias adoption evidence" else "not-planned",
    deprecation_removal = if (decision == "rename-with-alias") "human-approval-required; not during initial 0.2.0 development" else "not-planned",
    required_for_core_0.2.0 = name %in% current_core_exports,
    implementation_phase = if (decision == "rename-with-alias") "compatibility aliases before source-file reorganization" else if (name %in% current_core_exports) "core-0.2.0" else "preserve unchanged; reassess post-core",
    test_status = if (length(test_hits)) paste(length(test_hits), "referencing test/helper files") else "no direct static test reference found",
    documentation_status = if (length(documentation_hits)) paste(length(documentation_hits), "referencing documentation files") else "no direct static documentation reference found",
    risk = risk,
    evidence = collapse_values(c(
      paste0(source$file, ":", source$line_start, " verified definition"),
      "NAMESPACE export() registration",
      if (length(test_hits)) paste0("tests: ", collapse_values(test_hits)) else "",
      if (length(documentation_hits)) paste0("documentation: ", collapse_values(head(documentation_hits, 8L))) else ""
    )),
    notes = if (name == "annual_index") "Not equivalent to generic to_year(): it computes threshold-oriented annual indicators." else if (name %in% c("stock_mask", "crop_stock", "coast_dist")) "Retained as a specialized, documented extension; not required to implement the mandatory core workflow." else "",
    check.names = FALSE
  )
})
export_decisions <- do.call(rbind, export_decisions)
write_csv(export_decisions, "03-current-export-decisions.csv")

stopifnot(nrow(export_decisions) == 27L, setequal(export_decisions$current_function, exports))

review_resolution <- lapply(review_exports, function(name) {
  row <- export_decisions[export_decisions$current_function == name, , drop = FALSE]
  test_hits <- files_with_reference(test_files, name)
  doc_hits <- files_with_reference(doc_files, name)
  reason <- switch(name,
    annual_index = "The roadmap linked annual_index to to_year without establishing semantic equivalence.",
    coast_dist = "The roadmap did not place the coastal-distance enrichment workflow in a release tier.",
    crop_stock = "The roadmap did not decide whether the stock-specific crop remains public beside cube_mask.",
    stock_mask = "The roadmap did not decide whether stock-domain mask construction remains public beside generic masking."
  )
  fit <- switch(name,
    annual_index = "Post-core aggregation/indicator extension; distinct from generic temporal aggregation.",
    coast_dist = "Post-core geometry and domain-enrichment extension.",
    crop_stock = "Future stock-domain convenience layer built on stable generic selection/masking.",
    stock_mask = "Future stock-domain mask construction; preserve for compatibility."
  )
  alternative <- switch(name,
    annual_index = "retain-and-expand only after separating generic to_year aggregation from fisheries indicators",
    coast_dist = "internalize only after public usage evidence and a replacement workflow exist",
    crop_stock = "deprecate-later only after cube_mask reaches feature parity and migration docs exist",
    stock_mask = "deprecate-later only after a generic replacement preserves stock semantics"
  )
  data.frame(
    function_name = name,
    reason_for_review = reason,
    current_use = row$current_users_workflow,
    tests = row$test_status,
    documentation = row$documentation_status,
    roadmap_fit = fit,
    dependency_impact = paste0("Static callers: ", collapse_values(static_callers[[name]]), if (!length(static_callers[[name]])) "none" else "", "; public compatibility impact if changed."),
    recommended_decision = "retain",
    alternative_decision = alternative,
    human_approval_required = FALSE,
    evidence = row$evidence,
    notes = "Review is resolved for this phase: retain unchanged. A future breaking alternative would require a new human approval.",
    check.names = FALSE
  )
})
review_resolution <- do.call(rbind, review_resolution)
write_csv(review_resolution, "04-public-review-resolution.csv")

signature_text <- function(name) paste0(name, "(", definition_by_name[[name]]$signature, ")")
deprecation_plan <- do.call(rbind, lapply(names(rename_map), function(old_name) {
  new_name <- unname(rename_map[[old_name]])
  data.frame(
    old_function = old_name,
    new_function = new_name,
    rename_justification = switch(old_name,
      download_nc = "Names the Copernicus provider explicitly and leaves room for other ingestion backends.",
      layer_mean = "Uses the cube_* namespace and states that aggregation is vertical.",
      link_events = "Uses the cube_* namespace and generalizes point/event matching as a join."
    ),
    old_signature = signature_text(old_name),
    new_signature_proposed = paste0(new_name, "(", definition_by_name[[old_name]]$signature, ")"),
    behavior_identical = TRUE,
    wrapper_required = TRUE,
    warning_mechanism = "0.2.0 silent functional alias; begin lifecycle-style deprecation warning only in a later 0.2.x after adoption review",
    first_deprecated_version = "0.2.x, human approval required",
    minimum_alias_duration = "entire 0.2.0 development plus at least one documented 0.2.x release cycle",
    planned_removal_version = "unset; human approval required",
    documentation_required = "Document both names, identical signatures, migration example, lifecycle stage and removal policy.",
    tests_required = "Parity tests for values, classes, attributes, errors and forwarding of every argument; warning-state tests when enabled.",
    breaking_change = FALSE,
    notes = "Do not remove or warn from the alias during initial 0.2.0 development.",
    check.names = FALSE
  )
}))
write_csv(deprecation_plan, "05-deprecation-plan.csv")

core_functions <- c(
  "cm_connect", "cm_setup", "download_copernicus", "read_nc", "cube_collect", "ocean_cube",
  "cube_inspect", "cube_validate", "cube_crop", "cube_slice", "cube_extract", "cube_transect",
  "cube_locate", "cube_join", "to_day", "to_week", "to_month", "to_season", "to_year",
  "clim_day", "clim_week", "clim_month", "clim_season", "anom_diff", "anom_z", "signal_noise",
  "anom_percent", "anom_robust", "cube_diff", "cube_rate", "cube_slope", "cube_vertical_mean",
  "viz.map", "viz.series", "viz.profile", "viz.section", "viz.hovmoller", "viz.save",
  "write_nc", "write_table", "viz.animate_map", "viz.save_gif"
)

future_functions <- c(
  "read_stac", "read_thredds", "read_zarr", "cube_regrid", "cube_resample",
  "cube_gradient", "cube_vertical_gradient", "viz.vector", "write_stac", "write_zarr"
)

module_order <- c(
  "core-object" = 1L, "validation" = 2L, "metadata-cf" = 2L,
  "ingestion-copernicus" = 3L, "ingestion-erddap" = 3L, "ingestion-url" = 3L, "ingestion-netcdf" = 3L,
  "selection" = 4L, "extraction" = 4L, "matching" = 5L, "aggregation" = 6L,
  "climatology" = 7L, "anomaly" = 8L, "change" = 9L, "geometry" = 9L, "summary" = 9L,
  "visualization" = 10L, "export" = 10L, "animation" = 11L
)

mandatory_core <- c(
  "download_copernicus", "read_nc", "cube_inspect", "cube_validate", "cube_crop", "cube_slice",
  "cube_extract", "cube_transect", "viz.map", "viz.series", "viz.profile", "viz.section", "viz.hovmoller",
  "to_day", "to_week", "to_month", "to_season", "to_year", "clim_day", "clim_week", "clim_month",
  "clim_season", "anom_diff", "anom_z", "signal_noise", "anom_percent", "anom_robust", "cube_diff",
  "cube_rate", "cube_slope", "cube_locate", "cube_join", "write_nc", "write_table", "viz.save",
  "viz.save_gif", "viz.animate_map"
)

stopifnot(nrow(previous_proposed) == 114L, all(mandatory_core %in% core_functions), all(core_functions %in% previous_proposed$function_name))

api_release_tiers <- lapply(seq_len(nrow(previous_proposed)), function(i) {
  row <- previous_proposed[i, ]
  name <- row$function_name
  module <- row$module
  tier <- if (name %in% core_functions) "core-0.2.0" else if (name %in% future_functions) "future" else "post-core-0.2.x"
  exact_exists <- name %in% defined_names
  current_equivalent <- row$current_equivalent
  required_e2e <- name %in% mandatory_core || name %in% c("ocean_cube", "cube_collect", "cm_setup", "cm_connect", "cube_vertical_mean")
  complexity <- if (module %in% c("metadata-cf", "ingestion-erddap", "ingestion-url", "ingestion-netcdf", "selection", "visualization", "animation", "export") && !exact_exists) "high" else if (exact_exists) "low" else "medium"
  scientific_risk <- if (module %in% c("climatology", "anomaly", "change", "geometry", "summary", "metadata-cf")) "high" else if (module %in% c("aggregation", "matching", "validation")) "medium" else "low"
  backend_risk <- if (module %in% c("ingestion-copernicus", "ingestion-erddap", "ingestion-url", "ingestion-netcdf", "export", "animation")) "high" else if (module %in% c("selection", "extraction", "visualization")) "medium" else "low"
  reason <- if (tier == "core-0.2.0") {
    if (required_e2e) "Required for the smallest complete ingest-to-analysis-to-output workflow." else "Existing foundation retained to avoid regressions in the core release."
  } else if (tier == "future") {
    "Optional backend or advanced geometry/visualization capability; explicitly excluded as a core dependency."
  } else {
    "Valuable extension that can ship after the end-to-end core contract and validation are stable."
  }
  if (name == "to_year") current_equivalent <- "annual_index is related but not behaviorally equivalent"
  data.frame(
    function_name = name,
    module = module,
    exists_now = exact_exists,
    current_equivalent = current_equivalent,
    release_tier = tier,
    required_for_end_to_end_workflow = required_e2e,
    dependency_order = unname(module_order[[module]]),
    estimated_complexity = complexity,
    scientific_risk = scientific_risk,
    backend_risk = backend_risk,
    priority_reason = reason,
    notes = if (name %in% c("read_stac", "read_zarr", "write_stac", "write_zarr")) "STAC/Zarr must not become a core dependency." else if (name %in% c("cube_regrid", "cube_resample")) "Do not imply UGRID, swath or unstructured-grid support in core 0.2.0." else "",
    check.names = FALSE
  )
})
api_release_tiers <- do.call(rbind, api_release_tiers)
write_csv(api_release_tiers, "06-api-release-tiers.csv")

stopifnot(
  nrow(api_release_tiers) == 114L,
  setequal(api_release_tiers$function_name, previous_proposed$function_name),
  all(mandatory_core %in% api_release_tiers$function_name[api_release_tiers$release_tier == "core-0.2.0"])
)

module_for_internal <- function(file, name) {
  previous <- previous_functions[previous_functions$function_name == name & previous_functions$source_file == file, , drop = FALSE]
  if (nrow(previous) && nzchar(previous$module_current[[1]])) return(previous$module_current[[1]])
  if (grepl("backend|read_nc", file)) return("backend/netcdf")
  if (grepl("ocean_cube", file)) return("core-object")
  if (grepl("crop|slice|mask", file)) return("selection")
  if (grepl("extract|transect|collect", file)) return("extraction")
  if (grepl("clim", file)) return("climatology")
  if (grepl("anom", file)) return("anomaly")
  if (grepl("grid|polygon|coast|stock", file)) return("geometry/masks")
  if (grepl("month|annual", file)) return("aggregation")
  if (grepl("link_events", file)) return("matching")
  if (grepl("cm_setup|download_nc", file)) return("ingestion-copernicus")
  if (grepl("layer_mean", file)) return("summary")
  "internal-utils"
}

internal_definitions <- definitions[!definition_names %in% exports]
internal_decisions <- lapply(internal_definitions, function(definition) {
  name <- definition$name
  callers <- static_callers[[name]]
  calls <- static_callees[[name]]
  test_hits <- files_with_reference(test_files, name)
  doc_hits <- files_with_reference(doc_files, name)
  s3 <- name %in% s3_methods
  duplicate <- unname(definition_counts[[name]]) > 1L
  apparently_unused <- !length(callers) && !length(test_hits) && !length(doc_hits) && !s3
  dynamic_possible <- apparently_unused && (grepl("^\\.", name) || grepl("method|dispatch|callback|reader|writer|backend", name, ignore.case = TRUE))
  decision <- if (duplicate) "merge-candidate" else if (apparently_unused) "review" else "retain-internal"
  module <- module_for_internal(definition$file, name)
  data.frame(
    function_name = name,
    source_file = definition$file,
    called_by = collapse_values(callers),
    calls = collapse_values(calls),
    module = module,
    current_role = if (s3) "registered S3 method" else if (length(callers)) "static helper used by package functions" else if (length(test_hits)) "helper exercised directly by tests" else if (length(doc_hits)) "helper referenced by active documentation" else "no static consumer established",
    decision = decision,
    proposed_file = definition$file,
    duplicate = duplicate,
    apparently_unused = apparently_unused,
    dynamic_use_possible = dynamic_possible,
    tests = collapse_values(test_hits),
    risk = if (s3 || dynamic_possible) "high" else if (apparently_unused) "medium" else "low",
    evidence = collapse_values(c(
      paste0(definition$file, ":", definition$line_start, " verified top-level definition"),
      if (length(callers)) paste0("static callers: ", collapse_values(callers)) else "no static caller",
      if (length(test_hits)) paste0("tests: ", collapse_values(test_hits)) else "no direct static test reference",
      if (length(doc_hits)) paste0("documentation: ", collapse_values(head(doc_hits, 6L))) else "no active documentation reference",
      if (s3) "NAMESPACE S3method registration" else ""
    )),
    notes = if (apparently_unused) "Review before any cleanup: reflection, get(), do.call(), S3 dispatch or external scripts may evade static analysis. No removal is approved." else "Retain behavior; physical movement requires a later no-behavior-change commit.",
    check.names = FALSE
  )
})
internal_decisions <- do.call(rbind, internal_decisions)
write_csv(internal_decisions, "07-internal-function-decisions.csv")

stopifnot(
  nrow(internal_decisions) == 155L,
  !any(internal_decisions$decision == "internalize"),
  !any(internal_decisions$decision == "remove-candidate")
)

all_disk_files <- list.files(root, all.files = TRUE, no.. = TRUE, recursive = TRUE, full.names = TRUE)
all_disk_files <- all_disk_files[file.info(all_disk_files)$isdir %in% FALSE]
all_disk_paths <- sort(rel_path(all_disk_files))
all_disk_paths <- all_disk_paths[!startsWith(all_disk_paths, ".git/")]
all_disk_paths <- all_disk_paths[!startsWith(all_disk_paths, "docs/roadmap/decision-0.2.0/")]

tracked_paths <- sort(git_lines(c("ls-files")))
ignored_paths <- sort(git_lines(c("ls-files", "--others", "-i", "--exclude-standard")))

file_category <- function(path) {
  lower <- tolower(path)
  extension <- tolower(tools::file_ext(path))
  if (grepl("^R/.*[.]R$", path)) return("R source")
  if (grepl("^tests/", path)) return("tests")
  if (grepl("^man/", path)) return("man")
  if (grepl("^vignettes/", path)) return("vignettes")
  if (grepl("^handbook/", path)) return("handbook")
  if (grepl("^inst/examples/", path)) return("inst/examples")
  if (extension %in% c("nc", "nc4", "cdf")) return("NetCDF")
  if (extension %in% c("rds", "rda", "rdata")) return("RDS")
  if (extension %in% c("log", "out")) return("logs")
  if (extension %in% c("png", "jpg", "jpeg", "gif", "svg", "tif", "tiff")) return("figures")
  if (extension %in% c("html", "htm")) return("rendered HTML")
  if (grepl("(^|/)([.]cache|[.]quarto|cache|__pycache__|[.]pytest_cache|[.]Rcheck)(/|$)", path, ignore.case = TRUE)) return("caches")
  if (grepl("(^|/)([.]venv|venv|renv/library|[.]conda|conda-env)(/|$)", path, ignore.case = TRUE)) return("local environments")
  if (grepl("(^|/)(tmp|temp|scratch)(/|[-_.])", path, ignore.case = TRUE) && extension %in% c("R", "r", "ps1", "sh", "py")) return("temporary scripts")
  if (grepl("^artifacts/", path, ignore.case = TRUE)) return("artifacts")
  if (grepl("^docs/", path)) return("docs")
  if (basename(path) %in% c("DESCRIPTION", "NAMESPACE", "LICENSE", "LICENSE.md", "CITATION", "CITATION.cff")) return("package metadata")
  if (basename(path) %in% c(".gitignore", ".Rbuildignore", "_pkgdown.yml", ".lintr")) return("configuration")
  if (grepl("^(README|NEWS)", basename(path))) return("documentation root")
  if (grepl("^(data|inst)/", path)) return("package data/support")
  "other"
}

is_text_candidate <- function(path) {
  tolower(tools::file_ext(path)) %in% c("r", "rmd", "qmd", "md", "txt", "csv", "tsv", "yml", "yaml", "json", "toml", "rd", "html", "css", "js", "log", "out", "gitignore", "rbuildignore")
}

has_local_path <- function(path) {
  full <- file.path(root, path)
  size <- unname(file.info(full)$size)
  if (!is_text_candidate(path) || is.na(size) || size > 2e6) return(FALSE)
  text <- iconv(read_text(full), from = "", to = "UTF-8", sub = "")
  suppressWarnings(any(grepl("[A-Za-z]:[\\\\/]", text, perl = TRUE) | grepl("/(Users|home)/", text, perl = TRUE)))
}

file_decisions <- lapply(all_disk_paths, function(path) {
  category <- file_category(path)
  tracked <- path %in% tracked_paths
  ignored <- path %in% ignored_paths
  lower <- tolower(path)
  required_package <- tracked && (category %in% c("R source", "man", "package metadata", "package data/support") || path %in% c(".Rbuildignore"))
  required_tests <- category == "tests" || grepl("(^|/)(fixtures?|testdata)(/|$)", path, ignore.case = TRUE)
  required_docs <- category %in% c("man", "vignettes", "handbook", "inst/examples", "docs", "documentation root") || (category == "figures" && grepl("^(man/figures|vignettes/|handbook/|docs/)", path))
  generated <- category %in% c("man", "rendered HTML", "logs", "caches") ||
    (category == "documentation root" && path == "README.md") ||
    (category == "artifacts" && ignored) || grepl("(^|/)(doc|Meta)/", path)
  operational <- category %in% c("NetCDF", "RDS") ||
    (tolower(tools::file_ext(path)) %in% c("csv", "tsv", "parquet", "feather") && grepl("^(artifacts|data-raw|output|outputs)/", path, ignore.case = TRUE))
  reproducible <- generated && !operational
  duplicate_group <- if (category == "man") {
    paste0("roxygen-topic:", tools::file_path_sans_ext(basename(path)))
  } else if (path %in% c("README.md", "README.Rmd")) {
    "README-source-render"
  } else if (category %in% c("handbook", "rendered HTML") && grepl("handbook", lower)) {
    paste0("handbook:", tools::file_path_sans_ext(basename(path)))
  } else ""

  decision <- "review"
  destination <- ""
  if (category %in% c("R source", "tests", "man", "vignettes", "package metadata", "configuration", "documentation root", "package data/support")) {
    decision <- "retain"
  } else if (category == "docs" && tracked) {
    decision <- "retain"
  } else if (category == "handbook") {
    if (tolower(tools::file_ext(path)) %in% c("qmd", "rmd", "md")) {
      decision <- "merge-later"
      destination <- "vignettes/ after topic-level parity review; retain handbook until migration is accepted"
    } else decision <- "retain"
  } else if (category == "inst/examples") {
    decision <- "merge-later"
    destination <- "vignettes/ or tests/testthat/ according to whether the example is narrative or contractual"
  } else if (category %in% c("NetCDF", "RDS")) {
    if (required_tests || required_package) decision <- "retain" else {
      decision <- "relocate-later"
      destination <- "approved external data store or minimal test fixture; preserve until checksum and provenance are recorded"
    }
  } else if (category %in% c("caches", "local environments", "temporary scripts") && ignored && !tracked && reproducible) {
    decision <- "remove-candidate"
  } else if (ignored && generated && reproducible && category %in% c("artifacts", "logs", "rendered HTML")) {
    decision <- "ignore"
  } else if (category == "figures" && tracked && required_docs) {
    decision <- "retain"
  } else if (category == "figures" && ignored) {
    decision <- "ignore"
  } else if (category == "artifacts" && operational) {
    decision <- "relocate-later"
    destination <- "approved external artifact/data store"
  } else if (tracked) {
    decision <- "retain"
  }

  deletion_requires_approval <- decision == "remove-candidate"
  evidence <- collapse_values(c(
    if (tracked) "git ls-files: tracked" else "git ls-files: not tracked",
    if (ignored) "matched current ignore rules" else "not in ignored inventory",
    paste0("classified as ", category),
    if (generated) "path/type indicates generated output" else "path/type does not establish generated output",
    if (operational) "data extension/location may contain operational data" else ""
  ))
  data.frame(
    path = path,
    tracked = tracked,
    ignored = ignored,
    category = category,
    required_for_package = required_package,
    required_for_tests = required_tests,
    required_for_documentation = required_docs,
    generated = generated,
    reproducible = reproducible,
    contains_operational_data = operational,
    contains_local_path = has_local_path(path),
    duplicate_group = duplicate_group,
    decision = decision,
    destination = destination,
    deletion_requires_approval = deletion_requires_approval,
    evidence = evidence,
    notes = if (decision == "remove-candidate") "Candidate only: do not delete without explicit human approval and a fresh safety check." else if (decision == "review") "Insufficient evidence for an irreversible classification." else if (decision %in% c("relocate-later", "merge-later")) "No move or merge is authorized by this decision document alone." else "",
    check.names = FALSE
  )
})
file_decisions <- do.call(rbind, file_decisions)
write_csv(file_decisions, "08-file-cleanup-decisions.csv")

stopifnot(
  !any(file_decisions$decision == "remove"),
  all(file_decisions$deletion_requires_approval[file_decisions$decision == "remove-candidate"]),
  all(file_decisions$path != "docs/roadmap/decision-0.2.0")
)

functions_by_file <- split(definition_names, vapply(definitions, function(x) x$file, character(1)))
file_modules <- split(previous_functions$module_current, previous_functions$source_file)

module_migration <- lapply(sort(names(functions_by_file)), function(current_file) {
  current_functions <- sort(functions_by_file[[current_file]])
  modules <- sort(unique(file_modules[[current_file]]))
  modules <- modules[!is.na(modules) & nzchar(modules)]
  action <- "retain-file"
  proposed_file <- current_file
  phase <- "preserve through core stabilization"
  if (current_file == "R/backend-netcdf.R") {
    action <- "split-later"
    proposed_file <- "R/backend-netcdf-{metadata,index,read,selection}.R"
    phase <- "after alias introduction and baseline tests"
  } else if (current_file == "R/utils-internal.R") {
    action <- "split-later"
    proposed_file <- "R/internal-{assertions,coordinates,time,units}.R"
    phase <- "after public validation contract is stable"
  } else if (current_file == "R/download_nc.R") {
    action <- "rename-later"
    proposed_file <- "R/download-copernicus.R"
    phase <- "after download_copernicus alias parity tests"
  } else if (current_file == "R/layer_mean.R") {
    action <- "rename-later"
    proposed_file <- "R/cube-vertical-mean.R"
    phase <- "after cube_vertical_mean alias parity tests"
  } else if (current_file == "R/link_events.R") {
    action <- "rename-later"
    proposed_file <- "R/cube-join.R"
    phase <- "after cube_join alias parity tests"
  }
  relevant_tests <- sort(unique(unlist(lapply(current_functions, function(name) files_with_reference(test_files, name)))))
  relevant_docs <- sort(unique(unlist(lapply(current_functions, function(name) files_with_reference(doc_files, name)))))
  data.frame(
    current_file = current_file,
    current_functions = collapse_values(current_functions),
    proposed_file = proposed_file,
    proposed_module = collapse_values(modules),
    action = action,
    phase = phase,
    tests_affected = collapse_values(relevant_tests),
    documentation_affected = collapse_values(head(relevant_docs, 20L)),
    risk = if (action == "retain-file") "low" else if (action == "rename-later") "medium: source layout and roxygen topic links" else "high: broad internal dependency surface",
    notes = if (action == "retain-file") "Keep current physical layout in the cleanup phase." else "Plan only; move no file until behavior-preserving tests and rollback boundary are approved.",
    check.names = FALSE
  )
})
module_migration <- do.call(rbind, module_migration)
write_csv(module_migration, "10-module-migration-plan.csv")

documentation_policy <- c(
  "---",
  "title: \"oceancube 0.2.0 documentation policy\"",
  "output: html_document",
  "---",
  "",
  "# Decision",
  "",
  "oceancube will use a layered documentation model with one declared source of truth per purpose. Generated products are retained when required for package distribution or publication, but they are never edited as the canonical source.",
  "",
  "## Function reference",
  "",
  "Roxygen blocks beside R source are the canonical reference for public and internal function contracts. They own arguments, values, lifecycle state, examples and cross-references. The man/ directory is generated output from roxygen and must not be edited manually. A future roxygen regeneration requires a dedicated commit and a clean generated diff.",
  "",
  "## Complete workflows",
  "",
  "Vignettes are canonical for end-to-end, executable workflows. Each core workflow should start from a supported ingestion path, validate an ocean_cube, perform one scientific operation and produce or save an output. Expensive or network-dependent steps must use cached fixtures or non-evaluated acquisition chunks paired with an executable local equivalent.",
  "",
  "## README and published site",
  "",
  "README is the minimal entry point: installation, one short local workflow, links to lifecycle and vignettes, and no second copy of the complete reference manual. README.Rmd is canonical when present and README.md is its reviewed generated result. Pkgdown is the published site assembled from roxygen reference, vignettes and selected articles; pkgdown output is generated and should not become an independent source.",
  "",
  "## Roadmap and decisions",
  "",
  "docs/roadmap contains scope, audit and approved decision records. These documents explain why changes are authorized; they do not define runtime behavior and must not duplicate full function reference pages.",
  "",
  "## Handbook decision",
  "",
  "Decision: merge the handbook gradually into vignettes while retaining the handbook source until topic-level parity is demonstrated and approved. The handbook is not deleted, archived or treated as the permanent independent source during 0.2.0. Each migrated topic needs an owner, destination vignette, link audit and executable example check. Rendered handbook output is reproducible publication material, not canonical prose.",
  "",
  "## inst/examples decision",
  "",
  "Executable examples in inst/examples remain available during the transition. Narrative examples migrate to vignettes; compact behavior contracts migrate to tests; user-facing minimal examples may remain in inst/examples when runtime data and execution cost are controlled. No example is removed until its destination is executable and linked.",
  "",
  "## Duplicate documentation",
  "",
  "When the same material appears in roxygen, man, README, handbook, vignette and rendered HTML, select the canonical source by purpose above, replace secondary copies with concise links, and preserve unique scientific explanation before merging. Automated similarity is evidence for review, not approval to delete.",
  "",
  "## Executable examples",
  "",
  "Core examples must be deterministic, offline-capable, bounded in runtime and based on redistributable fixtures. Network credentials, local absolute paths and operational datasets are prohibited. Acquisition examples should support dry-run or mocked metadata paths. Every public core function needs at least one tested example or direct test covering its documented contract.",
  "",
  "## Generated HTML",
  "",
  "Generated HTML from vignettes, handbook or pkgdown is ignored or published from a clean build location. Track it only when the selected hosting mechanism requires committed output and that exception is documented. Never repair generated HTML by hand; regenerate it from its canonical source in a dedicated documentation commit.",
  "",
  "## Change control",
  "",
  "Documentation consolidation occurs before functional API expansion. Every migration records source, destination, links, examples and rollback. Deletion always requires a separate human approval after parity has been verified."
)
write_utf8(documentation_policy, "09-documentation-policy.Rmd")

cleanup_plan <- c(
  "---",
  "title: \"oceancube 0.2.0 cleanup execution plan\"",
  "output: html_document",
  "---",
  "",
  "# Guardrails",
  "",
  "Execute one approved commit at a time on dev-0.2.0. Begin each commit from a clean tree, keep main and v0.1.0 unchanged, run the stated checks, inspect the complete diff and stop on scope drift. A rollback means reverting only that commit after preserving diagnostic evidence.",
  "",
  "## Commit 1 — align ignore rules",
  "",
  "- Objective: update .gitignore and .Rbuildignore from approved rows in 08-file-cleanup-decisions.csv.",
  "- Files affected: .gitignore and .Rbuildignore only.",
  "- Allowed behavior: ignore reproducible local artifacts and exclude approved non-package inputs from source builds.",
  "- Prohibited behavior: deleting files, broad wildcards that hide source/tests/docs, or changing package runtime.",
  "- Required checks: git check-ignore samples; git ls-files verification; R CMD build inclusion inspection.",
  "- Rollback criterion: any tracked source, fixture, vignette or required metadata becomes unintentionally ignored/excluded.",
  "- Proposed commit: chore: align oceancube artifact ignore rules",
  "",
  "## Commit 2 — relocate approved artifacts",
  "",
  "- Objective: relocate only rows later approved as relocate-later, preserving provenance and checksums.",
  "- Files affected: an explicit human-approved path list plus manifest and ignore rules.",
  "- Allowed behavior: reversible moves with unchanged content and documented destination.",
  "- Prohibited behavior: deleting operational data, bulk-moving all prior review files, or touching R code.",
  "- Required checks: before/after checksums, git name-status review, fixture path tests and package build inventory.",
  "- Rollback criterion: checksum mismatch, broken fixture/document link, or any unapproved path in the diff.",
  "- Proposed commit: chore: relocate approved oceancube artifacts",
  "",
  "## Commit 3 — consolidate documentation",
  "",
  "- Objective: migrate one approved handbook or example topic at a time to its canonical destination.",
  "- Files affected: the selected handbook/inst example, destination vignette, links and documentation tests.",
  "- Allowed behavior: preserve scientific meaning while replacing duplicate prose with links.",
  "- Prohibited behavior: deleting the handbook, editing generated man pages directly, or changing function behavior.",
  "- Required checks: render the affected source, execute bounded examples, verify links and inspect visual output.",
  "- Rollback criterion: lost unique content, broken links, failed example or unreviewable generated churn.",
  "- Proposed commit: docs: consolidate one oceancube workflow",
  "",
  "## Commit 4 — introduce compatibility aliases",
  "",
  "- Objective: add download_copernicus, cube_vertical_mean and cube_join as behavior-identical public entry points while retaining old names.",
  "- Files affected: dedicated wrapper source, NAMESPACE through roxygen, tests and lifecycle documentation.",
  "- Allowed behavior: exact argument forwarding and identical values, classes, attributes and errors.",
  "- Prohibited behavior: warning from old names in initial 0.2.0, signature drift, alias removal or unrelated refactor.",
  "- Required checks: wrapper parity tests, current test suite, examples and package checks.",
  "- Rollback criterion: any observable result differs through the alias or existing user code warns/fails.",
  "- Proposed commit: feat: add 0.2.0 compatibility aliases",
  "",
  "## Commit 5 — reorganize R files without behavior change",
  "",
  "- Objective: perform one approved split or rename from 10-module-migration-plan.csv.",
  "- Files affected: one current R file, its proposed destination files and roxygen/test references.",
  "- Allowed behavior: move unchanged definitions with stable load order and exports.",
  "- Prohibited behavior: modifying signatures, algorithms, messages, defaults or exported names.",
  "- Required checks: normalized definition inventory before/after, full tests, package load/check and documentation diff.",
  "- Rollback criterion: inventory mismatch, load-order failure, changed output or documentation loss.",
  "- Proposed commit: refactor: modularize one oceancube source unit",
  "",
  "## Commit 6 — certify the reorganized baseline",
  "",
  "- Objective: run and record the complete baseline verification after approved structural changes.",
  "- Files affected: test/certification evidence only unless a separately scoped defect fix is approved.",
  "- Allowed behavior: execute tests, package build/check and static inventories.",
  "- Prohibited behavior: bundling fixes with certification or rewriting snapshots without review.",
  "- Required checks: testthat suite, R CMD build/check, exported API comparison and clean generated diff.",
  "- Rollback criterion: any regression without a separately approved corrective commit.",
  "- Proposed commit: test: certify oceancube 0.2.0 cleanup baseline",
  "",
  "## Commit 7 — expose public validation",
  "",
  "- Objective: implement cube_validate and cube_inspect as the first new public core contract.",
  "- Files affected: validation source, roxygen/NAMESPACE, focused tests, fixture and one vignette section.",
  "- Allowed behavior: non-destructive inspection and explicit validation diagnostics.",
  "- Prohibited behavior: silently mutating cubes, adding optional backends or broad metadata standardization.",
  "- Required checks: valid/invalid cube fixtures, class and diagnostic contract tests, package checks.",
  "- Rollback criterion: nondeterministic diagnostics, silent mutation or incompatible behavior for existing ocean_cube objects.",
  "- Proposed commit: feat: add public cube validation and inspection",
  "",
  "## Commit 8 — implement viz.map",
  "",
  "- Objective: add the first thin visualization wrapper over the stabilized cube contract.",
  "- Files affected: visualization source, tests, roxygen/NAMESPACE and one executable example.",
  "- Allowed behavior: return a documented plot object from validated 2D selections.",
  "- Prohibited behavior: implementing the full viz family, animation, backend downloads or hidden data transformation.",
  "- Required checks: object contract tests, representative visual snapshots where stable, examples and package checks.",
  "- Rollback criterion: plot output depends on undocumented state or requires expansion of the core data model.",
  "- Proposed commit: feat: add viz.map for validated cubes",
  "",
  "## Commit 9 and later — continue functional modules",
  "",
  "- Objective: implement remaining core tiers in dependency order: selection/extraction, aggregation, climatology, anomaly, change, matching, visualization, export and animation.",
  "- Files affected: one function or cohesive micro-module per commit with its own tests and documentation.",
  "- Allowed behavior: only the contract approved in 06-api-release-tiers.csv.",
  "- Prohibited behavior: pulling future STAC, Zarr, UGRID, swath or unstructured-grid support into core.",
  "- Required checks: focused scientific fixtures, backend error tests, full regression suite and package checks at module boundaries.",
  "- Rollback criterion: scientific contract ambiguity, optional backend becoming mandatory, or regression outside the module.",
  "- Proposed commit: feat: implement one approved oceancube core function",
  "",
  "# Execution gate",
  "",
  "This document authorizes no cleanup by itself. The next phase may execute only Commit 1 after a human approves the exact ignore-rule rows and diff."
)
write_utf8(cleanup_plan, "11-cleanup-execution-plan.Rmd")

count_decisions <- function(values, levels) {
  result <- setNames(integer(length(levels)), levels)
  tab <- table(values)
  result[names(tab)] <- as.integer(tab)
  result
}

export_counts <- count_decisions(export_decisions$decision,
                                  c("retain", "retain-and-expand", "rename-with-alias", "deprecate-later", "internalize", "review"))
tier_counts <- count_decisions(api_release_tiers$release_tier,
                               c("core-0.2.0", "post-core-0.2.x", "future"))
internal_counts <- count_decisions(internal_decisions$decision,
                                   c("retain-internal", "move-later", "merge-candidate", "rename-internal-later", "review", "remove-candidate"))
file_counts <- count_decisions(file_decisions$decision,
                               c("retain", "relocate-later", "merge-later", "ignore", "review", "remove-candidate"))
previous_review_files <- sum(previous_files$recommended_action == "review")

decision_report <- c(
  "---",
  "title: \"oceancube 0.2.0 cleanup decision\"",
  "output: html_document",
  "---",
  "",
  "# 1. Repository state",
  "",
  paste0("Decision analysis started on branch dev-0.2.0 at ", expected_head, ". The protected main and v0.1.0 references both resolve to ", expected_protected, ". The input tree was clean; this phase writes only under docs/roadmap/decision-0.2.0/."),
  "",
  "# 2. Count confirmation",
  "",
  paste0("The previous counts are confirmed: ", verified_top_count, " top-level oceancube functions in R/*.R, ", verified_export_count, " exports and ", verified_internal_count, " unexported top-level functions. The apparent inflation concern is resolved by AST scope: ", verified_local_count, " named nested functions are reported separately, anonymous callbacks are excluded, and NAMESPACE contains ", imported_function_count, " explicitly imported functions. The four S3 methods are top-level oceancube definitions and part of the 155 unexported functions."),
  "",
  "# 3. Decisions for the 27 exports",
  "",
  paste0("All 27 NAMESPACE exports have an individual decision in 03-current-export-decisions.csv: ", export_counts[["retain"]], " retain, ", export_counts[["retain-and-expand"]], " retain-and-expand, ", export_counts[["rename-with-alias"]], " rename-with-alias, ", export_counts[["deprecate-later"]], " deprecate-later, ", export_counts[["internalize"]], " internalize and ", export_counts[["review"]], " review."),
  "",
  "# 4. Resolution of the four prior review exports",
  "",
  "annual_index, coast_dist, crop_stock and stock_mask are resolved as retain. annual_index is not treated as an implementation of generic to_year because its threshold-indicator semantics differ. The three spatial/stock functions remain compatible specialized extensions outside the mandatory core. Any later internalization or deprecation requires replacement parity, usage evidence and human approval.",
  "",
  "# 5. Aliases and deprecation",
  "",
  "download_nc to download_copernicus, layer_mean to cube_vertical_mean and link_events to cube_join use behavior-identical wrappers. Old names remain functional and silent throughout initial 0.2.0 development. A warning can begin only in a later 0.2.x after adoption review; no removal version is approved.",
  "",
  "# 6. Realistic 0.2.0 core",
  "",
  paste0("The proposed API is reduced from 114 simultaneous targets to ", tier_counts[["core-0.2.0"]], " core functions. The core covers Copernicus and local NetCDF ingestion, public validation/inspection, crop/slice/extract/transect, five required static visualizations, time aggregation, climatology, anomaly, change, locate/join, NetCDF/table/plot/GIF output and map animation. ocean_cube, cube_collect, Copernicus setup/connect and cube_vertical_mean are included as foundations or compatibility commitments."),
  "",
  "# 7. Deferred functions",
  "",
  paste0(tier_counts[["post-core-0.2.x"]], " functions are post-core 0.2.x and ", tier_counts[["future"]], " are future. STAC, Zarr, THREDDS, regridding/resampling, vector visualization and advanced gradients are not core dependencies. UGRID, swath and unstructured-grid support are not introduced implicitly."),
  "",
  "# 8. Internal functions",
  "",
  paste0("All ", verified_internal_count, " current internal top-level functions have individual evidence in 07-internal-function-decisions.csv. ", internal_counts[["retain-internal"]], " are retain-internal and ", internal_counts[["review"]], " require review because no static consumer was established. No existing internal is mislabeled internalize and no removal candidate is approved without affirmative duplication/obsolescence evidence."),
  "",
  "# 9. File decisions",
  "",
  paste0("08-file-cleanup-decisions.csv re-inventories ", nrow(file_decisions), " current files outside the decision output directory and replaces the prior blanket-like review signal. The previous audit had ", previous_review_files, " review rows. Current decisions are evidence-based and conservative: ", file_counts[["retain"]], " retain, ", file_counts[["relocate-later"]], " relocate-later, ", file_counts[["merge-later"]], " merge-later, ", file_counts[["ignore"]], " ignore, ", file_counts[["review"]], " review and ", file_counts[["remove-candidate"]], " remove-candidate. Every removal candidate still requires human approval; no file is deleted."),
  "",
  "# 10. Documentation policy",
  "",
  "Roxygen is canonical for function reference; man is generated. Vignettes own complete workflows, README is minimal entry, pkgdown is published assembly and docs/roadmap records decisions. The handbook will merge gradually into vignettes but is retained until parity approval. inst/examples are routed to vignettes or tests according to purpose. Generated HTML is rebuilt, not hand-edited.",
  "",
  "# 11. Modularization",
  "",
  "Current files remain in place. The only proposed physical changes are later, test-gated splits of backend-netcdf.R and utils-internal.R plus later file renames aligned with the three compatibility aliases. Every other current R file remains until the core contract is stable.",
  "",
  "# 12. Cleanup sequence",
  "",
  "Future work is divided into reversible commits: ignore rules, approved artifact relocation, documentation consolidation, compatibility aliases, behavior-preserving R reorganization, baseline certification, public validation/inspection, viz.map, then remaining modules in dependency order. 11-cleanup-execution-plan.Rmd defines scope, prohibitions, tests and rollback for each.",
  "",
  "# 13. Risks",
  "",
  "- Static call analysis cannot prove absence of dynamic get/do.call use or external consumers.",
  "- Provider and NetCDF backends can force optional dependencies into core unless interfaces stay narrow.",
  "- Scientific aggregation, climatology, anomaly and change contracts need explicit calendar, missingness, units and depth semantics.",
  "- Documentation consolidation can lose unique scientific context unless parity is reviewed topic by topic.",
  "- Compatibility aliases can drift if wrappers are not tested for values, attributes, errors and all arguments.",
  "- Artifact classification can confuse operational data with reproducible output; checksums and provenance are required before relocation.",
  "",
  "# 14. Human approvals still required",
  "",
  "- Approve the exact .gitignore and .Rbuildignore additions for the first cleanup commit.",
  "- Approve explicit path lists and destinations before any artifact relocation or removal candidate action.",
  "- Approve handbook topic migrations only after content and executable-example parity.",
  "- Approve the later start of deprecation warnings and any eventual alias removal version.",
  "- Approve semantics for calendars, seasons, missing values, uncertainty, depth direction and units before scientific core implementations.",
  "- Approve optional backend dependencies and storage policy for NetCDF/RDS operational data.",
  "",
  "# 15. First functional modification proposal",
  "",
  "After the non-functional cleanup commits, the first API modification should be the three behavior-identical compatibility aliases in one isolated commit. The first genuinely new capability should then be cube_validate plus cube_inspect, because every later visualization and scientific module depends on a stable public cube contract. viz.map follows as the first visualization.",
  "",
  "# Decision",
  "",
  "Approve the tables as the cleanup decision baseline. The next authorized phase is limited to the first ignore-rule commit and must begin with human approval of its exact rows."
)
write_utf8(decision_report, "oceancube-0.2.0-cleanup-decision.Rmd")

human_approvals <- c(
  "Exact ignore and build-exclusion rules for Commit 1.",
  "Exact paths and destinations for any relocation or remove-candidate action.",
  "Topic-level handbook-to-vignette parity.",
  "Start of warnings and any removal version for compatibility aliases.",
  "Scientific semantics for calendar, season, missingness, units, depth and uncertainty.",
  "Optional backend dependencies and operational-data storage."
)

readme <- c(
  "# oceancube 0.2.0 — Cleanup decision",
  "",
  "Static, non-destructive decision review of the 0.1.0 codebase on dev-0.2.0.",
  "",
  "## Count reconciliation",
  "",
  "- Previous source-defined functions: 182",
  paste0("- Verified top-level source functions: ", verified_top_count),
  "- Previous internal functions: 155",
  paste0("- Verified internal top-level functions: ", verified_internal_count),
  paste0("- Named local nested functions, excluded: ", verified_local_count),
  paste0("- Imported namespace functions: ", imported_function_count),
  paste0("- Registered S3 methods: ", verified_s3_count),
  "",
  "## Current exports",
  "",
  "- Total: 27",
  paste0("- retain: ", export_counts[["retain"]]),
  paste0("- retain-and-expand: ", export_counts[["retain-and-expand"]]),
  paste0("- rename-with-alias: ", export_counts[["rename-with-alias"]]),
  paste0("- deprecate-later: ", export_counts[["deprecate-later"]]),
  paste0("- internalize: ", export_counts[["internalize"]]),
  paste0("- review: ", export_counts[["review"]]),
  "",
  "The four previous review exports are resolved as retain: annual_index, coast_dist, crop_stock and stock_mask.",
  "",
  "## Release tiers",
  "",
  paste0("- core-0.2.0: ", tier_counts[["core-0.2.0"]]),
  paste0("- post-core-0.2.x: ", tier_counts[["post-core-0.2.x"]]),
  paste0("- future: ", tier_counts[["future"]]),
  "",
  "## Internal functions",
  "",
  paste0("- retain-internal: ", internal_counts[["retain-internal"]]),
  paste0("- move-later: ", internal_counts[["move-later"]]),
  paste0("- merge-candidate: ", internal_counts[["merge-candidate"]]),
  paste0("- rename-internal-later: ", internal_counts[["rename-internal-later"]]),
  paste0("- review: ", internal_counts[["review"]]),
  paste0("- remove-candidate: ", internal_counts[["remove-candidate"]]),
  "",
  "## Files",
  "",
  paste0("- Previous audit review rows: ", previous_review_files),
  paste0("- retain: ", file_counts[["retain"]]),
  paste0("- relocate-later: ", file_counts[["relocate-later"]]),
  paste0("- merge-later: ", file_counts[["merge-later"]]),
  paste0("- ignore: ", file_counts[["ignore"]]),
  paste0("- review: ", file_counts[["review"]]),
  paste0("- remove-candidate: ", file_counts[["remove-candidate"]]),
  "",
  "No deletion is approved. Every remove-candidate requires a new safety check and explicit human approval.",
  "",
  "## Human decisions pending",
  "",
  paste0("- ", human_approvals),
  "",
  "## Outputs",
  "",
  "- 01-source-function-verification.csv",
  "- 02-count-reconciliation.csv",
  "- 03-current-export-decisions.csv",
  "- 04-public-review-resolution.csv",
  "- 05-deprecation-plan.csv",
  "- 06-api-release-tiers.csv",
  "- 07-internal-function-decisions.csv",
  "- 08-file-cleanup-decisions.csv",
  "- 09-documentation-policy.Rmd",
  "- 10-module-migration-plan.csv",
  "- 11-cleanup-execution-plan.Rmd",
  "- oceancube-0.2.0-cleanup-decision.Rmd",
  "- generate-decisions.R",
  "",
  "## Next recommended phase",
  "",
  "Execute only the first approved cleanup commit: align .gitignore and .Rbuildignore using an explicit, human-approved subset of the file decision table."
)
write_utf8(readme, "README.md")

expected_outputs <- c(
  sprintf("%02d-%s", 1:8, c(
    "source-function-verification.csv", "count-reconciliation.csv", "current-export-decisions.csv",
    "public-review-resolution.csv", "deprecation-plan.csv", "api-release-tiers.csv",
    "internal-function-decisions.csv", "file-cleanup-decisions.csv"
  )),
  "09-documentation-policy.Rmd", "10-module-migration-plan.csv", "11-cleanup-execution-plan.Rmd",
  "oceancube-0.2.0-cleanup-decision.Rmd", "README.md", "generate-decisions.R"
)

actual_outputs <- sort(basename(list.files(out_dir, full.names = TRUE)))
stopifnot(setequal(expected_outputs, actual_outputs))

cat("DECISION_GENERATION: PASS\n")
cat("TOP_LEVEL_FUNCTIONS=", verified_top_count, "\n", sep = "")
cat("LOCAL_NESTED_FUNCTIONS=", verified_local_count, "\n", sep = "")
cat("INTERNAL_TOP_LEVEL_FUNCTIONS=", verified_internal_count, "\n", sep = "")
cat("CORE_FUNCTIONS=", tier_counts[["core-0.2.0"]], "\n", sep = "")
cat("POST_CORE_FUNCTIONS=", tier_counts[["post-core-0.2.x"]], "\n", sep = "")
cat("FUTURE_FUNCTIONS=", tier_counts[["future"]], "\n", sep = "")
cat("FILES_CLASSIFIED=", nrow(file_decisions), "\n", sep = "")
