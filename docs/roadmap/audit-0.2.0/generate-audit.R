#!/usr/bin/env Rscript

# Reproducible, read-only static audit for oceancube 0.1.0.
# This script writes only into docs/roadmap/audit-0.2.0/.

options(stringsAsFactors = FALSE, warn = 1)

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(root, "docs", "roadmap", "audit-0.2.0")
roadmap_path <- file.path(root, "docs", "roadmap", "oceancube-v0.2.0-scope.Rmd")

stopifnot(
  basename(root) == "oceancube",
  dir.exists(out_dir),
  file.exists(roadmap_path)
)

rel_path <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  ifelse(startsWith(path, prefix), substring(path, nchar(prefix) + 1L), path)
}

git_lines <- function(args) {
  output <- system2("git", args, stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop("git command failed: git ", paste(args, collapse = " "), "\n", paste(output, collapse = "\n"))
  }
  enc2utf8(output)
}

write_csv <- function(x, filename) {
  destination <- file.path(out_dir, filename)
  if (!startsWith(normalizePath(dirname(destination), winslash = "/"), normalizePath(out_dir, winslash = "/"))) {
    stop("Refusing to write outside audit directory: ", destination)
  }
  utils::write.csv(x, destination, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

read_text <- function(path) {
  tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
}

collapse_nonempty <- function(x, sep = "; ") {
  x <- unique(x[nzchar(x) & !is.na(x)])
  if (length(x)) paste(x, collapse = sep) else ""
}

escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

call_pattern <- function(name) {
  paste0("(?<![[:alnum:]_.])", escape_regex(name), "[[:space:]]*\\(")
}

all_disk_files <- list.files(root, all.files = TRUE, no.. = TRUE, recursive = TRUE, full.names = TRUE)
all_disk_files <- all_disk_files[file.info(all_disk_files)$isdir %in% FALSE]
all_disk_rel <- sort(rel_path(all_disk_files))
all_disk_rel <- all_disk_rel[!startsWith(all_disk_rel, ".git/")]
all_disk_rel <- all_disk_rel[!startsWith(all_disk_rel, "docs/roadmap/audit-0.2.0/")]

tracked <- sort(git_lines(c("ls-files")))
untracked <- sort(git_lines(c("ls-files", "--others", "--exclude-standard")))
ignored <- sort(git_lines(c("ls-files", "--others", "-i", "--exclude-standard")))

classify_file <- function(path) {
  lower <- tolower(path)
  ext <- tolower(tools::file_ext(path))
  if (grepl("^R/.*[.]R$", path)) return("R-source")
  if (grepl("^tests/", path)) return("test")
  if (grepl("^vignettes/", path)) return("vignette")
  if (grepl("^handbook/", path)) return("handbook")
  if (grepl("^inst/examples/", path)) return("example")
  if (grepl("^docs/handbook/", path) || ext == "html") return("website")
  if (grepl("^man/", path) || grepl("^(README|NEWS)", basename(path)) || grepl("^docs/roadmap/", path)) return("documentation")
  if (basename(path) %in% c("DESCRIPTION", "NAMESPACE", "LICENSE", "CITATION.cff", "CITATION")) return("metadata")
  if (grepl("(^|/)([.]gitignore|[.]Rbuildignore|[.]lintr|_pkgdown[.]yml|.*[.]ya?ml)$", path, ignore.case = TRUE)) return("configuration")
  if (grepl("(^artifacts/|[.]Rcheck/|[.]log$|[.]tmp$|[.]cache/|[.]quarto/)", lower)) return("artifact")
  if (ext %in% c("nc", "nc4", "rds", "rda", "rdata", "csv", "tsv", "parquet", "feather")) return("data")
  if (ext %in% c("png", "jpg", "jpeg", "gif", "svg", "tif", "tiff")) return("image")
  if (grepl("(^doc/|^Meta/|[.]knit[.]md$)", path)) return("generated-output")
  "other"
}

is_generated <- function(path, type, is_ignored) {
  lower <- tolower(path)
  is_ignored || type %in% c("generated-output", "website", "artifact") ||
    grepl("(^docs/handbook/|^doc/|^Meta/|[.]quarto/|[.]Rcheck/|[.]html$|[.]log$)", lower)
}

role_for <- function(type) {
  switch(type,
    "R-source" = "package implementation",
    test = "verification",
    documentation = "reference or planning documentation",
    vignette = "long-form package workflow",
    handbook = "user handbook source or support file",
    example = "executable example",
    metadata = "package or release metadata",
    configuration = "tooling or build configuration",
    data = "data input or intermediate",
    "generated-output" = "generated build output",
    artifact = "development or certification artifact",
    image = "visual asset",
    website = "rendered website output",
    "uncategorized repository file"
  )
}

source_truth_for <- function(path, type, generated) {
  if (type == "R-source") return("R source and roxygen comments")
  if (type == "test") return("test source")
  if (type == "vignette") return("vignette source")
  if (type == "example") return("example source")
  if (type == "handbook" && tools::file_ext(path) %in% c("qmd", "Rmd", "css", "csv", "R")) return("handbook source")
  if (path == "README.Rmd") return("README source")
  if (path == "README.md") return("generated from README.Rmd; verify manually")
  if (grepl("^man/", path)) return("generated from roxygen comments")
  if (generated) return("reproducible source should be identified before cleanup")
  if (type %in% c("metadata", "configuration", "documentation")) return("tracked repository source")
  "review"
}

file_rows <- lapply(all_disk_rel, function(path) {
  full <- file.path(root, path)
  type <- classify_file(path)
  is_tracked <- path %in% tracked
  is_ignored <- path %in% ignored
  generated <- is_generated(path, type, is_ignored)
  duplicate <- grepl("^docs/handbook/", path) || path == "README.md" ||
    (grepl("^handbook/functions/", path) && tools::file_ext(path) == "qmd")
  obsolete <- grepl("([.]Rhistory$|[.]RData$|[.]RDataTmp$|[.]tmp$|[.]log$|[.]Rcheck/|[.]quarto/)", path, ignore.case = TRUE)
  evidence <- collapse_nonempty(c(
    if (is_tracked) "git ls-files: tracked" else "git ls-files: not tracked",
    if (is_ignored) "git ignore rules: ignored" else "not matched by ignore inventory",
    if (generated) "path/type indicates generated or development output" else "path/type indicates source or repository input",
    if (duplicate) "same topic has another source or rendered representation" else ""
  ))
  recommendation <- if (obsolete) "review" else if (generated && !is_tracked) "relocate" else if (duplicate) "merge" else if (is_tracked) "retain" else "review"
  data.frame(
    path = path,
    name = basename(path),
    extension = if (nzchar(tools::file_ext(path))) paste0(".", tools::file_ext(path)) else "",
    directory = dirname(path),
    size_bytes = unname(file.info(full)$size),
    tracked = is_tracked,
    ignored = is_ignored,
    file_type = type,
    role = role_for(type),
    generated = generated,
    source_of_truth = source_truth_for(path, type, generated),
    possible_duplicate = duplicate,
    possible_obsolete = obsolete,
    evidence = evidence,
    recommended_action = recommendation,
    notes = if (recommendation == "review") "No deletion decision without human review." else "",
    check.names = FALSE
  )
})
file_inventory <- do.call(rbind, file_rows)
write_csv(file_inventory, "01-file-inventory.csv")

namespace <- read_text(file.path(root, "NAMESPACE"))
exports <- sort(sub("^export\\(([^)]+)\\)$", "\\1", grep("^export\\(", namespace, value = TRUE)))
s3_entries <- grep("^S3method\\(", namespace, value = TRUE)
s3_methods <- sort(sub("^S3method\\(([^,]+),([^)]+)\\)$", "\\1.\\2", s3_entries))

parse_definitions <- function(path) {
  exprs <- parse(path, keep.source = TRUE)
  refs <- attr(exprs, "srcref")
  rows <- list()
  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]
    if (!is.call(expr) || !as.character(expr[[1]]) %in% c("<-", "=")) next
    lhs <- expr[[2]]
    rhs <- expr[[3]]
    if (!is.symbol(lhs) || !is.call(rhs) || !identical(rhs[[1]], as.name("function"))) next
    sr <- if (!is.null(refs) && length(refs) >= i) refs[[i]] else NULL
    if (is.null(sr)) sr <- attr(expr, "srcref")
    if (is.null(sr)) sr <- attr(rhs, "srcref")
    line_start <- if (!is.null(sr)) as.integer(sr)[1] else NA_integer_
    line_end <- if (!is.null(sr)) as.integer(sr)[3] else NA_integer_
    rows[[length(rows) + 1L]] <- list(
      name = as.character(lhs),
      file = rel_path(path),
      line_start = line_start,
      line_end = line_end,
      expression = rhs,
      formals = names(formals(eval(rhs, envir = baseenv())))
    )
  }
  rows
}

r_files <- sort(list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE))
definitions <- unlist(lapply(r_files, parse_definitions), recursive = FALSE)
defined_names <- sort(unique(vapply(definitions, `[[`, character(1), "name")))

definition_by_name <- setNames(definitions, vapply(definitions, `[[`, character(1), "name"))

static_callees <- lapply(definitions, function(def) {
  calls <- all.names(def$expression[[3]], functions = TRUE, unique = FALSE)
  intersect(calls, defined_names)
})
names(static_callees) <- vapply(definitions, `[[`, character(1), "name")

roadmap <- read_text(roadmap_path)
roadmap_text <- paste(roadmap, collapse = "\n")

retain_concepts <- c(
  "ocean_cube", "read_nc", "cube_collect", "cube_crop", "cube_slice", "cube_extract",
  "cube_transect", "cube_mask", "cube_cell_area", "cube_layer_thickness", "cube_cell_volume",
  "cube_polygon_weights", "to_month", "clim_day", "clim_month", "anom_diff", "anom_z",
  "signal_noise", "layer_mean", "link_events", "cm_setup", "cm_connect"
)
rename_map <- c(download_nc = "download_copernicus", layer_mean = "cube_vertical_mean", link_events = "cube_join")

test_files <- sort(list.files(file.path(root, "tests"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE))
doc_files <- sort(c(
  list.files(file.path(root, "man"), pattern = "[.]Rd$", full.names = TRUE),
  list.files(file.path(root, "vignettes"), recursive = TRUE, full.names = TRUE),
  list.files(file.path(root, "handbook"), recursive = TRUE, full.names = TRUE),
  list.files(file.path(root, "inst", "examples"), pattern = "[.]R$", full.names = TRUE),
  file.path(root, c("README.md", "README.Rmd", "NEWS.md")),
  roadmap_path
))
doc_files <- doc_files[file.exists(doc_files) & !file.info(doc_files)$isdir]

files_with_call <- function(files, function_name) {
  pattern <- call_pattern(function_name)
  rel_path(files[vapply(files, function(path) {
    any(grepl(pattern, read_text(path), perl = TRUE))
  }, logical(1))])
}

module_current <- function(file, name) {
  if (grepl("backend|read_nc", file)) return("backend/netcdf")
  if (grepl("ocean_cube", file)) return("core-object")
  if (grepl("cube_(crop|slice)", file)) return("selection")
  if (grepl("cube_(extract|transect|collect)", file)) return("extraction")
  if (grepl("clim", file)) return("climatology")
  if (grepl("anom", file) || name == "signal_noise") return("anomaly")
  if (grepl("grid|polygon|mask|coast|stock", file)) return("geometry/masks")
  if (grepl("month|annual", file)) return("aggregation")
  if (grepl("link_events", file)) return("matching")
  if (grepl("cm_setup|download_nc", file)) return("ingestion-copernicus")
  if (grepl("layer_mean", file)) return("summary")
  "internal-utils"
}

function_rows <- lapply(definitions, function(def) {
  name <- def$name
  exported <- name %in% exports
  s3 <- name %in% s3_methods
  generic <- if (s3) sub("[.].*$", "", name) else ""
  test_hits <- files_with_call(test_files, name)
  doc_hits <- files_with_call(doc_files, name)
  example_hits <- files_with_call(doc_files[grepl("^(inst/examples|vignettes|handbook)", rel_path(doc_files))], name)
  status <- if (name %in% names(rename_map)) "rename" else if (name %in% retain_concepts || s3) "retain" else if (!exported) "internalize" else if (grepl(paste0("(?<![[:alnum:]_.])", escape_regex(name), "(?![[:alnum:]_.])"), roadmap_text, perl = TRUE)) "expand" else "not-mentioned"
  recommendation <- if (status == "rename") "rename" else if (status == "internalize") "internalize" else if (status %in% c("retain", "expand")) "retain" else "review"
  callers <- names(static_callees)[vapply(static_callees, function(x) name %in% x, logical(1))]
  data.frame(
    function_name = name,
    source_file = def$file,
    line_start = def$line_start,
    line_end = def$line_end,
    exported = exported,
    s3_method = s3,
    s3_generic = generic,
    visibility = if (exported) "public" else "internal",
    module_current = module_current(def$file, name),
    direct_call_count = sum(static_callees[[name]] %in% defined_names),
    called_by_count = length(unique(callers)),
    tests_found = length(test_hits),
    documentation_found = length(doc_hits),
    examples_found = length(example_hits),
    roadmap_0.2_status = status,
    recommended_action = recommendation,
    proposed_name = if (name %in% names(rename_map)) unname(rename_map[name]) else name,
    evidence = collapse_nonempty(c(
      paste0(def$file, ":", def$line_start, "-", def$line_end),
      if (exported) "NAMESPACE export" else "not exported by NAMESPACE",
      if (s3) "NAMESPACE S3method" else "",
      if (length(test_hits)) paste0("direct-call test files: ", length(test_hits)) else "no direct-call test file found",
      if (name %in% retain_concepts) "roadmap section 21.3" else "",
      if (name %in% names(rename_map)) "roadmap section 21.4" else ""
    )),
    notes = if (status == "not-mentioned") "Roadmap decision requires human review." else "Static counts exclude dynamic dispatch.",
    check.names = FALSE
  )
})
function_inventory <- do.call(rbind, function_rows)
function_inventory <- function_inventory[order(function_inventory$function_name), ]
write_csv(function_inventory, "02-function-inventory.csv")

rd_for_function <- function(name) {
  files <- list.files(file.path(root, "man"), pattern = "[.]Rd$", full.names = TRUE)
  hits <- files[vapply(files, function(path) {
    lines <- read_text(path)
    any(grepl(paste0("^\\\\alias\\{", escape_regex(name), "\\}$"), lines, perl = TRUE))
  }, logical(1))]
  if (length(hits)) hits[1] else NA_character_
}

rd_field <- function(path, field) {
  if (is.na(path) || !file.exists(path)) return("")
  text <- paste(read_text(path), collapse = " ")
  match <- regexec(paste0("\\\\", field, "\\{([^{}]*(?:\\{[^{}]*\\}[^{}]*)*)\\}"), text, perl = TRUE)
  value <- regmatches(text, match)[[1]]
  if (length(value) >= 2L) gsub("[[:space:]]+", " ", value[2]) else ""
}

public_rows <- lapply(exports, function(name) {
  def <- definition_by_name[[name]]
  rd <- rd_for_function(name)
  tests <- files_with_call(test_files, name)
  examples <- files_with_call(doc_files[grepl("^(inst/examples|vignettes|handbook)", rel_path(doc_files))], name)
  decision <- if (name %in% names(rename_map)) "rename" else if (name %in% retain_concepts) "retain" else "review"
  proposed <- if (name %in% names(rename_map)) unname(rename_map[name]) else if (decision == "retain") name else ""
  strategy <- if (decision == "rename") paste0("temporary compatibility alias to ", proposed, " with gradual deprecation") else if (decision == "retain") "retain public name and expand only with compatible defaults" else "human decision required before API change"
  data.frame(
    function_name = name,
    source_file = if (!is.null(def)) def$file else "not found in top-level definitions",
    purpose_current = rd_field(rd, "title"),
    input = if (!is.null(def)) collapse_nonempty(def$formals, ", ") else "",
    output = rd_field(rd, "value"),
    documented = !is.na(rd),
    tested = length(tests) > 0L,
    example_available = length(examples) > 0L,
    roadmap_decision = decision,
    proposed_0.2_name = proposed,
    compatibility_strategy = strategy,
    evidence = collapse_nonempty(c(
      "NAMESPACE export",
      if (!is.na(rd)) rel_path(rd) else "no matching Rd alias",
      if (length(tests)) paste0(length(tests), " direct-call test files") else "no direct-call test file",
      if (name %in% names(rename_map)) "roadmap section 21.4" else if (name %in% retain_concepts) "roadmap section 21.3" else "roadmap review required"
    )),
    notes = "Purpose/output summarized from current Rd; verify semantics before redesign.",
    check.names = FALSE
  )
})
public_api <- do.call(rbind, public_rows)
write_csv(public_api, "03-public-api-current.csv")

proposal_modules <- list(
  "ingestion-copernicus" = c("cm_setup", "cm_connect", "download_copernicus"),
  "ingestion-erddap" = c("download_erddap"),
  "ingestion-url" = c("download_url", "source_search", "source_info", "source_variables", "read_stac"),
  "ingestion-netcdf" = c("read_nc", "read_opendap", "read_thredds", "read_zarr", "cube_ingest"),
  "core-object" = c("ocean_cube", "cube_collect"),
  validation = c("cube_validate", "cube_inspect", "cube_inventory", "cube_coverage", "cube_missing", "cube_quality", "cube_uncertainty", "cube_qc_filter"),
  "metadata-cf" = c("cube_standardize", "cube_units", "cube_calendar", "cube_lon_convention", "cube_depth_positive", "cube_cf_metadata"),
  selection = c("cube_crop", "cube_slice", "cube_align", "cube_regrid", "cube_resample", "cube_stack", "cube_merge", "cube_mask"),
  extraction = c("cube_extract", "cube_transect"),
  matching = c("cube_locate", "cube_assign", "cube_join"),
  aggregation = c("to_day", "to_week", "to_month", "to_season", "to_year", "cube_roll"),
  climatology = c("clim_day", "clim_week", "clim_month", "clim_season", "clim_year", "clim_custom"),
  anomaly = c("anom_diff", "anom_percent", "anom_ratio", "anom_z", "anom_robust", "anom_percentile", "signal_noise", "anom_cumsum", "anom_class"),
  change = c("cube_diff", "cube_rate", "cube_slope", "cube_rolling_slope", "cube_detrend", "cube_gradient", "cube_vertical_gradient"),
  geometry = c("cube_cell_area", "cube_layer_thickness", "cube_cell_volume", "cube_polygon_weights"),
  summary = c("cube_area_mean", "cube_volume_mean", "cube_vertical_integral", "cube_vertical_mean", "cube_region_summary", "cube_mean", "cube_median", "cube_sd", "cube_min", "cube_max", "cube_quantile", "cube_count", "cube_exceedance"),
  visualization = c("viz.map", "viz.series", "viz.profile", "viz.section", "viz.hovmoller", "viz.climatology", "viz.anomaly", "viz.slope", "viz.coverage", "viz.quality", "viz.uncertainty", "viz.vector", "viz.compare", "viz.save"),
  animation = c("viz.animate", "viz.animate_map", "viz.animate_profile", "viz.animate_section", "viz.animate_hovmoller", "viz.save_gif"),
  export = c("write_nc", "write_zarr", "write_rds", "write_table", "write_metadata", "write_stac")
)

core_20_1 <- unique(c(
  "cm_setup", "cm_connect", "source_search", "source_info", "source_variables", "download_copernicus", "download_erddap", "download_url", "read_nc", "read_opendap", "cube_ingest",
  "ocean_cube", "cube_validate", "cube_inspect", "cube_inventory", "cube_coverage", "cube_missing", "cube_quality", "cube_uncertainty", "cube_qc_filter", "cube_collect",
  "cube_crop", "cube_slice", "cube_extract", "cube_transect", "cube_align", "cube_stack", "cube_merge",
  "to_day", "to_week", "to_month", "to_season", "to_year", "cube_roll",
  "clim_day", "clim_week", "clim_month", "clim_season", "clim_year", "clim_custom",
  "anom_diff", "anom_percent", "anom_ratio", "anom_z", "anom_robust", "anom_percentile", "signal_noise", "anom_cumsum", "anom_class",
  "cube_diff", "cube_rate", "cube_slope", "cube_rolling_slope", "cube_detrend",
  "cube_locate", "cube_assign", "cube_join",
  proposal_modules$visualization, proposal_modules$animation,
  "write_nc", "write_rds", "write_table", "write_metadata"
))
future_20_2 <- c("read_zarr", "write_zarr", "read_thredds", "read_stac", "write_stac", "cube_regrid", "cube_resample", "cube_gradient", "cube_vertical_gradient")
equivalent_map <- c(
  download_copernicus = "download_nc",
  cube_vertical_mean = "layer_mean",
  cube_join = "link_events",
  to_year = "annual_index"
)

output_contract_for <- function(module) {
  switch(module,
    "core-object" = "ocean_cube",
    validation = "structured diagnostic table or inspected ocean_cube",
    "metadata-cf" = "ocean_cube with explicit metadata/provenance",
    "ingestion-copernicus" = "candidate table, local asset, connection, or ocean_cube",
    "ingestion-erddap" = "local asset or ocean_cube",
    "ingestion-url" = "candidate table, metadata, or local asset",
    "ingestion-netcdf" = "ocean_cube or backend descriptor",
    selection = "ocean_cube",
    extraction = "data.frame or ordered section table",
    matching = "ocean_match/data.frame",
    aggregation = "ocean_cube",
    climatology = "ocean_clim",
    anomaly = "ocean_anom/ocean_cube",
    change = "ocean_cube or slope summary",
    geometry = "geometry/weight arrays or tables",
    summary = "ocean_cube, array, or summary table with removed dimensions declared",
    visualization = "ggplot or explicit saved figure",
    animation = "ocean_animation or GIF",
    export = "written asset plus provenance",
    "roadmap-defined output; confirm during implementation"
  )
}

proposal_rows <- list()
seen <- character()
for (module in names(proposal_modules)) {
  for (name in proposal_modules[[module]]) {
    if (name %in% seen) next
    seen <- c(seen, name)
    equivalent <- if (name %in% names(equivalent_map)) unname(equivalent_map[name]) else if (name %in% exports) name else ""
    exists <- name %in% exports
    status <- if (name %in% future_20_2) "future-extension" else if (exists) "existing" else if (nzchar(equivalent)) "rename-required" else if (name %in% defined_names) "partial" else "new"
    priority <- if (name %in% future_20_2) "future" else if (name %in% core_20_1) "core" else if (module %in% c("metadata-cf", "geometry", "summary")) "high" else "medium"
    dependency <- switch(module,
      "core-object" = "base R; backend contract",
      validation = "ocean_cube; metadata CF; backend inspection",
      "metadata-cf" = "ocean_cube; CF metadata",
      "ingestion-copernicus" = "network client; credentials outside repository; NetCDF",
      "ingestion-erddap" = "HTTP/ERDDAP client; NetCDF",
      "ingestion-url" = "HTTP/STAC metadata",
      "ingestion-netcdf" = "ncdf4/OPeNDAP/Zarr backend as applicable",
      selection = "ocean_cube; backend block reads; sf for polygons",
      extraction = "selection; backend reads",
      matching = "selection; distance/time matching",
      aggregation = "calendar/time bounds; coverage",
      climatology = "aggregation; period/grouping metadata",
      anomaly = "ocean_clim; denominator safeguards",
      change = "time coordinates; minimum sample rules",
      geometry = "bounds/CRS; sf where required",
      summary = "geometry weights; explicit reductions",
      visualization = "ggplot2; explicit selection",
      animation = "gganimate; renderer",
      export = "format writer; metadata/provenance",
      "to be designed"
    )
    proposal_rows[[length(proposal_rows) + 1L]] <- data.frame(
      function_name = name,
      module = module,
      exists_in_0.1.0 = exists,
      current_equivalent = equivalent,
      implementation_status = status,
      priority = priority,
      dependencies_expected = dependency,
      output_contract = output_contract_for(module),
      notes = if (name %in% core_20_1) "Listed in roadmap section 20.1." else if (name %in% future_20_2) "Listed as planned extension in section 20.2." else "Explicitly proposed in roadmap functional sections.",
      check.names = FALSE
    )
  }
}
proposed_api <- do.call(rbind, proposal_rows)
proposed_api <- proposed_api[order(match(proposed_api$module, names(proposal_modules)), proposed_api$function_name), ]
write_csv(proposed_api, "04-public-api-proposed.csv")

transition_rows <- lapply(exports, function(name) {
  proposed <- if (name %in% names(rename_map)) unname(rename_map[name]) else if (name %in% proposed_api$function_name) name else ""
  transition <- if (name %in% names(rename_map)) "rename" else if (name %in% proposed_api$function_name) "retain" else "review"
  current_status <- if (name %in% names(rename_map)) "explicit rename in roadmap" else if (name %in% retain_concepts) "explicit retain concept" else "not explicitly resolved"
  phase <- if (transition == "rename") "Phase 1 decision, then owning implementation phase" else if (nzchar(proposed)) paste0("Roadmap module: ", proposed_api$module[match(proposed, proposed_api$function_name)]) else "Human review before cleanup"
  data.frame(
    current_function = name,
    current_status = current_status,
    proposed_function = proposed,
    transition = transition,
    breaking_change = transition %in% c("rename", "replace", "internalize", "deprecate"),
    compatibility_alias = if (transition == "rename") TRUE else FALSE,
    deprecation_required = if (transition == "rename") TRUE else FALSE,
    implementation_phase = phase,
    evidence = if (name %in% names(rename_map)) "roadmap section 21.4" else if (name %in% retain_concepts) "roadmap sections 20/21.3" else "NAMESPACE export; roadmap has no definitive mapping",
    notes = if (transition == "review") "Do not remove or deprecate without a human decision." else "",
    check.names = FALSE
  )
})
api_transition <- do.call(rbind, transition_rows)
write_csv(api_transition, "05-api-transition.csv")

parsed_names_in_file <- function(path) {
  tryCatch({
    expr <- parse(path, keep.source = FALSE)
    unique(all.names(expr, functions = TRUE))
  }, error = function(e) character())
}

test_rows <- list()
for (path in test_files) {
  rel <- rel_path(path)
  lines <- read_text(path)
  text <- paste(lines, collapse = "\n")
  symbols <- parsed_names_in_file(path)
  direct <- sort(intersect(symbols, defined_names))
  is_helper <- grepl("/helper-", rel)
  if (!length(direct)) direct <- ""
  success_count <- sum(grepl("expect_(equal|identical|true|false|type|s3_class|named|length|setequal|match)", lines))
  error_count <- sum(grepl("expect_(error|warning|condition)", lines))
  backend <- if (grepl("netcdf", text, ignore.case = TRUE) && grepl("memory", text, ignore.case = TRUE)) "memory+netcdf" else if (grepl("netcdf", text, ignore.case = TRUE)) "netcdf" else if (grepl("memory", text, ignore.case = TRUE)) "memory" else "unspecified"
  test_type <- if (is_helper) "helper/fixture" else if (grepl("baseline|contract|release|boundary", rel)) "contract/integration" else "unit/integration"
  for (name in direct) {
    module <- if (nzchar(name) && name %in% function_inventory$function_name) function_inventory$module_current[match(name, function_inventory$function_name)] else "repository infrastructure"
    test_rows[[length(test_rows) + 1L]] <- data.frame(
      test_file = rel,
      tested_function = name,
      test_type = test_type,
      direct_or_indirect = if (is_helper) "helper" else if (nzchar(name)) "direct parsed call" else "no package call parsed",
      backend = backend,
      expected_success_cases = success_count,
      expected_error_cases = error_count,
      roadmap_module = module,
      coverage_gap = if (is_helper) "Helper is evidence of fixture support, not a direct test assertion." else if (!nzchar(name)) "No source-defined function call found by static parse." else "Static mapping does not measure branch or line coverage.",
      notes = "Counts are expectation statements in the file, not per-function attribution.",
      check.names = FALSE
    )
  }
}
test_map <- do.call(rbind, test_rows)
write_csv(test_map, "06-test-map.csv")

documentation_candidates <- sort(unique(doc_files))
documentation_rows <- list()
all_api_names <- sort(unique(c(exports, proposed_api$function_name)))
for (path in documentation_candidates) {
  rel <- rel_path(path)
  lines <- read_text(path)
  text <- paste(lines, collapse = "\n")
  mentioned <- all_api_names[vapply(all_api_names, function(name) {
    grepl(paste0("(?<![[:alnum:]_.])", escape_regex(name), "(?![[:alnum:]_.])"), text, perl = TRUE)
  }, logical(1))]
  if (!length(mentioned)) mentioned <- ""
  source_type <- if (rel %in% c("README.md", "README.Rmd")) "README" else if (grepl("^man/", rel)) "man" else if (grepl("^vignettes/", rel)) "vignette" else if (grepl("^handbook/", rel)) "handbook" else if (grepl("^inst/examples/", rel)) "inst-example" else if (grepl("^docs/", rel)) "docs" else if (rel == "NEWS.md") "NEWS" else "documentation"
  canonical <- switch(source_type,
    man = "roxygen comments in R source",
    README = "README.Rmd for source; README.md for rendered entry",
    vignette = "vignette source for installable workflows",
    handbook = "handbook QMD/Rmd for extended user guidance",
    `inst-example` = "executable example source",
    docs = "roadmap for future decisions; generated website is non-canonical",
    NEWS = "NEWS.md for version history",
    "human review"
  )
  for (name in mentioned) {
    duplicate_group <- if (nzchar(name)) paste0("function:", name) else paste0("topic:", tools::file_path_sans_ext(basename(rel)))
    recommendation <- if (source_type %in% c("man", "README", "vignette", "handbook", "inst-example", "NEWS")) "retain" else if (grepl("^docs/handbook/", rel)) "merge" else "review"
    documentation_rows[[length(documentation_rows) + 1L]] <- data.frame(
      topic = tools::file_path_sans_ext(basename(rel)),
      function_name = name,
      source_type = source_type,
      source_file = rel,
      canonical_candidate = canonical,
      duplicate_group = duplicate_group,
      current = rel %in% tracked,
      roadmap_relevance = if (nzchar(name) && name %in% proposed_api$function_name) "explicit proposed API" else if (grepl("roadmap|0.2.0", rel, ignore.case = TRUE)) "planning source" else "current 0.1.0 documentation",
      recommended_action = recommendation,
      notes = "Topic overlap is evidence for review, not permission to delete.",
      check.names = FALSE
    )
  }
}
documentation_map <- do.call(rbind, documentation_rows)
write_csv(documentation_map, "07-documentation-map.csv")

artifact_candidates <- file_inventory[
  file_inventory$generated |
    file_inventory$file_type %in% c("artifact", "generated-output", "website", "data", "image") |
    grepl("^(artifacts|dev|sandbox|cache|venv|[.]venv|env|docs/handbook|auxdata|data-raw)/", file_inventory$path, ignore.case = TRUE),
]

contains_local_path <- function(path, tracked_file, size) {
  if (!tracked_file || is.na(size) || size > 1000000L) return("not-scanned")
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("R", "Rmd", "qmd", "md", "txt", "yml", "yaml", "csv", "json", "cff")) return("not-applicable")
  lines <- read_text(file.path(root, path))
  if (any(grepl("[A-Za-z]:[/\\\\]|/Users/|/home/", lines))) "true" else "false"
}

artifact_rows <- lapply(seq_len(nrow(artifact_candidates)), function(i) {
  row <- artifact_candidates[i, ]
  path <- row$path
  lower <- tolower(path)
  credential_name <- grepl("(^|/)([.]env|credentials?|tokens?|secrets?)([./_-]|$)", lower)
  required_package <- row$tracked && (grepl("^(inst|data|R|man|vignettes)/", path) || basename(path) %in% c("DESCRIPTION", "NAMESPACE"))
  required_tests <- row$tracked && grepl("^(tests|inst/extdata)/", path)
  reproducible <- row$generated || grepl("^(data-raw|dev)/", path)
  large <- !is.na(row$size_bytes) && row$size_bytes >= 5000000
  recommendation <- if (required_package || required_tests) "retain" else if (row$generated || row$ignored) "relocate" else "review"
  data.frame(
    path = path,
    tracked = row$tracked,
    ignored = row$ignored,
    generated = row$generated,
    reproducible = reproducible,
    required_for_package = required_package,
    required_for_tests = required_tests,
    contains_local_path = contains_local_path(path, row$tracked, row$size_bytes),
    contains_credentials = if (credential_name) "potential-name-match; content not exposed" else "no filename evidence; content not exhaustively inspected",
    contains_large_data = large,
    recommended_action = recommendation,
    evidence = collapse_nonempty(c(
      if (row$tracked) "tracked" else "not tracked",
      if (row$ignored) "ignored" else "not ignored",
      paste0("type=", row$file_type),
      paste0("size_bytes=", row$size_bytes),
      if (credential_name) "sensitive-looking filename; manual secure review" else ""
    )),
    notes = "No credentials were printed. Relocate/review is a proposal, not a deletion decision.",
    check.names = FALSE
  )
})
artifact_audit <- if (length(artifact_rows)) do.call(rbind, artifact_rows) else data.frame(
  path = character(), tracked = logical(), ignored = logical(), generated = logical(), reproducible = logical(),
  required_for_package = logical(), required_for_tests = logical(), contains_local_path = character(), contains_credentials = character(),
  contains_large_data = logical(), recommended_action = character(), evidence = character(), notes = character()
)
write_csv(artifact_audit, "08-artifact-audit.csv")

dependency_rows <- list()
for (caller in names(static_callees)) {
  callees <- unique(static_callees[[caller]])
  if (!length(callees)) next
  caller_def <- definition_by_name[[caller]]
  for (callee in callees) {
    callee_def <- definition_by_name[[callee]]
    dependency_rows[[length(dependency_rows) + 1L]] <- data.frame(
      caller = caller,
      callee = callee,
      caller_visibility = if (caller %in% exports) "public" else "internal",
      callee_visibility = if (callee %in% exports) "public" else "internal",
      caller_file = caller_def$file,
      callee_file = callee_def$file,
      relationship_type = "static direct symbol call",
      evidence = paste0(caller_def$file, ":", caller_def$line_start, "-", caller_def$line_end, " contains call symbol ", callee),
      check.names = FALSE
    )
  }
}
function_dependencies <- if (length(dependency_rows)) do.call(rbind, dependency_rows) else data.frame(
  caller = character(), callee = character(), caller_visibility = character(), callee_visibility = character(), caller_file = character(), callee_file = character(), relationship_type = character(), evidence = character()
)
write_csv(function_dependencies, "09-function-dependencies.csv")

module_file <- c(
  "core-object" = "cube-class.R", validation = "cube-validate.R", "metadata-cf" = "cube-standardize.R",
  "ingestion-copernicus" = "io-copernicus.R", "ingestion-erddap" = "io-erddap.R", "ingestion-url" = "io-url.R",
  "ingestion-netcdf" = "io-netcdf.R", selection = "cube-select.R", extraction = "cube-extract.R",
  matching = "cube-match.R", aggregation = "cube-aggregate.R", climatology = "cube-climatology.R",
  anomaly = "cube-anomaly.R", change = "cube-change.R", geometry = "cube-geometry.R", summary = "cube-summary.R",
  visualization = "viz-by-task.R", animation = "viz-animation.R", export = "export.R", "internal-utils" = "utils-internal.R"
)

proposed_module_for <- function(name, current_module) {
  target <- if (name %in% names(rename_map)) unname(rename_map[name]) else name
  hit <- proposed_api$module[match(target, proposed_api$function_name)]
  if (!is.na(hit)) return(hit)
  if (name %in% s3_methods) return("core-object")
  if (!name %in% exports) return("internal-utils")
  if (current_module == "geometry/masks") return("geometry")
  current_module
}

module_rows <- lapply(definitions, function(def) {
  current <- module_current(def$file, def$name)
  proposed <- proposed_module_for(def$name, current)
  proposed_file <- if (proposed %in% names(module_file)) unname(module_file[proposed]) else "review-module.R"
  data.frame(
    function_name = def$name,
    current_file = def$file,
    current_module = current,
    proposed_module = proposed,
    proposed_file = proposed_file,
    move_required = basename(def$file) != proposed_file,
    rename_required = def$name %in% names(rename_map),
    notes = if (!def$name %in% exports && proposed == "internal-utils") "Internal placement is a proposal; inspect cohesion before moving." else "Proposal only; no file was moved.",
    check.names = FALSE
  )
})
not_implemented <- proposed_api[!proposed_api$function_name %in% defined_names, ]
if (nrow(not_implemented)) {
  proposed_rows_for_map <- lapply(seq_len(nrow(not_implemented)), function(i) {
    item <- not_implemented[i, ]
    proposed_file <- if (item$module %in% names(module_file)) unname(module_file[item$module]) else "review-module.R"
    data.frame(
      function_name = item$function_name,
      current_file = "",
      current_module = "not-implemented",
      proposed_module = item$module,
      proposed_file = proposed_file,
      move_required = FALSE,
      rename_required = item$implementation_status == "rename-required",
      notes = "Proposed roadmap function not yet defined in R/*.R; target module/file requires implementation approval.",
      check.names = FALSE
    )
  })
  module_rows <- c(module_rows, proposed_rows_for_map)
}
module_map <- do.call(rbind, module_rows)
module_map <- module_map[order(module_map$proposed_module, module_map$function_name), ]
write_csv(module_map, "10-module-map.csv")

# Cross-table validation and summary material.
stopifnot(
  identical(sort(public_api$function_name), exports),
  nrow(public_api) == length(exports),
  all(file_inventory$file_type %in% c("R-source", "test", "documentation", "vignette", "handbook", "example", "metadata", "configuration", "data", "generated-output", "artifact", "image", "website", "other")),
  all(file_inventory$recommended_action %in% c("retain", "review", "relocate", "merge", "internalize", "deprecate", "remove-candidate")),
  all(function_inventory$visibility %in% c("public", "internal")),
  all(function_inventory$roadmap_0.2_status %in% c("retain", "rename", "expand", "deprecate", "internalize", "replace", "not-mentioned", "new-function-not-yet-implemented")),
  all(proposed_api$implementation_status %in% c("existing", "partial", "rename-required", "new", "future-extension")),
  all(proposed_api$priority %in% c("core", "high", "medium", "future")),
  all(api_transition$transition %in% c("retain", "rename", "expand", "replace", "internalize", "deprecate", "review"))
)

counts <- list(
  files_reviewed = nrow(file_inventory),
  tracked_files = sum(file_inventory$tracked),
  ignored_files = sum(file_inventory$ignored),
  source_functions = nrow(function_inventory),
  exports = length(exports),
  internal_functions = sum(function_inventory$visibility == "internal"),
  s3_methods = sum(function_inventory$s3_method),
  test_files = length(test_files),
  documentation_files = length(documentation_candidates),
  proposed_functions = nrow(proposed_api),
  new_functions = sum(proposed_api$implementation_status == "new"),
  retain = sum(api_transition$transition == "retain"),
  rename = sum(api_transition$transition == "rename"),
  expand = sum(api_transition$transition == "expand"),
  deprecate = sum(api_transition$transition == "deprecate"),
  deprecation_candidates = sum(api_transition$deprecation_required),
  internalize = sum(api_transition$transition == "internalize"),
  review = sum(api_transition$transition == "review"),
  files_review = sum(file_inventory$recommended_action == "review")
)

report <- c(
  "---",
  'title: "oceancube 0.2.0 — Auditoría reproducible del repositorio 0.1.0"',
  'author: "Auditoría estática asistida"',
  'date: "2026-08-03"',
  "output:",
  "  html_document:",
  "    toc: true",
  "    toc_depth: 3",
  "    number_sections: true",
  "---",
  "",
  "# 1. Estado inicial",
  "",
  "La auditoría parte de `dev-0.2.0` en `10814a6430398f966d6c4d8cf61d2796ad23c4cb`. `main` y `v0.1.0` permanecen en `93d2a79b11a6ae7622443ae068e6e2a2709c9324`. El árbol estaba limpio.",
  "",
  "# 2. Metodología",
  "",
  "Se usaron inventarios de Git, clasificación por ruta/tipo, `parse()` de R para definiciones superiores, análisis de símbolos de llamadas, NAMESPACE, Rd, pruebas, ejemplos y el roadmap. Los CSV son reproducibles mediante `generate-audit.R`. No se cargó ni ejecutó código del paquete.",
  "",
  "# 3. Limitaciones del análisis",
  "",
  "El análisis estático puede omitir dispatch S3, funciones locales, `get()`, `do.call()`, evaluación no estándar y llamadas dinámicas. Una aparición textual no equivale a una prueba directa. `review` se usa cuando la evidencia no permite una decisión segura.",
  "",
  "# 4. Inventario del repositorio",
  "",
  sprintf("Se revisaron %d archivos visibles fuera de `.git`: %d rastreados y %d ignorados. Véase `01-file-inventory.csv`.", counts$files_reviewed, counts$tracked_files, counts$ignored_files),
  "",
  "# 5. API pública actual",
  "",
  sprintf("NAMESPACE expone exactamente %d funciones. La tabla `03-public-api-current.csv` vincula fuente, contrato Rd, pruebas, ejemplos y decisión del roadmap.", counts$exports),
  "",
  "# 6. Funciones internas propias",
  "",
  sprintf("R/*.R contiene %d funciones superiores definidas por el paquete; %d son internas y %d son métodos S3 registrados.", counts$source_functions, counts$internal_functions, counts$s3_methods),
  "",
  "# 7. Mapa de pruebas",
  "",
  sprintf("Se mapearon %d archivos R de tests/helpers. `06-test-map.csv` distingue llamada parseada, helper/fixture y ausencia de llamada; los conteos de expectativas son por archivo, no por función.", counts$test_files),
  "",
  "# 8. Mapa documental",
  "",
  sprintf("Se revisaron %d archivos de README, man, vignettes, handbook, ejemplos, docs y NEWS. `07-documentation-map.csv` identifica grupos de solapamiento sin ordenar eliminaciones.", counts$documentation_files),
  "",
  "# 9. Artefactos y datos generados",
  "",
  "`08-artifact-audit.csv` separa tracked/ignored, reproducibilidad, necesidad para paquete/tests, tamaño y señales de rutas locales o nombres sensibles. No se imprimieron credenciales.",
  "",
  "# 10. Duplicación",
  "",
  "Los principales grupos para revisión son: roxygen/man frente a fichas del handbook; README.Rmd frente a README.md; QMD/Rmd frente a HTML renderizado; y ejemplos instalables frente a narrativas. Solapamiento no significa obsolescencia.",
  "",
  "# 11. API actual frente al roadmap",
  "",
  sprintf("El roadmap propone %d funciones únicas. `04-public-api-proposed.csv` y `05-api-transition.csv` documentan existencia, equivalencias, prioridad y compatibilidad.", counts$proposed_functions),
  "",
  "# 12. Funciones a conservar",
  "",
  sprintf("La matriz pública marca %d nombres como `retain`, sustentados por secciones 20 y 21.3 del roadmap o por coincidencia explícita de API.", counts$retain),
  "",
  "# 13. Funciones a renombrar",
  "",
  "El roadmap decide tres transiciones graduales: `download_nc()` → `download_copernicus()`, `layer_mean()` → `cube_vertical_mean()` y `link_events()` → `cube_join()`. Requieren alias y deprecación posterior, no cambios en esta fase.",
  "",
  "# 14. Funciones candidatas a deprecación",
  "",
  "No se declara ninguna eliminación inmediata. Las tres funciones renombradas son candidatas a deprecación gradual solo después de aprobar compatibilidad y cronograma.",
  "",
  "# 15. Funciones candidatas a internalización",
  "",
  sprintf("Se identificaron %d funciones no exportadas. La etiqueta `internalize` conserva su estado interno; no implica moverlas ni borrarlas.", counts$internal_functions),
  "",
  "# 16. Archivos candidatos a traslado",
  "",
  "Artefactos ignorados, builds, HTML renderizado, caches y datos operativos aparecen como `relocate` cuando la ruta/tipo aporta evidencia. Se debe confirmar su generador y necesidad antes de cualquier movimiento.",
  "",
  "# 17. Archivos que requieren revisión",
  "",
  sprintf("%d archivos tienen recomendación `review`, principalmente por incertidumbre, salida de desarrollo o falta de una fuente canónica inequívoca.", counts$files_review),
  "",
  "# 18. Brechas funcionales de 0.2.0",
  "",
  sprintf("%d funciones propuestas están marcadas `new`; las brechas dominantes son validación pública, metadatos CF, ingestión multifuente, visualización/animación, cambio temporal, asignación y exportación.", counts$new_functions),
  "",
  "# 19. Orden recomendado de implementación",
  "",
  "Seguir las fases del roadmap: aprobar limpieza; objeto/grilla/metadatos; validación; ingestión; selección/asignación; visualización; animación; agregación/climatología; anomalías/cambio; alineación/ponderación; exportación; documentación/publicación.",
  "",
  "# 20. Riesgos",
  "",
  "- Romper compatibilidad 0.1.0 al renombrar exports sin alias.\n- Confundir archivos ignorados con archivos eliminables.\n- Sobreestimar cobertura mediante coincidencia textual.\n- Perder semántica por dispatch o llamadas dinámicas no capturadas.\n- Duplicar documentación sin definir una fuente canónica.\n- Ampliar la API antes de estabilizar ocean_cube, metadatos y validación.",
  "",
  "# 21. Decisiones que requieren aprobación humana",
  "",
  "- Tabla definitiva retain/rename/deprecate/internalize/relocate.\n- Política y duración de aliases.\n- Fuente canónica de README/handbook/vignettes.\n- Tratamiento de artifacts, HTML y datos operativos.\n- Alcance core frente a future-extension.\n- Modularización física de R/*.R.",
  "",
  "# 22. Propuesta para la siguiente fase",
  "",
  "Aprobar explícitamente una tabla de decisiones de limpieza controlada basada en estos CSV. Solo después deben autorizarse movimientos, deprecaciones o eliminaciones, cada uno con pruebas y diff independiente."
)
writeLines(enc2utf8(report), file.path(out_dir, "oceancube-0.2.0-audit.Rmd"), useBytes = TRUE)

summary_lines <- c(
  "# oceancube 0.2.0 — Resumen ejecutivo de auditoría",
  "",
  "Auditoría estática y no destructiva del estado publicado 0.1.0 desde `dev-0.2.0`.",
  "",
  "## Conteos",
  "",
  paste0("- Archivos revisados: ", counts$files_reviewed),
  paste0("- Funciones propias definidas en R/: ", counts$source_functions),
  paste0("- Exports: ", counts$exports),
  paste0("- Funciones internas: ", counts$internal_functions),
  paste0("- Métodos S3: ", counts$s3_methods),
  paste0("- Archivos de pruebas/helpers: ", counts$test_files),
  paste0("- Archivos documentales revisados: ", counts$documentation_files),
  paste0("- Funciones propuestas para 0.2.0: ", counts$proposed_functions),
  paste0("- Funciones nuevas: ", counts$new_functions),
  paste0("- Candidatos `rename`: ", counts$rename),
  paste0("- Candidatos `deprecate`: ", counts$deprecation_candidates, " graduales ligados a `rename`; ", counts$deprecate, " inmediatos"),
  paste0("- Candidatos `internalize`: ", counts$internal_functions),
  paste0("- Archivos para `review`: ", counts$files_review),
  "",
  "## Transición pública",
  "",
  paste0("- retain: ", counts$retain),
  paste0("- rename: ", counts$rename),
  paste0("- expand: ", counts$expand),
  paste0("- deprecate: ", counts$deprecate),
  paste0("- internalize: ", counts$internalize),
  paste0("- review: ", counts$review),
  "",
  "## Principales riesgos",
  "",
  "- compatibilidad pública durante renombres;",
  "- falsos positivos al clasificar artefactos ignorados;",
  "- límites del análisis estático para S3 y llamadas dinámicas;",
  "- duplicación entre roxygen/man, handbook, vignettes y README;",
  "- crecimiento de API antes de estabilizar objeto, metadatos y validación.",
  "",
  "## Siguiente tarea recomendada",
  "",
  "Aprobar la tabla de decisiones de limpieza (retain/rename/deprecate/internalize/relocate/review) antes de cambiar archivos o API."
)
writeLines(enc2utf8(summary_lines), file.path(out_dir, "README.md"), useBytes = TRUE)

cat(sprintf(
  paste0(
    "AUDIT_GENERATION: PASS\n",
    "FILES_REVIEWED=%d\nSOURCE_FUNCTIONS=%d\nEXPORTS=%d\nINTERNAL_FUNCTIONS=%d\n",
    "S3_METHODS=%d\nTEST_FILES=%d\nDOCUMENTATION_FILES=%d\nPROPOSED_FUNCTIONS=%d\nNEW_FUNCTIONS=%d\n"
  ),
  counts$files_reviewed, counts$source_functions, counts$exports, counts$internal_functions,
  counts$s3_methods, counts$test_files, counts$documentation_files, counts$proposed_functions, counts$new_functions
))
