#!/usr/bin/env Rscript

# Static, non-destructive artifact relocation proposal for oceancube 0.2.0.
# Writes only beneath docs/roadmap/relocation-0.2.0/.

options(stringsAsFactors = FALSE, warn = 1)

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(root, "docs", "roadmap", "relocation-0.2.0")
audit_dir <- file.path(root, "docs", "roadmap", "audit-0.2.0")
decision_dir <- file.path(root, "docs", "roadmap", "decision-0.2.0")

stopifnot(basename(root) == "oceancube", dir.exists(out_dir), dir.exists(audit_dir), dir.exists(decision_dir))

git_lines <- function(args) {
  value <- system2("git", args, stdout = TRUE, stderr = TRUE)
  status <- attr(value, "status")
  if (!is.null(status) && status != 0L) stop("git failed: ", paste(args, collapse = " "), "\n", paste(value, collapse = "\n"))
  enc2utf8(value)
}

branch <- git_lines(c("branch", "--show-current"))
starting_commit <- git_lines(c("rev-parse", "HEAD"))
starting_message <- git_lines(c("log", "-1", "--pretty=%s"))
protected_main <- git_lines(c("rev-parse", "main"))
protected_tag <- git_lines(c("rev-list", "-n", "1", "v0.1.0"))

stopifnot(
  identical(branch, "dev-0.2.0"),
  identical(starting_message, "chore: define repository and build ignore rules"),
  identical(protected_main, "93d2a79b11a6ae7622443ae068e6e2a2709c9324"),
  identical(protected_tag, protected_main)
)

rel_path <- function(path) {
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  ifelse(startsWith(normalized, prefix), substring(normalized, nchar(prefix) + 1L), normalized)
}

read_text <- function(path) {
  value <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
  iconv(value, from = "", to = "UTF-8", sub = "")
}

collapse_values <- function(value, sep = "; ") {
  value <- unique(value[!is.na(value) & nzchar(value)])
  if (length(value)) paste(value, collapse = sep) else ""
}

write_csv <- function(value, filename) {
  destination <- file.path(out_dir, filename)
  stopifnot(identical(normalizePath(dirname(destination), winslash = "/", mustWork = TRUE), normalizePath(out_dir, winslash = "/", mustWork = TRUE)))
  utils::write.csv(value, destination, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

write_utf8 <- function(value, filename) {
  destination <- file.path(out_dir, filename)
  stopifnot(identical(normalizePath(dirname(destination), winslash = "/", mustWork = TRUE), normalizePath(out_dir, winslash = "/", mustWork = TRUE)))
  writeLines(enc2utf8(value), destination, useBytes = TRUE)
}

previous_inventory <- read.csv(file.path(audit_dir, "01-file-inventory.csv"), check.names = FALSE)
previous_documentation <- read.csv(file.path(audit_dir, "07-documentation-map.csv"), check.names = FALSE)
previous_artifacts <- read.csv(file.path(audit_dir, "08-artifact-audit.csv"), check.names = FALSE)
cleanup <- read.csv(file.path(decision_dir, "08-file-cleanup-decisions.csv"), check.names = FALSE)

relocation_previous <- cleanup[cleanup$decision == "relocate-later", , drop = FALSE]
removal_previous <- cleanup[cleanup$decision == "remove-candidate", , drop = FALSE]
stopifnot(nrow(relocation_previous) == 142L, nrow(removal_previous) == 8L)

tracked_paths <- sort(git_lines(c("ls-files")))
untracked_paths <- sort(git_lines(c("ls-files", "--others", "--exclude-standard")))
ignored_paths <- sort(git_lines(c("ls-files", "--others", "--ignored", "--exclude-standard")))

disk_files <- list.files(root, all.files = TRUE, no.. = TRUE, recursive = TRUE, full.names = TRUE)
disk_files <- disk_files[file.info(disk_files)$isdir %in% FALSE]
disk_paths <- sort(rel_path(disk_files))
disk_paths <- disk_paths[!startsWith(disk_paths, ".git/") & !startsWith(disk_paths, "docs/roadmap/relocation-0.2.0/")]
examined_paths <- sort(unique(c(disk_paths, cleanup$path)))

category_for <- function(path) {
  lower <- tolower(path)
  extension <- tolower(tools::file_ext(path))
  if (grepl("^R/.*[.]R$", path)) return("source")
  if (grepl("^tests/.*/fixtures?(/|$)", path, ignore.case = TRUE)) return("fixture")
  if (grepl("^tests/", path)) return("test")
  if (grepl("^man/", path)) return("man")
  if (grepl("^vignettes/", path)) return("vignette")
  if (grepl("^handbook/", path)) return("handbook")
  if (grepl("^docs/roadmap/", path)) return("roadmap")
  if (grepl("^inst/examples/", path)) return("example")
  if (grepl("^inst/extdata/", path)) return("fixture")
  if (grepl("(^|/)([.]cache|[.]quarto|cache|__pycache__|[.]pytest_cache)(/|$)", path, ignore.case = TRUE)) return("cache")
  if (grepl("(^|/)([.]venv|venv|env|[.]Rproj[.]user)(/|$)", path, ignore.case = TRUE)) return("environment")
  if (grepl("^(cin|downloads|data-raw/downloads)/", path, ignore.case = TRUE)) return("operational-input")
  if (grepl("^cout/", path, ignore.case = TRUE)) return("operational-output")
  if (extension %in% c("nc", "nc4", "cdf")) return("netcdf")
  if (extension %in% c("rds", "rda", "rdata")) return("rds")
  if (extension %in% c("csv", "tsv", "parquet", "feather")) return("table")
  if (extension %in% c("log", "out")) return("log")
  if (extension %in% c("gif", "mp4", "webm")) return("animation")
  if (extension %in% c("png", "jpg", "jpeg", "svg", "tif", "tiff")) return("figure")
  if (extension %in% c("html", "htm")) return("rendered-document")
  if (grepl("^(docs|pkgdown)/", path)) return("website")
  if (grepl("^artifacts/", path, ignore.case = TRUE)) return("operational-output")
  if (basename(path) %in% c("DESCRIPTION", "NAMESPACE", "LICENSE", "LICENSE.md", "NEWS.md", "README.md", "README.Rmd", "_pkgdown.yml", ".gitignore", ".Rbuildignore", ".lintr")) return("configuration")
  "other"
}

is_generated <- function(path, category) {
  category %in% c("rendered-document", "log", "cache") ||
    (category %in% c("figure", "animation", "website", "operational-output") && grepl("^(artifacts|docs|pkgdown|cout)/", path, ignore.case = TRUE)) ||
    path == "README.md" || grepl("(^|/)(doc|Meta)/", path)
}

is_operational <- function(path, category) {
  grepl("^(artifacts|cin|cout|downloads|data-raw/downloads)/", path, ignore.case = TRUE) ||
    (category %in% c("netcdf", "rds", "table") && !grepl("^(inst/extdata|tests/.*/fixtures?)/", path, ignore.case = TRUE))
}

lookup_cleanup <- function(path) {
  row <- cleanup[cleanup$path == path, , drop = FALSE]
  if (nrow(row)) row[1, , drop = FALSE] else NULL
}

file_state_rows <- lapply(examined_paths, function(path) {
  full <- file.path(root, path)
  exists <- file.exists(full) && !dir.exists(full)
  category <- category_for(path)
  prior <- lookup_cleanup(path)
  tracked <- path %in% tracked_paths
  untracked <- path %in% untracked_paths
  ignored <- path %in% ignored_paths
  generated <- is_generated(path, category)
  operational <- is_operational(path, category)
  required_package <- tracked && (category %in% c("source", "man", "fixture") || basename(path) %in% c("DESCRIPTION", "NAMESPACE", "LICENSE", "LICENSE.md"))
  required_tests <- category %in% c("test", "fixture") || (!is.null(prior) && identical(prior$required_for_tests[[1]], TRUE))
  required_docs <- category %in% c("man", "vignette", "handbook", "roadmap", "example", "website") || basename(path) %in% c("README.md", "README.Rmd", "NEWS.md", "_pkgdown.yml")
  previous_decision <- if (!is.null(prior)) prior$decision[[1]] else "not-previously-classified"
  verified <- if (!exists) "review-missing" else if (required_package || required_tests || required_docs) "retain" else if (!tracked && ignored && (generated || operational)) "leave-local" else if (previous_decision == "remove-candidate") "review-remove-candidate" else if (previous_decision == "relocate-later") "review-relocation" else if (tracked) "retain" else "review"
  evidence <- collapse_values(c(
    if (tracked) "git ls-files: tracked" else "git ls-files: not tracked",
    if (untracked) "git ls-files --others: untracked" else "not in non-ignored untracked inventory",
    if (ignored) "git ignored inventory: ignored" else "not in ignored inventory",
    if (exists) "filesystem: exists" else "filesystem: missing",
    paste0("category: ", category),
    if (generated) "generated-path/type evidence" else "not established as generated",
    if (operational) "operational location/type evidence" else "not classified as operational"
  ))
  data.frame(
    path = path,
    tracked = tracked,
    untracked = untracked,
    ignored = ignored,
    exists = exists,
    size_bytes = if (exists) unname(file.info(full)$size) else NA_real_,
    category = category,
    generated = generated,
    operational = operational,
    required_for_package = required_package,
    required_for_tests = required_tests,
    required_for_documentation = required_docs,
    previous_decision = previous_decision,
    verified_decision = verified,
    evidence = evidence,
    notes = if (!exists) "Previous inventory path is no longer present; no action without renewed evidence." else if (!tracked && ignored && operational) "Local operational file: do not use git mv." else "",
    check.names = FALSE
  )
})
file_state <- do.call(rbind, file_state_rows)
write_csv(file_state, "01-file-state.csv")

candidate_paths <- sort(unique(c(relocation_previous$path, removal_previous$path)))

search_roots <- c("R", "tests", "man", "vignettes", "inst", "README.md", "README.Rmd", "NEWS.md", "DESCRIPTION", "NAMESPACE", "_pkgdown.yml", ".github", "docs", "handbook")
search_files <- unlist(lapply(search_roots, function(path) {
  full <- file.path(root, path)
  if (!file.exists(full)) return(character())
  if (!dir.exists(full)) return(full)
  list.files(full, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
}), use.names = FALSE)
search_files <- unique(search_files[file.exists(search_files) & !file.info(search_files)$isdir])
search_rel <- rel_path(search_files)
keep_text <- tolower(tools::file_ext(search_files)) %in% c("r", "rmd", "qmd", "md", "rd", "txt", "csv", "tsv", "yml", "yaml", "json", "html", "css", "js", "toml", "gitignore", "rbuildignore") | basename(search_files) %in% c("DESCRIPTION", "NAMESPACE", ".gitignore", ".Rbuildignore")
keep_size <- file.info(search_files)$size <= 5e6
keep_scope <- !startsWith(search_rel, "docs/roadmap/relocation-0.2.0/")
search_files <- search_files[keep_text & keep_size & keep_scope]
search_rel <- rel_path(search_files)
search_lines <- setNames(lapply(search_files, read_text), search_rel)

reference_type_for <- function(path) {
  if (grepl("^R/", path)) return("source")
  if (grepl("^tests/", path)) return("test")
  if (grepl("^docs/roadmap/", path)) return("roadmap")
  if (grepl("^[.]github/", path)) return("workflow")
  if (grepl("^inst/examples/", path)) return("script")
  if (basename(path) %in% c("DESCRIPTION", "NAMESPACE", "_pkgdown.yml", ".gitignore", ".Rbuildignore")) return("configuration")
  if (grepl("^(man|vignettes|handbook|docs)/", path) || grepl("^(README|NEWS)", basename(path))) return("documentation")
  "unknown"
}

reference_rows <- list()
for (target in candidate_paths) {
  target_name <- basename(target)
  found <- FALSE
  for (reference_file in names(search_lines)) {
    if (identical(reference_file, target)) next
    lines <- search_lines[[reference_file]]
    exact_hits <- which(grepl(target, lines, fixed = TRUE) | grepl(gsub("/", "\\\\", target, fixed = TRUE), lines, fixed = TRUE))
    basename_hits <- if (nchar(target_name) >= 6L) which(grepl(target_name, lines, fixed = TRUE)) else integer()
    hits <- sort(unique(c(exact_hits, basename_hits)))
    if (!length(hits)) next
    found <- TRUE
    type <- reference_type_for(reference_file)
    for (line_number in hits) {
      exact <- line_number %in% exact_hits
      basename_reference <- line_number %in% basename_hits
      active <- type != "roadmap"
      reference_rows[[length(reference_rows) + 1L]] <- data.frame(
        target_path = target,
        target_name = target_name,
        reference_file = reference_file,
        reference_line = line_number,
        reference_type = type,
        exact_path_reference = exact,
        basename_reference = basename_reference,
        dynamic_reference_possible = !active && category_for(target) %in% c("netcdf", "rds", "table", "cache", "operational-input", "operational-output"),
        impact_if_moved = if (!active) "Roadmap/audit evidence only; no runtime update." else if (exact) "Exact path would break and must be updated." else "Basename reference requires contextual review.",
        required_update = active && (exact || basename_reference),
        evidence = paste0(reference_file, ":", line_number, " literal match"),
        notes = if (type == "roadmap") "Inventory or decision record; retain historical path evidence." else "Do not edit in this planning phase.",
        check.names = FALSE
      )
    }
  }
  if (!found) {
    reference_rows[[length(reference_rows) + 1L]] <- data.frame(
      target_path = target, target_name = target_name, reference_file = "", reference_line = NA_integer_,
      reference_type = "unknown", exact_path_reference = FALSE, basename_reference = FALSE,
      dynamic_reference_possible = category_for(target) %in% c("netcdf", "rds", "table", "cache", "operational-input", "operational-output"),
      impact_if_moved = "No literal reference found; dynamic use remains possible.", required_update = FALSE,
      evidence = "No exact-path or basename literal in searched scopes.", notes = "Static absence is not proof of no external or dynamic use.", check.names = FALSE
    )
  }
}
reference_map <- do.call(rbind, reference_rows)
write_csv(reference_map, "04-reference-map.csv")

active_references_for <- function(path) {
  rows <- reference_map[reference_map$target_path == path & reference_map$reference_type != "roadmap" & nzchar(reference_map$reference_file), , drop = FALSE]
  unique(rows$reference_file)
}

relocation_rows <- lapply(seq_len(nrow(relocation_previous)), function(i) {
  prior <- relocation_previous[i, ]
  state <- file_state[file_state$path == prior$path, , drop = FALSE][1, ]
  active_refs <- active_references_for(prior$path)
  reproducible <- identical(prior$reproducible, TRUE)
  required <- state$required_for_package || state$required_for_tests || state$required_for_documentation
  action <- "review"
  destination_type <- "undetermined"
  destination <- "human decision required"
  safe <- FALSE
  if (!state$exists) {
    action <- "review"
  } else if (!state$tracked && state$ignored && (state$operational || state$generated)) {
    action <- "leave-local"
    destination_type <- "local-ignored"
    destination <- prior$path
  } else if (required) {
    action <- "retain"
    destination_type <- "inside-repository"
    destination <- prior$path
  } else if (state$tracked && state$generated && reproducible && !length(active_refs)) {
    action <- "relocate"
    destination_type <- "archive"
    destination <- paste0("archive/generated/", prior$path)
    safe <- TRUE
  } else if (state$tracked && state$operational) {
    action <- "review"
    destination_type <- "outside-repository"
    destination <- paste0("external-archive/oceancube-legacy/", basename(prior$path))
  }
  data.frame(
    current_path = prior$path,
    tracked = state$tracked,
    exists = state$exists,
    category = state$category,
    size_bytes = state$size_bytes,
    generated = state$generated,
    reproducible = reproducible,
    required_for_package = state$required_for_package,
    required_for_tests = state$required_for_tests,
    required_for_documentation = state$required_for_documentation,
    referenced_by = collapse_values(active_refs),
    previous_destination = prior$destination,
    proposed_destination = destination,
    destination_type = destination_type,
    recommended_action = action,
    safe_for_batch_1 = safe,
    approval_required = action %in% c("relocate", "merge-later", "review", "remove-candidate"),
    risk = if (safe) "low" else if (action == "leave-local") "low: no movement" else if (state$tracked && state$operational) "high: provenance and external consumers unknown" else "medium",
    evidence = collapse_values(c(state$evidence, if (length(active_refs)) paste0("active references: ", collapse_values(active_refs)) else "no active literal reference", paste0("previous decision: ", prior$decision))),
    notes = if (action == "leave-local") "Ignored operational files must not be moved with Git." else if (state$tracked && state$operational) "Preserve until checksum, provenance, license, owner and destination retention policy are approved." else "",
    check.names = FALSE
  )
})
relocation_candidates <- do.call(rbind, relocation_rows)
write_csv(relocation_candidates, "02-relocation-candidates.csv")

removal_rows <- lapply(seq_len(nrow(removal_previous)), function(i) {
  prior <- removal_previous[i, ]
  state <- file_state[file_state$path == prior$path, , drop = FALSE][1, ]
  refs <- reference_map[reference_map$target_path == prior$path, , drop = FALSE]
  code_refs <- unique(refs$reference_file[refs$reference_type == "source"])
  test_refs <- unique(refs$reference_file[refs$reference_type == "test"])
  doc_refs <- unique(refs$reference_file[refs$reference_type %in% c("documentation", "roadmap")])
  config_refs <- unique(refs$reference_file[refs$reference_type %in% c("configuration", "workflow", "script")])
  active_refs <- unique(refs$reference_file[refs$reference_type != "roadmap" & nzchar(refs$reference_file)])
  eligible <- state$exists && !length(code_refs) && !length(test_refs) && !length(config_refs) &&
    !length(setdiff(doc_refs, refs$reference_file[refs$reference_type == "roadmap"])) &&
    identical(prior$reproducible, TRUE) && !state$required_for_package && !state$required_for_tests && !state$required_for_documentation
  decision <- if (eligible) "remove-after-approval" else "review"
  data.frame(
    path = prior$path,
    tracked = state$tracked,
    exists = state$exists,
    category = state$category,
    reason_previously_flagged = "Ignored, untracked and reproducible generated cache; candidate only.",
    duplicate_of = if (eligible) "reproducible dependency cache; no canonical repository copy required" else "",
    generated = state$generated,
    reproducible = identical(prior$reproducible, TRUE),
    referenced_by_code = collapse_values(code_refs),
    referenced_by_tests = collapse_values(test_refs),
    referenced_by_docs = collapse_values(doc_refs),
    referenced_by_config = collapse_values(config_refs),
    required_for_build = FALSE,
    required_for_check = FALSE,
    historical_value = if (eligible) "none established; third-party build cache" else "undetermined",
    recommended_decision = decision,
    approval_required = TRUE,
    evidence = collapse_values(c(state$evidence, if (length(active_refs)) paste0("active references: ", collapse_values(active_refs)) else "no active literal reference", "regenerable by pkgdown/sass dependency cache")),
    notes = if (eligible) "Do not delete in this phase. A later prompt must list exact paths and recheck existence and references." else "Insufficient evidence for removal recommendation.",
    check.names = FALSE
  )
})
removal_review <- do.call(rbind, removal_rows)
write_csv(removal_review, "03-remove-candidate-review.csv")

stopifnot(
  nrow(relocation_candidates) == 142L,
  nrow(removal_review) == 8L,
  setequal(relocation_candidates$current_path, relocation_previous$path),
  setequal(removal_review$path, removal_previous$path)
)

operational_policy <- c(
  "---",
  "title: \"oceancube 0.2.0 operational data and generated artifact policy\"",
  "output: html_document",
  "---",
  "",
  "# Purpose",
  "",
  "This policy separates local operational data, package fixtures, development results and rendered documentation. Location, provenance, references and build role determine treatment; file extension alone never does.",
  "",
  "## Local operational data",
  "",
  "The directories cin/, cout/, downloads/, data-raw/downloads/ and artifacts/ remain outside version control and outside the source-package tarball. They are recreated by documented scripts or external acquisition processes, never moved with git mv, and never confused with small package fixtures. Existing ignored and untracked files stay local unless a separate non-Git archival workflow is approved.",
  "",
  "Operational inputs require an owner, upstream identifier, acquisition date, checksum, license and recreation command. Operational outputs require the producing script, software version, parameters and input provenance. Neither category is a package source of truth.",
  "",
  "## Package fixtures",
  "",
  "Fixtures belong in inst/extdata/ or tests/testthat/fixtures/. They may be small NetCDF, JSON, CSV, RDS or image files when directly required by tests or examples. Every fixture must be tracked, minimal, redistributable, licensed, provenance-documented and referenced by an executable test or example. Large or operational datasets must not be relabeled as fixtures merely to place them in Git.",
  "",
  "## Development results",
  "",
  "Artifacts, logs, cache and temporary directories remain ignored. They are not sources of truth and do not enter future commits. Reproducible caches may become remove-after-approval candidates only after an exact-path recheck confirms that code, tests, documentation, configuration, build and check do not require them.",
  "",
  "## Rendered documentation",
  "",
  "- Rmd or Qmd source is canonical for narrative documentation.",
  "- Markdown is tracked only when it is the reviewed distribution format or an intentional generated README.",
  "- HTML is generated publication output and is not edited as source.",
  "- Generated images belong with their canonical Rmd/Qmd source only when publication requires tracking them.",
  "- The pkgdown site is generated from roxygen, vignettes and configuration.",
  "- docs/roadmap stores decisions and evidence, not runtime documentation.",
  "",
  "The tracked handbook/ source and any published docs/ site are not approved for deletion in this phase. Consolidation requires a separate parity review.",
  "",
  "## Decision gate",
  "",
  "No row in this proposal authorizes movement or deletion. Every external archive, Git move or cache removal requires an explicit path list, current checksums, reference updates, tests and human approval."
)
write_utf8(operational_policy, "05-operational-data-policy.Rmd")

leave_local_paths <- relocation_candidates$current_path[relocation_candidates$recommended_action == "leave-local"]
safe_paths <- relocation_candidates$current_path[relocation_candidates$safe_for_batch_1]
tracked_review_paths <- relocation_candidates$current_path[
  relocation_candidates$tracked & relocation_candidates$recommended_action == "review"
]
remove_paths <- removal_review$path

batch_row <- function(id, name, sequence, path, destination, action, reason, tracked, references,
                      updates, tests, rollback, risk, approval, notes = "") {
  data.frame(
    batch_id = id,
    batch_name = name,
    sequence = sequence,
    current_path = path,
    proposed_destination = destination,
    action = action,
    reason = reason,
    tracked = tracked,
    references_found = references,
    updates_required = updates,
    tests_required = tests,
    rollback_method = rollback,
    risk = risk,
    approval_required = approval,
    notes = notes,
    check.names = FALSE
  )
}

batch_rows <- list()
for (path in leave_local_paths) {
  row <- relocation_candidates[relocation_candidates$current_path == path, , drop = FALSE][1, ]
  batch_rows[[length(batch_rows) + 1L]] <- batch_row(
    "B00-no-action-operational", "No action for ignored operational data", 0L, path, path,
    "leave-local", "Untracked ignored operational/generated content must remain outside Git.",
    row$tracked, nzchar(row$referenced_by), "none", "none; no repository change",
    "not applicable; file remains in place", "low", FALSE,
    "Inventory only. Do not use git mv."
  )
}

if (length(safe_paths)) {
  for (path in safe_paths) {
    row <- relocation_candidates[relocation_candidates$current_path == path, , drop = FALSE][1, ]
    batch_rows[[length(batch_rows) + 1L]] <- batch_row(
      "B01-safe-generated-artifacts", "Safe generated artifacts", 1L, path, row$proposed_destination,
      "relocate", "Tracked, generated, reproducible and unreferenced with a defined reversible destination.",
      row$tracked, nzchar(row$referenced_by), "update exact references if discovered at execution time",
      "build/check inventory and targeted reference scan", "git mv destination back to original path",
      "low", TRUE
    )
  }
} else {
  batch_rows[[length(batch_rows) + 1L]] <- batch_row(
    "B01-safe-generated-artifacts", "Safe generated artifacts", 1L, "", "", "none",
    "No candidate satisfies every B01 safety condition.", FALSE, FALSE, "none", "none",
    "not applicable", "none", TRUE, "Intentionally empty; do not force a relocation."
  )
}

batch_rows[[length(batch_rows) + 1L]] <- batch_row(
  "B02-development-scripts", "Development scripts", 2L, "", "", "review",
  "No development script is approved for relocation by the 142 prior rows.", FALSE, FALSE,
  "future path-by-path review", "script-specific smoke tests", "not applicable until a path is approved",
  "medium", TRUE, "Empty planning batch."
)
batch_rows[[length(batch_rows) + 1L]] <- batch_row(
  "B03-duplicated-documentation", "Duplicated documentation", 3L, "", "", "review",
  "No tracked duplicated documentation is safe to move without canonical-source parity.", FALSE, FALSE,
  "link and canonical-source updates", "render and link checks", "not applicable until a path is approved",
  "medium", TRUE, "The generated docs/handbook worktree was resolved separately; tracked documentation remains untouched."
)
batch_rows[[length(batch_rows) + 1L]] <- batch_row(
  "B04-handbook-review", "Handbook review", 4L, "handbook/", "vignettes/ topic by topic", "review",
  "Handbook consolidation requires content parity and is outside this relocation proposal.", TRUE, TRUE,
  "topic links and published-site navigation", "render, executable examples and link checks",
  "restore the prior handbook source from Git", "high", TRUE, "No handbook move or deletion is approved."
)

if (length(tracked_review_paths)) {
  for (path in tracked_review_paths) {
    row <- relocation_candidates[relocation_candidates$current_path == path, , drop = FALSE][1, ]
    batch_rows[[length(batch_rows) + 1L]] <- batch_row(
      "B05-example-and-fixture-review", "Example and fixture review", 5L, path, row$proposed_destination,
      "review", "Tracked legacy data needs provenance, license, ownership and consumer review before classification.",
      TRUE, nzchar(row$referenced_by), "update consumers only after destination approval",
      "build/check, examples and data-load smoke tests", "restore from Git and archive checksum",
      "high", TRUE
    )
  }
} else {
  batch_rows[[length(batch_rows) + 1L]] <- batch_row(
    "B05-example-and-fixture-review", "Example and fixture review", 5L, "", "", "review",
    "No tracked data row requires this batch.", FALSE, FALSE, "none", "none", "not applicable",
    "none", TRUE, "Empty planning batch."
  )
}

for (path in remove_paths) {
  row <- removal_review[removal_review$path == path, , drop = FALSE][1, ]
  batch_rows[[length(batch_rows) + 1L]] <- batch_row(
    "B06-remove-candidate-review", "Generated cache removal review", 6L, path, "",
    row$recommended_decision, "Reproducible untracked cache with no active repository consumer found.",
    row$tracked,
    nzchar(collapse_values(c(row$referenced_by_code, row$referenced_by_tests, row$referenced_by_config))),
    "none when the final pre-delete reference scan remains empty",
    "re-run package documentation/site build after explicit approval",
    "regenerate pkgdown or sass cache from canonical inputs", "low", TRUE,
    "Deletion is not approved now; future command must use the exact literal path."
  )
}

relocation_batches <- do.call(rbind, batch_rows)
write_csv(relocation_batches, "06-relocation-batches.csv")

mapping_rows <- list()
for (i in seq_len(nrow(relocation_candidates))) {
  row <- relocation_candidates[i, ]
  batch <- if (row$recommended_action == "leave-local") "B00-no-action-operational" else if (row$safe_for_batch_1) "B01-safe-generated-artifacts" else if (row$tracked && row$category %in% c("rds", "netcdf", "fixture", "table")) "B05-example-and-fixture-review" else "B02-development-scripts"
  operation <- if (row$recommended_action == "relocate" && row$destination_type == "inside-repository") "git-mv" else if (row$recommended_action == "relocate" && row$destination_type == "outside-repository") "copy-then-verify" else if (row$recommended_action == "relocate" && row$destination_type == "archive") "archive" else "none"
  mapping_rows[[length(mapping_rows) + 1L]] <- data.frame(
    current_path = row$current_path,
    proposed_path = row$proposed_destination,
    action = row$recommended_action,
    reason = if (row$recommended_action == "leave-local") "Operational or generated ignored file remains at its local path." else if (row$recommended_action == "review") "Destination and provenance are not yet approved." else "Evidence-supported proposal subject to approval.",
    source_of_truth_after_move = if (row$recommended_action == "leave-local") "external upstream input or generating script" else if (row$recommended_action == "retain") row$current_path else "human-approved archive plus provenance manifest",
    references_to_update = row$referenced_by,
    git_operation_proposed = operation,
    commit_batch = batch,
    rollback_command = if (operation == "git-mv") paste0("git mv -- ", row$proposed_destination, " ", row$current_path) else if (operation %in% c("archive", "copy-then-verify")) "restore verified archive copy to original relative path" else "none",
    approved_now = FALSE,
    human_decision = if (row$approval_required) "approve destination and exact path action" else "accept no-action classification",
    notes = row$notes,
    check.names = FALSE
  )
}

for (i in seq_len(nrow(removal_review))) {
  row <- removal_review[i, ]
  mapping_rows[[length(mapping_rows) + 1L]] <- data.frame(
    current_path = row$path,
    proposed_path = "",
    action = row$recommended_decision,
    reason = "Reproducible generated cache; exact deletion remains subject to a new approval.",
    source_of_truth_after_move = "canonical package sources and reproducible pkgdown/sass dependency cache",
    references_to_update = collapse_values(c(row$referenced_by_code, row$referenced_by_tests, row$referenced_by_docs, row$referenced_by_config)),
    git_operation_proposed = if (row$recommended_decision == "remove-after-approval") "delete-after-approval" else "none",
    commit_batch = "B06-remove-candidate-review",
    rollback_command = "regenerate cache from canonical package and site sources",
    approved_now = FALSE,
    human_decision = "approve or reject exact cache removal",
    notes = row$notes,
    check.names = FALSE
  )
}
path_mapping <- do.call(rbind, mapping_rows)
write_csv(path_mapping, "07-path-mapping.csv")

rbuild_patterns <- read_text(file.path(root, ".Rbuildignore"))
rbuild_patterns <- rbuild_patterns[nzchar(rbuild_patterns) & !startsWith(trimws(rbuild_patterns), "#")]
rbuild_excluded <- function(path) {
  any(vapply(rbuild_patterns, function(pattern) {
    tryCatch(grepl(pattern, path, perl = TRUE), error = function(e) FALSE)
  }, logical(1)))
}

compatibility_rows <- lapply(seq_len(nrow(path_mapping)), function(i) {
  row <- path_mapping[i, ]
  state <- file_state[file_state$path == row$current_path, , drop = FALSE][1, ]
  destination <- row$proposed_path
  external <- startsWith(destination, "external-archive/") || startsWith(destination, "archive/")
  destination_ignored <- if (!nzchar(destination) || external) "not-applicable" else if (destination %in% ignored_paths || row$action == "leave-local") "ignored" else "not-ignored"
  destination_build <- if (!nzchar(destination) || external) "not-applicable" else if (rbuild_excluded(destination)) "excluded" else "included"
  tracked_risk <- if (state$tracked && row$git_operation_proposed == "none" && row$action %in% c("review", "retain")) "none until action is approved" else if (state$tracked) "tracked path requires reversible Git operation" else "no tracked-file risk"
  build_risk <- if (state$required_for_package || state$required_for_tests) "high: required content must remain available" else if (state$operational && destination_build == "included") "high: operational data could enter source package" else "low"
  decision <- if (state$required_for_package && destination_ignored == "ignored") "reject" else if (state$operational && destination_build == "included") "reject" else if (row$action %in% c("review", "remove-after-approval")) "review" else "compatible"
  data.frame(
    path = row$current_path,
    current_gitignore_status = if (state$ignored) "ignored" else if (state$tracked) "tracked-not-ignored" else "not-ignored",
    current_rbuildignore_status = if (rbuild_excluded(row$current_path)) "excluded" else "included",
    proposed_destination = destination,
    destination_gitignore_status = destination_ignored,
    destination_rbuildignore_status = destination_build,
    tracked_file_risk = tracked_risk,
    package_build_risk = build_risk,
    decision = decision,
    notes = if (decision == "reject") "Do not execute this mapping." else if (decision == "review") "Recheck ignore/build rules at execution time." else "Current no-action or proposed mapping does not expose required content.",
    check.names = FALSE
  )
})
ignore_build_compatibility <- do.call(rbind, compatibility_rows)
write_csv(ignore_build_compatibility, "08-ignore-build-compatibility.csv")

stopifnot(
  all(path_mapping$approved_now == FALSE),
  all(path_mapping$git_operation_proposed %in% c("none", "git-mv", "copy-then-verify", "archive", "delete-after-approval")),
  all(relocation_candidates$destination_type %in% c("inside-repository", "outside-repository", "local-ignored", "archive", "undetermined")),
  all(relocation_candidates$recommended_action %in% c("retain", "relocate", "leave-local", "merge-later", "review", "remove-candidate")),
  all(removal_review$recommended_decision %in% c("retain", "archive", "merge-later", "review", "remove-after-approval"))
)

batch_ids <- c(
  "B00-no-action-operational", "B01-safe-generated-artifacts", "B02-development-scripts",
  "B03-duplicated-documentation", "B04-handbook-review",
  "B05-example-and-fixture-review", "B06-remove-candidate-review"
)

batch_objective <- c(
  "Keep ignored, untracked operational data in place and outside Git.",
  "Relocate only tracked, generated, reproducible, unreferenced artifacts with a reversible destination.",
  "Review development scripts individually before any location change.",
  "Consolidate only documentation with an approved canonical source and content parity.",
  "Preserve the tracked handbook pending topic-level vignette parity.",
  "Resolve provenance and ownership of tracked legacy data before classifying it as fixture or archive material.",
  "Remove only explicitly approved reproducible caches after a fresh reference and existence check."
)
names(batch_objective) <- batch_ids

batch_commit <- c(
  "none: no repository action",
  "chore: relocate approved generated artifacts",
  "chore: relocate approved development scripts",
  "docs: consolidate approved duplicate documentation",
  "docs: migrate one approved handbook topic",
  "chore: archive approved legacy data fixture",
  "chore: remove approved generated cache files"
)
names(batch_commit) <- batch_ids

batch_section <- function(id) {
  rows <- relocation_batches[relocation_batches$batch_id == id, , drop = FALSE]
  paths <- rows$current_path[nzchar(rows$current_path)]
  destinations <- unique(rows$proposed_destination[nzchar(rows$proposed_destination)])
  references <- unique(c(
    reference_map$reference_file[
      reference_map$target_path %in% paths &
        reference_map$required_update &
        nzchar(reference_map$reference_file)
    ]
  ))
  commands <- if (!length(paths) || all(rows$action %in% c("none", "review", "leave-local"))) {
    "- none; this batch is not executable without a new approval"
  } else if (id == "B06-remove-candidate-review") {
    paste0("- Remove-Item -LiteralPath '", paths, "' -Force")
  } else {
    paste0("- proposed operation for ", paths, " -> ", rows$proposed_destination[nzchar(rows$current_path)])
  }
  c(
    paste0("## ", id),
    "",
    paste0("- Objective: ", batch_objective[[id]]),
    "- Prerequisites: clean dev-0.2.0 tree; exact current path inventory; current checksums; human approval; protected refs unchanged.",
    paste0("- Destinations: ", if (length(destinations)) collapse_values(destinations) else "none approved"),
    paste0("- References requiring updates: ", if (length(references)) collapse_values(references) else "none established; re-scan at execution time"),
    "- Files authorized to modify: only exact paths listed below and explicitly enumerated reference files after separate approval.",
    "- Files prohibited: DESCRIPTION, NAMESPACE, R/, tests/, man/, vignettes/, inst/, handbook/, .gitignore and .Rbuildignore unless a later prompt names one explicitly.",
    "- Proposed Git commands: planning text only; do not execute in this phase.",
    commands,
    "- Pre-validation: git status, git ls-files, git check-ignore, Rbuildignore evaluation, SHA-256, exact/basename reference search.",
    paste0("- Required tests: ", collapse_values(unique(rows$tests_required))),
    "- Acceptance: only approved paths change; checksums/provenance preserved; required content remains available; tests pass; clean final tree.",
    paste0("- Rollback: ", collapse_values(unique(rows$rollback_method))),
    paste0("- Proposed commit message: ", batch_commit[[id]]),
    "",
    "### Exact file list",
    "",
    if (length(paths)) paste0("- ", paths) else "- none",
    ""
  )
}

execution_plan <- c(
  "---",
  "title: \"oceancube 0.2.0 reversible artifact relocation execution plan\"",
  "output: html_document",
  "---",
  "",
  "# Execution boundary",
  "",
  paste0("This plan was prepared from clean commit ", starting_commit, ". It authorizes no move, copy, archive or deletion. Every future batch begins from a clean tree and is independently approved, tested and reversible."),
  "",
  unlist(lapply(batch_ids, batch_section), use.names = FALSE),
  "# Recommended first executable batch",
  "",
  "B01 is intentionally empty because no tracked relocation candidate satisfies every safety condition. The smallest executable future cleanup is B06-remove-candidate-review: eight ignored, untracked, reproducible cache files. It is low risk and non-functional, but still requires explicit human approval and a new exact-path reference check. It creates no Git deletion because the files are untracked; the proposed commit message applies only if tracked policy/evidence files are separately authorized.",
  "",
  "# Stop condition",
  "",
  "Stop if a path is missing, tracked state changes, a new active reference is found, a checksum differs, a destination is not approved, or the diff includes any path outside the approved batch."
)
write_utf8(execution_plan, "09-relocation-execution-plan.Rmd")

count_values <- function(values, levels) {
  result <- setNames(integer(length(levels)), levels)
  tab <- table(values)
  result[names(tab)] <- as.integer(tab)
  result
}

relocation_counts <- count_values(
  relocation_candidates$recommended_action,
  c("retain", "relocate", "leave-local", "merge-later", "review", "remove-candidate")
)
removal_counts <- count_values(
  removal_review$recommended_decision,
  c("retain", "archive", "merge-later", "review", "remove-after-approval")
)
existing_state <- file_state[file_state$exists, , drop = FALSE]
files_examined <- nrow(existing_state)
tracked_count <- sum(existing_state$tracked)
untracked_count <- sum(existing_state$untracked)
ignored_count <- sum(existing_state$ignored)
missing_previous <- sum(!file_state$exists)
safe_batch_count <- sum(relocation_candidates$safe_for_batch_1)
active_reference_rows <- reference_map[
  reference_map$reference_type != "roadmap" & nzchar(reference_map$reference_file), ,
  drop = FALSE
]
candidates_with_references <- length(unique(active_reference_rows$target_path))
references_requiring_updates <- sum(reference_map$required_update)
dynamic_reference_risks <- length(unique(reference_map$target_path[reference_map$dynamic_reference_possible]))
human_approvals <- sum(relocation_candidates$approval_required) + nrow(removal_review)

proposal_report <- c(
  "---",
  "title: \"oceancube 0.2.0 artifact relocation proposal\"",
  "output: html_document",
  "---",
  "",
  "# 1. Initial state",
  "",
  paste0("Analysis began on branch ", branch, " at ", starting_commit, " with latest message ", starting_message, ". The tracked and untracked working tree was clean. main and v0.1.0 both remained at ", protected_main, "."),
  "",
  "# 2. Methodology",
  "",
  "The proposal reconciles Git tracked, untracked and ignored inventories with current disk state and the prior audit/decision tables. Classification uses path, tracked state, package/test/documentation role, generation evidence, operational location, exact-path references and basename references. Extension alone never authorizes relocation or deletion.",
  "",
  "# 3. Real file state",
  "",
  paste0(files_examined, " existing files were examined: ", tracked_count, " tracked, ", untracked_count, " non-ignored untracked and ", ignored_count, " ignored. ", missing_previous, " historical inventory paths are absent and remain represented as missing review evidence rather than actionable files."),
  "",
  "# 4. Tracked, ignored and operational distinctions",
  "",
  "Tracked files require Git-aware, reversible operations. Untracked ignored operational files remain local and are never moved with Git. Generated output is not automatically disposable. Package fixtures remain tracked and minimal; operational inputs and outputs remain outside the package and require upstream or generation provenance.",
  "",
  "# 5. Review of 142 relocation rows",
  "",
  paste0("All 142 prior relocate-later rows were rechecked. Results: ", relocation_counts[["relocate"]], " verified relocate, ", relocation_counts[["leave-local"]], " leave-local, ", relocation_counts[["merge-later"]], " merge-later, ", relocation_counts[["retain"]], " retain and ", relocation_counts[["review"]], " review. The 140 leave-local files are ignored and untracked operational/generated content. The two review rows are tracked legacy RData under auxdata/ with no approved provenance or destination."),
  "",
  "# 6. Review of eight removal candidates",
  "",
  paste0("All eight prior candidates are ignored, untracked pkgdown/sass cache files. ", removal_counts[["remove-after-approval"]], " meet the evidence threshold for remove-after-approval and ", removal_counts[["review"]], " remain review. None is deleted or approved now; execution requires a fresh existence and reference check."),
  "",
  "# 7. References",
  "",
  paste0(candidates_with_references, " candidate paths have non-roadmap literal references and ", references_requiring_updates, " reference rows would require future updates if their targets moved. ", dynamic_reference_risks, " targets retain a dynamic-reference risk because static analysis cannot observe external scripts, constructed paths, load(), get() or other runtime indirection."),
  "",
  "# 8. Operational data policy",
  "",
  "cin/, cout/, downloads/, data-raw/downloads/ and artifacts/ remain local, ignored and outside the source package. They are recreated or reacquired and never moved with git mv.",
  "",
  "# 9. Fixture policy",
  "",
  "Only small, licensed, provenance-documented files directly used by tests or examples belong in inst/extdata/ or tests/testthat/fixtures/. NetCDF, JSON, CSV, RDS and images are allowed there when their role—not their extension—justifies tracking.",
  "",
  "# 10. Rendered documentation policy",
  "",
  "Rmd/Qmd and roxygen are canonical sources; Markdown, HTML, images and pkgdown output are generated or distribution forms with explicit ownership. No tracked handbook or published docs output is deleted by this proposal.",
  "",
  "# 11. Proposed batches",
  "",
  "Seven batches are defined. B00 records no-action operational paths. B01 is deliberately empty. B02-B05 are review gates for scripts, duplicate docs, handbook and legacy data. B06 contains the eight generated cache candidates but requires approval.",
  "",
  "# 12. Concrete destinations",
  "",
  "No destination is invented for ignored local files; they remain at current paths. The two tracked auxdata RData files have a conceptual external-archive destination only, pending provenance, license, ownership, checksum and retention approval. Cache candidates are regenerated on demand rather than moved.",
  "",
  "# 13. Risks",
  "",
  "- Static search can miss constructed paths, external scripts and runtime loading.",
  "- Legacy RData may contain unique or licensed objects despite no literal consumer.",
  "- Moving ignored operational files can silently break local workflows without producing a Git diff.",
  "- Archiving without checksums, ownership and retention policy can create unrecoverable provenance gaps.",
  "- Documentation consolidation can remove unique scientific context.",
  "- Cache deletion is low risk but still depends on the ability to regenerate the site toolchain.",
  "",
  "# 14. Files that must not be touched",
  "",
  "DESCRIPTION, NAMESPACE, .gitignore, .Rbuildignore, R/, tests/, man/, vignettes/, inst/, handbook/, existing docs/roadmap records, protected refs main and v0.1.0, and every operational path not explicitly approved remain unchanged.",
  "",
  "# 15. Human decisions",
  "",
  paste0(human_approvals, " candidate-level approvals remain: two tracked legacy-data classifications and eight cache-removal decisions. Human decisions must also define archive ownership/retention and confirm the site cache is reproducible in the current toolchain."),
  "",
  "# 16. First batch recommendation",
  "",
  paste0("B01 has ", safe_batch_count, " files and cannot be executed. Recommend B06-remove-candidate-review as the first executable future batch: eight exact untracked cache paths, low risk, non-functional and reversible by regeneration. Approval remains mandatory."),
  "",
  "# 17. Next prompt proposal",
  "",
  "The next prompt should approve or reject B06, repeat branch/clean-tree/existence/tracked/reference checks for the exact eight paths, authorize only explicit literal-path deletion when all checks pass, prohibit git clean and commits unless a tracked policy file is separately authorized, and verify the final tree.",
  "",
  "# Decision",
  "",
  "This proposal approves no file operation. The next authorized phase is to approve or reject the proposed first executable batch."
)
write_utf8(proposal_report, "oceancube-0.2.0-relocation-proposal.Rmd")

readme <- c(
  "# oceancube 0.2.0 — Artifact relocation proposal",
  "",
  "Non-destructive, reproducible review prepared from the clean ignore-rules baseline.",
  "",
  "## File state",
  "",
  paste0("- Files examined: ", files_examined),
  paste0("- Tracked files: ", tracked_count),
  paste0("- Untracked files: ", untracked_count),
  paste0("- Ignored files: ", ignored_count),
  paste0("- Missing historical inventory paths: ", missing_previous),
  "",
  "## Relocation review",
  "",
  "- Relocate-later candidates reviewed: 142",
  paste0("- Verified relocation candidates: ", relocation_counts[["relocate"]]),
  paste0("- Leave-local decisions: ", relocation_counts[["leave-local"]]),
  paste0("- Merge-later decisions: ", relocation_counts[["merge-later"]]),
  paste0("- Retain decisions: ", relocation_counts[["retain"]]),
  paste0("- Review decisions: ", relocation_counts[["review"]]),
  paste0("- Safe B01 files: ", safe_batch_count),
  "",
  "## Removal review",
  "",
  "- Remove candidates reviewed: 8",
  paste0("- Retain: ", removal_counts[["retain"]]),
  paste0("- Archive: ", removal_counts[["archive"]]),
  paste0("- Merge-later: ", removal_counts[["merge-later"]]),
  paste0("- Review: ", removal_counts[["review"]]),
  paste0("- Remove-after-approval recommendations: ", removal_counts[["remove-after-approval"]]),
  "",
  "No movement or deletion is approved in this phase.",
  "",
  "## Reference analysis",
  "",
  paste0("- Candidates with non-roadmap references: ", candidates_with_references),
  paste0("- Reference rows requiring future updates: ", references_requiring_updates),
  paste0("- Dynamic-reference risk targets: ", dynamic_reference_risks),
  "",
  "## Human approvals",
  "",
  paste0("- Candidate-level approvals required: ", human_approvals),
  "- Approve provenance, ownership, license, checksum and archive retention for the two tracked auxdata RData files.",
  "- Approve or reject exact removal of eight reproducible untracked caches after a fresh reference scan.",
  "",
  "## Outputs",
  "",
  "- 01-file-state.csv",
  "- 02-relocation-candidates.csv",
  "- 03-remove-candidate-review.csv",
  "- 04-reference-map.csv",
  "- 05-operational-data-policy.Rmd",
  "- 06-relocation-batches.csv",
  "- 07-path-mapping.csv",
  "- 08-ignore-build-compatibility.csv",
  "- 09-relocation-execution-plan.Rmd",
  "- oceancube-0.2.0-relocation-proposal.Rmd",
  "- generate-relocation-proposal.R",
  "",
  "## Recommended next action",
  "",
  "Approve or reject B06-remove-candidate-review. B01 is intentionally empty because no tracked relocation is currently safe."
)
write_utf8(readme, "README.md")

expected_outputs <- c(
  "01-file-state.csv", "02-relocation-candidates.csv", "03-remove-candidate-review.csv",
  "04-reference-map.csv", "05-operational-data-policy.Rmd", "06-relocation-batches.csv",
  "07-path-mapping.csv", "08-ignore-build-compatibility.csv", "09-relocation-execution-plan.Rmd",
  "oceancube-0.2.0-relocation-proposal.Rmd", "README.md", "generate-relocation-proposal.R"
)
actual_outputs <- sort(basename(list.files(out_dir, full.names = TRUE)))
stopifnot(setequal(expected_outputs, actual_outputs))

cat("RELOCATION_PROPOSAL_GENERATION: PASS\n")
cat("FILES_EXAMINED=", files_examined, "\n", sep = "")
cat("TRACKED=", tracked_count, "\n", sep = "")
cat("UNTRACKED=", untracked_count, "\n", sep = "")
cat("IGNORED=", ignored_count, "\n", sep = "")
cat("RELOCATE=", relocation_counts[["relocate"]], "\n", sep = "")
cat("LEAVE_LOCAL=", relocation_counts[["leave-local"]], "\n", sep = "")
cat("REVIEW=", relocation_counts[["review"]], "\n", sep = "")
cat("REMOVE_AFTER_APPROVAL=", removal_counts[["remove-after-approval"]], "\n", sep = "")
cat("SAFE_B01=", safe_batch_count, "\n", sep = "")
