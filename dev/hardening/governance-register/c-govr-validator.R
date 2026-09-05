args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[[1L]] else "."
root <- normalizePath(root, winslash = "/", mustWork = TRUE)
if (.Platform$OS.type == "windows") {
  invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", ".UTF-8")))
}

fail <- function(message) stop(message, call. = FALSE)
assert <- function(condition, message) if (!isTRUE(condition)) fail(message)
read_repo <- function(path) {
  utils::read.csv(file.path(root, path), stringsAsFactors = FALSE,
                  check.names = FALSE, encoding = "UTF-8")
}

register_path <- "docs/roadmap/post-0.2.0/roadmap-decisions.csv"
register <- read_repo(register_path)
assert(identical(names(register), c(
  "decision_id", "decision", "status", "rationale", "scope", "source",
  "date_recorded", "revisit_trigger"
)), "canonical register schema changed")
assert(!anyDuplicated(register$decision_id), "decision IDs are not unique")

baseline_text <- system2(
  "git",
  c("show", paste0(
    "731ab8b077834499ca29d16595c1744049630590:", register_path
  )),
  stdout = TRUE,
  stderr = TRUE
)
baseline_status <- attr(baseline_text, "status")
assert(is.null(baseline_status) || identical(baseline_status, 0L),
       "could not read the C10 canonical register")
baseline <- utils::read.csv(
  text = paste(baseline_text, collapse = "\n"),
  stringsAsFactors = FALSE,
  check.names = FALSE,
  encoding = "UTF-8"
)
assert(nrow(baseline) == 36L, "C10 register did not contain exactly 36 rows")
assert(nrow(register) == 39L, "reconciled register must contain 39 rows")
assert(identical(register[seq_len(36L), , drop = FALSE], baseline),
       "DEC-001 through DEC-036 were not preserved")

for (id in sprintf("DEC-%03d", 37:39)) {
  assert(sum(register$decision_id == id) == 1L,
         paste(id, "must occur exactly once"))
}
assert(sum(register$decision_id == "DEC-040") == 0L,
       "DEC-040 must remain absent")

expected_status <- c(
  "DEC-037" = "APPROVED — IMPLEMENTED/CERTIFIED C8",
  "DEC-038" = "APPROVED — IMPLEMENTED/CERTIFIED C9",
  "DEC-039" = "APPROVED — IMPLEMENTED/CERTIFIED C10"
)
for (id in names(expected_status)) {
  actual <- register$status[register$decision_id == id]
  assert(identical(actual, unname(expected_status[[id]])),
         paste(id, "has the wrong canonical status"))
}

source_paths <- list(
  "DEC-037" = c(
    "inst/architecture/oceancube-mixed-layer-v1.md",
    "inst/architecture/oceancube-density-teos10-v1.md",
    "dev/hardening/mixed-layer/README.md"
  ),
  "DEC-038" = c(
    "inst/architecture/oceancube-density-teos10-v2.md",
    "dev/hardening/thermodynamic-state/README.md"
  ),
  "DEC-039" = c(
    "inst/architecture/oceancube-density-diagnostics-v1.md",
    "inst/architecture/oceancube-stratification-v1.md",
    "dev/hardening/density-stratification/README.md"
  )
)
for (id in names(source_paths)) {
  source_field <- register$source[register$decision_id == id]
  for (path in source_paths[[id]]) {
    assert(file.exists(file.path(root, path)), paste("missing source", path))
    assert(grepl(path, source_field, fixed = TRUE),
           paste(id, "does not reference", path))
  }
}

evidence <- c(
  "DEC-037" = "dev/hardening/mixed-layer/README.md",
  "DEC-038" = "dev/hardening/thermodynamic-state/README.md",
  "DEC-039" = "dev/hardening/density-stratification/README.md"
)
for (id in names(evidence)) {
  content <- paste(readLines(file.path(root, evidence[[id]]), warn = FALSE,
                             encoding = "UTF-8"), collapse = "\n")
  assert(grepl(id, content, fixed = TRUE),
         paste("phase evidence does not state", id))
}

numbers <- as.integer(sub("DEC-", "", register$decision_id, fixed = TRUE))
assert(identical(sprintf("DEC-%03d", max(numbers) + 1L), "DEC-040"),
       "next available decision ID is not DEC-040")

rbuildignore <- readLines(file.path(root, ".Rbuildignore"), warn = FALSE)
assert(any(rbuildignore == "^dev($|/)"),
       ".Rbuildignore does not contain the exact dev exclusion")
policy_path <- file.path(
  root, "inst/architecture/oceancube-certification-evidence-policy-v1.md"
)
assert(file.exists(policy_path), "distributed evidence policy is missing")
policy <- paste(readLines(policy_path, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
assert(grepl("^dev($|/)", policy, fixed = TRUE),
       "policy does not document the exact dev exclusion")
assert(grepl("repository-only certification evidence", tolower(policy),
             fixed = TRUE),
       "policy does not distinguish repository-only evidence")

evidence_csv <- list.files(
  file.path(root, "dev/hardening/governance-register"),
  pattern = "\\.csv$", full.names = TRUE
)
for (path in evidence_csv) {
  parsed <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                            encoding = "UTF-8")
  assert(nrow(parsed) > 0L, paste("empty evidence CSV", basename(path)))
}

cat("C_GOVR_VALIDATOR: PASS\n")
cat("DECISION_ROWS: 39\n")
cat("NEXT_AVAILABLE_DECISION_ID: DEC-040\n")
