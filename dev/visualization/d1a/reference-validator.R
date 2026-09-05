root <- normalizePath(".", winslash = "/", mustWork = TRUE)

fail <- function(...) stop(..., call. = FALSE)
assert <- function(ok, message) if (!isTRUE(ok)) fail(message)

registry_path <- file.path(root, "dev/references/visualization/visualization-reference-registry.csv")
inventory_path <- file.path(root, "dev/visualization/d1a/global-visualization-inventory.csv")
renderer_path <- file.path(root, "dev/visualization/d1a/renderer-evaluation.csv")
decision_path <- file.path(root, "docs/roadmap/post-0.2.0/roadmap-decisions.csv")
exemplar_path <- file.path(root, "dev/references/visualization/visualization-design-exemplars.csv")

registry <- read.csv(registry_path, check.names = FALSE, stringsAsFactors = FALSE)
inventory <- read.csv(inventory_path, check.names = FALSE, stringsAsFactors = FALSE)
renderer <- read.csv(renderer_path, check.names = FALSE, stringsAsFactors = FALSE)
decisions <- read.csv(decision_path, check.names = FALSE, stringsAsFactors = FALSE)
exemplars <- read.csv(exemplar_path, check.names = FALSE, stringsAsFactors = FALSE)

required_columns <- c(
  "reference_id", "source_type", "authors_or_organization", "year", "title",
  "journal_or_project", "doi", "url", "uploaded_filename", "category",
  "reference_role", "visualization_topics", "decision_informed", "access_date",
  "status", "notes"
)
assert(identical(names(registry), required_columns), "reference registry schema mismatch")
assert(!anyDuplicated(registry$reference_id), "reference_id values must be unique")
assert(all(nzchar(registry$reference_id)), "reference_id must not be empty")

seed_ids <- c(
  "P001", "P002", "P003", "P004", "P005", "P006",
  "D001", "D002", "D003", "D004", "D005", "D006",
  "C001", "C002", sprintf("T%03d", 1:9), sprintf("V%02d", 1:9)
)
assert(all(seed_ids %in% registry$reference_id), "one or more mandatory seed references are absent")
assert(all(registry$reference_role %in% c(
  "PRIMARY", "DOMAIN_REFERENCE", "TOOL_REFERENCE", "DESIGN_EXEMPLAR", "FUTURE_REFERENCE"
)), "invalid reference_role")

has_doi <- nzchar(registry$doi)
has_url <- nzchar(registry$url)
assert(all(grepl("^10\\.[0-9]{4,9}/[^[:space:]]+$", registry$doi[has_doi])), "malformed DOI")
assert(all(grepl("^https?://[^[:space:]]+$", registry$url[has_url])), "malformed URL")

split_refs <- function(x) {
  refs <- trimws(unlist(strsplit(x[nzchar(x)], ";", fixed = TRUE), use.names = FALSE))
  unique(refs[nzchar(refs)])
}
used <- unique(c(split_refs(inventory$reference_ids), split_refs(renderer$reference_ids)))
unknown <- setdiff(used, registry$reference_id)
assert(length(unknown) == 0L, paste("unresolved evidence references:", paste(unknown, collapse = ", ")))

dec041 <- decisions[decisions$decision_id == "DEC-041", , drop = FALSE]
assert(nrow(dec041) == 1L, "DEC-041 must occur exactly once")
decision_ref_tokens <- unique(regmatches(dec041$source, gregexpr("[PDRCTV][0-9]{3}", dec041$source))[[1L]])
assert(length(decision_ref_tokens) > 0L, "DEC-041 must cite persistent reference IDs")
assert(all(decision_ref_tokens %in% registry$reference_id), "DEC-041 contains unresolved reference IDs")

assert(setequal(exemplars$reference_id, sprintf("V%02d", 1:9)), "V01-V09 design exemplars must all be registered")
assert(all(exemplars$classification == "DESIGN_EXEMPLAR"), "exemplars must be classified DESIGN_EXEMPLAR")
assert(all(sprintf("V%02d", 1:9) %in% registry$reference_id), "exemplars must also appear in the main registry")

copied_pdfs <- list.files(
  file.path(root, "dev/references/visualization"),
  pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE
)
assert(length(copied_pdfs) == 0L, "PDF files must not be copied into the visualization registry")

cat("REFERENCE_VALIDATOR: PASS\n")
cat("REFERENCES:", nrow(registry), "\n")
cat("RESOLVED_REFERENCES:", length(used), "\n")
cat("DESIGN_EXEMPLARS: 9\n")
