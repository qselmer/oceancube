root <- normalizePath(".", winslash = "/", mustWork = TRUE)
fail <- function(...) stop(..., call. = FALSE)
assert <- function(ok, message) if (!isTRUE(ok)) fail(message)
read_contract <- function(path) {
  read.csv(file.path(root, path), check.names = FALSE, stringsAsFactors = FALSE,
           fileEncoding = "UTF-8")
}

decisions <- read_contract("docs/roadmap/post-0.2.0/roadmap-decisions.csv")
assert(!anyDuplicated(decisions$decision_id), "decision IDs must be unique")
assert(sum(decisions$decision_id == "DEC-041") == 1L, "DEC-041 count mismatch")
assert(sum(decisions$decision_id == "DEC-042") == 1L, "DEC-042 count mismatch")
assert(sum(decisions$decision_id == "DEC-043") == 1L, "DEC-043 count mismatch")
assert(!any(decisions$decision_id == "DEC-044"), "DEC-044 must remain unallocated")

expected_ids <- c(
  "D2A-HOVMOLLER-TIME-DEPTH",
  "D2A-HOVMOLLER-TIME-LONGITUDE",
  "D2A-HOVMOLLER-TIME-LATITUDE"
)
expected_hashes <- c(
  "0b06899297131e9c28c5bac807ef4d0b91ed7c729e149e6bae523e0d9fc67573",
  "c254dea749cd982c45ef331d326138e75ea61abb1caf0625548a9786e80e47ee",
  "04e84b01dffedfe60ef1d3ec994b4ecda12a8ff97680fa0783a8089de1eae97a"
)

manifest <- read_contract("dev/gallery/visualization/manifest.csv")
d2a_manifest <- manifest[match(expected_ids, manifest$viz_id), , drop = FALSE]
assert(identical(d2a_manifest$viz_id, expected_ids), "D2A manifest rows missing")
assert(all(d2a_manifest$review_status == "APPROVED_BY_MAINTAINER"),
       "D2A manifest status mismatch")
assert(all(d2a_manifest$reviewer == "qselmer"), "D2A manifest reviewer mismatch")
assert(all(d2a_manifest$review_date == "2026-09-06"),
       "D2A manifest review date mismatch")
assert(all(manifest$review_status[!manifest$viz_id %in% expected_ids] ==
             "GENERATED_BASELINE_PENDING_MAINTAINER"),
       "unrelated gallery status changed")

review <- read_contract("dev/visualization/d2a/d2a-human-visual-review.csv")
assert(nrow(review) == 3L, "human review must contain three rows")
assert(identical(review$sha256, expected_hashes), "approved hash evidence mismatch")
assert(all(review$reviewer == "qselmer"), "human review identity mismatch")
assert(all(review$review_date == "2026-09-06"), "human review date mismatch")
assert(all(review$decision == "APPROVED"), "human review decision mismatch")

gallery <- read_contract("dev/visualization/d2a/d2a-gallery.csv")
approved <- gallery[gallery$artifact_stage == "POST_REVIEW_FIX_CANDIDATE", ]
approved <- approved[match(expected_ids, approved$viz_id), , drop = FALSE]
assert(identical(approved$sha256, expected_hashes), "gallery hash evidence mismatch")
assert(all(approved$review_status == "APPROVED_BY_MAINTAINER"),
       "gallery approval mismatch")
assert(all(approved$reviewer == "qselmer"), "gallery reviewer mismatch")

checklist <- read_contract("dev/visualization/d2a/d2a-visual-review-checklist.csv")
assert(all(checklist$review_status == "APPROVED_BY_MAINTAINER"),
       "visual checklist status mismatch")
assert(all(checklist$reviewer == "qselmer"), "visual checklist reviewer mismatch")
review_columns <- setdiff(names(checklist), c("viz_id", "reviewer", "review_status"))
assert(all(unlist(checklist[review_columns], use.names = FALSE) %in%
             c("APPROVED", "NOT_APPLICABLE")), "visual checklist is incomplete")

certification <- read_contract("dev/visualization/d2a/d2a-certification.csv")
final_status <- certification[certification$metric == "final_D2A_status", ]
assert(nrow(final_status) == 1L && final_status$observed == "COMPLETE / CERTIFIED",
       "D2A final certification mismatch")

description <- read.dcf(file.path(root, "DESCRIPTION"))
assert(unname(description[1L, "Version"]) == "0.2.0.9000", "version changed")
namespace <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
assert(sum(grepl("^export\\(", namespace)) == 49L, "public API count changed")

cat("DECISION_REGISTRY_UNIQUENESS_VALIDATOR: PASS\n")
cat("D2A_EVIDENCE_VALIDATOR: PASS\n")
cat("GALLERY_MANIFEST_VALIDATOR: PASS\n")
cat("API: 49\n")
cat("VERSION: 0.2.0.9000\n")
