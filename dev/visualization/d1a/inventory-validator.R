root <- normalizePath(".", winslash = "/", mustWork = TRUE)
fail <- function(...) stop(..., call. = FALSE)
assert <- function(ok, message) if (!isTRUE(ok)) fail(message)

path <- file.path(root, "dev/visualization/d1a/global-visualization-inventory.csv")
x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
required <- c(
  "viz_id", "family", "subfamily", "candidate_public_api", "candidate_style",
  "generic_or_specialized", "analysis_or_communication", "data_geometry",
  "required_dimensions", "required_semantics", "scientific_question", "static",
  "interactive", "animation", "3d", "candidate_renderer", "candidate_extensions",
  "priority", "target_phase", "reference_ids", "status", "notes"
)
assert(identical(names(x), required), "global inventory schema mismatch")
assert(nrow(x) >= 50L, "global inventory must contain at least 50 concepts")
assert(!anyDuplicated(x$viz_id), "viz_id values must be unique")
assert(all(nzchar(x$family)), "every concept must have a family")
assert(all(nzchar(x$generic_or_specialized)), "every concept must have a classification")
assert(all(nzchar(x$analysis_or_communication)), "every concept must have an analytical/communication classification")
assert(all(nzchar(x$target_phase)), "every concept must have a target phase")
assert(all(nzchar(x$candidate_renderer)), "every concept must name a renderer or DEFERRED")
assert(all(nzchar(x$notes)), "every concept must have rationale/notes")

families <- c(
  "A_GENERIC_FIELD", "B_TEMPORAL_CHANGE", "C_OCEAN_SPECIALIZED",
  "D_MULTIVARIATE_QC", "E_COMMUNICATION", "F_3D_SCIENTIFIC", "G_ANIMATION",
  "H_VECTOR_FLOW", "I_UNCERTAINTY_ENSEMBLE", "J_FUTURE_LARGE_DATA"
)
assert(all(families %in% x$family), "inventory does not cover every required top-level family")

public <- nzchar(x$candidate_public_api)
assert(all(nzchar(x$notes[public])), "public API candidates require explicit rationale")
large <- x$family == "J_FUTURE_LARGE_DATA" | (x$family == "F_3D_SCIENTIFIC" & x$subfamily == "volume")
assert(all(x$target_phase[large] == "0.5"), "large 3-D concepts must be bounded to 0.5")
assert(all(x$status[x$family == "H_VECTOR_FLOW"] == "DEFER_PHASE_E"), "vector/flow concepts must defer to Phase E")

api <- read.csv(
  file.path(root, "dev/visualization/d1a/api-candidate-matrix.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)
api_status <- c(
  "KEEP_EXISTING", "EXTEND_EXISTING", "NEW_PUBLIC_CANDIDATE", "STYLE_ONLY",
  "OVERLAY_ONLY", "INTERNAL_ONLY", "DEFER_PHASE_E", "DEFER_0.5", "OUT_OF_SCOPE"
)
assert(all(api$classification %in% api_status), "invalid API candidate classification")
assert(all(nzchar(api$rationale)), "every API candidate requires rationale")

renderer <- read.csv(
  file.path(root, "dev/visualization/d1a/renderer-evaluation.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)
renderer_status <- c("CORE", "OPTIONAL", "ADAPTER", "REFERENCE", "DEFERRED", "REJECTED")
assert(all(renderer$recommended_status %in% renderer_status), "invalid renderer recommendation status")
score_columns <- grep("_score$", names(renderer), value = TRUE)
assert(length(score_columns) >= 10L, "renderer matrix must contain serious 1-5 scoring")
assert(all(vapply(renderer[score_columns], function(v) all(v >= 1 & v <= 5), logical(1))), "renderer scores must be in 1-5")

required_evidence <- c(
  "README.md", "current-viz-api.csv", "current-viz-signatures.csv",
  "current-viz-dependencies.csv", "global-visualization-inventory.csv",
  "renderer-evaluation.csv", "ggplot-extension-evaluation.csv",
  "palette-policy.csv", "map-capabilities.csv", "3d-capabilities.csv",
  "animation-capabilities.csv", "communication-visualizations.csv",
  "specialized-ocean-visualizations.csv", "gallery-contract.csv",
  "api-candidate-matrix.csv", "phase-plan.csv", "d1a-certification.csv"
)
assert(all(file.exists(file.path(root, "dev/visualization/d1a", required_evidence))), "required D1A evidence file missing")

cat("INVENTORY_VALIDATOR: PASS\n")
cat("CONCEPTS:", nrow(x), "\n")
cat("FAMILIES:", length(unique(x$family)), "\n")
cat("PUBLIC_CANDIDATE_ROWS:", sum(public), "\n")
cat("LARGE_DATA_BOUNDED:", sum(large), "\n")
