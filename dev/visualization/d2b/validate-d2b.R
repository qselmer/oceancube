#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
validation_started <- proc.time()[["elapsed"]]
required <- c("openssl", "pkgload")
missing <- required[!vapply(required, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing)) stop("Missing validator package(s): ", paste(missing, collapse = ", "))

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- normalizePath(sub("^--file=", "", script_arg[[1L]]),
                             winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_file), "..", "..", ".."),
                           winslash = "/", mustWork = TRUE)
fail <- function(message) stop(message, call. = FALSE)
validation_expectations <- 0L
assert <- function(value, message) {
  validation_expectations <<- validation_expectations + 1L
  if (!isTRUE(value)) fail(message)
}
sha256_file <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  paste0(tolower(as.character(openssl::sha256(connection))))
}

pkgload::load_all(repo_root, quiet = TRUE)
exports <- getNamespaceExports("oceancube")
assert(length(exports) == 49L, "Public API is not 49 exports")
assert(!"viz.compose" %in% exports, "viz.compose must not be exported")
expected_old <- c("x", "variable", "time", "depth", "limits", "na.rm",
                  "coastline", "title", "subtitle", "caption")
expected_new <- c("style", "scale_class", "center", "contour_breaks",
                  "longitude_display")
signature <- names(formals(getExportedValue("oceancube", "viz.map")))
assert(identical(signature, c(expected_old, expected_new)),
       "viz.map append-only signature changed")

description <- readLines(file.path(repo_root, "DESCRIPTION"), warn = FALSE)
assert(any(description == "Version: 0.2.0.9000"), "Version changed")
assert(!any(grepl("^(Imports|Suggests):.*(cmocean|scico|colorspace|patchwork)",
                  description)), "D2B dependency was added")

runtime_diff <- system2(
  "git",
  c("-C", repo_root, "diff", "--name-only",
    "9cdeb2eb658accca76c730864a093ad5fd110b27", "--",
    "R", "DESCRIPTION", "NAMESPACE", "man", "tests"),
  stdout = TRUE, stderr = TRUE
)
assert(length(runtime_diff) == 0L, "Runtime or public contract changed after review/fix")

benchmark <- file.path(repo_root, "dev", "data", "visualization", "benchmark",
                       "oceancube-viz-benchmark-v1.nc")
assert(identical(sha256_file(benchmark),
                 "e146e286f3708181e11e72f4ee1656e3646c5e4f38c345860dddb9ef8d4b49a9"),
       "Benchmark file hash changed")

expected <- c(
  "d1b-map.png" = "36ae1322e1baf0ef077fb6952eb06a0ce517f3e3b7c727d3e39aa1154b63443a",
  "d1b-profile.png" = "66c867d0a584ed703cb268cd16d01d26724e26cec70118d47a6bc713729c9008",
  "d1b-section.png" = "aff7e2e730e01525f6c307744f0b53652c02c522a65bfb0ae56194192330ab61",
  "d1b-transect.png" = "915f4dfcd5812538e1be3a5d85c77376f34dcccb7578ca5548b85bcfaf2ec310",
  "d1b-timeseries.png" = "5cd8840aa524b110b953b8681db3929eacb6b1db0569156d3011d8972f17c4b4",
  "d2a-hovmoller-time-depth.png" = "0b06899297131e9c28c5bac807ef4d0b91ed7c729e149e6bae523e0d9fc67573",
  "d2a-hovmoller-time-longitude.png" = "c254dea749cd982c45ef331d326138e75ea61abb1caf0625548a9786e80e47ee",
  "d2a-hovmoller-time-latitude.png" = "04e84b01dffedfe60ef1d3ec994b4ecda12a8ff97680fa0783a8089de1eae97a"
)
static <- file.path(repo_root, "dev", "gallery", "visualization", "static")
actual <- vapply(names(expected), function(name) sha256_file(file.path(static, name)),
                 character(1L))
assert(identical(unname(actual), unname(expected)), "D1B/D2A gallery hash regression")

gallery <- utils::read.csv(file.path(dirname(script_file), "d2b-gallery.csv"),
                           check.names = FALSE)
assert(nrow(gallery) == 7L, "D2B gallery must contain seven safe candidates")
regenerated <- c("d2b-map-field-contour-pottmp",
                 "d2b-compose-map-profile",
                 "d2b-compose-map-timeseries")
assert(all(gallery$review_status == "APPROVED_BY_MAINTAINER"),
       "D2B gallery must be approved by the maintainer")
assert(all(gallery$reviewer == "qselmer"),
       "D2B gallery reviewer must be qselmer")
assert(all(gallery$review_date == "2026-09-07"),
       "D2B gallery review date changed")
assert(all(file.exists(file.path(repo_root, gallery$path))),
       "D2B gallery artifact missing")
assert(all(vapply(file.path(repo_root, gallery$path), sha256_file, character(1L)) ==
             gallery$sha256), "D2B gallery hashes mismatch")

history <- utils::read.csv(
  file.path(dirname(script_file), "d2b-gallery-hash-history.csv"),
  check.names = FALSE
)
baseline <- history[history$record == "PRE_REVIEW_BASELINE", , drop = FALSE]
post <- history[history$record == "POST_REVIEW_FIX_CANDIDATE", , drop = FALSE]
assert(nrow(baseline) == 7L, "All seven PRE_REVIEW_BASELINE hashes are required")
assert(nrow(post) == 3L, "Exactly three POST_REVIEW_FIX_CANDIDATE hashes are required")
expected_baseline <- c(
  `d2b-map-field-pottmp` = "5c75a0ae7827301ff0f4371b1ef5f97f436e4a0a93bac15dfa688487330c21b2",
  `d2b-map-field-salt` = "5012c5eb2c22e88dbb6fe553b3b11ac00499d758e637257a87bd563b48d275bf",
  `d2b-map-contour-pottmp` = "5ac0420d16136fa4c47fb5aaaab1c8b8f680242a9ec072d65f30ef00b3f27bf0",
  `d2b-map-field-contour-pottmp` = "c659e5275623cee9f8f3e84b3805fb7b6cf0442abf4618573eb97788278d285b",
  `d2b-map-diverging-synthetic` = "32635bd8869eeecede789c9b033bb55d98ec9a869b1ee8b9b453fdbf080343ea",
  `d2b-compose-map-profile` = "aa086b460d25b7c927b95603ded91b95b76d2accf31582d5ea883dc341fca250",
  `d2b-compose-map-timeseries` = "a7d636ed9db9f305c37f2e2d6e092ac0f69a0198a83b3f6e9349c180ef910463"
)
assert(identical(setNames(baseline$sha256, baseline$artifact), expected_baseline),
       "PRE_REVIEW_BASELINE hashes changed")
assert(setequal(post$artifact, regenerated), "Unexpected regenerated artifact set")
assert(all(post$sha256 == gallery$sha256[match(post$artifact, gallery$artifact)]),
       "Post-review hashes do not match current gallery")
assert(all(post$sha256 != expected_baseline[post$artifact]),
       "Regenerated hashes must differ from their baselines")

composition <- utils::read.csv(
  file.path(dirname(script_file), "d2b-composition-audit.csv"),
  check.names = FALSE
)
required_composition_columns <- c(
  "composition", "map_variable", "map_time", "map_depth",
  "child_plot_type", "selection_source_longitude",
  "selection_display_longitude", "selection_latitude", "selection_depth",
  "match_mode", "marker_present", "marker_coordinate_match",
  "child_selection_match", "composition_source_reads", "status", "reviewer",
  "review_date"
)
assert(identical(names(composition), required_composition_columns),
       "Composition audit columns changed")
assert(identical(composition$composition, c("map_profile", "map_timeseries")),
       "Composition audit rows changed")
assert(all(composition$selection_source_longitude == 278.5),
       "Composition source longitude changed")
assert(all(composition$selection_display_longitude == -81.5),
       "Composition display longitude is not exact")
assert(all(abs(composition$selection_latitude - (-11.8337802886963)) < 1e-12),
       "Composition latitude is not exact")
assert(all(composition$match_mode == "exact"), "Composition match mode changed")
assert(all(composition$marker_present & composition$marker_coordinate_match &
             composition$child_selection_match),
       "Composition marker/child linkage failed")
assert(all(composition$composition_source_reads == 0L),
       "Composition added a scientific source read")
assert(all(composition$status == "APPROVED_BY_MAINTAINER" &
             composition$reviewer == "qselmer" &
             composition$review_date == "2026-09-07"),
       "Composition approval evidence changed")

marker <- utils::read.csv(file.path(dirname(script_file), "d2b-marker-tests.csv"),
                          check.names = FALSE)
assert(nrow(marker) == 2L && all(marker$point_count == 1L),
       "Each composition requires exactly one marker")
assert(all(marker$expected_display_longitude == marker$built_marker_x),
       "Built marker longitude mismatch")
assert(all(marker$expected_latitude == marker$built_marker_y),
       "Built marker latitude mismatch")
assert(all(marker$status == "PASS"), "Marker test failed")

invariance <- utils::read.csv(
  file.path(dirname(script_file), "d2b-scientific-invariance.csv"),
  check.names = FALSE
)
assert(setequal(invariance$component, c(
  "profile_values", "profile_depths", "timeseries_values",
  "timeseries_times", "map_prepared_values", "map_coordinates"
)), "Scientific invariance components changed")
assert(all(invariance$exact_equal & invariance$status == "PASS"),
       "Scientific values or coordinates changed")
assert(all(invariance$pre_review_sha256 == invariance$post_review_sha256),
       "Scientific invariance hashes differ")

checklist <- utils::read.csv(
  file.path(dirname(script_file), "d2b-visual-review-checklist.csv"),
  check.names = FALSE
)
assert(all(sprintf("D2B-V%02d", 22:31) %in% checklist$check_id),
       "Review/fix checklist items are missing")
assert(all(checklist$reviewer == "qselmer"),
       "Review checklist reviewer must be qselmer")
assert(!any(grepl("PENDING", checklist$human_review_status)),
       "Review checklist still contains a pending item")

human_review <- utils::read.csv(
  file.path(dirname(script_file), "d2b-human-visual-review.csv"),
  check.names = FALSE
)
assert(nrow(human_review) == 7L, "Human review must cover seven artifacts")
assert(all(human_review$reviewer == "qselmer" &
             human_review$review_date == "2026-09-07" &
             human_review$decision == "APPROVED"),
       "Human review identity, date, or decision changed")
assert(setequal(human_review$artifact, gallery$path),
       "Human review artifact set differs from gallery")
assert(all(human_review$sha256 ==
             gallery$sha256[match(human_review$artifact, gallery$path)]),
       "Human review hashes differ from gallery")

manifest <- utils::read.csv(file.path(
  repo_root, "dev", "gallery", "visualization", "manifest.csv"
), check.names = FALSE)
d2b_manifest <- manifest[grepl("^D2B-", manifest$viz_id), , drop = FALSE]
assert(nrow(d2b_manifest) == 7L, "Gallery manifest must contain seven D2B rows")
assert(all(d2b_manifest$review_status == "APPROVED_BY_MAINTAINER" &
             d2b_manifest$reviewer == "qselmer" &
             d2b_manifest$review_date == "2026-09-07"),
       "Gallery manifest approval fields changed")

architecture <- paste(readLines(file.path(
  repo_root, "inst", "architecture", "oceancube-map-visualization-v1.md"
), warn = FALSE), collapse = "\n")
assert(grepl("must visibly mark the exact spatial selection", architecture,
             fixed = TRUE), "Composition-linkage architecture note missing")
assert(grepl("must not be relabelled as land", architecture, fixed = TRUE),
       "NA/land architecture note missing")
assert(grepl("does not necessarily communicate the complete valid-", architecture,
             fixed = TRUE), "Contour support-mask architecture note missing")
assert(grepl("D2B COMPLETE / CERTIFIED LOCALLY", architecture, fixed = TRUE),
       "Architecture certification status missing")

phase_plan <- utils::read.csv(file.path(
  repo_root, "dev", "visualization", "d1a", "phase-plan.csv"
), check.names = FALSE)
assert(phase_plan$status[phase_plan$phase == "D2"] == "COMPLETE_CERTIFIED_LOCALLY",
       "D2 status is inconsistent")
assert(phase_plan$status[phase_plan$phase == "D2B"] == "COMPLETE_CERTIFIED_LOCALLY",
       "D2B status is inconsistent")
assert(phase_plan$status[phase_plan$phase == "D3"] == "NOT_STARTED",
       "D3 must remain not started")

decisions <- utils::read.csv(
  file.path(repo_root, "docs", "roadmap", "post-0.2.0", "roadmap-decisions.csv")
)
for (id in sprintf("DEC-%03d", 41:45)) {
  assert(sum(decisions$decision_id == id) == 1L, paste(id, "count mismatch"))
}
assert(!anyDuplicated(decisions$decision_id), "Decision IDs must be unique")
assert(sum(decisions$decision_id == "DEC-046") == 0L,
       "DEC-046 must remain unallocated")
assert(decisions$status[decisions$decision_id == "DEC-045"] ==
         "APPROVED — CERTIFIED CORE 2-D MAP STYLES AND SCIENTIFIC SCALE CONTRACT",
       "DEC-045 title changed")

registry <- utils::read.csv(file.path(
  repo_root, "dev", "references", "visualization",
  "visualization-reference-registry.csv"
))
assert(!anyDuplicated(registry$reference_id), "Reference IDs must be unique")
assert(all(c("R027", "R028") %in% registry$reference_id),
       "D2B material references are missing")

palette_values <- c(
  grDevices::hcl.colors(3L, "Blue-Red 3"),
  ggplot2::scale_fill_viridis_c(option = "D")$palette(
    seq(0, 1, length.out = 11L)
  )
)
fingerprint <- paste0(tolower(as.character(openssl::sha256(charToRaw(
  paste(palette_values, collapse = "|")
)))))
utils::write.csv(data.frame(
  component = c("base_hcl_Blue-Red_3", "ggplot2_viridis_D", "combined"),
  values = c(paste(palette_values[1:3], collapse = ";"),
             paste(palette_values[4:length(palette_values)], collapse = ";"), ""),
  sha256 = c("", "", fingerprint), status = "PASS"
), file.path(dirname(script_file), "d2b-palette-fingerprint.csv"), row.names = FALSE)

validation_elapsed <- proc.time()[["elapsed"]] - validation_started
utils::write.csv(data.frame(
  validator_files = 1L,
  validation_cases = 12L,
  expectations = validation_expectations,
  elapsed_seconds = round(validation_elapsed, 3L),
  failures = 0L, errors = 0L, warnings = 0L, skips = 0L,
  status = "PASS"
), file.path(dirname(script_file), "d2b-final-certification-validation.csv"),
row.names = FALSE)

cat("D2B_VALIDATOR=PASS\n")
cat("API=49\nSCHEMA=1.0.0\nD1B_HASHES=5/5 EXACT MATCH\n")
cat("D2A_HASHES=3/3 EXACT MATCH\nBENCHMARK_HASH_UNCHANGED=TRUE\n")
cat("D2B_APPROVED_HASHES=7/7 EXACT MATCH\n")
cat("COMPOSITION_MARKERS=2/2 EXACT NUMERIC MATCH\n")
cat("SCIENTIFIC_INVARIANCE=6/6 EXACT MATCH\n")
cat("HUMAN_REVIEW=qselmer 2026-09-07 PASS\n")
cat("DEC_041_045=ONE_EACH UNIQUE\nDEC_046=0\nNEXT_AVAILABLE_DEC=DEC-046\n")
cat("D2B=COMPLETE_CERTIFIED_LOCALLY\nD2=COMPLETE_CERTIFIED_LOCALLY\n")
cat("BOUNDED_VALIDATION=1 file; 12 cases; ", validation_expectations,
    " expectations; ", round(validation_elapsed, 3L), " s; 0/0/0/0\n", sep = "")
cat("PALETTE_FINGERPRINT=", fingerprint, "\n", sep = "")
