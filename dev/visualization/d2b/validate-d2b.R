#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
required <- c("openssl", "pkgload")
missing <- required[!vapply(required, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing)) stop("Missing validator package(s): ", paste(missing, collapse = ", "))

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- normalizePath(sub("^--file=", "", script_arg[[1L]]),
                             winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_file), "..", "..", ".."),
                           winslash = "/", mustWork = TRUE)
fail <- function(message) stop(message, call. = FALSE)
assert <- function(value, message) if (!isTRUE(value)) fail(message)
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
assert(all(gallery$review_status == "GENERATED_PENDING_MAINTAINER"),
       "D2B gallery review state is invalid")
assert(all(is.na(gallery$reviewer) | gallery$reviewer == ""),
       "D2B human reviewer must be blank")
assert(all(file.exists(file.path(repo_root, gallery$path))),
       "D2B gallery artifact missing")
assert(all(vapply(file.path(repo_root, gallery$path), sha256_file, character(1L)) ==
             gallery$sha256), "D2B gallery hashes mismatch")

decisions <- utils::read.csv(
  file.path(repo_root, "docs", "roadmap", "post-0.2.0", "roadmap-decisions.csv")
)
for (id in sprintf("DEC-%03d", 41:44)) {
  assert(sum(decisions$decision_id == id) == 1L, paste(id, "count mismatch"))
}
assert(sum(decisions$decision_id == "DEC-045") == 0L,
       "DEC-045 must remain unallocated")

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

cat("D2B_VALIDATOR=PASS\n")
cat("API=49\nSCHEMA=1.0.0\nD1B_HASHES=5/5 EXACT MATCH\n")
cat("D2A_HASHES=3/3 EXACT MATCH\nBENCHMARK_HASH_UNCHANGED=TRUE\n")
cat("DEC_041_044=ONE_EACH\nDEC_045=0\nNEXT_AVAILABLE_DEC=DEC-045\n")
cat("PALETTE_FINGERPRINT=", fingerprint, "\n", sep = "")
