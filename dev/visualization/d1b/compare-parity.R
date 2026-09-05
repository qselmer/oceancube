args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Usage: compare-parity.R <baseline-rds> <candidate-rds> <evidence-dir>")
}
baseline <- readRDS(args[[1L]])
candidate <- readRDS(args[[2L]])
evidence <- args[[3L]]
dir.create(evidence, recursive = TRUE, showWarnings = FALSE)

functions <- names(baseline$plots)
public <- data.frame(
  function_name = functions,
  signature_identical = vapply(functions, function(name) {
    identical(baseline$signatures[[name]], candidate$signatures[[name]])
  }, logical(1)),
  return_class_identical = vapply(functions, function(name) {
    identical(baseline$plots[[name]]$class, candidate$plots[[name]]$class)
  }, logical(1)),
  scientific_data_identical = vapply(functions, function(name) {
    identical(baseline$plots[[name]]$data, candidate$plots[[name]]$data)
  }, logical(1)),
  attributes_identical = vapply(functions, function(name) {
    identical(baseline$plots[[name]]$oceancube_attributes,
              candidate$plots[[name]]$oceancube_attributes)
  }, logical(1)),
  status = "PASS",
  stringsAsFactors = FALSE
)

ggplot <- data.frame(
  function_name = functions,
  build_data_identical = vapply(functions, function(name) {
    identical(baseline$plots[[name]]$build_data,
              candidate$plots[[name]]$build_data)
  }, logical(1)),
  labels_identical = vapply(functions, function(name) {
    identical(baseline$plots[[name]]$labels, candidate$plots[[name]]$labels)
  }, logical(1)),
  layers_identical = vapply(functions, function(name) {
    identical(baseline$plots[[name]]$layer_count,
              candidate$plots[[name]]$layer_count) &&
      identical(baseline$plots[[name]]$geom_classes,
                candidate$plots[[name]]$geom_classes)
  }, logical(1)),
  mappings_identical = vapply(functions, function(name) {
    identical(baseline$plots[[name]]$mappings,
              candidate$plots[[name]]$mappings)
  }, logical(1)),
  scales_identical = vapply(functions, function(name) {
    identical(baseline$plots[[name]]$scale_classes,
              candidate$plots[[name]]$scale_classes)
  }, logical(1)),
  coordinates_identical = vapply(functions, function(name) {
    identical(baseline$plots[[name]]$coordinate_class,
              candidate$plots[[name]]$coordinate_class)
  }, logical(1)),
  status = "PASS",
  stringsAsFactors = FALSE
)

error_names <- names(baseline$errors)
errors <- data.frame(
  case = error_names,
  message_identical = vapply(error_names, function(name) {
    identical(baseline$errors[[name]]$message, candidate$errors[[name]]$message)
  }, logical(1)),
  class_identical = vapply(error_names, function(name) {
    identical(baseline$errors[[name]]$class, candidate$errors[[name]]$class)
  }, logical(1)),
  status = "PASS",
  stringsAsFactors = FALSE
)
warnings <- data.frame(
  function_name = functions,
  baseline_count = vapply(baseline$warnings, length, integer(1)),
  candidate_count = vapply(candidate$warnings, length, integer(1)),
  identical = vapply(functions, function(name) {
    identical(baseline$warnings[[name]], candidate$warnings[[name]])
  }, logical(1)),
  status = "PASS",
  stringsAsFactors = FALSE
)

public$status[!apply(public[, 2:5, drop = FALSE], 1L, all)] <- "FAIL"
ggplot$status[!apply(ggplot[, 2:7, drop = FALSE], 1L, all)] <- "FAIL"
errors$status[!errors$message_identical | !errors$class_identical] <- "FAIL"
warnings$status[!warnings$identical] <- "FAIL"

utils::write.csv(public, file.path(evidence, "d1b-public-parity.csv"), row.names = FALSE)
utils::write.csv(ggplot, file.path(evidence, "d1b-ggplot-parity.csv"), row.names = FALSE)
utils::write.csv(errors, file.path(evidence, "d1b-error-parity.csv"), row.names = FALSE)
utils::write.csv(warnings, file.path(evidence, "d1b-warning-parity.csv"), row.names = FALSE)

if (any(public$status != "PASS") || any(ggplot$status != "PASS") ||
    any(errors$status != "PASS") || any(warnings$status != "PASS")) {
  stop("Baseline-versus-candidate visualization parity failed.")
}
cat("BASELINE_VS_FINAL_PARITY=PASS\n")
