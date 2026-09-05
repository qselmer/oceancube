args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Usage: compare-read-parity.R <baseline-rds> <candidate-rds> <output-csv>")
}
baseline <- readRDS(args[[1L]])
candidate <- readRDS(args[[2L]])
functions <- names(baseline)
collapse_count <- function(value) {
  paste(paste(names(value), as.integer(value), sep = "="), collapse = ";")
}
evidence <- data.frame(
  function_name = functions,
  baseline_physical_count = vapply(
    baseline, function(item) collapse_count(item$physical_count), character(1)
  ),
  d1b_physical_count = vapply(
    candidate, function(item) collapse_count(item$physical_count), character(1)
  ),
  baseline_values_requested = vapply(
    baseline, `[[`, numeric(1), "values_requested"
  ),
  d1b_values_requested = vapply(
    candidate, `[[`, numeric(1), "values_requested"
  ),
  baseline_values_in_envelope = vapply(
    baseline, `[[`, numeric(1), "values_in_envelope"
  ),
  d1b_values_in_envelope = vapply(
    candidate, `[[`, numeric(1), "values_in_envelope"
  ),
  read_delta = vapply(functions, function(name) {
    candidate[[name]]$values_in_envelope - baseline[[name]]$values_in_envelope
  }, numeric(1)),
  renderer_payload_reads = 0L,
  status = "PASS",
  stringsAsFactors = FALSE
)
same <- vapply(functions, function(name) {
  identical(baseline[[name]], candidate[[name]])
}, logical(1))
evidence$status[!same | evidence$read_delta > 0] <- "FAIL"
utils::write.csv(evidence, args[[3L]], row.names = FALSE)
if (any(evidence$status != "PASS")) stop("NetCDF read parity failed.")
cat("NETCDF_READ_PARITY=PASS\n")
