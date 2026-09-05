args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: run-full-tests.R <result-csv>")
started <- proc.time()[["elapsed"]]
results <- devtools::test(reporter = "summary", stop_on_failure = FALSE)
elapsed <- proc.time()[["elapsed"]] - started
expectations <- unlist(lapply(results, `[[`, "results"), recursive = FALSE)
count_class <- function(class_name) {
  sum(vapply(expectations, inherits, logical(1), class_name))
}
metrics <- data.frame(
  files = length(unique(vapply(results, `[[`, character(1), "file"))),
  cases = length(results),
  expectations = length(expectations),
  elapsed_seconds = round(elapsed, 3),
  failures = count_class("expectation_failure"),
  errors = count_class("expectation_error"),
  warnings = count_class("expectation_warning"),
  skips = count_class("expectation_skip"),
  status = "PASS",
  stringsAsFactors = FALSE
)
if (any(metrics[c("failures", "errors", "warnings", "skips")] != 0L)) {
  metrics$status <- "FAIL"
}
utils::write.csv(metrics, args[[1L]], row.names = FALSE)
print(metrics)
if (metrics$status != "PASS") stop("Full test suite did not pass cleanly.")
cat("D1B_FULL_TEST_SUITE=PASS\n")
