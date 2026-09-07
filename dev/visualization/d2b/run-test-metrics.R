args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: run-test-metrics.R <filter-or-ALL> <result-csv>")
}
filter <- if (identical(args[[1L]], "ALL")) NULL else args[[1L]]
started <- proc.time()[["elapsed"]]
results <- devtools::test(filter = filter, reporter = "summary",
                          stop_on_failure = FALSE)
elapsed <- proc.time()[["elapsed"]] - started
expectations <- unlist(lapply(results, `[[`, "results"), recursive = FALSE)
count_class <- function(class_name) {
  sum(vapply(expectations, inherits, logical(1L), class_name))
}
metrics <- data.frame(
  filter = if (is.null(filter)) "ALL" else filter,
  files = length(unique(vapply(results, `[[`, character(1L), "file"))),
  cases = length(results), expectations = length(expectations),
  elapsed_seconds = round(elapsed, 3),
  failures = count_class("expectation_failure"),
  errors = count_class("expectation_error"),
  warnings = count_class("expectation_warning"),
  skips = count_class("expectation_skip"), status = "PASS",
  stringsAsFactors = FALSE
)
if (any(metrics[c("failures", "errors", "warnings", "skips")] != 0L)) {
  metrics$status <- "FAIL"
}
utils::write.csv(metrics, args[[2L]], row.names = FALSE)
print(metrics)
if (metrics$status != "PASS") stop("Test metrics did not pass cleanly.")
