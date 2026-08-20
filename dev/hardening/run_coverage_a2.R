#!/usr/bin/env Rscript

# oceancube 0.3.0-A2 line-coverage comparison.
#
# Run from any directory. The package is copied to a clean temporary snapshot
# so repository metadata and generated artifacts cannot affect instrumentation.

options(stringsAsFactors = FALSE)

if (!requireNamespace("covr", quietly = TRUE)) {
  stop("covr is required to measure the A2 hardening suite")
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else NA_character_
repo_root <- if (!is.na(script_file)) {
  normalizePath(file.path(dirname(script_file), "..", ".."), winslash = "/")
} else {
  normalizePath(".", winslash = "/")
}
output_dir <- file.path(repo_root, "dev", "hardening")

snapshot_parent <- tempfile("oceancube-a2-coverage-")
snapshot <- file.path(snapshot_parent, "oceancube")
dir.create(snapshot, recursive = TRUE)

excluded <- c(
  ".git", ".RData", ".Rhistory", ".Rproj.user", "artifacts",
  "oceancube.Rcheck"
)
entries <- list.files(repo_root, all.files = TRUE, full.names = TRUE, no.. = TRUE)
entries <- entries[!basename(entries) %in% excluded]
copied <- file.copy(entries, snapshot, recursive = TRUE, copy.date = TRUE)
if (!all(copied)) {
  stop("Could not create the clean A2 coverage snapshot: ", paste(basename(entries)[!copied], collapse = ", "))
}

coverage <- covr::package_coverage(snapshot, quiet = FALSE)
lines <- covr:::tally_coverage(coverage, by = "line")

summarize_lines <- function(data, groups) {
  keys <- interaction(data[groups], drop = TRUE, lex.order = TRUE)
  split_rows <- split(seq_len(nrow(data)), keys)
  rows <- lapply(split_rows, function(index) {
    values <- data$value[index]
    out <- data[index[[1L]], groups, drop = FALSE]
    out$expressions <- length(values)
    out$covered <- sum(values > 0)
    out$coverage_percent <- 100 * out$covered / out$expressions
    out
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

classify <- function(x) {
  as.character(cut(
    x,
    breaks = c(-Inf, 70, 80, 90, Inf),
    labels = c("priority-audit", "weak", "acceptable-review", "strong"),
    right = FALSE
  ))
}

by_file <- summarize_lines(lines, "filename")
names(by_file)[names(by_file) == "filename"] <- "file"
by_file$class <- classify(by_file$coverage_percent)
by_file <- by_file[order(by_file$coverage_percent, by_file$file), ]

by_function <- summarize_lines(lines, c("filename", "functions"))
names(by_function)[match(c("filename", "functions"), names(by_function))] <- c("file", "function_name")
by_function$class <- classify(by_function$coverage_percent)
by_function <- by_function[order(by_function$coverage_percent, by_function$file, by_function$function_name), ]

write_csv <- function(x, name) {
  utils::write.csv(
    x,
    file.path(output_dir, name),
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
}

write_csv(by_file, "coverage-a2-by-file.csv")
write_csv(by_function, "coverage-a2-by-function.csv")

a1_file <- utils::read.csv(file.path(output_dir, "coverage-by-file.csv"), check.names = FALSE)
a1_function <- utils::read.csv(file.path(output_dir, "coverage-by-function.csv"), check.names = FALSE)

comparison <- merge(
  a1_file[c("file", "expressions", "covered", "coverage_percent")],
  by_file[c("file", "expressions", "covered", "coverage_percent")],
  by = "file",
  all = TRUE,
  suffixes = c("_a1", "_a2")
)
comparison$delta_percentage_points <- comparison$coverage_percent_a2 - comparison$coverage_percent_a1
comparison <- comparison[order(-comparison$delta_percentage_points, comparison$file), ]
write_csv(comparison, "coverage-comparison.csv")

overall <- as.numeric(covr::percent_coverage(coverage))
summary <- data.frame(
  metric = c(
    "overall_line_coverage_percent", "functions_total",
    "functions_with_any_coverage", "functions_with_any_coverage_percent"
  ),
  value = c(
    overall,
    nrow(by_function),
    sum(by_function$covered > 0),
    100 * mean(by_function$covered > 0)
  ),
  stringsAsFactors = FALSE
)
write_csv(summary, "coverage-a2-summary.csv")

coverage_rds <- file.path(snapshot_parent, "coverage.rds")
saveRDS(coverage, coverage_rds)

cat(sprintf("A2_COVERAGE_PERCENT=%.15f\n", overall))
cat(sprintf("A2_FUNCTIONS=%d\n", nrow(by_function)))
cat(sprintf("A2_FUNCTIONS_WITH_ANY_COVERAGE=%d\n", sum(by_function$covered > 0)))
cat(sprintf("A2_COVERAGE_RDS=%s\n", normalizePath(coverage_rds, winslash = "/")))
