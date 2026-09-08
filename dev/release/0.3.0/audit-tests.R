test_root <- file.path("tests", "testthat")
files <- sort(list.files(test_root, pattern = "[.]R$", full.names = TRUE))

count_pattern <- function(lines, pattern) {
  sum(grepl(pattern, lines, perl = TRUE))
}

collapse_matches <- function(lines, pattern) {
  hits <- regmatches(lines, gregexpr(pattern, lines, perl = TRUE))
  values <- sort(unique(unlist(hits, use.names = FALSE)))
  values <- values[nzchar(values)]
  paste(values, collapse = ";")
}

audit_one <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  name <- basename(path)
  kind <- if (startsWith(name, "test-")) {
    "test"
  } else if (startsWith(name, "helper-")) {
    "helper"
  } else {
    "setup"
  }

  live_network <- count_pattern(
    lines,
    "download[.]file[(]|curl::|httr2?::|request[(]|req_perform[(]"
  )
  credential_access <- count_pattern(
    lines,
    "Sys[.]getenv[(]|keyring::|askpass::|[.]Renviron"
  )
  browser_calls <- count_pattern(lines, "browseURL[(]|rstudioapi::viewer[(]")
  shell_calls <- count_pattern(lines, "(^|[^[:alnum:]_])(system2?|shell)[(]")
  outside_state <- count_pattern(lines, "setwd[(]|[.]GlobalEnv|assign[(].*envir")
  artifact_refs <- count_pattern(lines, "artifacts[/\\\\]")
  write_calls <- count_pattern(
    lines,
    "writeLines[(]|writeBin[(]|saveRDS[(]|(^|[^[:alnum:]_])save[(]|file[.]create[(]|dir[.]create[(]|ncdf4::nc_create[(]"
  )
  temp_guards <- count_pattern(
    lines,
    "tempdir[(]|tempfile[(]|local_tempdir[(]|local_tempfile[(]|withr::local_"
  )
  optional_packages <- collapse_matches(
    lines,
    "(?<=skip_if_not_installed[(]\")[^\"]+"
  )
  suspicious_name <- grepl(
    "(^|[-_.])(tmp|temp|debug|scratch|manual|obsolete|old|backup|wip)([-_.]|$)",
    name,
    ignore.case = TRUE
  )

  forbidden <- live_network + credential_access + browser_calls +
    shell_calls + outside_state + artifact_refs
  decision <- if (forbidden > 0L || suspicious_name) "REVIEW" else "KEEP"

  data.frame(
    path = gsub("\\\\", "/", path),
    kind = kind,
    bytes = file.info(path)$size,
    test_cases = count_pattern(lines, "test_that[(]"),
    expectations = count_pattern(lines, "expect_[[:alnum:]_]+[(]"),
    skip_guards = count_pattern(lines, "skip(_if|_on|[(])"),
    optional_packages = optional_packages,
    live_network_calls = live_network,
    credential_access = credential_access,
    browser_calls = browser_calls,
    shell_calls = shell_calls,
    outside_process_state = outside_state,
    artifact_references = artifact_refs,
    write_calls = write_calls,
    temp_guards = temp_guards,
    suspicious_filename = suspicious_name,
    cran_offline_safe = forbidden == 0L,
    decision = decision,
    stringsAsFactors = FALSE
  )
}

audit <- do.call(rbind, lapply(files, audit_one))
output <- file.path("dev", "release", "0.3.0", "test-suite-audit.csv")
write.csv(audit, output, row.names = FALSE, na = "")

cat("FILES=", nrow(audit), "\n", sep = "")
cat("TEST_FILES=", sum(audit$kind == "test"), "\n", sep = "")
cat("CASES=", sum(audit$test_cases), "\n", sep = "")
cat("STATIC_EXPECTATIONS=", sum(audit$expectations), "\n", sep = "")
cat("REVIEW=", sum(audit$decision == "REVIEW"), "\n", sep = "")
cat("LIVE_NETWORK=", sum(audit$live_network_calls), "\n", sep = "")
cat("CREDENTIAL_ACCESS=", sum(audit$credential_access), "\n", sep = "")
cat("BROWSER_CALLS=", sum(audit$browser_calls), "\n", sep = "")
cat("SHELL_CALLS=", sum(audit$shell_calls), "\n", sep = "")
cat("OUTSIDE_STATE=", sum(audit$outside_process_state), "\n", sep = "")
cat("ARTIFACT_REFERENCES=", sum(audit$artifact_references), "\n", sep = "")
