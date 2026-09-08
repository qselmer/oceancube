# oceancube 0.3.0 CRAN code and effect audit

Audited on 2026-09-08 by static inspection of `R/`, `tests/testthat/`, and
`vignettes/` plus the repository test-audit script.

## Results

- No runtime calls to `setwd()`, `q()`, `browser()`, `debug()`, `View()`,
  `system()`, `system2()`, `shell()`, `download.file()`, `:::`, or
  `.Internal()`.
- No live-network, credential-access, browser, shell, outside-process-state,
  or repository-artifact references in the 84 test files audited.
- Runtime file creation is confined to deprecated `download_nc()`, whose
  caller explicitly supplies the output directory and initiates the provider
  operation.
- Deprecated `cm_setup()` may create a Python environment and install a module
  only when called explicitly; it is not invoked by installation, loading,
  tests, or examples.
- The TEOS-10 installation hint is message text only; the package never calls
  `install.packages()`.
- Normal tests and examples are offline, deterministic, non-interactive, and
  cross-platform by contract.

Result: PASS, subject to the complete test, install, build, and CRAN-check
gates that follow.
