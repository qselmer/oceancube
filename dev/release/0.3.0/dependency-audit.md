# oceancube 0.3.0 dependency audit

Audited on 2026-09-08 against parsed runtime namespace use in `R/`.

## Strong dependencies

- `cli`, `ggplot2`, `grDevices`, `ncdf4`, `rlang`, `stats`, and `utils` are
  directly used by the runtime and remain in `Imports`.
- `R (>= 4.2.0)` remains the only `Depends` entry.
- No separate `SystemRequirements` field is needed by the package core.

## Optional dependencies

- `gsw (>= 1.2-0)` is required only by the explicit TEOS-10 paths and is
  checked by `.teos_require_dependency()` before namespace calls.
- `sf` is required only for polygon geometry or an explicit `sf`/`sfc`
  coastline and is checked before namespace calls.
- `reticulate` is confined to the deprecated provider helpers, which check it
  before use. Installing Python modules remains an explicit user action.
- `knitr` and `rmarkdown` build vignettes; `testthat` and `withr` support the
  test suite.

Installation and loading of the core require no network, credentials, Python,
provider account, or external command-line program.
