# OCEANCUBE 0.3.0 — CRAN release certification

Certification date: 2026-09-08  
Package: `oceancube`  
Version: `0.3.0`  
Public API: 49 exports  
Certified package-source SHA: `e46b53083663403e12e3c76cb5902f6a0ce2ed44`

The certification record itself is repository-only and excluded from the
source package. Its permanent commit is the final `origin/main` resolved after
the last documentation-only CI run. The package source certified below is the
`e46b530...` ancestor; no source-package file changes in the certification
record commit.

## Release boundary

- Scientific contracts through D2B are frozen for 0.3.0.
- All 13 audited legacy decisions are closed through bounded deprecation.
- The six certified visualization functions are `viz.map()`, `viz.profile()`,
  `viz.section()`, `viz.transect()`, `viz.timeseries()`, and
  `viz.hovmoller()`.
- D3, D4, D5, remote/lazy/Zarr backends, regridding, advanced grids,
  multi-file support, and a new acquisition framework are not implemented.
- No `v0.3.0` tag or GitHub release was created.
- No CRAN submission was performed.

## Repository and source package

- Repository size at final source certification: 48,056,648 bytes
  (45.83 MiB), including ignored local check/build outputs.
- `.git` size: 24,261,386 bytes (23.14 MiB).
- Cleanup baseline: 738.34 MB before, 36.07 MB after cleanup, with the
  original reflog bundle preserved externally.
- CRAN-ready tarball:
  `D:\.packages\oceancube\oceancube_0.3.0.tar.gz`
- Tarball size: 1,793,655 bytes.
- Tarball SHA-256:
  `1d2f500c717a9f5125dab66f318bd6ee2c30d2807d33a75eaa8c03e82fd79e4e`.
- Tarball entries: 293.
- Forbidden repository/development paths in tarball: 0.
- The Windows checkout contains long internal Codex Git-ref paths, so the
  final build used an exact immutable `git archive HEAD` snapshot followed by
  `R CMD build`. This avoids copying `.git`; it does not alter package source.

## Local verification

### Full source test suite

- Test files: 84.
- Test cases: 876.
- Expectations: 6,624.
- Failures: 0.
- Errors: 0.
- Warnings: 0.
- Skips: 0.
- Elapsed time: 1,426.738 seconds.

### Clean install and load

- Installation into a fresh temporary library: PASS.
- `library(oceancube)`: PASS.
- Namespace exports: 49.
- Network required for install/load: no.
- Credentials required for install/load: no.
- Package-side files written on load: none.

### Final `R CMD check --as-cran`

- Input: the tarball identified above, rebuilt from `main`.
- R: 4.5.1 (ucrt), Windows 11 x64 build 26200.
- Errors: 0.
- Warnings: 0.
- Notes: 1.
- Literal summary: `Status: 1 NOTE`.
- Note classification: inherent first-submission note (`New submission`).
- Future timestamps: OK.
- Temporary-directory detritus: OK.
- Tests installed from tarball: 16 minutes, OK.
- Vignettes rebuilt: 322 seconds, OK.
- PDF manual: OK.
- HTML manual: OK.

## Remote verification

### Development candidate

- Source SHA: `e46b53083663403e12e3c76cb5902f6a0ce2ed44`.
- R-CMD-check run: <https://github.com/qselmer/oceancube/actions/runs/34258859035>.
- Ubuntu: SUCCESS.
- macOS: SUCCESS.
- Windows: SUCCESS.

### Main source certification

- `origin/main` source SHA:
  `e46b53083663403e12e3c76cb5902f6a0ce2ed44`.
- R-CMD-check run: <https://github.com/qselmer/oceancube/actions/runs/34260144662>.
- Ubuntu: SUCCESS.
- macOS: SUCCESS.
- Windows: SUCCESS.
- Pages run: <https://github.com/qselmer/oceancube/actions/runs/34260144653>.
- Pages: SUCCESS.
- The final repository-only certification commit must also have a green
  Ubuntu/macOS/Windows matrix before task closure.

## Release audits

### Name and URLs

- CRAN current `PACKAGES`: zero case-insensitive `oceancube` matches.
- CRAN archive `oceancube/`: HTTP 404.
- Bioconductor 3.23 release software: zero matches.
- Bioconductor 3.24 devel software: zero matches.
- DESCRIPTION repository URL: HTTP 200.
- BugReports URL: HTTP 200.
- pkgdown URL: HTTP 200.
- CRAN incoming URL checks: PASS.
- Automated spelling package was unavailable locally; Rd syntax, metadata,
  contents, usage, examples, vignettes, and manuals all passed CRAN check.

### Dependency audit

- PASS; see `dependency-audit.md`.
- Imports match runtime use: `cli`, `ggplot2`, `grDevices`, `ncdf4`, `rlang`,
  `stats`, and `utils`.
- Optional packages remain in Suggests and are guarded where required.
- Python, credentials, internet, and external services are not required for
  core installation, loading, tests, or examples.

### License and data audit

- PASS; see `bundled-data-license-audit.csv` and the versioned fixture
  manifests.
- Five included NOAA-derived NetCDF fixtures have recorded provider, product,
  purpose, byte size, SHA-256, attribution, and provenance.
- Development visualization downloads and gallery outputs are excluded from
  the source tarball.

### Network and side-effect audit

- PASS; see `cran-code-effect-audit.md`.
- Tests and examples are offline, deterministic, and non-interactive.
- No credentials, tokens, passwords, API keys, or signed URLs are bundled.
- No unrequested install, browser, shell, home-directory write, or live-network
  action occurs during install, load, tests, or examples.

## Git consolidation and recovery

- Permanent local branch target: `main` only.
- Permanent remote branch target: `origin/main` only.
- Historical tags preserved:
  - `v0.1.0` -> `93d2a79b11a6ae7622443ae068e6e2a2709c9324`
  - `v0.2.0` -> `d83008066ba3b1f3ea8df3e7ca3001d472f20308`
- Removed unique backup branch preserved at:
  `D:\.packages\.oceancube-backup\oceancube-c-exit-blocked-4c491-20260908.bundle`.
- Backup bundle head:
  `4c4914531932db74c1e8d7c85f5fc457cc6d67da`.
- Backup bundle size: 20,511,945 bytes.
- Backup bundle SHA-256:
  `71e39c67541848bf96a32a290554c9377847cb1f65a1673cf30ce6206b298001`.
- `git bundle verify`: PASS; complete history recorded.

## Decision

OCEANCUBE 0.3.0  
CRAN RELEASE CANDIDATE  
COMPLETE / CERTIFIED

CRAN readiness: YES, subject only to the maintainer's explicit manual CRAN
submission. Tagging and GitHub Release creation remain deferred until CRAN
acceptance.
