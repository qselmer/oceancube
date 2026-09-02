# OCEANCUBE 0.3.0-C9 thermodynamic-state evidence

C9 implements DEC-038 through the sole new export
`thermodynamic_state()`. The engine is a bounded TEOS-10 state bridge, not a
general GSW wrapper. It supports exact CF-identified SP/SA and in-situ/pt0/CT,
two deterministic pressure origins, strict source domains and funnel policy,
five ordered outputs, current CF descriptor 1.0.0, Provenance V1, QA, and
one-read deferred NetCDF execution.

The dependency audit used the current CRAN package page, CRAN platform checks,
TEOS-10 GSW-R/GSW-C repositories, package source, license, APIs, and official
reference vectors. `gsw` 1.2-0 passed isolated Windows install/load and
official numerical parity. CRAN reports current `OK` checks for Linux,
Windows, and both macOS architectures, so DEC-038 places it in `Suggests`.

WOA23 monthly T/S cell means, OISST, and oxygen-only fixtures remain negative
scientific tests. C9 adds no density MLD, pycnocline, N2, dynamic height,
fallback equation, Python, or vendored code. Final certification passed 77
test files, 806 cases, 6,184 expectations, source build, `R CMD check` with
status `OK`, and an isolated installed public-API smoke. Exact metrics and the
tarball checksum are recorded in `c9-certification.csv`.
