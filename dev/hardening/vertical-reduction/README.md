# OCEANCUBE 0.3.0-C2 — vertical reduction evidence

C2 adds exactly `layer_integral(x, depth)` and implements DEC-031 through the
shared `.vertical_reduce()` engine. Certified inputs require DEC-029 metric
depth, DEC-030 explicit valid bounds, full geometric coverage and exact CF
vertical cell-mean semantics. Integration uses metres, symbolic units, a
piecewise-constant cell-mean assumption and strict missingness. No false
`depth: sum`, current `standard_name`, interpolation, extrapolation, conversion,
static, multifile or remote claim is introduced.

The CSV files record the supported subset, classifier, numerical policies,
analytic and WOA results, deferred reads, regressions, API delta and final local
certification. B7-001 remains PARTIALLY-CLOSED; A3B-001 is CLOSED, A3B-002 is
OPEN, A3B-003 is OPEN-RECLASSIFIED, Gate B remains SATISFIED for DEC-029 and
0.3.0-C remains IN PROGRESS.

## Local certification

The full source suite completed with 70 files, 683 cases and 5,486 expectations
in 979.25 seconds: zero failures, errors, warnings and skips. The direct build
encountered only the governed `.git/refs/codex/turn-diffs` copy defect, so the
clean-snapshot fallback excluded `.git` and `artifacts` and built
`oceancube_0.2.0.9000.tar.gz`. Its SHA-256 is
`532CFBA9516D193B29DB64339701A7C3CFBAC98F95D3A3788987166908A83345`.
`R CMD check --no-manual` completed `Status: OK` (0/0/0) under the native
Windows UTF-8 locale. Installation to an isolated library and the public-only
synthetic, WOA, missingness and expected-rejection smoke all passed.
