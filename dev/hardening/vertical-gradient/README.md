# OCEANCUBE 0.3.0-C4 — vertical gradient evidence

C4 implements DEC-033 through the sole new export `depth_gradient()`. The
scientific primitive is a signed adjacent-level first-order secant against
canonical physical ocean depth in metres, positive downward. Output locations
are source-unit midpoints and have no physical layer bounds.

Current C3 sampling and C1/C2 reduction descriptors take precedence over stale
source declarations. Certified point values and cell means remain separate;
C3 cell reconstructions and C2 metric integrals are rejected. Explicit support
gaps are not filled: every pair records `spacing_m`, `support_relation`, and
`support_gap_m`. Missing endpoints invalidate only their adjacent pairs.

The CSV files record the supported subset, semantic resolution, analytic
results, irregular spacing, sign/unit invariance, gap evidence, derived-input
policy, governed WOA behavior, regression matrix, API audit and final local
certification. A3B-001 remains CLOSED, A3B-002 OPEN, A3B-003
OPEN-RECLASSIFIED, B7-001 PARTIALLY-CLOSED, Gate B SATISFIED and 0.3.0-C IN
PROGRESS.

## Local certification

The final source suite completed with 72 files, 720 cases and 5,672
expectations in 1,008.01 seconds: zero failures, errors, warnings and skips.
The direct repository build reproduced the known Codex-internal `.git` ref
copy defect, so the governed build used a source snapshot excluding only
`.git` and ignored `artifacts`. `R CMD build` passed and produced
`oceancube_0.2.0.9000.tar.gz` (2,974,809 bytes; SHA-256
`430FE54A8713D40C764682F408EE6E34DE3B23C73BEC62D2B8F202249BDF4E44`).
`R CMD check --no-manual` completed with `Status: OK`, zero errors, zero
warnings and zero notes. The isolated installed-package public smoke passed
with 42 exports and all analytic, derived-input, geometry-safety and governed
real-data cases. Machine-readable metrics are in `c4-certification.csv`.
