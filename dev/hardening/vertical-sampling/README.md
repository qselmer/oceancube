# OCEANCUBE 0.3.0-C3 — vertical sampling evidence

C3 implements DEC-032 through the sole new export `depth_sample()`. The C2
classifier remains the one semantic authority: CF cell means resolve to exact
source-cell sampling under a piecewise-constant reconstruction, and CF point
values resolve to exact-point retrieval or local linear interpolation. Mixed
automatic plans preserve a distinct method per variable.

The bounded contract requires DEC-029 metric ocean depth. Cell sampling also
requires DEC-030 explicit valid bounds. Explicit gaps, shared interior cell
boundaries, outside-domain requests and extrapolation are rejected before a
payload read. Sampled cubes keep field units and requested depth coordinates
but carry no physical cell bounds, preventing false downstream thickness,
volume or integration support.

The CSV files record the supported subset, method resolution, analytic results,
gap/boundary and missingness policies, WOA behavior, deferred reads, downstream
safety, regressions, API delta and final local certification. A3B-001 remains
CLOSED, A3B-002 OPEN, A3B-003 OPEN-RECLASSIFIED, B7-001 PARTIALLY-CLOSED, Gate B
SATISFIED for DEC-029 and 0.3.0-C IN PROGRESS.

## Local certification

The final source suite completed with 71 files, 702 cases and 5,584
expectations in 984.74 seconds: zero failures, errors, warnings and skips.
The direct repository build encountered the known Codex-internal `.git` ref
copy defect, so the governed build used a source snapshot excluding only
`.git` and ignored `artifacts`. `R CMD build` passed and produced
`oceancube_0.2.0.9000.tar.gz` (2,964,799 bytes; SHA-256
`A4773C038030294612D22B14AC21AF7D309DCFBEF7C891500C37BB26213487D6`).
`R CMD check --no-manual` completed with `Status: OK`, zero errors, zero
warnings and zero notes. The isolated installed-package smoke passed with 41
exports, the exact `depth_sample(x, depth, method)` signature, all three
method plans, deferred sampling, downstream geometry rejection and governed
WOA/OISST/ETOPO cases. The machine-readable metrics are recorded in
`c3-certification.csv`.
