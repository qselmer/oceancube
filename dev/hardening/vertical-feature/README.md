# OCEANCUBE 0.3.0-C5 — vertical feature evidence

C5 implements DEC-034 through the sole new export `depth_feature()`. It
consumes only current certified C4 gradient cubes, ranks one generic candidate
per longitude, latitude, time and variable, and performs zero NetCDF scientific
payload reads. Absolute, positive and negative polarity remain explicit.

Local policy admits contiguous support and point brackets but excludes gapped
secants. All-support policy may return a gapped secant only under the truthful
`GAPPED_SECANT_CANDIDATE` label. Effective-zero profiles remain flat, tied
scores remain ambiguous, and incomplete profiles cannot claim a guaranteed
global maximum. C4 midpoint, canonical depth, bracket, spacing, gap, signed
gradient and units remain visible. Localization half-span is a vertical
resolution scale, not statistical uncertainty.

The CSV files record the supported subset, polarity and support policies,
status taxonomy, analytic/tie/missingness/gap results, governed WOA behavior,
table contract, regressions, API audit and final local certification. A3B-001
remains CLOSED, A3B-002 OPEN, A3B-003 OPEN-RECLASSIFIED, B7-001
PARTIALLY-CLOSED, Gate B SATISFIED and 0.3.0-C IN PROGRESS.

## Local certification

The final source suite completed with 73 files, 738 cases and 5,811
expectations in 280.14 seconds: zero failures, errors, warnings and skips.
The governed 1,391-file source snapshot built a 2,985,657-byte tarball with
SHA-256
`7984E0C1244F9C388851CD65AEA7E5238EAF0B9B7D146F3C6DDA25D714A54369`.
`R CMD check --no-manual` completed with Status OK and zero errors, warnings or
notes. The isolated installed-package smoke passed through public APIs only,
including the 43-export boundary, synthetic policy cases, WOA23 and direct
input rejections. Exact gate results are recorded in `c5-certification.csv`.
