# OCEANCUBE 0.3.0-C6 — transition-layer evidence

C6 implements DEC-035 through the sole new export `transition_layer()`. Exact
preserved source CF `standard_name`, followed by a bounded compatible-unit
check, establishes temperature or salinity identity. Variable names and
`long_name` never do. Thermocline uses the strongest eligible negative
temperature gradient; halocline uses the strongest eligible absolute salinity
gradient and retains its sign.

C6 composes C4 and C5. It does not duplicate gradient calculation, ranking,
ties, missingness, completeness, or local/all support. Direct source profiles
and certified gradients with original source semantics are supported;
C1/C2/C3 products and gradients derived from C1/C3 remain excluded. All
diagnostics are unthresholded candidates. Gapped and incomplete states remain
visible, and localization half-span remains resolution rather than statistical
uncertainty.

The CSV files record standard-name and unit contracts, diagnostic and variable
resolution, synthetic results, derived-input policy, governed WOA metadata and
results, bounded I/O, regressions, API audit, and final local certification.
Oxycline is deferred because an upper/lower interpretation requires an oxygen
minimum or OMZ-core branch context absent from gradient polarity alone.

The final source suite completed with 74 files, 757 cases and 5,914
expectations in 1,154.40 seconds: zero failures, errors, warnings and skips.
The direct repository build reproduced the known non-package
`.git/refs/codex/turn-diffs` copy defect, so the governed build used a complete
1,410-file, 39,983,724-byte snapshot excluding only `.git` and `artifacts`.
`R CMD build` passed and produced a 2,997,272-byte tarball with SHA-256
`174F0FDA920A5D550CC99BC59E7FE62AD73F81F25C30064D7B41DC7F8A4566F8`.
UTF-8 `R CMD check --no-manual` completed `Status: OK` with zero errors,
warnings, or notes. A public-API-only smoke from the installed tarball also
passed. Commit metrics are recorded after the sole bounded local commit.
