# OCEANCUBE 0.3.0-C10 — density and stratification evidence

DEC-039 certifies density-threshold MLD, an operational pycnocline candidate,
and signed TEOS-10 N2. Evidence is offline and traceable to C9, C4/C5, GSW-R,
and CF. Density MLD accepts only certified potential density referenced to
0 dbar, defaults to a positive 0.03 kg m-3 departure from 10 m, and never uses
an absolute departure. Pycnocline selection composes the C4 signed gradient
and C5 positive-polarity candidate engines without adding a threshold, width,
or smoothing. Stratification delegates exclusively to `gsw_Nsquared()`, keeps
signed N2 and `p_mid`, splits finite runs at missing states, and preserves
explicit support gaps.

## Local certification

The final source suite completed with 78 files, 817 cases, and 6,242
expectations in 1,244.21 seconds: zero failures, errors, test warnings, and
skips. The GSW-dependent C10 tests executed. Direct `R CMD build .` reproduced
only the governed Codex `.git/refs/codex/turn-diffs` copy defect; the bounded
fallback uses a complete source snapshot excluding `.git` and ignored build
artifacts. `R CMD check --no-manual` completed with `Status: OK`, zero errors,
warnings, and notes under the Windows UTF-8 locale. The installed public smoke
uses an isolated library containing `gsw` 1.2.0 and the built oceancube tarball;
all required C10 scenarios and C6-C9 regressions pass with 48 exports and no
internal calls.
