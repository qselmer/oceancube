# OCEANCUBE 0.3.0-C8 mixed-layer evidence

C8 implements DEC-037 through the sole new export `mixed_layer_depth()`.
Runtime certification is restricted to direct point-valued temperature
profiles and the configurable first absolute-departure threshold from a
near-surface reference. Reference/crossing interpolation, physical ordering,
metre/kilometre equivalence, gaps, missingness, inversions, open bottoms,
Provenance V1, and QA are governed explicitly.

The accompanying density document is architecture only. C8 calculates no
density or potential density, adds no TEOS-10 dependency, performs no pressure
conversion, and implements no density, gradient, hybrid, pycnocline, or `N2`
diagnostic. WOA23 temperature cell means and surface-only OISST are negative
regressions, not fabricated MLD products.

Evidence tables freeze the method, source semantics, reference and crossing
rules, gap and missingness policies, analytic outcomes, regression surface,
future density paths, public API delta, and local certification.

The final source suite ran 76 files, 789 cases, and 6,097 expectations in
1,370.660 seconds with no failures, errors, warnings, or skips under the native
Windows UTF-8 locale. Direct `R CMD build .` encountered the governed Codex
`.git/refs/codex/turn-diffs` copy defect, so the approved clean snapshot
fallback excluded only `.git` and transient artifacts. It produced a
3,034,990-byte tarball with SHA-256
`971EFD71FBD56E337FAF0D1F0A2ECD61731D170E43C17450DD7F57AFFDDED1FB`.
`R CMD check --no-manual` returned `Status: OK` (0 errors, 0 warnings, 0
notes), and the isolated installed public smoke passed with 46 exports and no
internal `:::` calls.
