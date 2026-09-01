# OCEANCUBE 0.3.0-C7 oxygen-profile evidence

C7 implements DEC-036: branch-aware upper/lower oxycline candidates and an
explicit-threshold `oxygen_boundary()` API. The evidence tables freeze the CF
vocabulary and bounded units, core/branch/gap policies, synthetic and WOA23
results, one-read deferred-I/O audit, regressions, API delta, and local
certification. The governed WOA23 January fixture is a deterministic 3 x 3 x
47 Peru subset; 20 micromoles per kilogram is test evidence only, never a
package default.

The final source suite ran 75 files, 774 cases, and 6,027 expectations in
1,422.780 seconds with no
failures, errors, warnings, or skips. The governed snapshot build produced a
3,021,949-byte tarball with SHA-256
`8B95368D0BEF590019C0EAA5BB156784A6CFA8CEAC10918A203942C8DB329EED`.
`R CMD check --no-manual` returned 0 errors, 0 warnings, and 0 notes; the
isolated installed public smoke passed with 45 exports.
