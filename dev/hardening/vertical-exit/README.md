# C-EXIT vertical ocean science evidence

This directory is the machine-readable local exit record for C1–C10. The
review starts from certified C-GOVR commit `9865434038b1c7dbbe4da3959341d27d15029ee6`
on `dev-0.3.0`. It is a test, documentation, and governance change only:
`R/`, `DESCRIPTION`, `NAMESPACE`, dependencies, version, and the 48-export
public surface are unchanged.

The integrated test is
`tests/testthat/test-vertical-science-global-exit.R`. It composes bounded
reductions, temperature gradient/feature/thermocline, temperature MLD, oxygen
branches and threshold boundaries, TEOS-10 state, density MLD, pycnocline, and
N-squared. It also checks exact signatures, source-CF immutability, semantic
distinctions, gap rejection, memory materialization, selection, provenance
carriage, and serialization. The test is required to pass in two fresh R
sessions with identical outcomes.

The CSV files freeze the chain, matrices, fixture hashes, metadata, execution,
and regression evidence. The final source suite reports 79 files, 824 cases,
6,311 expectations, and 1,700.24 seconds with zero failures, errors, test
warnings, or skips. The focused global test passes twice in fresh sessions.

Direct `R CMD build .` reproduces only the governed Codex
`.git/refs/codex/turn-diffs` recursive-copy defect. A complete clean snapshot
excluding `.git` and ignored build artifacts produces
`oceancube_0.2.0.9000.tar.gz`. Its final size and SHA-256 are reported by the
local certification rather than embedded in the self-referential source
archive. `R CMD check --no-manual` reports `Status: OK`, with zero errors,
warnings, and notes and with C9/C10 `gsw` tests executed.

Installed-package smokes pass both dependency modes. Without `gsw`, ordinary
public operations work and both TEOS entry points return the deterministic
dependency error. With `gsw 1.2.0`, the existing installed C10 smoke exercises
the C1-C10 public chain with 48 exports and zero internal `:::` calls.

C-GOVR already reconciled DEC-037 through DEC-039 exactly once and closed
`CEXIT-GOV-001` plus `CEXIT-SPEC-001`. The C-EXIT validator preserves DEC-001
through DEC-036, verifies DEC-037 through DEC-040 exactly once, and leaves
DEC-041 unallocated. Under the certified evidence policy this directory is
repository-only; the distributed architecture and policy files, not this
directory, are required in the source tarball.
