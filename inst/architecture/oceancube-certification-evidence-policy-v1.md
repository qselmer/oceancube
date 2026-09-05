# Certification evidence policy v1

This document defines where oceancube's canonical distributed contract and
its repository-only certification evidence live. It governs certification
packaging; it does not change scientific runtime behavior.

## A. Canonical package and runtime contract

The installed and source-distributed contract belongs in package surfaces
included by normal R package rules. Depending on the subject, these are
`R/`, `man/`, `inst/architecture/`, `DESCRIPTION`, `NAMESPACE`, and `tests/`.
Public behavior, metadata semantics, architecture requirements, dependencies,
exports, and executable regressions must be defined on those surfaces.

This policy file is itself a distributed architecture contract and must be
present in the source tarball at
`inst/architecture/oceancube-certification-evidence-policy-v1.md`.

## B. Repository-only certification evidence

`dev/hardening/**` contains version-controlled repository evidence such as
audit tables, benchmark evidence, certification CSVs, local smoke scripts,
fixture-derivation evidence, and development-only reproducibility records.
These records support review and reproduction in the repository, but they are
not necessarily installed-package files or source-tarball payload.

The current `.Rbuildignore` rule `^dev($|/)` intentionally excludes this
repository-only evidence from package distribution. The rule must not be
removed merely to put hardening evidence in a tarball, and the hardening CSVs
must not be copied wholesale into an installed package.

The earlier C-EXIT requirement that `dev/hardening/vertical-exit/` be present
in the source tarball was therefore a certification-specification mistake, not
a scientific package defect. Finding `CEXIT-SPEC-001` is closed by C-GOVR.

## Canonical decision authority

`docs/roadmap/post-0.2.0/roadmap-decisions.csv` is the canonical decision
register. A phase artifact may propose or record a decision during
implementation, but every decision claimed
`APPROVED — IMPLEMENTED/CERTIFIED` must be reconciled into that register before
the enclosing global phase exit can pass.

Decision IDs are allocated sequentially in the canonical register. A later
phase must not claim a new approved decision ID without reconciling it before
global exit. C-GOVR reconciles DEC-037 through DEC-039 without renaming them;
DEC-040 remains the next unassigned ID.

The absence of repository-only hardening evidence from a governed source
tarball is a passing result when the distributed contract is present and the
documented `.Rbuildignore` policy is in force.
