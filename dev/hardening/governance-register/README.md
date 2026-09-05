# OCEANCUBE 0.3.0-C-GOVR governance-register evidence

C-GOVR reconciles the already certified C8, C9, and C10 decisions into the
canonical post-0.2.0 register. It adds DEC-037, DEC-038, and DEC-039 exactly
once, preserves DEC-001 through DEC-036 byte-for-byte as parsed CSV rows, and
leaves DEC-040 unassigned.

The blocked premature C-EXIT commit is preserved only on local branch
`backup/c-exit-blocked-4c491` at
`4c4914531932db74c1e8d7c85f5fc457cc6d67da`. Active development was restored
to C10 (`731ab8b077834499ca29d16595c1744049630590`) before this bounded
reconciliation. No C-EXIT files were recovered.

This directory is repository-only certification evidence under
`inst/architecture/oceancube-certification-evidence-policy-v1.md`. The
intentional `.Rbuildignore` rule `^dev($|/)` excludes it from the distributed
source tarball. C1-C10 remain technically COMPLETE/CERTIFIED; C-EXIT remains
blocked pending remote C-GOVR certification and a fresh bounded rerun.

Future approved/implemented/certified decisions must be reconciled into
`roadmap-decisions.csv` before their enclosing global phase exit. IDs are
allocated sequentially; DEC-040 is next but is not allocated here.
