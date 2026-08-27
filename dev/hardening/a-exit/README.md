# 0.3.0-A-EXIT hardening certification

This directory is the auditable exit record for the `0.3.0-A` hardening
stage. The review started from
`a31c73c9559e878333d0286f11c357f2bb6d8444` on `dev-0.3.0` with a clean
working tree. It is a governance and regression certification, not a feature
implementation.

## Outcome

- `0.3.0-A complete`: **TRUE**.
- ready to begin `0.3.0-B CF + interoperability`: **TRUE**.
- Gate B, whose canonical meaning is permission to begin vertical science:
  **UNSATISFIED**.
- unresolved S0 blockers: **0**.
- unresolved S1 blockers: **0**.
- public API: **39 exports**; `cube_open()` remains the sole post-A5 export.
- package version: **0.2.0.9000**.
- scientific results, `R/`, `NAMESPACE`, `DESCRIPTION`, and dependency fields:
  **unchanged**.

The distinction above is intentional. A-EXIT completes the hardening stage and
permits the next CF/interoperability work block. Gate B can only be satisfied
after that work has sufficiently defined the CF/vertical coordinate contract,
redistributable depth evidence, and approved vertical primitive semantics.

## Executed certification

The Windows R 4.5.1 UTF-8 run executed the current parity, P1-P8 property,
provenance, coast-distance, governed-real-data, deferred-I/O, scientific, time,
privacy, file-identity, serialization, and lifecycle tests. The complete suite
reported 62 files, 612 `test_that()` cases, 4,979 expectations, 0 failures,
0 errors, 0 test warnings, and 0 skips in 559.940 seconds. The elapsed time is
informational; the A6 counts and outcomes are unchanged.

The A6 raw evidence was audited rather than regenerated: 140 isolated peak-RSS
rows, TINY/SMALL/MEDIUM coverage, a plausible 1.02875 calibration ratio, no RED
classification, connection delta zero, a passing rename/restore probe, and a
passing offline OISST smoke remain intact. LARGE-LOCAL was not run.

## Test inventory evolution

Counts are evidence of suite evolution, not a quality metric by themselves.

| Milestone | Files | Cases | Dynamic expectations |
|---|---:|---:|---:|
| A1 | 40 | 506 | 3,675 |
| A2 | 45 | 533 | 4,198 |
| A3b | 46 | 540 | 4,284 |
| A4-EXIT | 60 | 598 | 4,857 |
| A5b | 62 | 612 | 4,979 |
| A6 / A-EXIT | 62 | 612 | 4,979 |

The source snapshot passes a `.git`-free clean-snapshot build and
`R CMD check --no-manual` with `Status: OK` (0 errors, 0 warnings, 0 notes)
under the native Windows `English_United States.utf8` locale. An isolated
installed-package check confirms version `0.2.0.9000`, 39 namespace exports,
zero exported functions missing documentation, public `cube_open()` and
`read_nc()`, and the public OISST eager/deferred/materialization smoke. The
final tarball size and SHA-256 are reported with the commit certification.

## Supported contracts retained

- `read_nc()` is public and eager, returns the memory backend, and has `x$data`.
- `cube_open()` is public and experimental for one local, read-only NetCDF
  file. It returns the ordinary `ocean_cube`/list class with `x$storage` and no
  `x$data`, performs deferred bounded reads, and does not add remote,
  multi-file, provider, Zarr, or lazy-DAG behavior.
- `cube_collect(netcdf)` materializes to memory;
  `cube_collect(memory)` is an exact no-op.
- Deferred file identity remains path + size + mtime + compatible schema.
  Deleted, moved, or changed sources fail deterministically. No persistent
  `ncdf4` handle is retained.
- Provenance V1 remains schema `1.0.0`, has zero active legacy runtime
  producers, one schema across all certified output families, deterministic
  multi-input lineage, semantic/privacy/serialization validity, and explicit
  QA and CF separation.
- Polygon weights retain the exact compatibility alias
  `attr(x, "provenance") == attr(x, "oceancube_provenance")`.
- `coast_dist()` controls S2 locally, restores caller state on success and
  error, and records `oceancube:s2_coast_distance v1` without changing polygon
  numerical semantics.

## Real-data and external boundaries

The governed offline fixture set remains exactly OISST v2.1, ETOPO 2022 v1,
and WOA23 v3.3 (1,333,084 committed bytes). Each fixture has source and final
SHA-256 evidence, derivation records, attribution/legal review, and CI-safe
status. Package tests use no provider network.

OISST passes the supported public path with explicit `depth_name = "zlev"`.
ETOPO remains a truthful static no-time expected limitation. WOA remains a
truthful `months since` expected limitation. Copernicus redistribution remains
unknown and maintainer-only; successful real Python/auth/network integration is
optional. None blocks hardening exit.

Source acquisition remains separate from cube opening/reading. Provider
adapters plus a common planner/manifest/fetch layer remain future roadmap
concepts. Copernicus Marine, NOAA, NASA Earthdata, ERDDAP, local,
OPeNDAP/THREDDS, and future Zarr are vision items, not current implementations.

The package boundary is unchanged: oceancube owns field-to-field,
field-to-profile/section/geometry, and physical-diagnostic preparation;
spatind owns indicator/index/inference/regime construction. No function moves
between packages in A-EXIT.

## Accepted and next-stage debt

`A4R-001` and `A6-001` are accepted S3 nonblocking debt. The latter documents
the exact current sparse-selection behavior: logical non-contiguous selection
is read through the minimum enclosing physical NetCDF envelope, causing
bounded over-read. Any future optimization must preserve order, duplicates,
values, parity, and provenance.

`A3B-001`, `A3B-002`, `A3B-003`, and the remaining bounded `A1-002` NetCDF/time
debt are inputs to `0.3.0-B`, alongside open decisions `DEC-015` and `DEC-023`.
They are not implemented here.

## Evidence index

- `a-exit-certification.csv`: machine-readable acceptance facts.
- `a-exit-phases.csv`: complete A1-through-A6 phase reconciliation.
- `a-exit-findings.csv`: every hardening finding and its exit disposition.
- `a-exit-gates.csv`: Gate A, hardening exit, and Gates B-E.
- `a-exit-regression-matrix.csv`: executed and inherited regression evidence.

The single recommended next subphase is
**0.3.0-B1 — CF METADATA FOUNDATION AND CF ENGINE DECISION**. This A-EXIT
record does not execute or authorize B1 implementation by itself.
