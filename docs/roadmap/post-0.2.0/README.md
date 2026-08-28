# oceancube — canonical post-0.2.0 roadmap

This directory is the design gate for oceancube development after the stable
0.2.0 release and through the provisional 1.0.0 scientific contract. It is an
architecture and provenance record, not an implementation plan that authorizes
code changes by itself.

## Status

- Canonical roadmap revision: v1
- Stable release: oceancube 0.2.0
- Stable tag: `v0.2.0`
- Current development cycle: 0.2.0.9000 toward 0.3.0
- Active development branch: `dev-0.3.0`
- Gate A: satisfied
- 0.3.0-A-EXIT: hardening phase complete and certified on 2026-08-27
- Ready to begin 0.3.0-B CF + interoperability: true
- 0.3.0-B: in progress
- 0.3.0-B1: CF metadata foundation and engine decision complete
- 0.3.0-B2: CF metadata preservation engine implementation and local package
  certification complete; remote CI governs final closure
- Gate B (permission to begin vertical science): unsatisfied
- 0.3.0-A3b: governed real-data fixtures and offline-CI evidence completed
- 0.3.0-A4a: provenance V1 architecture and compatibility design completed
- 0.3.0-A4b1: internal provenance V1 core engine and legacy normalizer completed
- 0.3.0-A4b2: linear core runtime provenance producers migrated to V1
- 0.3.0-A4b3a: temporal and multi-input producers plus delegating wrappers migrated to V1
- 0.3.0-A4b3b: table, geometry, layer, coast, and stock producers migrated to V1
- 0.3.0-A4-EXIT: Provenance V1 globally certified; A4 closed
- 0.3.0-A4R: coast distance standardized on locally controlled spherical S2;
  `A4B3B-001` closed
- 0.3.0-A5a: `cube_open()` approved as the one experimental public entry for
  local read-only deferred NetCDF
- 0.3.0-A5b: exact `cube_open()` contract implemented and certified; `read_nc()`
  remains eager
- DEC-024: approved for the exact OISST/ETOPO/WOA23 fixture set
- DEC-019: approved — implemented/certified by A4a through A4-EXIT evidence
- DEC-018: approved — implemented/certified by A5b; `A1-004` closed
- DEC-015: approved — HYBRID IMPLEMENTED FOUNDATION; canonical plain-R
  metadata is owned by oceancube and ncdfCF remains an optional
  oracle/reference/adapter
- Public API: 39 exports; `cube_open` is the sole A5b addition
- Runtime producer migrations: all identified producers complete and globally certified
- Hardening findings: no unresolved S0/S1 blocker; accepted debt and
  0.3.0-B inputs remain open under explicit classifications

The historical `docs/roadmap/oceancube-v0.2.0-scope.Rmd` and the audit,
decision, relocation, and auxdata directories remain immutable evidence of how
0.2.0 was planned and reconciled. They are not the current source of future API
commitments. In particular, their approximately 114 proposed functions must not
be interpreted as 114 future exports.

## Authoritative files

- `oceancube-roadmap-v1.Rmd` is the human-readable architecture, staged
  roadmap, dependency graph, gates, boundary, and technical-debt register.
- `roadmap-provenance.csv` is the machine-readable source of roadmap work
  packages and their provenance.
- `roadmap-decisions.csv` separates approved architecture from open, deferred,
  and superseded decisions.

Proposals become implementation work only after their prerequisites and the
relevant roadmap gate have been satisfied. A row marked `PROPOSED` is not a
public API promise. Candidate function names remain provisional until a
separate API and methodological review approves them.

A-EXIT certifies `0.3.0-A` complete after A1-A6 evidence, with `A1-009`
closed, 39 exports retained, and no unresolved S0/S1 blocker. This permits the
staged `0.3.0-B` CF/interoperability block to begin, but Gate B remains
unsatisfied because its canonical vertical-science prerequisites have not yet
been met. B1 establishes the lossless CF 1.13 architecture and HYBRID engine
decision. B2 implements the versioned `x$metadata$cf` source/current model,
single scanner, simple links, schema validation, shared conflict-aware axis
resolver, eager/deferred parity, exact collect preservation, and safe
transformation handoff without new exports or dependencies. OISST `zlev` now
resolves semantically in both readers and `A3B-001` is closed. `DEC-023`,
`A3B-002`, and `A3B-003` remain open. The single recommended next subphase is
`0.3.0-B3 — CF SUPPORTED-SUBSET INTERPRETATION AND VALIDATION`. No vertical,
provider, multifile, remote, Zarr, or 3-D implementation is authorized by B2.

## Change control

Future roadmap changes should update the provenance matrix first, reference an
existing or new decision ID when maintainer judgment is required, preserve
unique IDs, and keep proposals distinct from approved decisions. Runtime
contracts continue to live in source, tests, and roxygen documentation; this
area records why later changes may be authorized.
