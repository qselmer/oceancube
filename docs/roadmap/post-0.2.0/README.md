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
- Public API: 39 exports; `cube_open` is the sole A5b addition
- Runtime producer migrations: all identified producers complete and globally certified

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

A5b implements `cube_open()` as one experimental public source-opening
primitive for the existing local read-only deferred NetCDF backend. `read_nc()`
remains eager, `cube_collect()` remains the explicit materialization boundary,
`DEC-018` is implemented/certified, and `A1-004` is closed. This does not
complete 0.3.0-A or satisfy Gate B: `A1-009` remains open. The single
recommended next subphase is A6. No CF, vertical, provider, multifile, remote,
Zarr, or 3-D work is authorized by A5b.

## Change control

Future roadmap changes should update the provenance matrix first, reference an
existing or new decision ID when maintainer judgment is required, preserve
unique IDs, and keep proposals distinct from approved decisions. Runtime
contracts continue to live in source, tests, and roxygen documentation; this
area records why later changes may be authorized.
