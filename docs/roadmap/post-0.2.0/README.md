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
  certification complete
- 0.3.0-B3: CF supported-subset interpretation and validation implemented and
  certified
- 0.3.0-B4: hybrid CF time/calendar representation architecture approved;
  bounded core runtime implemented and certified in B5
- 0.3.0-B5: calendar-aware CF time engine implemented for fixed elapsed units,
  supported historical calendars, hybrid base compatibility, selection,
  preservation, and explicit Tier-2 temporal-analysis guards
- 0.3.0-B6: generic bounded climatological-time semantics implemented;
  annual WOA23 remains safely rejected and A3B-003 is OPEN-RECLASSIFIED
- 0.3.0-B7: CF vertical semantics and explicit-bounds metric geometry
  implemented and certified for the governed WOA23 January fixture
- Gate B (permission to begin vertical science): satisfied for dimensional
  metric ocean depth with certified CF semantics and explicit bounds
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
- DEC-015: approved — HYBRID ACTIVE SUPPORTED-SUBSET ENGINE; canonical plain-R
  metadata is owned by oceancube and ncdfCF remains an optional
  oracle/reference/adapter
- DEC-023: approved — HYBRID CALENDAR-AWARE TIME MODEL; CORE RUNTIME
  IMPLEMENTED/CERTIFIED in B5; current exact Date/POSIXct values remain
  unchanged, non-base calendars use oceancube-owned plain state, and CFtime is
  an optional transient development oracle only
- DEC-029: approved — dimensional metric ocean depth with certified CF
  semantics and explicit bounds is the authorized Gate-B subset
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
staged `0.3.0-B` CF/interoperability block to begin. B1 establishes the
lossless CF 1.13 architecture and HYBRID engine
decision. B2 implements the versioned `x$metadata$cf` source/current model,
single scanner, simple links, schema validation, shared conflict-aware axis
resolver, eager/deferred parity, exact collect preservation, and safe
transformation handoff without new exports or dependencies. B3 adds the
versioned CF 1.13 supported-subset contract, deterministic metadata-only
diagnostics, structural relationship checks, explicit value/unit/table/grammar
deferrals, and separate source/current interpretation. OISST `zlev` now
resolves semantically in both readers and `A3B-001` is closed. B4 approves a
hybrid calendar-aware time model. B5 implements its bounded plain-R runtime for
fixed elapsed units, supported historical calendars, eager/deferred parity,
selection, preservation, and explicit unsupported temporal analytics while
keeping exact Date/POSIXct behavior. `DEC-023` is core-runtime certified. B6
adds bounded generic climatological time while safely rejecting the inconsistent
annual WOA23 encoding. B7 implements the CF vertical descriptor and certifies
the dimensional metric ocean-depth subset with explicit bounds against the
governed WOA23 January product. Gate B is SATISFIED under DEC-029. `A3B-002`
remains OPEN and `A3B-003` remains OPEN-RECLASSIFIED; neither static-field
support nor provider repair was introduced. Height/pressure conversion,
parametric evaluation, legacy reductions, multifile, remote, Zarr, and 3-D
remain outside this authorization.

C1 implements DEC-030 behind that bounded authorization. One internal vertical
support engine now governs explicit-bounds thickness, volume, and layer means;
certified layer weights use exact interval overlap and require full union
coverage before payload reads. Zero coverage remains missing, gaps are rejected,
descending storage and m/km conversion are explicit, and current derived bounds
and provenance are truthful. The historical centre-derived `layer_mean()` path
is numerically unchanged and explicitly uncertified, so B7-001 is
PARTIALLY-CLOSED rather than erased. Gate B remains SATISFIED only for the
DEC-029 metric-depth subset; 0.3.0-C is IN PROGRESS and C2 is next.

C2 implements DEC-031 and adds exactly one export, `layer_integral(x, depth)`.
The shared reducer admits metric integration only for certified CF vertical
cell means with explicit bounds and full coverage, computes canonical metre
overlap, applies strict integral missingness, and records truthful current
metadata plus Provenance V1. Point/sum/other/ambiguous semantics and non-depth
vertical kinds remain unsupported. B7-001 remains PARTIALLY-CLOSED, Gate B
remains bounded to DEC-029, and 0.3.0-C remains IN PROGRESS.

## Change control

Future roadmap changes should update the provenance matrix first, reference an
existing or new decision ID when maintainer judgment is required, preserve
unique IDs, and keep proposals distinct from approved decisions. Runtime
contracts continue to live in source, tests, and roxygen documentation; this
area records why later changes may be authorized.
