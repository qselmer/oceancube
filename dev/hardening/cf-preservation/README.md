# OCEANCUBE 0.3.0-B2 — CF metadata preservation engine

Status: **runtime foundation and local package certification complete; remote
CI pending exact-SHA push authorization**.

Normative architecture:
[`inst/architecture/oceancube-cf-metadata-foundation-v1.md`](../../../inst/architecture/oceancube-cf-metadata-foundation-v1.md).

The production engine implements the B1 HYBRID decision without adding a
runtime dependency. `R/cf-metadata.R` is the single native NetCDF metadata
scanner, simple-link interpreter, strict schema validator, current-view
builder, transformation guard, and common conflict-aware axis resolver.

## Implemented contract

- canonical location: `x$metadata$cf`;
- outer schema: `oceancube_metadata 1.0.0`;
- CF schema: `oceancube_cf_metadata 1.0.0`;
- plain-R, deterministic, path-private and exactly serializable state;
- immutable source declaration, globals, every dimension, every variable,
  observed ordering, attributes, roles, and linked relationships;
- source IDs are path-qualified and group-safe;
- no scientific array, NetCDF handle, connection, external pointer, R6,
  ncdfCF, RNetCDF, or CFtime object enters canonical metadata;
- `read_nc()` and `cube_open()` use the same scanner and resolver;
- `cube_collect()` preserves metadata exactly;
- selection updates the compact current view; meaning-changing operations
  preserve source and mark current derivation pending.

The scanner calls no `ncvar_get()` and can inspect ETOPO without time and WOA
before unsupported `months since` decoding. Current cube representability and
time-decoding rejections remain unchanged.

## Common axis resolver

The resolver gathers explicit override, coordinate/dimension structure,
`axis`, `standard_name`, `units`, `positive`/formula evidence, and bounded
known-name fallback before returning `RESOLVED`, `AMBIGUOUS`, `CONFLICT`, or
`UNRESOLVED`. Explicit overrides remain highest authority but cannot suppress
strong contradictions.

OISST `zlev` is now resolved by `positive=down` in both eager and deferred
readers without an override. Explicit `depth_name="zlev"` remains supported.
The source metadata trees and collected numerical values are identical across
the two paths. `A3B-001` is therefore **CLOSED** by semantic evidence, not by
an alias alone.

## Bounded interpretation

Simple `coordinates`, `bounds`, `climatology`, `ancillary_variables`,
`cell_measures`, `grid_mapping`, and `formula_terms` links preserve raw text,
tokens, candidates, resolved paths, parser status, and deterministic link
status. Extended CF 1.13 `grid_mapping` remains raw and
`DEFERRED_EXTENDED`. `cell_methods`, flags, `standard_name`, units, unknown
attributes, and provider history are preserved exactly without full grammar,
masking, conversion, lookup, printing, or provenance promotion.

## Metadata size smoke

The governed OISST fixture produced 232,976 metadata bytes for 221,184 logical
double-payload bytes (ratio 1.053313). The deliberately tiny CF-rich synthetic
fixture produced 269,576 metadata bytes for 128 payload bytes (ratio
2106.0625). These descriptive ratios reflect R list overhead on tiny fixtures;
recursive checks prove neither object contains a scientific array or live
backend object.

## Governance outcome

- `DEC-015`: **APPROVED — HYBRID IMPLEMENTED FOUNDATION**.
- `A3B-001`: **CLOSED**.
- `A1-002`: **PARTIALLY-CLOSED**; decoder convergence, time/calendar, static
  fields, and other CF branches remain.
- `A3B-002`: **OPEN**; preservation is not static-cube support.
- `A3B-003`: **OPEN**; preservation is not months-since interpretation.
- `DEC-023`: **OPEN**; no calendar representation or dependency was selected.
- Gate B: **UNSATISFIED**; vertical science remains unauthorized.
- Public claim: **CF-aware; supports a documented subset of CF 1.13**.

## Evidence index

- `b2-schema-contract.csv`: canonical schema and invariant results.
- `b2-real-fixture-results.csv`: OISST, ETOPO, and WOA runtime evidence.
- `b2-link-resolution-results.csv`: link-family and diagnostic coverage.
- `b2-axis-resolution-results.csv`: common resolver evidence and conflicts.
- `b2-eager-deferred-metadata-parity.csv`: shared-source semantics.
- `b2-metadata-propagation-audit.csv`: current operation behavior.
- `b2-regression-matrix.csv`: numerical, temporal, provenance, package, and CI
  certification ledger.

## Local package certification

- full suite: 64 files, 627 cases, 5,095 expectations, 0 failures, 0 errors,
  0 warnings, 0 skips, 622.530 seconds;
- direct `R CMD build .`: known `.git/refs/codex/turn-diffs` copy defect
  reproduced; approved clean-snapshot fallback PASS;
- `R CMD check --no-manual`: `Status: OK` with UTF-8 Windows locale, 0 errors,
  0 warnings, 0 notes;
- isolated installed-package smoke: PASS with 39 exports, eager/deferred OISST
  metadata, semantic `zlev`, exact source parity, exact collect preservation,
  and exact numerical parity.

GitHub Actions remains unrun until the bounded commit exists and its exact SHA
is authorized for a non-force push to `origin/dev-0.3.0`.

## Next boundary

The single recommended next subphase is:

**0.3.0-B3 — CF SUPPORTED-SUBSET INTERPRETATION AND VALIDATION**.

B3 is not implemented here. Static cubes, duration-month decoding,
non-Gregorian calendars, formula evaluation, coordinate transformation,
vertical science, providers, multifile, remote I/O, and Zarr remain outside
B2.
