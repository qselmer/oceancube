# Vertical ocean science exit v1

Decision: **DEC-040 — APPROVED — IMPLEMENTED/CERTIFIED C-EXIT**.

This record closes the local scientific-integration review of C1 through C10.
It does not widen any supported subset, add an algorithm, or authorize remote
closure. Phase C is globally closed only after the exact C-EXIT commit also
passes the repository's remote Ubuntu, macOS, and Windows checks.

## Canonical physical frame

Cross-component reasoning uses dimensional metric ocean depth in metres,
positive downward. Metre and kilometre encodings and ascending and descending
storage order are equivalent after normalization. Positive-up coordinates are
accepted only by operations whose certified input contract explicitly permits
them; named transition diagnostics correctly reject them rather than silently
reinterpret an unsupported profile.

The semantic classes remain distinct:

- source point observations;
- source cell means over explicit bounds;
- sampled points;
- bounded vertical means and integrals;
- adjacent-level signed gradients;
- feature and transition tables;
- temperature- and density-threshold MLD tables;
- certified TEOS-10 point state; and
- signed N-squared values at adjacent-pair depth and pressure midpoints.

No output is promoted to another class merely because its dimensions agree.
Cell means may support C4 gradients while C8 temperature MLD and C9
thermodynamic state reject them: the former is a secant between representative
cell values, while the latter contracts require point states for threshold
localization and nonlinear thermodynamics.

## Composition contract

C1 and C2 share explicit-bounds overlap and coverage rules. C3 reconstructs
only under its point or cell contract. C4 produces signed secants from actual
metric spacing. C5 ranks candidates without creating physical interpretation.
C6 and C7 add variable-aware and branch-aware interpretations while preserving
C4/C5 evidence. C8 temperature MLD is a threshold-crossing definition, not a
gradient feature. C9 is the sole certified TEOS-10 state producer. C10 consumes
that C9 state for density MLD, pycnocline candidates, and signed N-squared; it
does not recompute the equation of state.

Temperature and density MLD are separate certified definitions. A pycnocline
is an unthresholded positive density-gradient candidate; N-squared is a signed
stability quantity evaluated by GSW at pressure midpoints. Neither is a proxy
for the other. The default density threshold remains 0.03 kg m-3; explicit
thresholds remain explicit. Negative N-squared is retained without smoothing.

## Gaps, missingness, boundaries, and ambiguity

Explicit support gaps are never silently bridged. `support = "local"` excludes
or terminates at gaps; `support = "all"` may inspect across them only when the
operation's certified contract records the gapped relation. Missing values are
not imputed. Open profile boundaries, flat profiles, ties, disjoint oxygen
cores, incomplete profiles, and unresolved crossings remain explicit statuses.
Feature localization half-span is resolution evidence, not statistical
uncertainty. No universal OMZ, ODZ, thermocline, pycnocline, or MLD threshold is
introduced by this exit.

## Metadata, lineage, and execution

Immutable source CF metadata remains separate from derived current metadata.
Every derived family retains only applicable current descriptors; stale
descriptors are cleared or rejected. Provenance V1 is the lineage mechanism,
and QA reports bounded reads, methods, support, status counts, and dependency
versions without leaking local paths. Derived products serialize exactly and
remain memory-backed. Deferred NetCDF sources use bounded reads and scientific
tests require no provider network.

The governed real-data inventory is OISST v2.1, ETOPO 2022 v1, WOA23 annual
temperature/salinity, WOA23 January temperature/salinity, and WOA23 January
oxygen. Their committed SHA-256 values match the fixture manifest. OISST is a
supported surface/time regression fixture. ETOPO remains the explicit static
no-time limitation (`A3B-002 = OPEN`). Annual WOA remains the inconsistent
climatological-time limitation (`A3B-003 = OPEN-RECLASSIFIED`). The monthly
WOA fixtures are the positive vertical and oxygen evidence.

## Governance boundary

C-GOVR is complete and certified locally and remotely at
`9865434038b1c7dbbe4da3959341d27d15029ee6`. It reconciled DEC-037, DEC-038,
and DEC-039 exactly once into the current canonical registry and closed
`CEXIT-GOV-001` and `CEXIT-SPEC-001`. DEC-040 records this global local-exit
decision exactly once; DEC-041 remains the next available identifier.

The distribution policy remains intentionally asymmetric:
`inst/architecture/**` is distributed package architecture, while
`dev/hardening/**` is repository-only certification evidence excluded by
`.Rbuildignore`. Repository evidence is therefore not required in a source
tarball and its absence there is a passing result.

`A3B-001 = CLOSED`, `A3B-002 = OPEN`,
`A3B-003 = OPEN-RECLASSIFIED`, and `B7-001 = PARTIALLY-CLOSED` remain truthful.
Gate B remains satisfied for the certified dimensional metric-depth subset.
The legacy centre-inferred `layer_mean()` path remains compatible but
uncertified.

The C chain remains inside the public package boundary: ocean-data geometry,
physical-field transformations, thermodynamics, and diagnostics are in scope;
ecological indicators, stock or population inference, regime analysis, grid
regridding, vector-ocean infrastructure, providers, and visualization are not.

## Exit decision

Within their explicitly certified supported subsets, C1–C10 are globally
coherent. Phase C is **COMPLETE/CERTIFIED LOCALLY — REMOTE EXIT PENDING**.
The next permissible subphase after remote C-EXIT is
**0.3.0-D1 — OCEANOGRAPHIC VISUALIZATION ARCHITECTURE AND CONTRACT**. This
record does not start D1.
