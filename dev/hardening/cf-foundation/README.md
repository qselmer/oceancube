# OCEANCUBE 0.3.0-B1 — CF metadata foundation and engine decision

Status: **B1 architecture/prototype complete; runtime implementation not
started**.

Normative standard: **CF Metadata Conventions 1.13**.

Decision: **DEC-015 APPROVED — HYBRID**.

The selected design combines an oceancube-owned, versioned, plain-R canonical
metadata model with a lightweight native structural/preservation layer and
supported-subset interpreter. `ncdfCF` remains an optional development oracle,
reference implementation, or future adapter. No ncdfCF, RNetCDF, or CFtime
dependency was added to the package.

The full contract is
[`inst/architecture/oceancube-cf-metadata-foundation-v1.md`](../../../inst/architecture/oceancube-cf-metadata-foundation-v1.md).

## Outcome

- `0.3.0-A`: remains **COMPLETE / CERTIFIED**.
- `0.3.0-B`: **IN PROGRESS**.
- B1: **COMPLETE**, subject to package certification and authorized CI.
- Gate B: **UNSATISFIED**; vertical science is not authorized.
- `DEC-015`: **APPROVED — HYBRID**.
- `DEC-023`: **OPEN**.
- `A1-002`: **PARTIALLY-CLOSED / transferred to B implementation**.
- `A3B-001`, `A3B-002`, `A3B-003`: remain **OPEN**, with explicit staged
  assignments.
- Public claim: **CF-aware; supports a documented subset of CF 1.13**.
- Runtime, API, scientific results, package dependencies, `R/`, tests,
  `NAMESPACE`, and `DESCRIPTION`: unchanged by B1.

## Current-reader audit

The eager reader discovers source structure only to construct a materialized
canonical cube. It retains selected variable units and decoded canonical axes,
but does not retain global attributes, ordered attribute sets, source types,
physical variable layouts, or linked CF constructs. Packing is applied by
ncdf4 and its source declaration is lost.

The deferred backend retains substantially more for selected variables:
source type, physical dimensions/order, canonical permutations, fill/missing
and packing fields, units, long name, standard name, and selected dimension
axis evidence. It still loses all globals, unknown attributes, unselected
variables, bounds, climatology, coordinate links, cell methods/measures,
ancillaries/flags, grid mappings, and formula terms. Its metadata construction
also currently requires successful canonical time decoding.

The exact gap is in `b1-current-metadata-parity.csv`. Neither reader currently
provides a lossless CF metadata model.

## Preserve, interpret, validate, use

B1 keeps four layers distinct:

```text
PRESERVE -> INTERPRET -> VALIDATE -> USE / TRANSFORM
```

Unknown metadata survives preservation. A later interpreter may mark it
uninterpreted or deferred. Validation covers only a declared subset, and a
current cube may still reject a source that the metadata scanner can inspect.
This is the basis for scanning static ETOPO and WOA's rejected time metadata
without inventing values.

The proposed canonical location is `x$metadata$cf`, under an independently
versioned `x$metadata`. The source tree is recorded once; current semantics use
references plus compact changes/invalidations. Provenance records operations
and QA records diagnostics. Neither substitutes for CF metadata.

## Attribute and link evidence

The native prototype records each attribute's name, observed source order, raw
value, length, R representation, and exact source primitive type when the
scanner can retrieve it. Public ncdf4 1.24 does not expose every attribute's
primitive type; it is recorded as unavailable rather than guessed. Attribute
order returned by ncdf4 is preserved as observed. Variable order remains
mandatory.

The prototype keeps raw and interpreted forms for:

- `coordinates`;
- `bounds`;
- `climatology`;
- `ancillary_variables`;
- `cell_measures`;
- `grid_mapping`;
- `formula_terms`.

It emits deterministic resolved, missing, self-reference, duplicate,
ambiguous, unknown-form, and deferred-extended statuses. Formula-term
self-reference is assessed by relation, because it can be legitimate for a
parametric coordinate term. Extended CF 1.13 `grid_mapping` syntax is kept raw
and deferred in the initial B2 scope rather than truncated.

## Real fixtures

### OISST

The fixture declares `CF-1.6, ACDD-1.3`. Its coordinate variables have no
`axis` or `standard_name` attributes. `lon`/`lat` are identified by degree
units, time by `days since`, and `zlev` only by metres, its descriptive long
name, and `positive=down`.

Without an explicit `depth_name`, `read_nc()` fails because its literal depth
fallback excludes `zlev`; `cube_open()` succeeds because the deferred resolver
uses `positive=down`. The future fix is a shared conflict-aware semantic
resolver, not a provider alias alone. `A3B-001` remains open until runtime work.

### ETOPO

The fixture does not declare CF, but has X/Y longitude/latitude coordinates and
`z:coordinates = "lat lon"`. Elevation is metres relative to the geoid with
negative ocean values. The native prototype and ncdfCF inspect it without
time. Current `ocean_cube` construction still rejects it because the current
model requires time. `A3B-002` remains open; no time is fabricated.

### WOA23

The fixture declares `CF-1.6`, has X/Y/Z/T axes, depth bounds, a
latitude-longitude grid mapping, standard names/units for temperature and
practical salinity, and exact multi-clause `cell_methods`.

Time has raw value `4614`, units `months since 1955-01-01 00:00:00`, no
calendar attribute, and a distinct `climatology` link whose raw bounds are
`4212, 5028`. The CF default calendar is standard, while use of `month`/`year`
time units is strongly discouraged. UDUNITS duration month, calendar month,
and climatological monthly bins are not interchangeable. The scanner preserves
all of them before decoding; current readers remain unchanged and reject the
units. `A3B-003` and `DEC-023` stay open.

## Native prototype

`b1-prototype.R` is non-package code. It:

- scans globals, dimensions, variables, attributes, types available through
  public ncdf4, order, and small coordinate evidence;
- constructs plain-R schema `oceancube_cf_metadata` version `1.0.0`;
- resolves the seven required link families;
- uses path-capable variable identity and many-to-many roles;
- creates a deterministic temporary CF-rich NetCDF file;
- exercises link diagnostics without committing binary data;
- scans OISST, ETOPO, and WOA independently of cube representability;
- checks exact serialize/unserialize identity.

Executed result:

```text
B1_NATIVE_PROTOTYPE: PASS
schema: oceancube_cf_metadata 1.0.0
synthetic links: 10
serialization: PASS
OISST native variables=8 links=0
ETOPO native variables=3 links=2
WOA23 native variables=11 links=14
```

Run with the package's existing dependency only:

```powershell
Rscript --vanilla dev/hardening/cf-foundation/b1-prototype.R
```

If an isolated `ncdfCF` library is on `.libPaths()`, the same run also records
oracle summaries. Package tests do not require it.

## ncdfCF and CFtime comparison

An isolated, uncommitted CRAN library was used:

| Package | Version | Role |
|---|---:|---|
| ncdfCF | 0.8.2 | optional oracle/reference |
| RNetCDF | 2.11.2 | ncdfCF low-level dependency |
| CFtime | 1.7.3 | ncdfCF calendar dependency and DEC-023 candidate |

ncdfCF opened all real fixtures. It handled static ETOPO, recognized WOA
bounds/climatology and grid mapping, and traversed a two-group file with
duplicate basenames. It inferred OISST X/Y/T but left `zlev` generic despite
`positive=down`. It rendered WOA's duration-month values in 2339. The combined
CF-rich synthetic file failed in both `open_ncdf()` and `peek_ncdf()` with
`attempt to apply non-function`; the failure is recorded without attributing it
to an individual construct that was not isolated.

CFtime 1.7.3 constructed the standard, proleptic Gregorian, Julian, noleap,
all-leap, 360-day, none, UTC, and TAI candidates and offers ordering,
subsetting, bounds, formatting, and grouping facilities. This is useful but
does not settle ordinary `Date`/`POSIXct` compatibility, perpetual calendar
semantics, leap seconds, grouping, serialization policy, or every current
temporal operation. Exact `identical()` object state did not survive the B1
serialization probe even though displayed values did. `DEC-023` remains open
and CFtime was not added to package metadata.

## Engine scorecard

All criteria use 1 (poor) through 5 (best). For cost and maintenance burden, 5
means low. CF coverage and loss risk have weight 3. Control, API stability,
backend independence, Zarr compatibility, serialization, and error
transparency have weight 2 or 3; remaining criteria have weight 1.

| Candidate | Score | Result |
|---|---:|---|
| Native only | 119 | rejected as sole authority |
| ncdfCF core | 104 | rejected as canonical runtime authority |
| Hybrid | 145 | **selected** |
| Embedded ncdfCF objects | 83 | rejected |

Hybrid combines lossless ownership/control with independent comparison and
does not make a live NetCDF/R6 object canonical state. A future adapter may be
optional; B1 introduces no mandatory dependency.

## Axis and group strategy

The future resolver collects explicit override, structural relationship,
`axis`, `standard_name`, `units`, `positive`/vertical formula, and known-name
evidence before deciding. It reports `RESOLVED`, `AMBIGUOUS`, `CONFLICT`, or
`UNRESOLVED`. Explicit override is highest, but strong contradictory metadata
is a hard conflict rather than silently overwritten. Weak/unlabeled overrides
are accepted with a diagnostic.

The group prototype showed ncdf4 path identities `g1/x`, `g2/x`, `g1/temp`,
and `g2/temp`; ncdfCF showed the same hierarchy. Current readers are not group
certified. The canonical model uses path-qualified identity so later scoping
cannot be blocked by a flat-name decision.

## Propagation and future storage

`b1-transformation-metadata-matrix.csv` distinguishes immutable source
metadata from current field semantics. `cube_collect()` must not change
meaning. Selection updates extents; aggregation, climatology, anomaly, trend,
layer operations, geometry/table outputs, and masking must explicitly derive,
preserve, or invalidate metadata.

CF 1.13 anomaly support later requires `anomaly_wrt`, a norm ancillary
variable, anomaly coordinates, bounds/climatology, and matching cell methods.
Current `cube_anomaly()` provenance lineage does not by itself create CF
anomaly metadata.

The model contains no NetCDF handle or local-path authority, so it can later
support multifile conflict reconciliation, remote resources, and Zarr. Those
features remain out of B1/B2 scope. Provider access/authentication remains a
separate layer.

## Evidence index

- `b1-current-metadata-parity.csv`: exact eager/deferred retention gap.
- `b1-cf-attribute-inventory.csv`: relevant CF 1.13 preservation priorities.
- `b1-cf-engine-options.csv`: weighted engine decision.
- `b1-calendar-requirements-matrix.csv`: evidence required before DEC-023 can close.
- `b1-cf-coverage-matrix.csv`: truth table for current and future claims.
- `b1-real-fixture-cf-matrix.csv`: OISST/ETOPO/WOA structural evidence.
- `b1-ncdfcf-comparison.csv`: actual oracle comparison and versions.
- `b1-transformation-metadata-matrix.csv`: future propagation policy.
- `b1-prototype.R`: deterministic native scanner/link prototype.

## Local test certification

With `English_United States.utf8`, the unchanged complete suite reported 62
files, 612 `test_that()` cases, 4,979 expectations, 0 failures, 0 errors, 0
test warnings, and 0 skips. Elapsed time was 2,667.750 seconds. The longer
elapsed time is informational; the exact A-EXIT case and expectation counts are
unchanged.

## Next subphase

The only recommended next subphase is:

**0.3.0-B2 — CF METADATA PRESERVATION ENGINE IMPLEMENTATION**.

B2 is not executed here. Gate B remains unsatisfied.
