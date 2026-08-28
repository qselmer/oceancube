# oceancube CF metadata foundation V1

Status: **B1 architecture approved; B2 preservation foundation and B3
supported-subset interpretation/validation implemented**.

Decision: **DEC-015 = APPROVED — HYBRID ACTIVE SUPPORTED-SUBSET ENGINE**.

Normative reference: [CF Metadata Conventions
1.13](https://cfconventions.org/Data/cf-conventions/cf-conventions-1.13/cf-conventions.html)
and its [conformance requirements and
recommendations](https://cfconventions.org/Data/cf-documents/requirements-recommendations/conformance-1.13.html).

This document defines the internal metadata contract implemented through B3.
It does not authorize vertical science,
static cubes, new calendars, grid transformations, writing, remote I/O,
multifile I/O, or Zarr.

## Scope and claim

oceancube will describe itself as:

> CF-aware; supports a documented subset of CF 1.13.

It will not claim “CF-compliant” or “full CF support” until a separate,
versioned conformance coverage gate exists. A source declaration such as
`CF-1.6` is evidence supplied by its writer, not proof that oceancube has
validated the file.

The foundation is governed by four separate stages:

1. **PRESERVE** — retain structure and attributes without silent loss.
2. **INTERPRET** — resolve supported CF meanings and relationships.
3. **VALIDATE** — diagnose coherence of the explicitly supported subset.
4. **USE / TRANSFORM** — apply those meanings to cube operations.

Failure or deferral at a later stage does not erase the preserved source
record. Unsupported constructs are normally `PRESERVED_UNINTERPRETED`, not a
hard error. A hard error is appropriate when a requested current
`ocean_cube` cannot be represented safely.

## Metadata, provenance, and QA

The three planes remain independent:

- **CF metadata** says what the source or current field means.
- **Provenance V1** says how the current product was produced.
- **QA** records diagnostics about quality or execution.

Provider `history`, `source`, and `institution` are source metadata. They are
not copied into `provenance$history`. CF flag variables and
`ancillary_variables` are provider scientific/quality resources; they are not
`x$qa`. Performance telemetry and file execution diagnostics are not CF
metadata.

## Canonical location and version

The canonical location is top-level `x$metadata`, with the CF model at
`x$metadata$cf`. Metadata is semantic state and therefore cannot be owned only
by `x$storage`. It must be identical in meaning before and after
`cube_collect()` and remain usable by memory, NetCDF, future remote, and future
Zarr representations.

The implemented shape is:

```r
x$metadata <- list(
  schema_name = "oceancube_metadata",
  schema_version = "1.0.0",
  cf = list(
    schema_name = "oceancube_cf_metadata",
    schema_version = "1.0.0",
    source = list(
      declaration = list(...),
      global_attributes = list(...),
      dimensions = list(order = ..., map = ...),
      variables = list(order = ..., map = ...),
      links = list(...)
    ),
    current = list(
      axes = list(...),
      variables = list(...),
      links = list(...),
      semantic_status = ...
    ),
    interpretation = list(...),
    diagnostics = list(...)
  )
)
```

`oceancube_cf_metadata` is versioned independently of the package,
Provenance V1, and the NetCDF storage descriptor. The B2 implementation
preserves these boundaries and version independence.

The complete raw source structure occurs once. `current` uses source object IDs
plus compact overrides, invalidations, derived attributes, and selection facts;
it does not duplicate the source tree and does not maintain a recursive
transformation history. Provenance already owns transformation history.

A manually constructed or legacy cube may have `metadata = NULL`. The public
`ocean_cube()` signature remains unchanged; internal attachment validates and
adds canonical metadata only for source-backed ingestion.

## Plain-state and data boundary

Canonical metadata contains only serializable plain-R values. It contains no
connection, external pointer, `ncdf4` handle, R6 object, external package
object graph, or credential.

It records descriptors, not full data-bearing arrays. Bounds, climatology
bounds, cell measures, ancillary fields, and formula variables stay data
resources. Metadata records their path, primitive type, dimensions,
attributes, roles, and relationships. A very small scalar or bounded coordinate
summary may be retained when it is explicitly part of the descriptor; complete
SST, bounds, area, or ancillary arrays are never copied into metadata.

## Attribute record

Every attribute, including unknown attributes, has an ordered record:

```text
name
observed source order
source primitive type, when retrievable
source-type availability/status
R type and class
vector length
raw value
scope and owner path
```

Raw text is not destructively trimmed, recased, or rewritten. Interpretation
may add a normalized copy. `units`, `standard_name`, `calendar`, and provider
`history` retain their original representation.

With `ncdf4` 1.24, `ncatt_get()` returns attributes by netCDF attribute index,
so the B1 prototype preserves the observed order. Public `ncdf4` does not
expose the exact primitive type of every attribute; the record therefore uses
an explicit unavailable status rather than inferring `NC_FLOAT` versus
`NC_DOUBLE` from an R double. B2 must preserve a primitive type when its chosen
scanner can retrieve it and must never fabricate one.

Variable order remains mandatory. Attribute order is preserved as observed,
but has no semantic priority.

## Global metadata

Root globals include the exact `Conventions`, `title`, `institution`, `source`,
`history`, `references`, `comment`, `featureType`, and unknown attributes.
Group attributes use the same record and a path-qualified group owner.

Large or sensitive source attributes remain preserved but are not printed by
default. A future `cube_inspect()` view may show only declaration, axis
resolution, variable names, standard names, units, bounds/CRS availability,
and diagnostic counts. Full values require an explicit detailed inspection.

## Dimensions and variables

Every dimension descriptor preserves:

- path-qualified source name and observed order;
- length and unlimited status;
- coordinate-variable association, if any;
- group/path identity.

A dimension need not have a coordinate variable.

Every variable descriptor preserves:

- path-qualified source name, type, and source order;
- source dimensions and their order;
- ordered raw attributes;
- zero or more interpreted roles;
- whether it is data-bearing or is only a metadata container.

Path-qualified identity is canonical. A basename is a display/fallback name,
not a globally unique identity.

## Role vocabulary

The initial many-to-many role vocabulary is:

```text
data
dimension_coordinate
auxiliary_coordinate
bounds
climatology_bounds
grid_mapping
cell_measure
ancillary
formula_term
quality_flag
geometry
unknown
```

A variable may have multiple roles. In particular, formula terms can also be
coordinates, and an ancillary variable can itself have bounds or other
relationships. Roles do not replace the source descriptor.

## Link model

Linked attributes retain both the exact provider string and parsed records:

```text
source path
attribute name
optional key/term/measure
raw target token
resolved path, if unique
candidate paths
status
raw complete attribute value
parser status
```

Initial statuses are `RESOLVED`, `MISSING_TARGET`, `SELF_REFERENCE`,
`DUPLICATE_REFERENCE`, `AMBIGUOUS`, `UNKNOWN_FORM`, and
`DEFERRED_EXTENDED`. Self-reference is relation-specific: it is invalid for a
coordinate's own `bounds`, but can be valid for the coordinate term in some CF
`formula_terms` definitions.

The production foundation supports simple links for `coordinates`, `bounds`, `climatology`,
`ancillary_variables`, `cell_measures`, `grid_mapping`, and `formula_terms`.
CF 1.13 extended `grid_mapping` syntax is preserved exactly but initially
marked `DEFERRED_EXTENDED`; B2 does not silently reduce it to the first token.

`climatology` remains distinct from `bounds`. No preservation layer infers
bounds or assumes a coordinate is the midpoint of its cell.

## Axis evidence and conflict policy

The future common resolver collects all evidence before ranking it:

1. explicit user override;
2. coordinate-variable/dimension relationship;
3. `axis`;
4. `standard_name`;
5. `units`;
6. `positive`, vertical standard name, and `formula_terms`;
7. known-name fallback.

An explicit override is the highest selection authority, but it does not erase
contradictory metadata. Each candidate receives all evidence and one of:

```text
RESOLVED
AMBIGUOUS
CONFLICT
UNRESOLVED
```

Strong evidence that assigns one coordinate to incompatible axes is
`CONFLICT`, regardless of evaluation order. Multiple unique candidates are
`AMBIGUOUS`. No evidence is `UNRESOLVED`.

An override of an unlabeled or weak known-name candidate is accepted with a
recorded `OVERRIDDEN` diagnostic. If the selected coordinate's own strong CF
signals contradict the requested axis, creation of a canonical cube errors by
default; the override cannot make incoherent latitude into longitude. Conflicts
on unused objects are preserved as diagnostics unless they prevent requested
schema resolution. Reusing one physical dimension for incompatible canonical
axes is always an error.

## Real-fixture findings

### OISST and A3B-001

The governed fixture declares `CF-1.6, ACDD-1.3`. Its coordinate variables do
not contain `axis` or `standard_name` attributes. Longitude and latitude are
identified by `degrees_east` and `degrees_north`; time by
`days since 1978-01-01 12:00:00`. `zlev` has value zero, units `meters`, long
name `Sea surface height`, and `positive=down`, but no `axis` and no
`standard_name`.

Before B2, `read_nc()` used explicit overrides and literal name fallbacks, so
`zlev` failed while the deferred resolver used `positive=down`. B2 replaces
both paths with one conflict-aware semantic resolver. Both readers now resolve
`zlev` from `positive=down` without an override, explicit mapping remains
supported, conflict tests pass, and numerical parity is exact. `A3B-001` is
**CLOSED**.

### ETOPO and A3B-002

The fixture has no `Conventions` attribute. It has CF-like longitude/latitude
coordinates with X/Y, standard names, and degree units. Variable `z` is
elevation relative to the geoid in metres and explicitly links `coordinates =
"lat lon"`; ocean values are negative. There is no time axis and no vertical
coordinate or `positive` attribute.

The B1 scanner and ncdfCF both understand its metadata without a time axis.
The current five-dimensional temporal `ocean_cube` cannot represent it. No
time is fabricated. `A3B-002` remains open for a later static-field contract,
not B2 preservation.

### WOA23 and A3B-003

The fixture declares `CF-1.6`. Its raw time value is `4614`, units are `months
since 1955-01-01 00:00:00`, and no calendar attribute is present, so CF's
default is the standard calendar. `time:climatology` points to
`climatology_bounds`, whose raw values are `4212, 5028`. This is distinct from
ordinary coordinate bounds.

CF/UDUNITS `month` denotes a duration unit; it must not automatically be
treated as a provider-intended calendar month or a climatological bin. CF 1.13
strongly recommends not using `month` or `year` as time units. The declaration
is preservable, while its intended scientific meaning requires adjudication.
The current decoder correctly refuses to reinterpret it.

Temperature and salinity have standard names and units, link the WOA CRS, and
carry the exact `cell_methods` string `area: mean depth: mean time: mean within
years time: mean over years`. Longitude, latitude, and depth have bounds;
depth is 0–200 m and positive down. `A3B-003` remains open. B2 preserves these
facts but does not decode them.

## CFtime and DEC-023

The isolated B1 review used CFtime 1.7.3. It constructed `standard`,
`gregorian`, `proleptic_gregorian`, `julian`, `365_day`/`noleap`,
`366_day`/`all_leap`, `360_day`, `none`, `utc`, and `tai` instances. It offers
ordering/indexing, timestamp formatting, bounds, subsetting slabs, and calendar
grouping helpers.

These capabilities are promising but not a complete oceancube contract:

- `none` has distinct perpetual-experiment semantics;
- UTC/TAI introduce leap-second and validity requirements new in CF 1.13;
- current `Date`/`POSIXct` selectors, grouping, printing, and downstream
  operations require compatibility design;
- R6/external package objects are not canonical plain metadata state;
- exact object identity did not survive the B1 serialize/unserialize probe,
  even though displayed timestamps did.

`DEC-023` therefore remains OPEN. B1 adds the concrete
`b1-calendar-requirements-matrix.csv` requirements matrix but
does not add CFtime to `DESCRIPTION` or reinterpret WOA.

## Engine decision: DEC-015

Four candidates were scored from 1 (poor) to 5 (best). Coverage and
metadata-loss risk have weight 3; control, API stability, backend independence,
future Zarr compatibility, serialization, and error transparency have weight
2 or 3; the remaining criteria have weight 1. For cost and burden, 5 means low
cost/burden. The detailed evidence is in `b1-cf-engine-options.csv`.

The selected architecture is **HYBRID**:

```text
NetCDF or future storage
  -> lightweight structural/raw scanner
  -> oceancube-owned plain canonical metadata model
  -> oceancube supported-subset interpreter and diagnostics
  -> current cube-schema resolver
  -> eager or deferred data decoder/backend

ncdfCF
  -> optional development oracle, reference implementation, or future adapter
```

oceancube owns the canonical contract and never embeds ncdfCF objects. The
scanner/interpreter is shared by future `read_nc()` and `cube_open()` while
their data access remains eager versus deferred. ncdfCF is not a runtime
dependency in B1.

The isolated comparison used ncdfCF 0.8.2, RNetCDF 2.11.2, and CFtime 1.7.3
from CRAN. It opened all three real fixtures, handled static ETOPO, recognized
WOA bounds and grid mapping, and supplied valuable group/calendar evidence.
It displayed WOA's duration-month offsets as dates in 2339 and failed to open
the combined B1 CF-rich synthetic fixture with `attempt to apply non-function`.
Both are discrepancies to adjudicate, not authority over the standard.

Pure native interpretation was rejected as the sole authority because of
coverage/maintenance risk. Pure ncdfCF authority was rejected because it would
couple oceancube semantics to a NetCDF-specific mutable object model. Embedding
ncdfCF R6 objects was rejected for serialization, lifecycle, API-stability,
and future-storage reasons.

## Eager/deferred convergence and encoding debt

`read_nc()` and `cube_open()` should eventually share one metadata scan,
interpreter, link resolver, and axis resolver. They continue to differ in
scientific data materialization.

The common model naturally provides one encoding descriptor for `_FillValue`,
`missing_value`, `scale_factor`, and `add_offset`. That assigns the known eager
`missing_value` divergence to later shared decoder work. B2 implements the
shared preservation and axis layers but deliberately does not change decoding.
`A1-002` remains partially closed and moves to later calendar/decoder stages.

## Transformations and current metadata

The source layer is immutable. Operations update only the current semantic
view and record the operation in Provenance V1. Selection preserves applicable
semantics and subsets logical extents; aggregation, climatology, anomaly,
trend, and layer summaries must derive or invalidate attributes and links
rather than copy them blindly. The full policy is in
`b1-transformation-metadata-matrix.csv`.

CF 1.13 anomaly semantics require more than adding `_anomaly` to a standard
name. A future integration must represent `anomaly_wrt`, its norm ancillary
variable, anomaly coordinates, bounds/climatology, and matching cell methods.
Current `cube_anomaly()` secondary climatology lineage remains Provenance V1
evidence until that semantic mapping is implemented.

## Groups, multifile, remote, and Zarr

The B1 group prototype showed that `ncdf4` exposes path-qualified names such as
`g1/temp` and `g2/temp`, while ncdfCF traverses the hierarchy and applies CF
scope. The B2 scanner now preserves those identities and conservatively scopes
simple links without flattening duplicate basenames. Current cube readers are
still not group-certified; full CF group resolution is later.

Future multifile reconciliation compares declarations, variable metadata, and
current semantics by path. Identical metadata can be shared; conflicts become
explicit diagnostics; differing provider histories remain per source rather
than concatenated into runtime provenance. Time concatenation is a separate
schema/coordinate operation.

Authentication, download, catalog, provider planning, and resource acquisition
remain outside CF interpretation. Remote resources feed the same scanner after
access is established. The canonical model has no handle or local-path
assumption, so future Zarr can map its attributes and arrays into the same
semantic model without pretending it is NetCDF.

## Security and inspection

Source metadata may contain local paths, very large strings, or accidental
credentials. Preservation does not imply automatic printing, logging, or
provenance copying. Default inspection redacts or summarizes sensitive-looking
values and large attributes while retaining them in the source record. A
future explicit detailed accessor requires DEC-014 API review; extending
`cube_inspect()` is preferred if sufficient.

## B2 implementation and evolution boundary

Schema evolution is explicit and independently versioned. Unknown fields are
ignored only under a compatible minor-version policy; destructive changes
require migration. Storage adapters may add scanner-specific diagnostics but
cannot change semantic meaning.

B2 implements preservation, simple link resolution, conflict-aware axis
evidence, and eager/deferred scanner convergence. It does not thereby authorize
static cubes, duration-month decoding, non-Gregorian calendars, formula
evaluation, CRS transformation, advanced grids, remote/multifile I/O, Zarr,
writing, or vertical science.

After B2:

- `DEC-015`: **APPROVED — HYBRID IMPLEMENTED FOUNDATION**;
- `DEC-023`: **OPEN**;
- `A1-002`: **PARTIALLY-CLOSED; decoder/calendar/static/other CF work remains**;
- `A3B-001`: **CLOSED**;
- `A3B-002`, `A3B-003`: **OPEN, assigned to staged B work**;
- `0.3.0-A`: **COMPLETE / CERTIFIED**;
- `0.3.0-B`: **IN PROGRESS; B1 and B2 COMPLETE**;
- Gate B: **UNSATISFIED**.

## B3 supported-subset interpretation and validation

B3 implements the interpreter and validator immediately after the immutable
source scan, simple relationship resolution, and role classification. Its
public claim is exactly **CF-aware; supports a documented subset of CF 1.13**.
Neither a source-level `PASS` nor a declared `CF-*` token is a claim of full CF
conformance.

The internal definition and validator are version `1.0.0`, use CF 1.13 and its
Conformance Requirements and Recommendations as normative references, and
record the validation scope as `oceancube_supported_subset`. The compact
source summary records rule, pass, fail, warning, deferred, and not-applicable
counts without timestamps or machine state. A missing `Conventions` attribute
is `NOT_DECLARED`, not invalid; an older declared version remains provider
evidence and is not re-certified as CF 1.13.

Every diagnostic has a stable rule/code, separate status and severity, scope,
source identity, attribute, requirement kind, message, CF section, current-cube
blocking flag, and value-read flag. Status is one of `PASS`, `FAIL`,
`DEFERRED`, or `NOT_APPLICABLE`; severity is one of `ERROR`, `WARNING`,
`INFO`, or `DEFERRED`; rule kind distinguishes `REQUIREMENT`,
`RECOMMENDATION`, and `OCEANCUBE-SAFETY`. Source-wide failures on unrelated
variables do not automatically block ingestion. Existing required-axis and
canonical-shape safety failures remain the only current-cube blocking class in
this phase.

The implemented metadata-only subset covers path/dimension identity,
dimension and auxiliary/scalar coordinate classification, common B2 axis
evidence, simple `coordinates`, `ancillary_variables`, `bounds`,
`climatology`, `cell_measures`, `grid_mapping`, and `formula_terms`
relationships, bounded `cell_methods` classification, structural flag and
missing/range metadata, and explicit preservation states for
`standard_name` and `units`. Bounds/climatology value checks, measure-unit and
general unit conformance, standard-name and mapping-name table lookup, complex
cell-method grammar, formula semantics/evaluation, and extended grid mapping
remain explicit deferrals. No validator path calls `ncvar_get()`.

Source interpretation is immutable and backend-independent. Current cube
interpretation is separate, selection-aware, and remains
`DERIVATION_PENDING` after meaning-changing transformations. Eager and
deferred readers share source interpretation and diagnostics; `cube_collect()`
preserves all metadata exactly and does not revalidate after a representation
change. Diagnostics remain CF metadata, never Provenance V1 operations or
automatic QA entries.

The authoritative coverage and rule evidence is in
`dev/hardening/cf-validation/`. OISST, ETOPO, and WOA all scan with supported-
subset `PASS`; OISST remains ingestible without a `zlev` override, ETOPO still
has no fabricated time, and WOA still rejects unsupported `months since`
decoding while preserving its climatology and complete `cell_methods` text.
Thus `A3B-001` remains closed, `A3B-002`, `A3B-003`, and `DEC-023` remain open,
and Gate B remains unsatisfied.
