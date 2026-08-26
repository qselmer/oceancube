# oceancube provenance V1 design

## Status and scope

This document is the normative design target for A4b. It specifies provenance
schema `1.0.0`; it does not implement the schema, change the public API, or
replace scientific variable, grid, or QA metadata.

As of A4b3b, the internal core engine in `R/provenance.R` implements this
contract and its legacy normalizer, and every identified runtime producer emits V1:
`ocean_cube()`, materialized and deferred NetCDF ingestion, `cube_slice()`,
`cube_crop()`, NetCDF-to-memory `cube_collect()`, `cube_mask()`, temporal
aggregation, climatology, anomaly, signal/noise, trend, extraction, transect,
polygon weights, depth-layer mean, coast distance, and stock crop. Delegating
wrappers reuse canonical records. A4-EXIT still must perform the global audit
before A4 or A1-003 can close; this implementation status is not an early exit
certification.

The selected architecture is **C — hybrid**: a flat primary history plus a flat
registry of secondary lineages used by multi-input operations. It preserves the
readability of an append-only history without forcing a full entity/activity
graph into the runtime. The model is conceptually compatible with W3C PROV, but
does not require RDF, PROV-O, JSON-LD, UUIDs, databases, or content hashes.

## Architectural boundaries

| Concern | Governing question | Examples | Location |
|---|---|---|---|
| Provenance | How did this object come to exist? | source identity, operation, parameters, method, software, lineage inputs | `x$provenance` or `oceancube_provenance` attribute |
| QA | How did execution/read quality behave? | valid-count arrays, coverage arrays, read amplification, physical reads, residual diagnostics, elapsed time, memory | `x$qa` or a QA attribute |
| CF/scientific metadata | What do the values and grid mean? | units, standard names, bounds, cell methods, grid mapping, calendars | cube variables, coordinates, and future CF metadata |

QA and CF metadata may be summarized by provenance when needed to identify an
input or output, but their complete objects must not be copied into history.

## Canonical top-level shape

```r
provenance <- list(
  schema_version = "1.0.0",
  source = list(
    identity = list(
      label = NULL,
      dataset_id = NULL,
      provider = NULL,
      product = NULL,
      version = NULL,
      doi = NULL,
      fixture_id = NULL,
      checksum = NULL
    ),
    locator = NULL,
    metadata = NULL
  ),
  time = list(
    source = NULL,
    current = NULL
  ),
  history = list(),
  lineages = list(),
  extensions = list()
)
```

The top-level `source`, `time`, and `history` fields are required lists. Their
contents may be empty when the information is genuinely unavailable. Missing
facts remain `NULL`; implementations must not invent provider, DOI, calendar,
or checksum values.

`schema_version` and operation `software$version` are independent. The target
package version `0.3.0` may use provenance schema `1.x`.

## Source contract

`source$identity` contains portable identifiers when known. It is provider
neutral and supports OISST, ETOPO, WOA23, memory cubes, and later sources without
NOAA-specific fields. A governed fixture may supply `provider`, `product`,
`version`, `doi`, `fixture_id`, and an existing checksum. None is universally
mandatory.

`source$locator` is optional and non-semantic:

```r
list(
  type = "file",            # file, url, or other registered type
  value = "oisst.nc",       # sanitized locator
  basename = "oisst.nc",
  portable = FALSE
)
```

An absolute local path is never scientific identity. A stable public product
URL may be retained as a locator, but a signed URL, access token, credential,
secret, user-information component, or authentication query string must never
be stored. Local locators may be omitted or reduced to a basename for portable
serialization.

`source$metadata` is optional, small identity-adjacent metadata. Unknown user or
provider-specific material is preserved under `extensions`, not promoted to
canonical semantics. CF field metadata does not belong here.

Checksums are optional. A checksum already supplied by a governed fixture is
valuable identity evidence, but V1 never hashes large local or remote sources
automatically.

## Time contract

`time$source` preserves how the original temporal coordinate was represented
and decoded. It may contain the established fields `source_class`,
`source_timezone`, `source_offset`, `calendar`, `calendar_defaulted`, `cf_units`,
`cf_origin`, `decoder`, `decode_status`, and `normalization`.

`time$current` describes the output axis and contains compact values such as
`kind`, `class`, `timezone`, `calendar`, `count`, `start`, and `end`. `kind`
distinguishes at least:

- `historical` for observed or model timestamps;
- `recurring_climatology` for climatological pseudo-time;
- `trend_anchor` for the single trend output anchor;
- `static` for a future field with no time axis.

An operation that changes time records compact input/output temporal summaries
in its history record and updates `time$current`. `time$source` remains the
original decoding lineage. A climatology changes `current` to recurring time;
an anomaly restores the primary historical time; a trend changes it to an
anchor. A4b must not solve unsupported calendars or `months since` decoding.

## Flat primary history

`history` is ordered oldest to newest and append-only. Every record has this
canonical shape:

```r
list(
  id = "op_001",
  operation = "read_nc",
  parameters = list(
    requested = list(...),
    resolved = list(...)
  ),
  inputs = list(
    list(
      role = "source",
      lineage_ref = "primary",
      entity_ref = "source",
      summary = list(
        backend = "netcdf",
        shape = c(longitude = 36L, latitude = 48L, depth = 1L,
                  time = 4L, variable = 4L),
        variables = c("sst", "anom", "err", "ice"),
        time_kind = "historical"
      )
    )
  ),
  output = list(
    entity_ref = "op_001:output",
    backend = "memory",
    shape = c(...),
    variables = c(...),
    time_kind = "historical"
  ),
  scientific_method = NULL,
  software = list(package = "oceancube", version = "0.2.0.9000"),
  execution = NULL
)
```

Operation names are stable public/internal contract names: `ocean_cube`,
`read_nc`, `cube_slice`, `cube_crop`, `cube_extract`, `cube_collect`,
`cube_aggregate_time`, `cube_climatology`, `cube_anomaly`, `signal_noise`,
`cube_trend`, and the other audited producers. Method variants belong in
`scientific_method`, not in alternative operation names.

Parameters contain scientifically meaningful requested arguments and compact
resolved choices. They must not contain full cubes, coordinate arrays, full
index vectors, memory addresses, environments, or temporary paths. Ranges,
selected variable names, selection counts, match/tolerance policy, and output
shape are sufficient for crop/slice/extract records.

Input/output summaries may record backend, shape, variables, time kind/class,
and compact coordinate ranges. Backend and local locator fields are
non-semantic representation metadata; shape, variables, and time semantics are
semantic. Full coordinates and values are prohibited.

## Operation identifiers and multiple lineages

Operation IDs are deterministic, local to one lineage, and sequential:
`op_001`, `op_002`, and so on. The source entity is `source`; operation outputs
are referenced as `op_NNN:output`. No random UUID is used.

The root history is the `primary` lineage. `lineages` is an optional flat named
registry for additional canonical lineages. Registry keys are deterministic in
first-use order: `lineage_001`, `lineage_002`, and so on. Each entry contains
`source`, `time`, and a flat `history`; it must not contain a nested `lineages`
registry. During a merge, existing registries are flattened and references are
rewritten deterministically.

`history[[i]]$inputs` is a generic ordered list. Each input records `role`,
`lineage_ref`, `entity_ref`, and a compact summary. A tuple of `lineage_ref` and
`entity_ref` is the lineage reference; a content hash is not required.

For `cube_anomaly(source, climatology)`:

1. the source cube remains the primary flat history;
2. the climatology canonical lineage is stored once as `lineage_001`;
3. `cube_anomaly` appends one primary history record;
4. its inputs reference the primary current entity with role `source` and the
   secondary current entity with role `climatology`;
5. the source and climatology trees are not embedded in `parent`.

This is a compact PROV-inspired derivation graph without the implementation
burden of a full entity/activity graph.

## Scientific method and software

`scientific_method` is optional and used when an operation has a meaningful
computational definition. Package-owned identifiers use an `oceancube:` prefix,
for example:

- `oceancube:equal_observation_weighted_mean`, version `1`;
- `oceancube:two_stage_equal_year_weighting`, version `1`;
- `oceancube:difference`, version `1`;
- `oceancube:standardized_z`, version `1`;
- `oceancube:ols_elapsed_time_linear`, version `1`.

These identifiers are package definitions, not external standards or DOIs.

Every operation record requires `software$package` and `software$version`.
A4b should resolve the version through one tested internal helper backed by the
loaded package metadata/DESCRIPTION and verify identical behavior under an
installed package, `R CMD check`, and `devtools::load_all()`. It must not use a
wall-clock or installed older package as a fallback.

## Timestamp and execution policy

Wall-clock timestamps are optional and never required for V1 validity. If
retained, they live under `operation$execution$recorded_at`. Platform, elapsed
time, memory, and read counters are also non-semantic execution/QA information.
They are excluded from scientific provenance equivalence.

The current `_utc` fields and `.make_provenance()` date/system snapshot have
debugging value but impose nondeterminism, equality cost, and privacy risk.
A4b migrates them losslessly to `extensions$legacy` when present; it does not
copy them into canonical semantic records.

## Constructor, ingestion, collect, and table outputs

- `ocean_cube()` with no provenance initializes `source`, `time`, and an empty
  history. Construction is an entity initialization, not automatically an
  operation. Internal reconstruction normalizes/preserves provenance but never
  appends a spurious `ocean_cube` operation.
- `read_nc()` is an ingestion activity and appends exactly one `read_nc`
  operation after initializing source/time.
- `cube_collect()` appends exactly one operation when NetCDF representation is
  materialized to memory. Calling it on an already-memory cube remains a no-op
  and returns the object without a new record.
- `cube_extract()` appends its operation and attaches the same complete V1
  schema as `attr(result, "oceancube_provenance")`. It does not use a separate
  table schema. Other table outputs should converge on that attribute name.
- `cube_extract()`, `cube_transect()`, and `cube_polygon_weights()` use the
  internal `oceancube_qa` attribute for bounded-read, matching, and geometry
  diagnostics that do not belong in semantic V1. Existing scientific table
  columns and dedicated selection/path/coverage attributes remain unchanged.
- `cube_polygon_weights()` uses `oceancube_provenance` canonically and retains
  `provenance` temporarily as an exact V1 alias because published examples and
  regression tests relied on that attribute name.

## Legacy and user metadata migration

Migration is hybrid and deterministic:

1. recursively walk recognized legacy `parent` chains oldest to newest;
2. normalize each recognized operation exactly once;
3. treat anomaly `parent$source` as primary and `parent$climatology` as a
   secondary lineage;
4. move backend/read counters to QA mappings, not canonical history;
5. move `_utc`, local paths, system snapshots, and unrecognized legacy residue
   to `extensions$legacy`;
6. preserve opaque user provenance losslessly under `extensions$user` without
   assigning it V1 semantic meaning.

Existing 0.2 cubes migrate lazily on the first provenance-aware operation. The
input object remains byte-for-byte unchanged; only the derived output receives
canonical V1 provenance. Newly constructed 0.3 cubes receive V1 immediately.
An explicit migration function may be considered later but is not part of A4b
or the public API by default.

Lossless opaque preservation applies only to serialization-safe, credential-free
values. If migration detects a token, password, signed/authenticated locator, or
other secret-like value, it must abort with a sanitization condition rather than
silently discard the value or place a credential in V1.

Recognizable legacy provenance is accepted. Opaque user provenance is wrapped
losslessly. A malformed object claiming V1 is not silently repaired: validation
reports an error and provenance-aware mutation aborts. An unsupported declared
future schema, such as `2.0.0`, is preserved opaquely on pass-through but is not
reinterpreted; an operation that must understand it aborts with an informative
unsupported-schema condition.

## Semantic equivalence

The conceptual internal projection `provenance_semantic(x)` retains:

- schema major/minor fields relevant to interpretation;
- portable source identity and supplied checksums;
- source/current time semantics;
- ordered operation IDs/names;
- requested/resolved scientific parameters;
- lineage/entity references and semantic input/output summaries;
- scientific method IDs/versions;
- software package/version;
- semantic secondary lineages.

It excludes:

- `source$locator` and absolute/local paths;
- wall-clock timestamps and all `execution` fields;
- backend names in input/output summaries;
- read counts, amplification, blocks, elapsed time, memory, and diagnostic
  arrays;
- opaque `extensions$user` and `extensions$legacy`.

Two provenance objects are semantically equivalent when their normalized V1
semantic projections are deeply equal after canonical name/order normalization.
Execution metadata may differ without changing equivalence.

## Serialization and growth

Canonical fields are limited to `NULL`, logical, integer, finite double,
character, `Date`, UTC `POSIXct`, plain lists, and small data frames only when a
list representation would lose a real tabular contract. Environments,
connections, external pointers, functions, R6 objects, and unevaluated language
objects are forbidden.

For one lineage, size is O(number of operations). For merges, size is O(total
unique operation records across the primary and flat secondary lineages).
Operation records are bounded summaries; full coordinates, values, indices,
parent objects, and repeated lineage trees are forbidden.

## Output-type scope

Core V1 covers `ocean_cube`, `stock_cube`, extraction/transect/polygon-weight
base data frames carrying the `oceancube_provenance` attribute, anomaly outputs,
and existing `ocean_clim` compatibility output. A future written NetCDF file and
a verified spatind exchange object remain future adapters; the portable polygon
weight attribute is only the current package-boundary contract and does not
claim a spatind implementation or new public API.

## Version evolution

- Backward-compatible optional fields or newly registered optional operation
  metadata retain major version `1`.
- A required-field change, semantic reinterpretation, reference-model change,
  or incompatible validation rule requires schema `2.0.0`.
- The package reading an older supported V1 performs deterministic
  normalization to its latest supported 1.x representation.
- The package never downcasts an unsupported future major version.

## Internal A4b helpers

Proposed internal helpers, with no export by default:

- `.provenance_normalize()` — accept `NULL`, V1, recognizable legacy, or opaque
  user metadata and return canonical V1;
- `.provenance_validate()` — validate schema, types, references, records, safe
  values, and supported version;
- `.provenance_append()` — normalize without mutating the input and append
  exactly one bounded operation;
- `.provenance_merge_lineages()` — flatten/rewrite deterministic secondary
  lineage references;
- `.provenance_semantic()` — produce the equality projection;
- `.provenance_software_version()` — resolve package version consistently.

No public `cube_provenance()` accessor is approved for 0.3.0-A4. Direct
`x$provenance` and the table attribute remain sufficient until usage evidence
justifies an export.

## Open decisions deliberately deferred

- whether user evidence later justifies a public `cube_provenance()` accessor;
- whether an explicit public migration function is ever necessary;
- the exact NetCDF/CF-history and RO-Crate export adapters;
- verified spatind API names and ownership after its repository is available;
- whether later schemas adopt content-derived global entity identifiers;
- the separately governed lazy-I/O, CF/calendar, vertical, and 3-D contracts.

## Cross-package and future export compatibility

Spatind may consume the complete V1 attribute, retain the primary and secondary
lineages, and append records whose `software$package` is `spatind`. It need not
parse arbitrary nested `parent` trees. This defines a shared extension shape,
not an existing spatind API.

Future adapters may map V1 to:

- a compact human-readable CF `history` entry during `cube_write_netcdf()`;
- NetCDF global provenance attributes;
- an RO-Crate containing datasets, scripts, fixtures, workflows, and citations;
- a W3C PROV representation of entities, activities, derivations, and software
  agents.

CF `history` remains file-level text and is not the internal schema. RO-Crate,
RDF, PROV-O, and JSON-LD remain optional export/package representations, never
runtime dependencies for each transformation.

## A4b verification contract

A4b must cover schema validity, legacy migration, append-once behavior, flat
growth, anomaly multi-input lineage, source/current time preservation,
serialization roundtrip, semantic determinism, exclusion of optional execution
metadata, locator portability, memory/NetCDF parity, no input mutation, and
spatind-ready table provenance.

The governed OISST fixture supplies the real-data sequence
`read_nc → cube_crop → cube_aggregate_time`. ETOPO and WOA retain their current
expected reader limitations and must not trigger CF implementation during A4b.

## Architectural references

- [W3C PROV-DM](https://www.w3.org/TR/prov-dm/) supplies the conceptual Entity,
  Activity, Agent, and Derivation mapping.
- [CF Conventions 1.13](https://cfconventions.org/Data/cf-conventions/cf-conventions-1.13/cf-conventions.html)
  supplies the file-level textual `history` relationship and scientific metadata
  boundary.
- [RO-Crate 1.1](https://www.researchobject.org/ro-crate/specification/1.1/)
  informs a future research-object export only.
