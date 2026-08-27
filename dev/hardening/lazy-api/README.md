# OCEANCUBE 0.3.0-A5a — public deferred NetCDF API decision

## Decision

**DEC-018 is APPROVED.** A5b shall add exactly one experimental public entry:

```r
cube_open <- function(
  file,
  vars = NULL,
  lon_name = NULL,
  lat_name = NULL,
  depth_name = NULL,
  time_name = NULL,
  source = "netcdf",
  dataset_id = NULL
)
```

For 0.3.0 this function opens one existing local NetCDF file as a read-only,
deferred `<ocean_cube>`. It reads structural metadata and coordinate values but
does not read scientific variable arrays. It returns the existing public class
`c("ocean_cube", "list")`, with `backend = "netcdf"` represented by its
versioned `x$storage` descriptor and without `x$data` or a persistent NetCDF
handle.

The function is public but explicitly experimental in 0.3.0. Its name and
source-to-cube role are public; the nested storage schema remains internal and
versioned. Users should inspect supported facts through `cube_inspect()` and
materialize explicitly through `cube_collect()`.

`read_nc()` remains unchanged and eager by default. A5a changes no production
runtime, tests, dependency, export, NAMESPACE, or DESCRIPTION. The public API
therefore remains at 38 exports until A5b.

## Terminology

The implemented backend provides:

| Capability | Current state | Meaning |
|---|---:|---|
| deferred I/O | yes | Scientific values stay on disk until requested. |
| bounded I/O | yes | Supported operations can request only required blocks or indices. |
| lazy compute DAG | no | Transformations are not retained as a graph for later compute. |

Documentation must call this the **deferred NetCDF backend**. “Lazy” alone is
not technically accurate and is not used in the selected function name.

## Starting evidence

A5a began from clean `dev-0.3.0` at
`0463a066effad72158c290e1fe62601e3246e3fb`. Local and remote development refs
matched; `main` and `origin/main` were
`40bf4b16755ffb48c08eaf22e0678ac7cf683040`; peeled `v0.2.0` was
`d83008066ba3b1f3ea8df3e7ca3001d472f20308`.

## Current eager `read_nc()` architecture

The public `read_nc()` contract is:

- one existing local file; URL schemes are not a supported contract;
- `vars = NULL` selects all non-coordinate variables known to the eager reader;
- explicit `lon_name`, `lat_name`, `depth_name`, and `time_name` overrides;
- known-name coordinate guessing when overrides are absent;
- immediate full `ncdf4::ncvar_get()` calls for every selected variable;
- canonical `longitude × latitude × depth × time × variable` output;
- a complete `x$data` array and memory backend;
- canonical V1 ingestion operation `read_nc`.

The connection is opened inside the call and closed with `on.exit()`. Values
are read before `read_nc()` returns. Existing calls without new arguments must
continue to return this materialized representation throughout 0.3.0.

## Current internal deferred architecture

The internal backend is implemented, not a future placeholder:

- `.new_netcdf_storage()` builds a serializable version-1 descriptor;
- `.new_netcdf_cube()` builds a normal `ocean_cube/list` without `x$data`;
- `.validate_netcdf_storage()` validates the descriptor and, when requested,
  its physical source identity;
- `.cube_read_netcdf()` translates canonical indices into physical reads;
- `.cube_read_block_netcdf()` performs canonical block reads;
- `.cube_read_spatial_pairs_netcdf()` performs bounded point-pair reads with
  one connection and one physical call per unique spatial pair and variable.

The implemented contract is local-file-only, read-only, 1-D rectilinear,
single-source, and compatible-variable-only. It supports surface cubes through
a singleton `NA_real_` depth, ordinary depth cubes, multiple variables sharing
one logical grid, arbitrary physical dimension permutations, Date and POSIXct
time supported by the shared decoder, `_FillValue`, `missing_value`,
`scale_factor`, and `add_offset`. Missing sentinels are detected before packing
is decoded, and scale/offset are applied exactly once.

Noncontiguous and reordered canonical indices use the minimum enclosing
physical envelope followed by local reindexing. That preserves order and
duplicates but can over-read a sparse envelope. Spatial-pair reads use a
minimum enclosing depth interval. These are bounded-I/O mechanisms, not a lazy
transformation graph.

### Object, lifecycle, validation, and inspection

The deferred object has the same public class as a memory cube. `x$storage` is
present, `x$data` is absent, and no `ncdf4` object, connection, external pointer,
or array payload is stored. Each operation validates source identity, opens a
connection, and closes it through `on.exit()`, including error paths.

`print.ocean_cube()` and `summary.ocean_cube()` use logical header metadata and
do not read scientific values. `cube_inspect(missing = "auto")` and
`cube_inspect(missing = "none")` remain metadata-only for NetCDF cubes;
`missing = "full"` is an explicit warned full read. `cube_validate()` produces
a validation table and does not read a complete payload.

### Serialization and file identity

`saveRDS()`/`readRDS()` round trips preserve the descriptor without an open
connection. A restored cube remains usable if the same source exists unchanged.
The first public identity policy is:

```text
normalized absolute path + file size + mtime
identity_policy = size_mtime_error
```

Deletion, conversion to a directory, size change, mtime change, or detectable
schema change produces a deterministic informative error before or during a
read. Moving the file makes the original normalized path unavailable and also
fails. This is an acceptable documented 0.3.0 limitation, not an A5 blocker.

The policy is inexpensive but not cryptographic: an in-place content change
that preserves both size and mtime can evade the identity tuple, although
physical schema revalidation catches many structural changes. Hashing,
relocation helpers, and stronger source identity remain future work.

Connection-per-operation and serializable descriptors avoid sharing a live
handle, but A5a makes no claim of fork, distributed, or parallel safety because
those modes have not been certified.

### Provenance and privacy

Deferred and eager opening retain the canonical scientific ingestion operation
`read_nc`. `memory` versus `netcdf` is representation metadata, not a different
scientific method. `cube_collect()` remains a real, separately recorded
representation transition.

The normalized local locator may be retained in `x$storage` and QA so execution
can find and validate the file. It remains non-semantic in Provenance V1 and is
not included as portable scientific identity. Print and summary do not disclose
it. The 0.3.0 contract has no remote URL, credential, token, or authentication
surface.

## Stale architecture audit

The historical `dev/architecture/netcdf-backend-contract.md` is useful design
provenance but is not an authoritative statement of current runtime state.
A5a does not broadly rewrite it.

| Section / statement | Classification | Runtime evidence |
|---|---|---|
| §1 “specification for a later milestone”, “backend designed”, “deferred reading pending” | STALE | Backend constructor, dispatcher, block/index/pair readers and tests exist. |
| §2 backend, physical/logical axes, and deferred-vs-lazy definitions | CURRENT | Runtime preserves canonical 5-D axes and materializes whenever an operation asks for values. |
| §3 description of eager `read_nc()` | CURRENT | Eager reader still opens, reads complete selected variables, permutes, and returns memory. |
| §3 “absence of serializable descriptor” and list of information always lost | STALE FOR DEFERRED; CURRENT FOR EAGER | Deferred storage retains identity, raw time, packing, physical types, dimension maps, and coordinate resolution. |
| §4 ncdf4 diagnostics | CURRENT EVIDENCE | The current fixture and backend tests exercise packing and physical permutations. |
| §5 initial scope | CURRENT | Local, read-only, rectilinear, compatible single-file variables remain the implemented boundary. |
| §§6–7 descriptor alternatives/recommendation | CURRENT AS IMPLEMENTED | Grouped `x$storage`, no extra public class, descriptor version 1. |
| §§8–21 lifecycle, identity, coordinates, mapping, variables, time, packing, reads, noncontiguous indices, write policy | CURRENT AS IMPLEMENTED | Direct runtime and test evidence cover each contract; sparse envelopes can over-read. |
| §22 “future `cube_collect()`; not implemented” | STALE | Public `cube_collect()` fully materializes deferred input and is an identity no-op for memory input. |
| §23 provenance | PARTIALLY CURRENT | Product/backend facts are retained, but V1 now keeps local path and execution timestamps non-semantic and QA-scoped. |
| §24 validation | CURRENT AS IMPLEMENTED | Logical descriptor and physical file/schema checks exist. |
| §25 “future size estimator” | PARTIALLY CURRENT | Logical byte estimates exist in inspection/read diagnostics; no general new public estimator is approved. |
| §26 designed errors | CURRENT AS IMPLEMENTED | Deterministic file, schema, calendar, grid, variable, index, and read-only errors exist. |
| §27 separate opener recommendation, name pending | PARTIALLY CURRENT → RESOLVED BY A5a | Separate entry remains correct; A5a approves `cube_open()`. |
| §28 “future internal API, not implemented” | STALE | The proposed storage, validation, connection, schema, translation, decode, and permutation responsibilities are implemented, sometimes under evolved helper names. |
| §29 decision matrix | PARTIALLY CURRENT | All backend rows remain valid; the public-entry row changes from PENDING to APPROVED `cube_open()`. |
| §30 read flow | CURRENT | Matches the dispatcher and decode/permutation path. |
| §31 “future tests; do not add until functions exist” | STALE | Descriptor, read, block, noncontiguous, lifecycle, mutation, serialization, and parity tests exist. |
| §§32–34 limitations and approval questions | FUTURE/PARTIALLY CURRENT | Limitations remain; backend questions are resolved, while caching, remote, multifile, advanced calendars, and public implementation remain future work. |

A5b may reconcile this historical document after implementing the approved
public entry.

## Public API gap

A user can call public `read_nc()` but cannot request the existing deferred
representation without `oceancube:::` calls. The backend is present; the clean
public entry is missing. This is exactly A1-004.

## Candidate evaluation

The scorecard is in `a5a-api-options.csv`. Positive criteria use 1 (poor) to 5
(best); complexity and surprise use 1 (low) to 5 (high). The formula is:

```text
3*backward_compatible
+ 3*semantic_clarity
+ 2*accurate_terminology
+ 2*discoverability
+ future_local_multifile + future_remote + future_provider_architecture
+ future_zarr + future_lazy_DAG
- 2*implementation_complexity
- 2*migration_complexity
- 2*user_surprise_risk
```

`new_export_count` is reported but is not secretly penalized: API minimality is
already represented by complexity, migration, surprise, and the explicit
one-export justification.

### Candidate A — `read_nc(lazy = TRUE/FALSE)`

This needs no new export and can preserve the default, but “lazy” overclaims the
current compute model. The same reader call would return objects with different
storage, file-lifetime, serialization, and direct-`x$data` behavior. It is
discoverable but semantically overloaded. **Rejected.**

```r
x <- read_nc("sst.nc", vars = "sst", lazy = TRUE)
cube_crop(x, longitude = c(276, 280), latitude = c(-18, -12))
cube_collect(x)
```

### Candidate B — `read_nc(mode = ...)` or `backend = ...`

`mode = c("memory", "deferred")` uses accurate terms and is better than a
boolean. `backend = "netcdf"` is ambiguous because eager `read_nc()` also reads
NetCDF. Both still overload a materializing verb and make return/lifecycle
semantics argument-dependent. They also tie future source opening to the eager
reader signature. **Rejected.**

```r
x <- read_nc("sst.nc", vars = "sst", mode = "deferred")
cube_inspect(x)
cube_collect(x)
```

### Candidate C — dedicated opener

`cube_open()` expresses a source-to-deferred-cube transition and pairs directly
with `cube_collect()`. It adds one narrowly justified export without changing
any existing call. Its generic name leaves room for later local collections,
remote resources, or Zarr only after separate contracts.

`open_nc()` is terse but less discoverable and NetCDF-locked;
`open_netcdf()` is clearer but still format-locked; `cube_open_netcdf()` is
explicit but long and also prevents a generic future opener. `read_nc_deferred`
still sounds like a read/materialize function. **`cube_open()` approved.**

```r
x <- cube_open("sst.nc", vars = "sst")
cube_inspect(x)
small <- cube_crop(x, longitude = c(276, 280), latitude = c(-18, -12))
point <- cube_extract(x, longitude = 278, latitude = -14,
                      match = "nearest", mode = "series")
x_mem <- cube_collect(x)
```

### Candidate D — `ocean_cube(storage = ...)`

This leaks a backend descriptor into the scientific in-memory constructor,
obscures I/O and file-lifetime semantics, and would make an internal versioned
schema appear public. It has no meaningful clarity advantage over one opener.
**Rejected.**

### Candidate E — make `read_nc()` deferred by default

This would break direct `x$data` consumers, performance expectations, file
lifetime, serialization assumptions, tests, and downstream code. No evidence
justifies that surprise. **Rejected.**

## Exact selected contract for A5b

1. `cube_open()` accepts one existing local file in 0.3.0.
2. It is read-only and rejects URI schemes, remote endpoints, and directories.
3. `vars` is a character vector of unique NetCDF data-variable names or NULL.
4. `vars = NULL` discovers, using header/metadata only, every source variable
   except the resolved 1-D longitude, latitude, depth, and time coordinate
   variables, preserving source order. It does not read scientific arrays.
5. The complete selected set is then subject to the existing compatibility
   contract. Mixed surface/depth or incompatible grids fail informatively; the
   implementation does not silently skip variables. Users can pass `vars`
   explicitly to open separate compatible cubes.
6. Explicit dimension-name overrides take precedence. Otherwise the deferred
   resolver uses CF `standard_name`/`axis`/units evidence plus known-name
   fallback.
7. The return type is `c("ocean_cube", "list")`, with `x$storage` and no
   `x$data` or persistent handle.
8. The public contract is experimental in 0.3.0; the nested descriptor schema
   is inspectable but internal/versioned.
9. `cube_collect()` is the canonical explicit deferred-to-memory boundary.
10. Canonical provenance ingestion operation remains `read_nc`; backend is
    non-semantic representation metadata.

The `vars = NULL` rule intentionally mirrors eager reader intent, not every
detail of its older name-only parser. No payload may be materialized merely to
discover variables.

## Coordinate discovery and future parser convergence

The eager reader currently uses explicit overrides or fixed names
(`longitude/lon/x`, `latitude/lat/y`, `time/date`, `depth/deptht/lev/z`). The
deferred backend first uses CF evidence and then known names; it additionally
recognizes conventions such as `t` and `level`, while the eager reader alone has
the historical `date` fallback. The backend also retains physical maps and
packing metadata that eager ingestion discards.

This divergence is not an A5a runtime blocker and must not be silently unified
here. It is 0.3.0-B CF/interoperability debt related to A1-002. After A5b, a
future target invariant should be:

```r
read_nc(file, ...) ~= cube_collect(cube_open(file, ...))
```

That means one schema/coordinate/time/packing parser with eager collection as a
representation choice. It is a recommendation for 0.3.0-B design, not authority
to refactor now. A3B-001 (`zlev`), A3B-002 (static field), and A3B-003 (`months
since`) remain open and unchanged.

## Materialization and operation behavior

The complete 38-export audit is in `a5a-operation-matrix.csv`.

- `cube_slice()` and `cube_crop()` accept deferred input, issue bounded reads,
  and return independent memory cubes.
- `cube_extract()` and `cube_transect()` issue bounded reads and return tables.
- `cube_mask()` always returns a same-shape memory cube; `keep = "inside"`
  bounds its source read to the retained spatial rectangle, while `outside` may
  read the complete spatial extent.
- Aggregate, climatology, anomaly, trend, layer, and built-in monthly paths use
  bounded backend reads but return materialized results.
- `annual_index()`, `crop_stock()`, public eager `read_nc()`, and deferred
  `cube_collect()` require complete source materialization. The deprecated
  custom reducer path in `to_month()` also does.
- Geometry helpers, `stock_mask()`, `coast_dist()`, validation, and default
  inspection do not read scientific payload values.
- Of current exported functions that return an `ocean_cube`, only
  `coast_dist()` preserves a deferred input representation; it attaches a
  coordinate-derived `dc` matrix and retains `x$storage`. Other cube-producing
  transformations materialize their output.

`cube_collect(memory_cube)` is an observable no-op and returns the same object.
`cube_collect(netcdf_cube)` reads the complete logical 5-D payload, creates an
independent memory cube, preserves scientific metadata, and appends
`cube_collect` provenance. The result remains usable after source deletion.

## Numerical and metadata parity prototypes

The reproducible dev-only script is `a5a-prototype.R`; measured evidence is in
`a5a-parity-results.csv`. It uses only current internals and the existing
governed OISST fixture.

### OISST

For `sst`, `anom`, `err`, and `ice` with explicit `depth_name = "zlev"`, eager
and deferred→collect have identical lon, lat, singleton depth, time, variable
order, decoded scientific values, and missingness. Slice, crop, extract,
transect, daily aggregate, full collect, and RDS-restored reads all pass exact
scientific parity. Source/dataset arguments use the same defaults in the
prototype. Provenance represents the same ingestion method; collected output
correctly has one additional representation-transition record.

### Synthetic packed/permuted fixture

Coordinates, dimensions, variable order, slice, crop, extract, transect, and
RDS behavior agree. Full and aggregate equality differ at exactly one value:
the eager reader retains the fixture's `missing_value` sentinel, whereas the
deferred backend converts it to `NA` before applying scale/offset. Existing
backend regression tests classify and repair this same baseline difference.

Classification: **EXPECTED CURRENT DIFFERENCE / eager parser debt**, not an A5
blocker. The deferred result follows the stronger missing/packing contract. It
must be reconciled only through a separately tested shared parser, not hidden
by A5a documentation.

Units, source, dataset_id, calendar/time class, and logical coordinates are
preserved for supported cases. Internal backend metadata is richer by design
(physical maps, packing attributes, file identity). Backend and locator
differences are representation/QA facts, not scientific provenance mismatches.

## Performance and memory smoke

These micro-fixture timings are elapsed seconds averaged over 20 calls on the
local Windows/R environment; they are smoke evidence, not A6 benchmarks:

| Fixture | eager open/full | deferred descriptor | deferred crop | deferred extract | collect |
|---|---:|---:|---:|---:|---:|
| synthetic 3×2×2×4×2 | 0.2110 | 0.3505 | 0.2210 | 0.1485 | 0.2000 |
| OISST 36×48×1×4×4 | 0.0560 | 0.0735 | 0.2125 | 0.1465 | 0.2355 |

On tiny files, open-per-operation and validation dominate, so bounded calls can
be slower than a full read. No large-scale speed claim follows. The relevant
architectural evidence is that crop/extract invoke bounded physical plans and
do not require a full payload.

The OISST deferred object was 73,896 bytes versus a 262,504-byte collected
object and a 221,184-byte raw double payload estimate. On the extremely small
synthetic fixture, descriptor overhead (60,184 bytes) exceeds the 768-byte raw
payload and the collected object (34,288 bytes), as expected. Descriptor
construction contains no scientific array. Peak RSS and scaling remain open
A1-009/A6 work.

## External ecosystem review

The selected verb follows a recurring resource-opening pattern without copying
another ecosystem's compute model:

- [xarray `open_dataset()`](https://docs.xarray.dev/en/stable/generated/xarray.open_dataset.html)
  opens/decodes a Dataset and can use private lazy indexing or Dask chunks;
  oceancube takes only the “open resource” naming cue and does **not** claim a
  Dask-like graph.
- [stars `read_ncdf()`](https://r-spatial.github.io/stars/reference/read_ncdf.html)
  exposes a `proxy` choice and metadata-only proxy behavior. Oceancube avoids
  mode-dependent `read_nc()` semantics because eager compatibility is already
  established.
- [terra `rast()`](https://rspatial.github.io/terra/reference/rast.html) opens
  file-backed rasters without loading values, but its C++ pointer objects have
  session/serialization limitations. Oceancube deliberately retains a pure-R
  serializable descriptor instead.
- [Arrow `open_dataset()`](https://arrow.apache.org/docs/r/articles/dataset.html)
  opens metadata/schema and delays query execution until `collect()`. The
  open/collect vocabulary is useful, but oceancube does not implement Arrow's
  lazy query optimizer.
- [ncdfCF `open_ncdf()`](https://r-cf.github.io/ncdfCF/reference/open_ncdf.html)
  reads/interprets metadata without reading CF variable data, supporting the
  distinction between opening and extracting values.
- [tidync](https://docs.ropensci.org/tidync/reference/tidync-package.html)
  records dimension filters without reading data until `hyper_array()` or
  `hyper_tibble()`, reinforcing explicit delayed extraction.

No package is added as a dependency, and DEC-012's no-Python-core boundary is
unchanged.

## Future compatibility

`cube_open()` separates opening from acquisition. Provider authentication,
catalog search, download planning, and APIs for Copernicus, NOAA, NASA
Earthdata, ERDDAP, OPeNDAP/THREDDS, or other providers remain downstream and
must not enter A5b.

The singular local-file signature is deliberately narrow. A later reviewed
contract may add `files =` or a separate collection opener after defining
cross-file variable/grid/time compatibility. Remote opening requires separate
identity, retry, caching, credential-redaction, and range/OPeNDAP semantics.
Zarr requires its own storage/chunk contract. The generic `cube_open()` name
does not block these directions, but A5a commits to none of them.

The current deferred resolver also leaves room for future CF support for
`zlev`, static fields, nonstandard calendars, and richer axes. ETOPO and WOA23
limitations remain untouched; no unsupported path was forced into the
prototype.

## Finding and gate status

- **DEC-018:** OPEN → APPROVED. Rationale: evidence selects one experimental
  `cube_open()` entry with explicit local/read-only/deferred semantics and an
  established `cube_collect()` boundary.
- **A1-004:** OPEN → PARTIALLY-CLOSED. The full public contract is approved;
  it is not closed until A5b implements and certifies the export.
- **A1-002:** unchanged PARTIALLY-CLOSED; parser convergence belongs to future
  NetCDF/CF work.
- **A1-009:** unchanged OPEN; peak-memory and stress evidence remain A6.
- **0.3.0-A complete:** FALSE.
- **Gate B:** FALSE.

The single next subphase is **A5b — PUBLIC DEFERRED NETCDF API
IMPLEMENTATION**. A5a does not authorize A5b execution, A6, CF, provider APIs,
vertical science, regridding, 3-D work, or a merge to main.

## A5b implementation certification

A5b implements the exact DEC-018 signature as the sole new public export.
`cube_open()` delegates to `.new_netcdf_storage()` and
`.new_netcdf_cube()`; no dimension, decoding, connection, identity, or
provenance logic is duplicated. The storage constructor now accepts
`variables = NULL`, discovers `nc$var` entries in source order after excluding
dimension-coordinate variables, and subjects the complete result to the same
compatibility validation as an explicit selection. Discovery and construction
read coordinates but never invoke scientific block-read machinery.

The public regression suite covers explicit and discovered variables, empty
discovery, incompatible grids and vertical mixes, local-resource errors,
print/summary/inspection privacy, validation, RDS restore, moved/changed source
errors, bounded slice/crop/extract/transect paths, temporal paths, coast-distance
backend preservation, and governed offline OISST eager parity. The packed
synthetic eager `missing_value` difference remains an explicitly expected
parser debt; no eager behavior was changed.

Consequently, `DEC-018` is **APPROVED — IMPLEMENTED/CERTIFIED** and `A1-004`
is **CLOSED**. The package has 39 exports, with `cube_open` as the only new
name. `read_nc()` remains eager and unchanged, Version remains 0.2.0.9000,
dependencies are unchanged, and 0.3.0-A remains incomplete because `A1-009`
peak-memory/stress evidence remains open. The single next subphase is **A6 —
PEAK MEMORY, STRESS AND HARDENING PERFORMANCE CERTIFICATION**; A5b does not
execute it.

The final local suite contains 62 files, 612 cases and 4,979 expectations. It
completed in 481.18 seconds with zero failures, errors, warnings, or skips.
The bounded performance smoke is recorded in `a5b-runtime-results.csv`; it is a
regression indicator and makes no broad performance claim. A clean-snapshot
`R CMD build` passed after the direct build encountered only the known Codex
Git-ref copy limitation; `R CMD check --no-manual` completed with `Status: OK`
(0 errors, 0 warnings, 0 notes). The installed package exposes
`oceancube::cube_open`, its help topic, and exactly 39 exports.
