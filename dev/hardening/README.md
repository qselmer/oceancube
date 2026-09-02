# oceancube 0.3.0 hardening baseline

## Purpose and status

This directory records the bounded 0.3.0-A1 audit at commit
`a8245e22a23485cac7ab3970f14c8a22b9b82138`. It is measurement evidence, not
an optimization, release threshold, public API, or claim that 0.3.0-A is
complete. A1 found no S0 scientific-correctness blocker.

The baseline is deliberately outside `tests/testthat/`. It changes no package
runtime code, exports, signatures, dependency declarations, or scientific
expected values. Large inputs, caches, profiling HTML and provider data are not
versioned here.

## A2 contract strengthening

The 0.3.0-A2 phase preserves the A1 evidence above and adds deterministic
contract tests rather than replacing the baseline. A custom base-R/testthat
generator in `tests/testthat/helper-cube-generators.R` creates small valid
five-dimensional cubes with normal or singleton axes, Date or UTC POSIXct time,
irregular sequences, constant/index/linear fields, multiple variables, and
seven structured missingness patterns. `A2_GENERATOR_SEED` is `303002`; current
generators are arithmetic and consume no random stream, so the seed reserves a
stable strategy for later deterministic cases. No property-testing dependency
was added.

A2 directly covers full crop/slice/collect identities, source non-mutation,
untouched dimensions, units, exact linear trends, analytical
climatology/anomaly behavior, all five visualization data paths, common
memory/NetCDF validation outcomes, mocked Copernicus client contracts, packed
NetCDF values, bounded singleton blocks, and supported time edges. Exact and
operation-specific absolute tolerances are recorded in
`tolerance-policy.csv`; backend and invariance conclusions are updated only for
executed evidence.

Successful Python/Copernicus integration remains outside the default suite. A
future optional class may use `OCEANCUBE_EXTERNAL_TESTS=true`, but must not run
in ordinary CI, manage credentials, create environments, install packages, or
download provider data without a separately approved contract. A2 implements
no external class and uses no network.

Reproduce A2 coverage with:

```powershell
$env:LC_ALL = 'English_United States.utf8'
Rscript --vanilla dev/hardening/run_coverage_a2.R
```

The runner creates a clean temporary source snapshot, leaves the A1 CSVs
unchanged, and writes phase-specific file/function summaries plus an A1-to-A2
comparison. On local Windows, use an installed UTF-8 locale such as the one
above; `C.UTF-8` is not assumed. This is a harness/environment requirement and
does not weaken Unicode package tests.

The executed A2 line result is 91.3319%, compared with 90.3872% in A1
(+0.9447 percentage points). Functions with any covered line increased from
242 to 247 of 254. The three previously zero-covered exports now measure
`cm_setup()` 75%, `cm_connect()` 57.1429%, and `download_nc()` 92.8571%.
`.read_cf_time()` and `.slice_time_numeric()` reached 100%; other low NetCDF
internals did not all move, which is why A1-002 is only partially closed.

## A3b governed real-data fixtures

A3b preserves the A1/A2 evidence and adds one focused seven-case REAL-DATA test
file backed by three committed NOAA/NCEI subsets. The OISST v2.1 fixture is the
current-runtime contract; ETOPO 2022 static elevation and WOA23 native
climatological time are metadata-pass/current-expected-limitation contracts.
No scientific runtime, public API, package dependency, version, Python
environment, credential path, or provider call in default tests is added.

The fixtures total 1,333,084 bytes. OISST is 64,088 bytes, ETOPO is 1,211,653
bytes, and WOA23 is 57,343 bytes. ETOPO deliberately retains provider Float32
values: its size is above the preferred 1 MB target but below the reportable
2 MB bound and the combined total is below 3 MB. All three scripts produced the
same SHA-256 on a second run. Exact source/final hashes, terms, attribution,
access methods, current classifications, and future assignments are in
`real-data/` and beside the committed fixtures.

The final local suite has 46 files, 540 cases and 4,284 dynamic expectations
in 222.97 seconds with zero failures, errors, warnings or skips. This is +1
file, +7 cases and +86 expectations relative to the preserved A2 measurement.
The source tarball grew from 1,591,567 to 2,829,421 bytes (+1,237,854).
`R CMD build` required the established clean-snapshot fallback solely because
the desktop `.git/refs/codex/turn-diffs` tree cannot be copied by R; `R CMD
check --no-manual` on the resulting tarball ended `Status: OK` with 0 errors,
0 warnings and 0 notes.

## A4a versioned provenance architecture

A4a is design-only evidence. It audits 21 current provenance-producing paths,
including cube, table, legacy-wrapper, geometry, and deferred-NetCDF shapes. A
read-only OISST pipeline measured recursive parent depth increasing from 0 to 5
and serialized provenance increasing from 1,838 to 19,453 bytes; the anomaly
merge nearly doubled the preceding lineage because both complete parents were
embedded.

The selected V1 architecture is a hybrid: one flat ordered primary history plus
a flat registry of secondary lineages referenced by deterministic local
operation/entity IDs. Timestamps, local locators, backend/read metrics, and
opaque legacy/user metadata are excluded from semantic equivalence. Existing
0.2 objects migrate lazily into derived outputs without mutating their inputs.
The normative design is in `inst/architecture/` and the audit, field contract,
operation map, migration map, and candidate comparison are in `provenance/`.

At the A4a close, `A1-003` was only partially closed because runtime helpers
and tests did not yet exist. `DEC-019` remained approved. A4a itself changed no
runtime, tests, dependencies, exports, fixtures, or version and did not
complete 0.3.0-A or satisfy Gate B; the A4b1 status below records the subsequent
core implementation.

## A4b1 provenance V1 core engine

A4b1 implements the approved internal engine in one runtime module: exact V1
validation, legacy/opaque normalization, deterministic append, flat secondary
lineage registration, semantic projection, serialization/security checks and
loaded-package version resolution. Five focused test files plus one helper
exercise all 75 field-contract rows, current OISST legacy migration, current
anomaly migration, nested-lineage flattening, semantic equality, round trips,
non-mutation and 1/5/10/25/50-operation growth. At 50 operations the serialized
record is 47,221 bytes (944.42 bytes per operation), with no recursive parent
tree multiplication.

At the A4b1 close the state was deliberately dual: runtime producers,
`ocean_cube()`, and `read_nc()` still emitted legacy provenance. `A1-003`
therefore remained `PARTIALLY-CLOSED`; `DEC-019` remained APPROVED. Neither
0.3.0-A nor Gate B was complete.

## A4b2 core runtime provenance migration

A4b2 migrates the linear core lifecycle to V1: construction, materialized and
deferred NetCDF ingestion, slice, crop, NetCDF-to-memory collect, and mask.
These producers append flat, sequential operation records, preserve source and
source-time identity, keep detailed read/index diagnostics in QA, and accept
NULL, V1, legacy, and safe opaque provenance without mutating inputs. Memory
collect remains an exact no-op. Complex temporal, multi-input, table/geometry,
and wrapper producers remain legacy for A4b3; a narrow bridge preserves a V1
primary parent through that temporary mixed state and normalizes it without
lineage loss. `A1-003` remains `PARTIALLY-CLOSED`; `DEC-019` remains APPROVED.
Neither 0.3.0-A nor Gate B is complete.

## A4b3a temporal and multi-input provenance migration

A4b3a migrates `cube_aggregate_time()`, `cube_climatology()`,
`cube_anomaly()`, `signal_noise()`, and `cube_trend()` to bounded V1 operation
records. Anomaly keeps its source in the primary history and registers the
climatology once as a role-labelled secondary lineage. The compatibility
wrappers `to_month()`, `clim_month()`, `clim_day()`, `anom_diff()`, and
`anom_z()` reuse the delegated canonical operation without duplication;
`signal_noise()` records its real transformation after standardized anomaly.

Synthetic and governed OISST tests certify historical, recurring-climatology,
restored-historical, and trend-anchor time transitions; input non-mutation;
memory/NetCDF numerical parity; QA retention; semantic determinism; RDS
roundtrips; privacy; and single-copy multi-input growth. The final representative
V1 chain is 19,511 bytes versus the 19,453-byte recursive A4a reference; the
criterion is bounded scaling, not a tiny-object size win. The remaining legacy
producers are exactly `cube_extract`, `cube_transect`,
`cube_polygon_weights`, `layer_mean`, `coast_dist`, and `crop_stock`.
`A1-003` remains `PARTIALLY-CLOSED`, `DEC-019` remains APPROVED, and A4b3b is
still required. Neither 0.3.0-A nor Gate B is complete.

## A4b3b final runtime provenance migration

A4b3b migrates `cube_extract()`, `cube_transect()`,
`cube_polygon_weights()`, `layer_mean()`, `coast_dist()`, and `crop_stock()` to
the common V1 schema. Table QA is carried internally under `oceancube_qa`;
selection, path, coverage, values, and geometry-weight columns remain intact.
The historical polygon `provenance` attribute remains an exact V1 compatibility
alias because documented examples and tests relied on it. The polygon engine
truthfully records controlled s2 intersection; coast distance remains
numerically unchanged and records the actual global s2 state without claiming
a guaranteed s2 scientific method.

The final source scan has zero active legacy runtime producers and retains only
legacy parsers, compatibility recognition, the polygon alias, and the unused
`.make_provenance()` helper definition for A4-EXIT review. `A1-003` remains
`PARTIALLY-CLOSED`, `DEC-019` remains APPROVED, and A4-EXIT is next. Neither
0.3.0-A nor Gate B is complete.

The final local suite executes 59 files, 589 cases, and 4,695 expectations in
523.060 seconds with zero failures, errors, warnings, or skips. The clean-source
tarball is 2,861,995 bytes; `R CMD check --no-manual` ends `Status: OK` with
zero errors, warnings, and notes. `DESCRIPTION`, dependencies, `NAMESPACE`, the
38-export API, version, scientific values, `main`, and `v0.2.0` remain guarded.

## A4-EXIT global Provenance V1 certification

A4-EXIT reconciles every A4a operation row, recursively classifies the requested
runtime patterns, and certifies the seven cube/table output families against one
strict V1 schema. Executable evidence covers deterministic operation and lineage
IDs, 0.2 migration, one-copy anomaly lineage, source/time semantics, Date and UTC
POSIXct, serialization/RDS, sentinel privacy/security, QA and CF boundaries,
offline OISST lineage, synthetic temporal, table, geometry, stock, backend
parity, scientific invariants, and linear growth through 50 operations.

The final runtime result is zero active legacy producers. `.make_provenance()`
is retained as one compatibility/test helper definition with zero runtime calls;
three tests use it to construct historical inputs. `A1-003` is `CLOSED` and
`DEC-019` remains APPROVED with implementation status IMPLEMENTED/CERTIFIED.
A4 implementation, global certification, and phase closure are TRUE.

`A4B3B-001` remains OPEN. It does not block A4 because V1 records the resolved
s2 state without a false scientific-method claim, but it blocks the 0.3.0-A
hardening exit because coast-distance values materially depend on caller-global
state. The single recommended next subphase is A4R before A5. Neither 0.3.0-A
nor Gate B is complete.

The final local suite executes 60 files, 598 cases, and 4,857 expectations in
339.180 seconds with zero failures, errors, warnings, or skips. The final
clean-source tarball is 2,865,585 bytes; `R CMD check --no-manual` ends
`Status: OK` with zero errors, warnings, and notes. Runtime `R/`, DESCRIPTION,
dependencies, NAMESPACE, the 38-export API, version, and scientific values are
unchanged.

## A4R coast-distance reproducibility certification

A4R compares spherical S2 and ellipsoidal GeographicLib/PROJ distances on nine
bounded synthetic cases before selecting the canonical method. The methods are
measurably different, with 0.006138% to 0.254413% relative difference across
non-zero cases, but show no pathological behavior. `coast_dist()` now reuses
`.with_s2_geometry()`, restores the caller's state on success and error, and
produces identical output and semantic provenance from initial S2 TRUE/FALSE
states. The ordinary S2=TRUE result remains binary-identical.

The operation emits `oceancube:s2_coast_distance` version 1 and compact
spherical-S2 semantics. `A4B3B-001` is `CLOSED`. Existing polygon behavior is
documented and retained; its interior-zero versus boundary-distance ambiguity
is non-blocking S3 finding `A4R-001`. A5b subsequently closes `A1-004` and
implements/certifies `DEC-018`; `A1-009` is the principal remaining 0.3.0-A
blocker. 0.3.0-A and Gate B remain incomplete, and A6 is the single recommended
next subphase.

## Reproduce the bounded baseline

The normal smoke/standard runner is:

```powershell
$env:OCEANCUBE_A1_COVERAGE_ROOT = '<directory containing coverage.rds, coverage-by-file.csv and coverage-by-function.csv>'
Rscript --vanilla dev/hardening/run_baseline.R
```

The coverage inputs were produced locally with `covr::package_coverage()` from
a clean source snapshot that excluded `.git` and `artifacts`. `covr` is a local
developer tool and was not added to `DESCRIPTION`. The runner deterministically
regenerates the inventory, provenance audit, and TINY/SMALL memory baselines.
Seed: `303001`. The benchmark date is the CSV file/commit date; environment
metadata, commit, R/platform/CPU and dependency versions are in
`baseline-summary.csv`.

Profiles:

- **smoke** — TINY memory tier; seconds; CI-safe once a dedicated workflow is approved.
- **standard** — SMALL plus future MEDIUM and temporary NetCDF tiers; minutes; maintainer/optional CI.
- **stress** — LARGE-LOCAL, long time axes, high missingness and sparse reads; explicit maintainer opt-in only.

No MEDIUM, LARGE-LOCAL, multi-hour, provider-download, or stress run occurred in
A1. Normal testthat remains the authoritative fast deterministic suite.

## Evidence map

- `test-inventory.csv`: 40 files, 506 `test_that()` cases and 3,151 static
  `expect_*()` call sites, classified by inspected bodies/assertions. Taxonomies
  are non-exclusive. The executed test suite is the source for 3,675 dynamic
  expectations.
- `test-taxonomy-summary.csv`: non-exclusive taxonomy totals.
- `coverage-by-file.csv` and `coverage-by-function.csv`: 90.3872% overall line
  baseline, 254 functions and file/function classes.
- `backend-parity.csv` and `netcdf-read-baseline.csv`: asserted result and read
  behavior, including intentional materialization.
- `invariance-matrix.csv`: required contracts and current evidence.
- `benchmark-plan.csv`, `benchmark-matrix.csv` and `hardening-baseline.csv`: tier
  design, benchmark axes and 24 bounded timing/allocation observations.
- `provenance-audit.csv`: actual field frequency across nine operation classes.
- `fixture-manifest.csv`: A1 governance-first inventory/proposals plus appended
  A3b executed rows; historical proposal rows are retained.
- `real-data/`: A3a first-party source/legal scoring, fixture policy, proposed
  manifest and specialized contract matrix plus A3b phase-specific executed
  fixture, contract and REAL-DATA taxonomy evidence; provider binaries live
  only under `tests/testthat/fixtures/real-data/`.
- `findings.csv`: classified gaps and remediation routing.
- `baseline-summary.csv`: compact machine-readable status/environment summary.
- `coverage-a2-by-file.csv`, `coverage-a2-by-function.csv`,
  `coverage-a2-summary.csv`, and `coverage-comparison.csv`: A2 remeasurement and
  the preserved A1-to-A2 comparison.
- `tolerance-policy.csv`: exact versus operation-specific absolute numerical
  contracts used by direct parity tests.
- `a2-summary.csv`: compact executed A2 certification evidence.
- `a3b-summary.csv`: before/after tests, taxonomy, fixture/package sizes,
  reproducibility, local build/check and guardrail evidence; three-OS CI is
  recorded externally after the authorized push.

## Coverage and quality

The A1 line baseline is strong by the project heuristic. Of 254 detected
functions, 242 (95.2756%) have at least one covered expression; this is an
inventory statistic, not proof of full function contracts. Line percentage is
also not contract completeness. Lowest files are `R/cm_setup.R` (0%),
`R/download_nc.R` (0%), `R/cube_collect.R` (73.02%),
`R/backend-netcdf.R` (81.78%), `R/utils-internal.R` (83.04%) and
`R/backend-index.R` (84.67%). Untested exports are `cm_connect`, `cm_setup` and
`download_nc`. Critical below-70% internals are recorded in
`baseline-summary.csv`.

Separate quality gaps remain around calendar/timezone boundaries, irregular
sampling, structured NA patterns, singleton dimensions, coordinate
monotonicity, depth orientation, packed NetCDF values, sparse lazy subsets,
zero/near-zero anomaly variance, constant trends, empty selections and boundary
tolerance. These are not negated by the overall percentage.

## Property and invariance strategy

Reviewed approaches are `quickcheck`, `hedgehog`, and custom deterministic
base-R/testthat generators. `quickcheck` offers generated examples, failure
reproduction and shrinking; `hedgehog` offers composable generators and
shrinking. Neither is installed or justified as a package dependency in A1.

Recommendation: first build small, deterministic domain generators around the
strict `ocean_cube` validity contract, with fixed seeds and explicit failing
inputs. Re-evaluate `quickcheck` as a developer/test dependency when the
generator contract stabilizes and shrinking provides clear value. This avoids
high discard rates and opaque invalid cubes while preserving reproducibility.

Primary documentation reviewed: [covr](https://covr.r-lib.org/reference/covr-package.html),
[bench](https://bench.r-lib.org/),
[quickcheck](https://armcn.github.io/quickcheck/reference/for_all.html),
[hedgehog](https://cran.r-universe.dev/hedgehog/hedgehog.pdf),
[profvis](https://profvis.r-lib.org/articles/profvis.html), and R's
[memory-profiling notes](https://developer.r-project.org/memory-profiling.html).

Candidate properties are full-extent crop identity, all-coordinate slice
identity, collect identity, memory/NetCDF equivalence, identity aggregation,
climatological anomaly centering, exact linear-trend recovery, canonical
coordinate order, source non-mutation, untouched-dimension preservation and a
valid provenance parent chain. They are design targets, not all implemented.

## Backend and NetCDF reads

No mismatch was found in existing direct parity assertions. Exact evidence is
strong for slice, crop, extract and collect; temporal/transect engines are
tolerance-equivalent. Direct parity gaps remain for validation and systematic
visualization data preparation. Geometry payload parity is not applicable
where results depend only on coordinates.

No accidental full-cube read was found in the representative NetCDF paths.
`read_nc()` and `cube_collect()` intentionally materialize; `cube_inspect(...,
missing = "full")` warns before the documented full scan. Anomaly and trend
stream the source in blocks but intentionally materialize their final memory
result. These semantics must not be misclassified as regressions.

## Benchmark and memory interpretation

A1 used `system.time()`, `Rprofmem()` and `object.size()` and explicitly lacked
a reliable process peak. A6 closes that instrumentation gap with calibrated OS
peak RSS from a fresh live x64 R process per scenario, deterministic
TINY/SMALL/MEDIUM tiers, matched controls, three/three/two replicates, bounded
read accounting, governed OISST smoke and repeated backend stress. `Rprofmem`,
`object.size()` and elapsed time remain secondary diagnostics.

The A6 calibration observed a 138,076,160-byte median peak increment for a
touched 128 MiB allocation (ratio 1.02875). Across 128 required scientific
workers there was no RED classification. MEDIUM anomaly and trend were GREEN;
aggregate and climatology were AMBER because their prepared-input peak masked
the operation increment, so no empirical exponent is asserted. Deferred crop
and transect were strongly bounded and GREEN. Sparse slice/extract were AMBER:
their 720 logical values require a 1,036,800-value enclosing NetCDF envelope,
but both still used roughly 47% less incremental peak than eager `read_nc()`
and 60% less than `cube_collect()`. This non-blocking characterization is
retained as `A6-001`.

Stress completed 75 alternating deferred operations plus 50 descriptor/error
cycles with RSS growth of 901,120 bytes between initial/final checkpoints,
connection delta zero, 50/50 expected errors and a successful NetCDF
rename-and-restore probe. `A1-009` is therefore CLOSED and A6 is COMPLETE.
Full methodology and raw evidence are in `performance/README.md` and its five
canonical CSV files. These are empirical results for the certified environment,
not universal complexity or memory promises and not authority to optimize.

## Real-data governance

The local Copernicus RDS exists at the historical maintainer path, is 3,554,623
bytes on disk (12,536,024-byte R object), has shape 73x85x18x7x2, variables
`thetao`/`so`, units, source/product metadata and provenance. SHA-256 is recorded
in `fixture-manifest.csv`. Its redistribution permission is **UNKNOWN**, so it
is usable for local maintainer smoke only and is not CI-eligible or committed.

A3a retains the A1 manifest as historical evidence and records its phase-specific
review in `real-data/`. A3b executes the maintainer-approved orthogonal suite:
OISST v2.1 for surface/time, ETOPO 2022 v1 60 arc-second bedrock elevation, and
WOA23 v3.3 1-degree annual all-decades temperature/salinity. All have exact
source/product identity, first-party terms, retrieval date, deterministic
derivation, source/final checksums, tested contract and offline-CI decision.
MUR and GEBCO_2025 remain historical alternates; Copernicus stays local and
`A1-005` stays partially closed. `A1-010` closes and `DEC-024` is approved only
for the exact three-source set. 0.3.0-A and Gate B remain incomplete.

## Deferred NetCDF API decision evidence

A5a approves DEC-018 as an architecture contract. A5b implements and certifies
the selected option and closes `A1-004`.

| Option | Main benefit | Principal cost/risk |
|---|---|---|
| `read_nc(..., lazy = TRUE)` | Smallest visible API addition and familiar reader | Overloads a currently materializing verb; serialization/resource lifetime become mode-dependent |
| `cube_open(...)` | Clear deferred-resource semantics, discoverable lifecycle, good future multi-file direction | New public export and lifecycle contract; must specify identity, serialization, closure and moved files |
| `ocean_cube(source = ...)` | One constructor entry | Overloads an in-memory constructor and hides I/O/lifetime semantics |
| Separate backend abstraction while keeping `read_nc()` materializing | Maximum compatibility and clean backend extensibility | Discoverability depends on the abstraction chosen; may duplicate concepts |

The implemented contract is one experimental `cube_open()` public entry while
preserving eager `read_nc()` materialization. It opens one local read-only
NetCDF source through the existing serializable deferred descriptor;
`cube_collect()` remains the explicit memory transition. A5b certifies
compatibility, serialization, identity, operations, terminology and metadata-
only `vars = NULL` discovery in `lazy-api/`. Multifile and remote behavior
remain future contracts; A6 subsequently certifies peak memory and stress.

**0.3.0-A5b status (2026-08-26): public deferred NetCDF entry implemented and
certified.** `cube_open()` is the sole new export and delegates to the existing
storage and cube constructors. Metadata-only `vars = NULL`, public bounded
operations, OISST parity, serialization/file identity, V1 provenance and
deferred `coast_dist()` preservation are executable contracts. `read_nc()`
remains eager and unchanged. `DEC-018` is implemented/certified, `A1-004` is
closed. A6 subsequently closes `A1-009` without changing the A5b runtime.

## Provenance audit and proposed schema

The preserved A1 nine-class matrix remains historical baseline evidence. A4a
extends source inspection to 21 producing paths and finalizes provenance schema
V1 design without implementing it. The selected hybrid has independent
`schema_version`, portable `source`, source/current `time`, flat `history`, a
flat secondary `lineages` registry, and opaque `extensions`. Operation records
contain deterministic IDs, requested/resolved parameters, generic input
references, bounded output summaries, optional scientific-method IDs, required
software package/version, and optional non-semantic execution metadata.

The A4a migration policy recognizes legacy operations oldest to newest,
deduplicates wrapper/core records, preserves opaque metadata losslessly, moves
read and numerical diagnostics to QA, and never treats local paths or
wall-clock timestamps as scientific semantics. Implementation and executable
compatibility evidence remain A4b work.

## Proposed 0.3.0-A exit criteria

0.3.0-A may be considered complete only when:

1. Every core dual-backend operation has direct parity evidence with an explicit tolerance or justified non-applicability.
2. Critical public functions and below-70% I/O/time helpers have contract-focused tests; no numerical percentage substitutes for this review.
3. Deterministic property/invariance generators cover identity, non-mutation, coordinates, units, time and provenance.
4. There is no open S0 correctness defect.
5. At least one real-data policy and one small CI fixture are approved with license, derivation and checksum.
6. Smoke/standard/stress profiles are reproducible, with NetCDF read accounting and reliable peak-memory instrumentation.
7. DEC-018 has a maintainer-approved public deferred-I/O decision.
8. A versioned provenance schema and backward-compatibility policy are approved and tested.

The separate A-EXIT review in `a-exit/` reconciles every A1-A6 phase and all 16
findings, reruns the current regression suite, and certifies these criteria.
As of 2026-08-27, `0.3.0-A` is COMPLETE/CERTIFIED and staged 0.3.0-B
CF/interoperability work may begin. Gate B remains UNSATISFIED: it means
permission to begin vertical science and still requires a sufficiently defined
CF/vertical-coordinate contract, redistributable depth evidence, and approved
vertical primitive semantics. No CF, vertical, provider, calendar, or runtime
change is part of A-EXIT.

## B6 CF climatological time adjudication

B6 implements exact elapsed UDUNITS month/year duration units and a compact
generic climatology descriptor under `metadata$cf$current`. It separates
representative coordinates from potentially discontinuous support envelopes,
requires the bounded within-years/over-years cell-method pattern, and guards
ordinary temporal analytics on already-climatological fields.

The governed WOA23 fixture exactly preserves official annual raw time `4614`
and climatology bounds `4212,5028`. Literal CF/UDUNITS decoding places that
support in 2306–2373, conflicting with the product's own 1955 start. Seasonal
and monthly official siblings use coherent smaller offsets. The annual fixture
therefore remains deterministically rejected without a WOA/provider branch or
an inferred offset correction. `A3B-003` is reclassified; Gate B remains
UNSATISFIED and B7 is not started. Full evidence is in `cf-climatology/`.

## B7 CF vertical semantics and Gate B

B7 supersedes the forward-looking Gate-B statement above without rewriting the
historical A-EXIT or B6 evidence. The `oceancube_cf_vertical` 1.0.0 current
descriptor, governed WOA23 January fixture, eager/deferred/collect parity,
explicit CF-bounds thickness and rectilinear volume, and safe unsupported-case
handling are certified in `cf-vertical/`. Gate B is now SATISFIED only for
dimensional metric ocean depth with certified CF semantics and explicit bounds.
The annual WOA finding remains OPEN-RECLASSIFIED and static fields remain OPEN.
The next authorized work is `0.3.0-C1 — VERTICAL PRIMITIVES AND
EXPLICIT-BOUNDS HARDENING`; B7 does not execute it.

## C1 vertical primitives and explicit-bounds hardening

C1 executes the bounded work authorized by B7. `cube_layer_thickness()`,
`cube_cell_volume()`, and `layer_mean()` now share an internal vertical support
engine. Certified CF metric-depth layer means use exact cell/layer overlap,
scale-aware full-union coverage, indexed reads, exact output bounds, truthful
current vertical metadata, and provenance V1 parameters. Partial coverage and
gaps error before payload access; zero coverage returns missing. The historical
centre-derived path remains numerically unchanged and uncertified, making
B7-001 PARTIALLY-CLOSED. Evidence is in `vertical-primitives/`; Gate B remains
SATISFIED for the same DEC-029 subset and 0.3.0-C is IN PROGRESS.

## C2 certified vertical reduction and column integration

C2 approves and implements DEC-031. `layer_mean()` and the sole new export
`layer_integral()` share one reducer and the C1 explicit-bounds support engine.
Certified integration is limited to exact CF vertical cell means, canonical
metre overlap, full geometric coverage, piecewise-constant cell means and
strict missingness. Point, sum, other and ambiguous value semantics are
rejected before payload access. Source CF metadata remains immutable; current
derived semantics and symbolic units are explicit and no false `depth: sum`
claim is created. B7-001 remains PARTIALLY-CLOSED because legacy centre
inference remains, Gate B remains SATISFIED for DEC-029, and 0.3.0-C remains IN
PROGRESS.

## C3 certified vertical depth sampling and interpolation

C3 approves and implements DEC-032 with the sole new export `depth_sample()`.
It reuses C2 value semantics: cell means use explicit-cell containment under a
piecewise-constant reconstruction, while point values use exact matches or
local two-point linear interpolation. Mixed automatic plans remain truthful per
variable. Geometry, gaps, boundaries and domains are resolved before one union
depth read; missing values are never imputed. Sampled outputs deliberately have
no physical layer bounds, so thickness, volume, integration and even bounded
layer means cannot falsely certify them. No C3-specific residual finding was
opened; B7-001 remains PARTIALLY-CLOSED, Gate B remains SATISFIED for DEC-029,
and 0.3.0-C remains IN PROGRESS.

## C4 certified vertical gradient primitives

C4 approves and implements DEC-033 with the sole new export
`depth_gradient()`. A single internal metric-depth resolver converts B7 depth
coordinates to metres positive downward before signed adjacent-pair secants.
Original/C3-derived point values and original/C1-derived cell means remain
distinct; C3 cell reconstructions, C2 integrals, unsupported semantic classes
and second derivatives are rejected. Every pair records representative
spacing and explicit support-gap state, while no gap is filled. Gradient
midpoints have symbolic per-metre units and no physical layer bounds. No C4
finding was opened; B7-001 remains PARTIALLY-CLOSED, Gate B remains SATISFIED,
and 0.3.0-C remains IN PROGRESS.

## C5 certified vertical feature detection

C5 approves and implements DEC-034 with the sole new export
`depth_feature()`. It consumes the current C4 gradient descriptor and payload
without recalculating gradients or reading NetCDF. Absolute, positive and
negative polarity are explicit. Local ranking includes contiguous and point
brackets but excludes gapped secants; `support = "all"` may return only a
truthfully labelled gapped candidate. Scale-aware ties remain ambiguous,
effective-zero absolute profiles remain flat, and incomplete profiles may
return only an observed candidate. The table retains C4 midpoint, canonical
depth, source pair, spacing, gap and signed units; spacing/2 is a localization
scale, not uncertainty. No C5 finding was opened; B7-001 remains
PARTIALLY-CLOSED, Gate B remains SATISFIED, and 0.3.0-C remains IN PROGRESS.

## C6 variable-aware transition-layer diagnostics

C6 approves and implements DEC-035 with the sole new export
`transition_layer()`. Exact preserved source CF `standard_name`, followed by a
bounded compatible-unit check, establishes temperature or salinity identity;
variable names and `long_name` never do. A thermocline candidate is the
strongest eligible negative temperature gradient, while a halocline candidate
is the strongest eligible absolute salinity gradient. Both compose C4/C5,
remain unthresholded candidates, and retain basis, sign, support gaps, ties,
incompleteness and resolution limits. Direct C1/C2/C3 products and gradients
derived from C1/C3 remain excluded. Oxycline is deferred to C7 because gradient
sign alone cannot establish the upper or lower branch around an oxygen minimum.
Gate B remains SATISFIED, B7-001 remains PARTIALLY-CLOSED, and 0.3.0-C remains
IN PROGRESS.

## C7 branch-aware oxygen-profile diagnostics

C7 approves and implements DEC-036 with the sole new export
`oxygen_boundary()`, while preserving the `transition_layer()` signature and
adding `upper_oxycline`/`lower_oxycline` values. Preserved CF oxygen identity
and bounded same-family units gate all results. A complete-profile minimum
resolves the core before C4/C5 gradients are ranked on physical upper or lower
branches. Threshold zones require an explicit user criterion; point crossings,
cell-mean brackets, gaps, and open edges retain different evidence strength.
The governed WOA23 January oxygen fixture certifies a 47-cell 0–1000 m profile
subset and one-read deferred I/O. No density conversion, universal ODZ
threshold, saturation, AOU, smoothing, or width is added. Gate B remains
SATISFIED, B7-001 remains PARTIALLY-CLOSED, and 0.3.0-C remains IN PROGRESS.

## C8 mixed-layer depth and density architecture

C8 approves and implements DEC-037 through the sole new export
`mixed_layer_depth()`. The certified runtime is restricted to direct CF
point-temperature profiles and the first absolute temperature departure from a
configurable physical reference (defaults 10 m and 0.2 K/degree-Celsius
interval). Exact/interpolated references and crossings require local adjacent
support; gaps, missing paths, inversions, multiple crossings, open bottoms,
physical storage order, units, provenance, and QA remain explicit. WOA23 cell
means and surface-only OISST remain rejected. A separate TEOS-10 architecture
governs future SA/CT/pressure and existing-density paths without adding a
dependency or calculating density, potential density, pressure, pycnocline, or
N2. Gate B remains SATISFIED, B7-001 remains PARTIALLY-CLOSED, and 0.3.0-C
remains IN PROGRESS.

## C9 TEOS-10 thermodynamic state

C9 approves and implements DEC-038 through the sole new export
`thermodynamic_state()`. Exact preserved CF identities authorize six bounded
SP/SA and in-situ/potential/Conservative Temperature paths. Sea pressure is
either derived with `gsw_p_from_z(-depth_m, latitude)` or supplied by an exact
CF `sea_water_pressure` variable. `gsw` 1.2-0 is optional via `Suggests`; its
75-term funnel gates every complete source state, and the output preserves
full reference-pressure potential density, metadata, Provenance V1, and QA.
Cell means, generic salinity, runtime installation, fallbacks, density MLD,
pycnocline, and N2 remain outside C9. Gate B remains SATISFIED, B7-001 remains
PARTIALLY-CLOSED, and 0.3.0-C remains IN PROGRESS.
