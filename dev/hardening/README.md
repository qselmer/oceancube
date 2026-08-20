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
- `fixture-manifest.csv`: governance-first fixture inventory/proposals.
- `findings.csv`: classified gaps and remediation routing.
- `baseline-summary.csv`: compact machine-readable status/environment summary.
- `coverage-a2-by-file.csv`, `coverage-a2-by-function.csv`,
  `coverage-a2-summary.csv`, and `coverage-comparison.csv`: A2 remeasurement and
  the preserved A1-to-A2 comparison.
- `tolerance-policy.csv`: exact versus operation-specific absolute numerical
  contracts used by direct parity tests.
- `a2-summary.csv`: compact executed A2 certification evidence.

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

A1 used `system.time()`, `Rprofmem()` and `object.size()` because they are
available in base R. `microbenchmark`, `profvis` and `lobstr` were inspected;
`bench` is the preferred future routine tool because it combines high-resolution
timing, allocation and garbage-collection metrics, but it was not installed or
added. `Rprof`/profvis are diagnostic profilers, not baseline runners. A reliable
process peak was not available; `Rprofmem` reports allocation events, not RSS.

TINY/SMALL timings are sub-second and too coarse for complexity claims. Over
this tested range, validation/selection/geometry appear approximately constant
or low linear, while temporal engines show higher allocation growth. This is
only empirical scaling evidence. In SMALL, allocation/input ratios were about
52.2x for climatology, 43.0x for trend and 18.0x for anomaly, making them the
first memory-instrumentation candidates. These values are reference observations,
not pass/fail promises and not authority to optimize algorithms.

## Real-data governance

The local Copernicus RDS exists at the historical maintainer path, is 3,554,623
bytes on disk (12,536,024-byte R object), has shape 73x85x18x7x2, variables
`thetao`/`so`, units, source/product metadata and provenance. SHA-256 is recorded
in `fixture-manifest.csv`. Its redistribution permission is **UNKNOWN**, so it
is usable for local maintainer smoke only and is not CI-eligible or committed.

The minimal recommended 0.3-A set is: (1) keep this Copernicus case local until
terms and derivation are recorded; (2) approve one tiny, pinned, redistributable
surface-SST fixture with quality/missingness; and (3) approve one coarse static
bathymetry fixture. Ocean colour and reanalysis remain later candidates. Every
fixture needs source/product identity, terms, retrieval date, derivation script,
checksum, tested contract and explicit CI decision.

## Lazy NetCDF API decision evidence

DEC-018 remains open; no option is implemented.

| Option | Main benefit | Principal cost/risk |
|---|---|---|
| `read_nc(..., lazy = TRUE)` | Smallest visible API addition and familiar reader | Overloads a currently materializing verb; serialization/resource lifetime become mode-dependent |
| `cube_open(...)` | Clear deferred-resource semantics, discoverable lifecycle, good future multi-file direction | New public export and lifecycle contract; must specify identity, serialization, closure and moved files |
| `ocean_cube(source = ...)` | One constructor entry | Overloads an in-memory constructor and hides I/O/lifetime semantics |
| Separate backend abstraction while keeping `read_nc()` materializing | Maximum compatibility and clean backend extensibility | Discoverability depends on the abstraction chosen; may duplicate concepts |

The leading candidate for maintainer decision is `cube_open()` while preserving
`read_nc()` materialization, because the verb makes deferred resource lifetime
explicit. This is evidence, not approval; compatibility, serialization, file
identity, testing and multi-file behavior must be resolved first.

## Provenance audit and proposed schema

Source identity remains observable in all nine audited operation classes, but
no object has a `schema_version`; none consistently records package version or
timestamp; and operation, parameters, parent/source, backend and scientific
method are structurally inconsistent. The proposed minimal schema is:
`schema_version`, `operation`, `parameters`, `parent/source`, `source_identity`,
`package_version`, `backend`, `timestamp`, and `scientific_method`. Schema
implementation remains open and requires a compatibility/migration decision.

## Proposed 0.3.0-A exit criteria

0.3.0-A may be considered complete only when:

1. Every core dual-backend operation has direct parity evidence with an explicit tolerance or justified non-applicability.
2. Critical public functions and below-70% I/O/time helpers have contract-focused tests; no numerical percentage substitutes for this review.
3. Deterministic property/invariance generators cover identity, non-mutation, coordinates, units, time and provenance.
4. There is no open S0 correctness defect.
5. At least one real-data policy and one small CI fixture are approved with license, derivation and checksum.
6. Smoke/standard/stress profiles are reproducible, with NetCDF read accounting and reliable peak-memory instrumentation.
7. DEC-018 has a maintainer-approved public lazy-I/O decision.
8. A versioned provenance schema and backward-compatibility policy are approved and tested.

Current A1 evidence satisfies the baseline/design portion only; it does not
declare 0.3.0-A complete.
