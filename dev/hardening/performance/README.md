# OCEANCUBE 0.3.0-A6 peak-memory and stress certification

## Certification outcome

A6 measured the released 0.2.0 runtime at development SHA
`9613ab09c73434e034a6a16a405eb887773e5e34` without changing an algorithm,
public API, dependency, `R/`, `NAMESPACE`, or `DESCRIPTION`. The required TINY,
SMALL, and MEDIUM tiers completed, the OS monitor calibration passed, repeated
deferred-backend stress passed, and no RED memory defect was observed.

The result is **A1-009 CLOSED** and **A6 COMPLETE**. This does not complete the
0.3.0-A gate: that remains false until the separate `0.3.0-A-EXIT — HARDENING
EXIT AND GATE B READINESS CERTIFICATION` is executed.

This is measurement evidence, not an optimization milestone. A6 did not add
chunked computation, a lazy graph, CF discovery, provider/download changes,
remote I/O, Zarr, multi-file datasets, cache changes, or new dependencies.

## Measurement architecture

Every result row came from exactly one fresh `Rscript --vanilla` process using
an isolated installation of the exact repository SHA. On Windows the wrapper
retained the live x64 R process handle and sampled
`System.Diagnostics.Process.PeakWorkingSet64`; the observed maximum is the
primary peak process RSS metric. `sampled_working_set_peak_bytes` is retained as
a cross-check.

The calibration used three isolated control/allocation pairs. A 128 MiB raw
vector was allocated and one byte per 4096-byte page was touched. Median peak
RSS increased by 138,076,160 bytes, or 1.02875 times the requested 134,217,728
bytes. This was inside the predefined plausible range of 0.70–1.50, so
scientific measurement proceeded.

Each operation has a matched control with the same package load and setup:

- memory operations use an input-cube control;
- anomaly operations use a source-plus-climatology control;
- `cube_open()` and `read_nc()` use the package/no-input control;
- collect and bounded NetCDF operations use a prepared deferred-cube control.

The difference between operation peak and its paired control is reported only
as an **incremental peak estimate**. It is not an allocation theorem. A zero
estimate means the process-lifetime setup peak masked the measured operation,
not that the operation allocated no memory.

`Rprofmem`, `object.size()`, and elapsed time are secondary diagnostics. They do
not replace OS peak RSS and must not be compared as if allocated-byte totals
were resident memory.

## Canonical tiers

| Tier | Shape (lon×lat×depth×time×var) | Logical values | Raw double bytes | Replicates |
|---|---:|---:|---:|---:|
| TINY | 6×4×3×24×2 | 3,456 | 27,648 | 3 |
| SMALL | 12×8×4×48×2 | 36,864 | 294,912 | 3 |
| MEDIUM | 36×24×10×120×3 | 3,110,400 | 24,883,200 | 2 |
| LARGE-LOCAL | 72×48×20×365×4 | 100,915,200 | 807,321,600 | skipped |

MEDIUM uses the contract minimum of two isolated replicates because exact
monthly climatology is the dominant resource case (median 81.95 seconds in
this environment). LARGE-LOCAL was not run because
`OCEANCUBE_RUN_LARGE_STRESS=1` was not set; it is explicitly optional and is
not required to close A1-009.

Memory fixtures use a deterministic formula, monthly calendar axis, multiple
variables, and deterministic missingness. NetCDF fixtures use the same tiers
and were constructed before measured workers, locally and without network
access. Fixture creation is therefore not part of an operation peak.

## MEDIUM peak results

Medians are shown in MiB. Incremental values are matched-control estimates.

| Scenario | Process peak | Incremental estimate | Output | Elapsed s | Class |
|---|---:|---:|---:|---:|---|
| `cube_aggregate_time()` memory | 176.52 | 0.00 | 2.009 | 3.480 | AMBER |
| `cube_climatology()` memory | 176.68 | 0.00 | 4.799 | 81.950 | AMBER |
| `cube_anomaly(..., "difference")` | 272.81 | 71.05 | 23.791 | 0.935 | GREEN |
| `cube_anomaly(..., "z")` | 272.86 | 71.10 | 23.791 | 1.280 | GREEN |
| `cube_trend()` memory | 224.49 | 19.53 | 0.235 | 6.865 | GREEN |
| `cube_open()` | 95.88 | 15.27 | 0.068 | 0.805 | GREEN |
| `read_nc()` | 193.93 | 113.32 | 23.768 | 1.200 | GREEN |
| `cube_collect()` | 255.86 | 148.14 | 23.778 | 1.460 | GREEN |
| `cube_slice()` deferred | 167.35 | 59.63 | 0.046 | 0.830 | AMBER |
| `cube_crop()` deferred | 121.48 | 13.76 | 0.213 | 0.750 | GREEN |
| `cube_extract()` deferred | 167.50 | 59.78 | 0.080 | 1.040 | AMBER |
| `cube_transect()` deferred | 122.20 | 14.48 | 0.043 | 0.285 | GREEN |

`cube_aggregate_time()` and `cube_climatology()` are AMBER because their
MEDIUM operation peak did not exceed the deterministic input-setup peak; the
incremental scaling exponent is therefore unidentifiable. Their absolute
process peaks remain recorded and neither case is RED.

## Deferred versus full materialization

At MEDIUM, `read_nc()` had a 113.32 MiB incremental estimate and
`cube_collect()` 148.14 MiB. All representative bounded workflows were lower:

| Workflow | Logical selected | Physical read | Read amplification | Incremental MiB | Below eager | Below collect |
|---|---:|---:|---:|---:|---:|---:|
| slice | 720 | 1,036,800 | 1,440 | 59.63 | 47.4% | 59.7% |
| crop | 21,645 | 21,645 | 1 | 13.76 | 87.9% | 90.7% |
| extract | 720 | 1,036,800 | 1,440 | 59.78 | 47.2% | 59.6% |
| transect | 48 | 48 | 72 bounding-rectangle ratio | 14.48 | 87.2% | 90.2% |

This quantitatively certifies the deferred benefit. Contiguous crop and paired
transect reads are strongly bounded. The deliberately sparse slice/extract
selectors require a large enclosing NetCDF envelope; they remain materially
below full materialization but are AMBER because their empirical incremental
scaling is uncertain/high for a bounded operation. This is preserved as
finding A6-001 rather than hidden.

## Scaling interpretation

SMALL-to-MEDIUM contains 84.375 times more logical input values. For scenarios
with positive matched-control estimates, the empirical exponent is
`log(peak ratio) / log(84.375)`. It is descriptive, not a required theorem.

| Scenario | Empirical alpha | Class |
|---|---:|---|
| anomaly difference | 0.596 | GREEN |
| anomaly standardized | 0.459 | GREEN |
| trend | 0.218 | GREEN |
| cube_open | 0.122 | GREEN |
| read_nc | 0.512 | GREEN |
| cube_collect | 0.722 | GREEN |
| slice | 1.002 | AMBER |
| crop | 0.430 | GREEN |
| extract | 0.687 | AMBER |
| transect | 0.371 | GREEN |

The A6 evidence policy marks materializing/descriptor operations GREEN through
alpha 1.35 and bounded operations GREEN through alpha 0.50; uncertain or
higher constant/scaling cases up to 1.75 are AMBER, and only apparently
pathological behavior above 1.75 is RED. These thresholds are a local
classification aid, not universal package guarantees. Aggregate and
climatology are AMBER because alpha is not identifiable from a zero
incremental MEDIUM estimate.

## Stress and handles

One SMALL deferred descriptor completed 75 alternating crop, extract, and
transect operations plus 50 create/discard descriptor cycles with 50 expected
selection errors.

- checkpoint RSS: 99,131,392 bytes initially, 100,032,512 bytes finally;
- checkpoint growth: 901,120 bytes;
- maximum checkpoint RSS: 114,302,976 bytes;
- connection count: 3 before and 3 after, delta 0;
- expected descriptor errors: 50/50;
- post-GC NetCDF rename-and-restore probe: PASS;
- classification: GREEN.

The bounded oscillation and successful rename provide no evidence of a retained
cube list, connection leak, or file-handle leak.

## Governed OISST smoke

The committed NOAA/NCEI OISST v2.1 fixture ran offline through `cube_open()`,
crop, extract, and collect. All worker statuses and deterministic correctness
signatures passed. Collect materialized 27,648 logical values with 15,744
finite and 11,904 missing values, preserving the governed fixture's real
packing/missingness behavior. No credential, provider call, or network access
was used.

## Package regression certification

The focused backend-parity, real-data, deferred-I/O, selection, transect, and
temporal test set passed first. The complete unchanged package suite then
passed under the Windows UTF-8 locale: 62 test files, 612 `test_that()` cases,
4,979 expectations, zero failures, zero warnings, and zero skips. The initial
shell-inherited `C.UTF-8` value is unsupported by this R 4.5.1 Windows build and
caused the Unicode-path fixture to fail; setting the native Windows
`English_United States.utf8` locale made that focused test and the complete
suite pass. No package or test code was changed for locale handling.

The source package built successfully from a `.git`-free mechanical source
copy (avoiding Windows worktree-internal path length limits). Standard
`R CMD check --no-manual` finished `Status: OK`: 0 errors, 0 warnings, 0 notes.
The additional `--as-cran` check had 0 errors and 0 warnings plus three
non-code NOTEs for development-version/CITATION incoming checks, unavailable
clock verification, and repository web files. The public export count remains
exactly 39. `R/`, `NAMESPACE`, and `DESCRIPTION` are byte-unchanged from the
A5b starting SHA.

## Evidence files

- `a6-scenarios.csv`: complete calibration, required-tier, OISST, stress, and
  LARGE-LOCAL skip inventory;
- `a6-peak-rss-results.csv`: 140 raw isolated-worker rows, including matched
  controls, calibration pairs, and OISST smoke;
- `a6-scaling-results.csv`: min/median/max peak summaries, secondary metrics,
  SMALL-to-MEDIUM exponents, and GREEN/AMBER/RED classifications;
- `a6-stress-results.csv`: ten periodic RSS checkpoints and handle/error
  evidence;
- `a6-environment.csv`: portable environment and calibration record;
- `a6-runner.R`, `a6-worker.R`, `a6-monitor.ps1`: reproducible harness.

No evidence file records a username, hostname, credential, or absolute
repository path. Generated NetCDF files, isolated package libraries, worker
stdout/stderr, and raw `Rprofmem` traces live under ignored `artifacts/a6/`.

Run from repository root with:

```powershell
& 'C:\path\to\Rscript.exe' --vanilla dev/hardening/performance/a6-runner.R
```

`OCEANCUBE_A6_RESUME=1` reuses complete ignored worker records after an
instrumentation-only interruption. `OCEANCUBE_A6_FORCE_SCENARIO` accepts a
comma-separated scenario list to refresh selected cached measurements. Neither
option changes the package runtime.
