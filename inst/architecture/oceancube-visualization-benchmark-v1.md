# Canonical visualization benchmark v1

## Purpose and scope

The canonical visualization benchmark gives Phase D one small, realistic,
traceable physical-ocean cube for integration, gallery, and visual-plausibility
work. It supports maps, profiles, sections, Hovmöller views, future T-S work,
and bounded volumetric experiments. It does not define a new public acquisition
API, provider abstraction, scientific derivation engine, interpolation engine,
or renderer.

The canonical identity is:

```text
benchmark ID      = oceancube-viz-benchmark
benchmark version = 1
canonical file    = dev/data/visualization/benchmark/oceancube-viz-benchmark-v1.nc
```

## Real-data provenance

Version 1 is an exact-value subset of the NOAA/NCEP Global Ocean Data
Assimilation System (GODAS) annual monthly 2024 potential-temperature and
salinity files distributed by NOAA PSL. The official product page, annual
OPeNDAP endpoints, access date, request geometry, use terms, file checksum, and
scientific-content fingerprint are recorded in repository manifests.

The selected region represents the northern-central Peruvian Humboldt Current
System. Native source longitude uses the 0-360 convention: 276.5E to 287.5E is
83.5W to 72.5W. The other extents are approximately 19.834S to 3.167S, 5 to
459 m, and the twelve monthly means of 2024.

## Source-selection policy

Selection prioritizes authoritative provenance, coherent temperature and
salinity, four real physical coordinates, a complete non-provisional year,
explicit redistribution terms, credential-free machine access, stable annual
files, and bounded server-side reads. Higher nominal resolution does not
outweigh ambiguous redistribution or private account state. At least three
credible sources are compared in the persistent source-candidate audit before
a benchmark version is adopted.

## Exact-value and no-transformation contract

Benchmark creation preserves decoded finite source values exactly and retains
source missingness. It performs no regridding, interpolation, smoothing,
spatial averaging, temporal averaging, climatology, anomaly, trend, density,
or other scientific derivation. Coordinate values for longitude, latitude,
depth, and time are exact selected source values.

The source coordinate named `level` is structurally named `depth` in the
canonical file without altering values, metre units, positive-down direction,
or axis meaning. The malformed-but-unambiguous GODAS origin spelling
`00:00:0.0` is normalized to the equivalent `00:00:00`; the original string is
retained in namespaced provenance and all numeric source times remain exact.

## Source-grid subsampling semantics

`SOURCE_GRID_SUBSAMPLING` means deterministic retention of every Nth source
longitude and/or latitude. It creates no coordinate, interpolation, average,
or regridded cell. Version 1 does **not** use horizontal source-grid
subsampling: longitude stride is 1 and latitude stride is 1 within the bounded
region. Its 18-level vertical selection is an explicit list of native source
levels, not vertical interpolation.

## Redistribution policy

A binary benchmark may be versioned only when authoritative provider/license
evidence clearly permits subset redistribution. GODAS version 1 qualifies:
the official product page states no usage restrictions and asks for NOAA PSL
acknowledgment; NOAA-created digital media are generally public-domain unless
annotated otherwise. This repository retains attribution, labels the file as a
derived subset, and does not imply NOAA endorsement.

If future permission is unclear, the binary must not be committed. Only the
acquisition recipe, manifests, and obtainable source hashes may be versioned,
with status `BINARY NOT VERSIONED — REDISTRIBUTION NOT CERTIFIED`.

## Repository, size, and distribution

The benchmark lives under `dev/`, remains excluded by `.Rbuildignore`, and is
never a runtime or package-check network dependency. NetCDF-4 lossless
compression is allowed. Ten MiB is preferred and twenty MiB is an absolute
gate. Git LFS is not introduced solely for this benchmark.

## Versioning and replacement

Any change to the source product, time period, geographic domain, variables,
coordinate subset, subsampling, missingness, or numeric content requires a new
benchmark version or an explicit governed replacement decision. Version 1 is
never silently overwritten. Both the container SHA-256 and a deterministic
scientific-content SHA-256 identify it.

## Relationship to synthetic fixtures and Phase D

The real benchmark supplements but does not replace deterministic synthetic
fixtures. Synthetic fixtures remain authoritative for edge cases, controlled
NA patterns, irregular-spacing cases, exact-selection/error semantics, and
hidden-reduction tests. The real benchmark is authoritative for oceanographic
plausibility, visual realism, multidimensional real-data integration, and
gallery evaluation.

Preview images have status `BENCHMARK_PREVIEW_NOT_STYLE_CERTIFIED`; they prove
recognizable data structure but do not certify Phase-D visual style. D-VIZDATA
does not start D2B or add any public API.
