# Welcome
This tracked consolidated compatibility surface summarizes the **oceancube 0.2.0 core**. It is neither authoritative nor reproducibly generated; the Quarto chapter, function-card, and table sources listed in `_quarto.yml` are authoritative.

- Canonical contract: `longitude × latitude × depth × time × variable`
- Backends: in-memory and read-only local NetCDF
- Current 0.2.0 API: 38 exports
- Experimental in 0.x: `cube_transect()`, `cube_polygon_weights()`, and `cube_trend()`

Start with @sec-introduction, then use the workflow and function-reference chapters.
# Introduction {#sec-introduction}
`oceancube` gives ocean observations one validated five-dimensional shape while allowing storage to remain in memory or in a local NetCDF file. It solves a recurring problem: scientific operations should not depend on how values happen to be stored.

The package constructs, reads, validates, inspects, selects, extracts,
visualizes, masks, aggregates, computes climatologies, anomalies, and
descriptive per-cell temporal slopes, and describes geometry. It deliberately
does **not** calculate spatial indicators, significance, or structural change;
those responsibilities belong to downstream analysis such as `spatind`.

## Design goals
Reproducibility, explicit dimensions, read-only external data access, small composable operations, serialisable provenance, and stable outputs.
# Mental model
The logical cube always has five axes, including singleton axes. A surface field uses one depth position (often `NA`) rather than dropping depth. Variables form a logical fifth axis even when a NetCDF stores them as separate physical variables.

```text
ocean_cube
├── coordinates: longitude, latitude, depth, time, variable
├── metadata: units, extents, provenance, mask, QA
└── backend: memory | netcdf
```

A coordinate is a scientific value; an index is its integer position. Logical order is canonical; physical NetCDF order may differ and is translated by the backend. Missing values remain `NA`; units and provenance travel with derived cubes.
# The ocean_cube object
Create a memory cube from a five-dimensional array. Validation checks axis lengths, names, order, time values, units, and backend invariants.

```r
library(oceancube)
x <- ocean_cube(lon=c(-80,-79), lat=-12, depth=0,
  time=as.Date(c('2020-01-01','2020-02-01')), vars='temperature',
  data=array(1:4, dim=c(2,1,1,2,1)))
x
summary(x)
```

`cube_collect()` turns a lazy selection into an independent memory-backed cube. Serialisation preserves descriptors and provenance; a NetCDF descriptor still requires its unchanged source file until collected.
# Data access
`cm_setup()`, `cm_connect()`, and `download_nc()` coordinate optional Copernicus acquisition without storing credentials. `read_nc()` opens a **local, read-only** NetCDF descriptor. No OPeNDAP, THREDDS, cloud store, or write support is claimed.

A NetCDF backend validates file identity, maps physical axes to the canonical contract, and reads only requested blocks when possible. `cube_collect()` explicitly materialises values.
# Selection and extraction
| Function | Spatial selection | Temporal selection | Output |
|---|---|---|---|
| `cube_slice()` | discrete Cartesian product | discrete | cube |
| `cube_crop()` | rectangular intervals | interval | cube |
| `cube_extract()` | Cartesian product | discrete | table |
| `cube_transect()` | ordered spatial pairs | one time | table |
| `link_events()` | one match per row | one match per row | enriched table |

They are not interchangeable: slice/crop preserve cube semantics, extract/transect produce analytical tables, and event linkage preserves the original event rows.
# Masks and spatial context
`cube_crop()` reduces axes without changing retained values. `cube_mask()` preserves every axis and converts cells outside polygons to `NA` using cell-centre semantics. `cube_polygon_weights()` does not read cube values: it returns geometry intersections and fractions.

| Operation | Preserves all axes | Modifies values | Returns cube | Semantics |
|---|---:|---:|---:|---|
| crop | no | no | yes | domain reduction |
| mask | yes | yes (`NA`) | yes | cell centre |
| polygon weights | n/a | no values read | no | geometric fraction |

`stock_mask()`, `crop_stock()`, and `coast_dist()` preserve established spatial-context workflows.
# Time, climatology, and anomalies
`cube_aggregate_time()`, `cube_climatology()`, and `cube_anomaly()` are the
canonical aggregation, recurrent-reference, and anomaly engines.
`to_month()`, `clim_month()`, `clim_day()`, `anom_diff()`, and `anom_z()` remain
compatibility helpers. `signal_noise()` returns standardized climatological
anomaly magnitude (`abs(z)`) by default or signed `z` on request; it is not a
general signal-to-noise ratio. `annual_index()` and `layer_mean()` provide
annual and vertical summaries.

`cube_trend()` computes descriptive per-cell OLS slopes against actual elapsed
Date or UTC POSIXct time. It accepts raw, aggregated, and anomaly cubes, rejects
recurrent climatology pseudo-time, and provides no Sen/Mann--Kendall testing,
inference, change-point, or regime analysis.

Always inspect time coverage, missingness, sample counts, and units before interpreting climatology products.
# Grid geometry and weights
For rectilinear grids, `cube_cell_area()` returns horizontal area, `cube_layer_thickness()` derives vertical thickness, and `cube_cell_volume()` combines both. `layer_integral()` integrates certified CF vertical cell means over explicit metric-depth bounds using metre overlap and strict missingness. `depth_sample()` reconstructs values at requested metric depths using cell containment for cell means or local linear interpolation for point values. `depth_gradient()` computes signed adjacent-level secants with respect to positive-down depth in metres, and `depth_feature()` reduces a certified gradient cube to one conservative candidate or diagnostic status per profile and variable. Sampled and gradient outputs have no certified layer bounds. `cube_polygon_weights()` returns sparse intersections suitable for downstream aggregation.

CRS must be known and compatible. Curvilinear/unstructured grids and general antimeridian handling are outside the current core. Polygon weights are experimental during the 0.x series.
# Public function reference
The authoritative, categorized 43-export development index is
[handbook/09-function-reference.qmd](09-function-reference.qmd). It links to
one card per public export and lists canonical engines before compatibility
helpers; this consolidated surface does not duplicate that index.
# Integrated workflows
The authoritative workflows are maintained in
[handbook/10-workflows.qmd](10-workflows.qmd). New users follow
construct/read -> validate/inspect -> select/extract -> visualize -> optional
aggregation -> climatology/anomaly or descriptive trend. Compatibility helpers
are secondary. Recurrent climatology pseudo-time is never a trend input.
# Boundary with spatind
## oceancube produces
Cubes, values, coordinates, indices, masks, areas, thicknesses, volumes,
intersections, weights, units, provenance, and descriptive per-cell temporal
slopes.

## spatind will calculate
2-D and 3-D indicators, centres of gravity, occupied area/volume, dispersion,
inertia, elongation, isotropy, concentration, Gini, patchiness, spatial or
indicator-level trend interpretation, uncertainty, regimes, and inference.

These are architectural responsibilities, not a claim that future `spatind` functions already exist.
# Git and release workflow
The persistent branch is `main`. Future `feature/*`, `fix/*`, `docs/*`, and `release/*` branches are temporary: create one objective per branch, test, merge or open a PR, then delete only after ancestry is verified.

Never force the history of `main`. Use annotated tags for versions and attach GitHub Releases to those tags. ‘Only main remains’ means completed work branches are removed; it does not forbid future branches.
# Troubleshooting
| Problem | Cause | Solution |
|---|---|---|
| array is not 5-D | dropped/missing axis | reshape with all five axes |
| missing axis or out-of-domain index | invalid selector | inspect coordinates and valid positions |
| coordinate/nearest outside tolerance | no valid match | widen tolerance deliberately or correct input |
| NetCDF missing or modified | descriptor source changed | restore the exact file or recreate descriptor |
| variable missing | mapping mismatch | inspect NetCDF variables |
| CRS missing/incompatible | geometry metadata absent | define/transform CRS explicitly |
| invalid geometry | topology error | validate with `sf` |
| antimeridian | unsupported geometry | split/normalise before use |
| depth bounds absent | thickness cannot be inferred safely | supply bounds |
| unknown units | metadata incomplete | assign verified units |
| polygon has no centres | cell-centre mask selects none | inspect polygon/grid |
| curvilinear grid | unsupported by the current core | regrid outside oceancube |
| materialisation too large | full cube exceeds memory | select/crop before collect |
| experimental function | 0.x evolution | pin version and test outputs |
| Quarto/HTML unavailable | toolchain/configuration | run `quarto check` and render locally |
| branch not integrated/deletion rejected | ancestry check fails | merge first; never use `-D` |
| tag already exists | version collision | inspect tag; never overwrite published tag |
# Cheat sheet
The authoritative canonical-first quick reference is
[handbook/14-cheat-sheet.qmd](14-cheat-sheet.qmd). It covers create/read,
validate/inspect, selection, all five visualizations, aggregation, climatology,
anomaly, descriptive trend, geometry, and the secondary compatibility helpers.
