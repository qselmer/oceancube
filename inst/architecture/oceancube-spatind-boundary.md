# Boundary between oceancube and spatind

## Purpose

`oceancube` standardizes, reads, subsets, and geometrically weights
multidimensional ocean data. Its geometric products describe the grid and the
intersection between grid cells and user-supplied features.

`spatind` is the downstream owner of 2-D and 3-D spatial indicators and of
indicator-level inference. The local `spatind` repository was unavailable when
this contract was written, so this document does not claim that any particular
`spatind` function already exists.

## Responsibilities

| Capability | oceancube | spatind | Status |
|---|---|---|---|
| Standardize an ocean cube | Owner | Consumer | Implemented |
| Read/subset cube values | Owner | Consumer | Implemented |
| Cell bounds, areas, thicknesses, and volumes | Owner | Consumer | Implemented |
| Polygon-cell fractions and effective areas/volumes | Owner | Consumer | Implemented |
| Join values to geometric weights | Supplies stable keys | Chooses analysis join | Contract defined |
| Calculate 2-D/3-D spatial indicators | Does not calculate | Owner | Outside this milestone |
| Infer trends, regimes, and uncertainty for indicators | Does not calculate | Owner | Outside this milestone |

`cube_mask()` classifies cell centres and may mask cube values. In contrast,
`cube_polygon_weights()` measures feature-specific polygon-cell intersection,
does not union features, and never reads or changes scientific cube values.

## Objects exchanged

The upstream object is an `ocean_cube`. The exchange object is a plain,
self-contained data frame returned by `cube_polygon_weights()`. It does not
retain the cube, its backend, scientific arrays, or `sf` geometries.

The minimum 2-D columns are:

- `feature_id`, `feature_order`;
- `longitude_index`, `latitude_index`, `cell_index`;
- `longitude`, `latitude`;
- `lon_min`, `lon_max`, `lat_min`, `lat_max`;
- `cell_area_m2`, `overlap_area_m2`, `fraction_cell_covered`,
  `effective_area_m2`;
- `polygon_area_m2`, `intersected_grid_area_m2`,
  `fraction_polygon_covered_by_grid`.

The 3-D format adds:

- `depth_index`, `depth`, `depth_min`, `depth_max`;
- `layer_thickness_m`, `cell_volume_m3`, `effective_volume_m3`.

The index columns are the stable merge keys. Downstream value tables should
join by `longitude_index`, `latitude_index`, and, in 3-D, `depth_index`.
`feature_order` disambiguates duplicate feature identifiers without altering
their original values.

## Units and geometry

Horizontal bounds and polygon coordinates use geographic degrees in EPSG:4326.
Areas use square metres. Vertical bounds must declare metres or kilometres and
are never inferred from depth centres. Thicknesses exported in weight tables use
metres; volumes use cubic metres. Fractions are dimensionless and constrained to
`[0, 1]`.

All cell areas and polygon intersections use the same `sf` + s2 geodesic engine.
Only finite, strictly monotonic, one-dimensional rectilinear horizontal axes are
accepted. Curvilinear, UGRID, duplicate, non-monotonic, and antimeridian cases
require a future explicit contract.

## Provenance

Geometry matrices and arrays record units, method, CRS, and bounds source.
Weight tables additionally record dimension, feature count, feature coverage,
intersection count, zero-row policy, vertical source, and intended downstream
consumer. These attributes describe geometric production; they are not
indicator results.

## Permitted and prohibited functions

Permitted public geometry functions in `oceancube` are
`cube_cell_area()`, `cube_layer_thickness()`, `cube_cell_volume()`, and
`cube_polygon_weights()`. Existing read/subset functions may prepare the value
table separately.

Functions that summarize polygon values or calculate centres of gravity,
occupied area/volume, Gini, inertia, spatial trends, or other ecological
indicators are prohibited in this package boundary. Such operations belong to
the downstream indicator layer. No nonexistent `spatind` API is prescribed
here.

## 2-D workflow

1. Validate an `ocean_cube` and its rectilinear horizontal coordinates.
2. Resolve or infer horizontal cell bounds.
3. Calculate geodesic cell areas and polygon-cell intersections.
4. Produce a keyed 2-D weight table.
5. Read/subset values separately and join them by horizontal indices.
6. Pass the joined table to a user-selected downstream `spatind` workflow.

## 3-D workflow

1. Complete the 2-D geometry workflow.
2. Supply explicit, unit-bearing vertical bounds.
3. Calculate layer thickness and cell volume.
4. Expand each feature-cell intersection in cube depth order.
5. Produce a keyed 3-D table with effective volume.
6. Join values by horizontal and depth indices downstream.

The package flow is:

```text
ocean_cube
    ↓
geometry and weights with oceancube
    ↓
values table + weights
    ↓
2-D/3-D indicators with spatind
    ↓
inference, trends, and regimes with spatind
```

## Future decisions

Future milestones may define curvilinear or unstructured grids, antimeridian
splitting, additional vertical units, CF bounds discovery beyond the current
descriptor, versioned exchange schemas, and verified adapters to actual
`spatind` APIs. Those decisions must preserve the rule that geometry is not an
indicator and must be based on an auditable downstream repository/API.
