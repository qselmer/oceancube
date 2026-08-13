# oceancube 0.1.0

## Core architecture

* Defines a validated five-dimensional `ocean_cube` contract ordered as longitude, latitude, depth, time, and variable.
* Keeps physical storage behind a shared backend interface while preserving public results and legacy workflows.

## Backends

* Supports in-memory arrays and serializable descriptors for read-only local NetCDF files.
* Adds controlled block reads, non-contiguous selections, materialization with `cube_collect()`, and explicit file-change diagnostics.

## Selection and extraction

* Adds discrete slices, closed-range crops, point/profile/series extraction, and ordered transects without interpolation.
* Stabilizes `cube_transect()` for the 0.2.0 contract while preserving its
  public signature and ordered extraction semantics: CRS-bearing paths can no
  longer be ignored, matching warnings expose legacy or unbounded nearest
  selection, requested-to-matched Haversine distance is retained, ambiguous
  duplicate axes/selectors are rejected, and validation enters through
  `cube_validate(strict = TRUE)`.

## Masks

* Adds polygon cell-centre masks and retains the established stock-mask workflow.

## Geometry and weights

* Adds horizontal cell area, layer thickness, cell volume, and sparse polygon weights for rectilinear grids.

## Climatology and anomalies

* Preserves monthly and daily climatologies, absolute and standardized anomalies, vertical summaries, and event linkage through the backend interface.

## Documentation

* Expands the README, reference manuals, educational scripts, citation metadata, architecture boundary, and release vignettes.

## Known limitations

* NetCDF access is read-only and local. Writing, OPeNDAP, THREDDS, Zarr, cloud stores, parallel reading, and chunk pipelines are not implemented.
* Grids must be rectilinear. Curvilinear and unstructured grids, interpolation, regridding, and general antimeridian handling are outside this release.
* Geometry features require the optional `sf` package. Spatial indicators and inference belong to the separate `spatind` package.
