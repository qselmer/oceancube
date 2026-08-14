# oceancube 0.1.0

## Time and calendar foundation (0.2.0 development)

* Stabilizes `signal_noise()` as the legacy standardized climatological
  anomaly-magnitude helper: the default is `abs(z)` and `signed = TRUE`
  returns the canonical signed z anomaly. The argument is now strictly a
  single non-missing logical, and compact transformation metadata is recorded
  in QA and provenance. Despite its historical name, this is not a general
  signal-to-noise ratio estimator.
* Adds `cube_anomaly()` as the canonical difference and standardized-anomaly
  engine for intact `cube_climatology()` results. It enforces exact recurring
  group, coordinate, variable, unit, calendar, and time-class alignment;
  applies explicit leap-day rules; and returns a memory cube with QA and
  provenance diagnostics.
* Processes lazy NetCDF sources with bounded selective reads. `anom_diff()` and
  `anom_z()` now adapt compatible legacy climatologies and delegate to this
  single scientific engine; `signal_noise()` retains its signed/absolute
  standardized-anomaly definition through the same path.
* Corrects unsafe legacy anomaly alignment: older workflows could accept grids
  whose axes had equal lengths but different coordinate values. Anomalies now
  require exact longitude, latitude, depth, variable order, unit, calendar, and
  source time-class compatibility.
* Adds `cube_climatology()` for recurrent daily, monthly, and DJF/MAM/JJA/SON
  climatologies. It first forms day-year, month-year, or season-year means and
  then gives each valid replicate equal weight, avoiding pooled-observation
  sampling-density bias.
* Returns climatological means in `data` and aligned sample SD in
  `climatology$sd`, with complete canonical Date or UTC POSIXct pseudo-time
  cycles, explicit reference-period clipping, optional replicate counts and
  coverage, and bounded lazy-NetCDF execution.
* Migrates `clim_day()` and `clim_month()` to compatibility wrappers over the
  new core. This is a scientific behavior change: `min_n` now counts valid
  yearly daily replicates, monthly observations are aggregated within year
  before climatology, and `leap = "feb28"` merges February 28 and 29 inside a
  leap year so that the year contributes only one replicate.
* Adds `cube_aggregate_time()` for time-only day, ISO-week, calendar-month,
  DJF/MAM/JJA/SON season, and calendar-year aggregation. Date output remains
  Date; POSIXct output remains UTC POSIXct; internal empty periods are retained.
* Provides controlled mean, sampled-value sum, min, max, and exact median
  reducers with finite-value and `min_n` rules, equal observation weighting,
  irregular-sampling warnings, and optional cell-aligned observation-count
  diagnostics.
* Aggregates lazy NetCDF cubes through selective bounded period/block reads
  instead of materializing the complete multi-period cube.
* Makes `to_month()` a compatibility wrapper over the core for supported
  reducers. Arbitrary functions remain temporarily available through a warned,
  deprecated legacy full-read path; the wrapper also documents its legacy
  POSIXct-to-Date output exception.
* Preserves `POSIXct` instants and sub-day/fractional-second precision instead
  of truncating them to `Date`; stored `POSIXct` coordinates are normalized to
  UTC without changing their epoch values.
* Gives eager and lazy NetCDF readers the same UTC `POSIXct` CF-time decoder,
  including seconds, minutes, hours, days, fractional/negative offsets, and
  offset-bearing origins.
* Defaults missing CF calendar metadata to `standard`, supports the approved
  Gregorian-family scope, and now errors for unsupported non-Gregorian
  calendars rather than reinterpreting them as Gregorian.
* Requires canonical time axes to be unique and strictly increasing. Duplicate
  and unsorted coordinates now error with migration guidance; coordinates and
  aligned data are never sorted or repaired automatically.

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
* Adds `viz.transect()` for static horizontal and distance-by-depth transect
  plots driven exclusively by the stabilized `cube_transect()` data contract.
* Adds `viz.timeseries()` for raw, stable-chronological point series driven by
  selective `cube_extract(mode = "series")` calls, with explicit closed time
  bounds and no aggregation, smoothing, or interpolation.

## Masks

* Adds polygon cell-centre masks and retains the established stock-mask workflow.

## Geometry and weights

* Adds horizontal cell area, layer thickness, cell volume, and sparse polygon weights for rectilinear grids.

## Climatology and anomalies

* Provides canonical daily, monthly, and seasonal difference or standardized
  anomalies, while preserving legacy anomaly wrappers, vertical summaries,
  and event linkage through the backend interface.

## Documentation

* Expands the README, reference manuals, educational scripts, citation metadata, architecture boundary, and release vignettes.

## Known limitations

* NetCDF access is read-only and local. Writing, OPeNDAP, THREDDS, Zarr, cloud stores, parallel reading, and chunk pipelines are not implemented.
* Grids must be rectilinear. Curvilinear and unstructured grids, interpolation, regridding, and general antimeridian handling are outside this release.
* Geometry features require the optional `sf` package. Spatial indicators and inference belong to the separate `spatind` package.
