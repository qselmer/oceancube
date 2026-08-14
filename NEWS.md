# oceancube 0.2.0

## Validation, inspection, and visualization

* Adds `cube_validate()` for non-destructive contract diagnostics and
  `cube_inspect()` for compact dimension, coordinate, storage, missingness, and
  provenance summaries.
* Adds `viz.map()`, `viz.section()`, `viz.profile()`, `viz.transect()`, and
  `viz.timeseries()`. They return static `ggplot` objects from explicit cube
  selections without interpolation, smoothing, imputation, or animation.

## Selection and extraction

* Stabilizes the backward-compatible `cube_transect()` contract. CRS-bearing
  paths can no longer be silently ignored, matching diagnostics retain
  requested-to-cell distance, and ambiguous axes or selectors are rejected.

## Time and calendar foundation

* Adds `cube_aggregate_time()` for day, ISO-week, calendar-month,
  meteorological-season, and calendar-year aggregation with finite-value and
  `min_n` policies. Date stays Date and POSIXct stays UTC POSIXct.
* Adds `cube_climatology()` for recurrent daily, monthly, and seasonal cycles.
  Period-year replicates receive equal weight; explicit leap-day policies,
  reference-period clipping, sample SD, and optional coverage diagnostics are
  retained with the recurring pseudo-time cycle.
* Adds `cube_anomaly()` for difference and standardized anomalies. Coordinates,
  variables, units, calendars, and Date/POSIXct source classes must align
  exactly. Non-finite values are masked; zero or non-finite SD produces a
  missing standardized anomaly, while negative finite SD is invalid.
* Adds `cube_trend()` for descriptive per-cell linear slopes against actual
  elapsed historical time. Date and irregular/subdaily UTC POSIXct sampling
  are supported, with output units per year, day, hour, or second. The method
  performs no significance inference, Sen/Theil--Sen slope, Mann--Kendall test,
  breakpoint, change-point, or regime analysis. Recurring climatology
  pseudo-time is rejected.
* Preserves POSIXct instants and fractional-second precision, normalizes stored
  timestamps to UTC without changing instants, supports the approved
  Gregorian-family calendars, and rejects duplicate, unsorted, or unsupported
  time axes instead of silently repairing them.

## Compatibility and corrected behavior

* `to_month()`, `clim_day()`, and `clim_month()` delegate supported workflows
  to the canonical aggregation and climatology engines. Climatology `min_n`
  counts valid period-year replicates, and `leap = "feb28"` no longer
  double-weights leap years.
* `anom_diff()` and `anom_z()` adapt compatible legacy climatologies and
  delegate to `cube_anomaly()`. This corrects legacy acceptance of equal-sized
  but coordinate-, variable-, unit-, calendar-, or time-class-misaligned data.
* `signal_noise()` retains its historical name for compatibility but documents
  its quantity precisely as the standardized climatological anomaly magnitude,
  `abs(z)`, by default. `signed = TRUE` returns signed `z`; it is not a generic
  signal-to-noise ratio estimator.

## Backends and package boundary

* Temporal engines and visualization selections use bounded reads from lazy
  local NetCDF sources rather than materializing the full source. Analytical
  cube outputs are materialized in memory where their contracts require it.
* `oceancube` owns descriptive cube transformations. Spatial indicators,
  inference, uncertainty, and structural/regime analysis remain downstream
  responsibilities such as `spatind`.

## Known limitations

* NetCDF access is read-only and local. Writing, OPeNDAP, THREDDS, Zarr, cloud
  stores, and unstructured or curvilinear-grid analytics are not implemented.

# oceancube 0.1.0

## Core architecture

* Defines a validated five-dimensional `ocean_cube` contract ordered as
  longitude, latitude, depth, time, and variable.
* Keeps physical storage behind a shared backend interface while preserving
  public results and legacy workflows.

## Backends

* Supports in-memory arrays and serializable descriptors for read-only local
  NetCDF files.
* Adds controlled block reads, non-contiguous selections, materialization with
  `cube_collect()`, and explicit file-change diagnostics.

## Selection, masks, and geometry

* Adds discrete slices, closed-range crops, point/profile/series extraction,
  ordered transects, polygon cell-centre masks, horizontal cell area, layer
  thickness, cell volume, and sparse polygon weights for rectilinear grids.

## Climatology and anomalies

* Preserves monthly and daily climatologies, absolute and standardized
  anomalies, vertical summaries, and event linkage through the backend
  interface.

## Known limitations

* NetCDF access is read-only and local. Grids must be rectilinear. Geometry
  features require the optional `sf` package; spatial indicators and inference
  belong to the separate `spatind` package.
