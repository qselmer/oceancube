# oceancube 0.2.0.9000

## Development

- Certifies the C1-C10 vertical-ocean engine as globally coherent within its
  bounded supported subsets. DEC-040 freezes canonical positive-down metric
  depth, distinct value and diagnostic semantics, explicit gap/missingness
  behavior, the C9-to-C10 TEOS-10 chain, signed N-squared, immutable source CF,
  Provenance V1, and the repository-only hardening-evidence policy. Phase C is
  complete and certified locally; remote exit remains pending.
- Adds `stratification()` and extends `mixed_layer_depth()` plus
  `transition_layer()` with the certified C10 density-MLD, pycnocline-candidate,
  and signed TEOS-10 N-squared paths over C9 state. Temperature and density MLD,
  pycnocline and maximum N-squared remain separate scientific quantities.
- Adds `thermodynamic_state()` as the certified C9 TEOS-10 bridge from exact
  CF Practical/Absolute Salinity and in-situ/potential/Conservative
  Temperature to SA, CT, sea pressure, in-situ density, and reference-pressure
  potential density. The compiled `gsw` dependency is optional via `Suggests`;
  complete source states must pass its 75-term funnel, while cell means,
  generic salinity identities, fallbacks, density MLD, pycnocline, and N2
  remain rejected unless covered by the separately certified C10 contract.
- Adds `mixed_layer_depth()` for the first locally supported absolute
  temperature departure from a configurable near-surface reference. C8 is
  restricted to direct CF point-temperature profiles; gaps, missing paths,
  inversions, open bottoms, provenance, and QA remain explicit. Density-based,
  gradient, and hybrid MLD definitions are deferred to a separately certified
  TEOS-10 thermodynamic engine; the density method is now certified separately
  through C9/C10 state.
- Adds `oxygen_boundary()` and extends `transition_layer()` with branch-aware
  `upper_oxycline` and `lower_oxycline` candidates. Oxygen identity is gated by
  preserved CF `standard_name` and same-family units; the core, gaps,
  cell-mean brackets, explicit user threshold, provenance, and QA remain
  visible. No universal OMZ/ODZ threshold is introduced.
- Opened the development cycle toward oceancube 0.3.0.
- Adds `transition_layer()` as the bounded variable-aware interpretation layer
  above `depth_gradient()` and `depth_feature()`. Preserved source CF
  `standard_name` and compatible units authorize unthresholded thermocline
  (negative temperature-gradient) and halocline (absolute salinity-gradient)
  candidates; gaps, ties, incompleteness, sign, basis, provenance, and QA
  remain explicit.
- Adds `depth_feature()` for conservative strongest-gradient candidate
  detection on certified `depth_gradient()` outputs. Absolute, positive, and
  negative polarity are explicit; local support excludes gapped secants, ties
  remain ambiguous, and incomplete profiles cannot claim a guaranteed global
  maximum.
- Adds `depth_gradient()` for signed adjacent-level vertical secants with
  respect to canonical positive-down depth in metres. Point and certified
  cell-mean semantics remain distinct; midpoint outputs have no layer bounds,
  and explicit support gaps are recorded rather than filled.
- Adds `depth_sample()` for certified metric-depth reconstruction: exact
  source-cell sampling for CF cell means and local linear interpolation for CF
  point values. It rejects explicit gaps, ambiguous interior boundaries and
  extrapolation, and sampled outputs never inherit physical layer bounds.
- Adds `layer_integral()` for certified metric integration of CF vertical cell
  means over explicit depth bounds. It uses canonical metre overlaps, strict
  missingness, symbolic derived units, indexed deferred reads, and never
  substitutes the unrelated CF `depth: sum` cell method.
- Adds exact elapsed UDUNITS month/year decoding and a bounded generic CF
  climatological-time descriptor under current CF metadata. Climatological
  cubes are guarded from ordinary temporal analytics; inconsistent provider
  coverage is rejected without provider-specific repair.

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
