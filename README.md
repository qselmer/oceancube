
# oceancube

`oceancube` represents rectilinear ocean data with one validated
contract and a canonical dimension order: longitude, latitude, depth,
time, and variable. The oceancube 0.2.0 release freezes a 38-export API
for validation, inspection, storage-aware selection and extraction,
visualization, temporal processing, masking, and grid geometry in
reproducible marine workflows.

## Status and installation

The current stable source release is oceancube 0.2.0. The corresponding
annotated tag and GitHub Release are available from the [project
repository](https://github.com/qselmer/oceancube). The package is not
currently distributed through CRAN. Install the repository with:

``` r
# install.packages("remotes")
remotes::install_github("qselmer/oceancube")
```

## The `ocean_cube` contract

An `ocean_cube` has coordinate vectors and a five-dimensional logical
shape `[longitude, latitude, depth, time, variable]`. Surface products
retain a singleton logical depth dimension. Metadata such as units,
source, provenance, quality information, and extents remain aligned with
those axes.

The memory backend stores the array in the object. The internal NetCDF
backend stores a serializable, read-only descriptor and reads requested
blocks from a local file. `cube_collect()` makes either representation
an independent memory cube. `read_nc()` is the public compatibility
reader and currently materializes a local NetCDF file in memory.

## Canonical workflow

Start by validating the complete cube contract and inspecting its
dimensions, coordinates, storage, missingness policy, and provenance.
Validation diagnoses without repairing or reordering scientific data.

`cube_validate()` returns one row per contract check, while
`cube_inspect()` returns a compact structural summary. `cube_slice()`
selects positions or exact/nearest coordinate values; `cube_crop()`
selects closed coordinate ranges. Neither operation interpolates.
`cube_extract()` returns cells as long or wide tables for points,
profiles, time series, or Cartesian selections. `cube_transect()`
extracts ordered horizontal or vertical paths.

The five static visualization helpers return `ggplot` objects without
changing the input cube: `viz.map()` draws one horizontal layer,
`viz.section()` a vertical plane, `viz.profile()` one depth profile,
`viz.transect()` an ordered horizontal or depth transect, and
`viz.timeseries()` one raw point series.

`cube_mask()` applies polygon cell-centre coverage while preserving the
5D shape. `stock_mask()` supports the established stock-oriented mask
workflow. Polygon operations require the optional `sf` package.

The canonical temporal pipeline starts with `cube_aggregate_time()`,
constructs a recurring reference cycle with `cube_climatology()`, and
computes difference or standardized anomalies with `cube_anomaly()`.
`cube_trend()` estimates a descriptive per-cell linear slope against
actual elapsed historical time. A climatology uses recurring pseudo-time
and is therefore not a valid trend input. Grid primitives include
`cube_cell_area()`, `cube_layer_thickness()`, `cube_cell_volume()`, and
`cube_polygon_weights()`.

The development API also provides `layer_integral()` for a deliberately narrow
CF subset: dimensional metric ocean depth, explicit valid bounds, full
geometric coverage, and variables declared as vertical cell means. Integration
uses metre overlap and a piecewise-constant cell-mean assumption. Point values,
pre-accumulated vertical sums, pressure/height conversions, parametric axes,
interpolation, and extrapolation are rejected rather than guessed.

`depth_sample()` extends that bounded vertical foundation without changing
discrete selection: CF cell means are sampled from their explicit containing
cell, while CF point values may use local two-point linear interpolation.
Explicit gaps, shared interior cell boundaries and extrapolation are rejected.
The result has requested point-depth coordinates but no certified layer bounds.

`depth_gradient()` provides the first certified derivative primitive. It
computes signed adjacent-level secants with respect to physical ocean depth in
metres, positive downward, and locates them at source-unit midpoints. Point and
cell-mean inputs remain semantically distinct, irregular spacing is honored,
and support gaps are diagnosed without interpolation. Gradient outputs carry
symbolic per-metre units and no physical layer bounds.

## Minimal example

``` r
library(oceancube)

lon <- c(-80, -79)
lat <- c(-12, -11)
depth <- c(5, 15)
time <- as.Date("2020-01-01") + 0:119
values <- array(seq_len(2 * 2 * 2 * 120), dim = c(2, 2, 2, 120, 1))

x <- ocean_cube(
  lon = lon, lat = lat, depth = depth, time = time,
  data = values, vars = "temperature", units = "degC",
  source = "example"
)

validation <- cube_validate(x)
inspection <- cube_inspect(x, missing = "none")

x_sub <- cube_crop(
  x, longitude = c(-80, -79), latitude = c(-12, -11)
)
map <- viz.map(x_sub, "temperature", time = time[1], depth = 5)

monthly <- cube_aggregate_time(x_sub, by = "month")
clim <- cube_climatology(monthly, by = "month")
anom <- cube_anomaly(monthly, clim, type = "difference")
trend <- cube_trend(monthly)

stopifnot(
  nrow(validation) > 0L,
  inherits(inspection, "ocean_cube_inspection"),
  inherits(map, "ggplot"),
  inherits(anom, "ocean_cube"),
  inherits(trend, "ocean_cube")
)
```

## Compatibility helpers

Established workflows may continue to use `to_month()`, `clim_day()`,
`clim_month()`, `anom_diff()`, `anom_z()`, and `signal_noise()`. New
workflows should prefer the canonical engines above. Despite its
historical name, `signal_noise()` is not a generic signal-to-noise
estimator: it returns the standardized climatological anomaly magnitude,
`abs(z)`, by default, or signed `z` with `signed = TRUE`. These
compatibility helpers are not deprecated as a group.

## Package boundary

`oceancube` owns data-cube representation, local reading, validation,
inspection, selection, extraction, visualization, temporal
transformations, descriptive per-cell slopes, masking, geometry, and
geometric weights. It does not calculate 2D or 3D spatial indicators,
trend significance, or indicator-level inference; those belong to
`spatind`. See the [formal package
boundary](https://qselmer.github.io/oceancube/handbook/11-spatind-boundary.html).

## OceanCube Handbook

The navigable [OceanCube
Handbook](https://qselmer.github.io/oceancube/handbook/) explains the
five-dimensional contract, backends, the current 42-export development API
(the frozen 38-export release plus `cube_open()`, `layer_integral()`,
`depth_sample()`, and `depth_gradient()`), checked
workflows, the `spatind` boundary, troubleshooting, and the project’s
Git release policy. Its executable sources live in
[`handbook/`](handbook/).

## Limitations and roadmap

The current core supports canonical rectilinear cubes and a read-only
local NetCDF backend. Final analytical outputs may materialize in
memory. NetCDF writing, OPeNDAP, THREDDS, Zarr, curvilinear or
unstructured grids, regridding, interpolation, cloud stores, and general
antimeridian handling are outside the current scope. Trend output is
descriptive: Sen/Theil–Sen slopes, Mann–Kendall tests, significance
inference, breakpoints, change points, and regime analysis are not
implemented.

## License and citation

`oceancube` is released under the MIT License. Use
`citation("oceancube")` for the citation generated from installed
package metadata. Repository citation metadata is also available in
`CITATION.cff`.

## Security

Do not store Copernicus credentials in scripts, package files,
repositories, or examples. Use local authentication through the official
Copernicus Marine tooling or environment variables.
