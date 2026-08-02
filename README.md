# oceancube

`oceancube` represents rectilinear ocean data with one validated contract and a
canonical dimension order: longitude, latitude, depth, time, and variable. The
package provides storage-aware selection, extraction, masking, temporal
summaries, and grid geometry for reproducible downstream marine analyses.

## Status and installation

Version 0.1.0 is the first public API release. Install a local release tarball
with `install.packages("oceancube_0.1.0.tar.gz", repos = NULL, type = "source")`,
or install the development repository with:

``` r
# install.packages("remotes")
remotes::install_github("qselmer/oceancube")
```

## The `ocean_cube` contract

An `ocean_cube` has coordinate vectors and a five-dimensional logical shape
`[longitude, latitude, depth, time, variable]`. Surface products retain a
singleton logical depth dimension. Metadata such as units, source, provenance,
quality information, and extents remain aligned with those axes.

The memory backend stores the array in the object. The internal NetCDF backend
stores a serializable, read-only descriptor and reads requested blocks from a
local file. `cube_collect()` makes either representation an independent memory
cube. `read_nc()` is the public compatibility reader and currently materializes
a local NetCDF file in memory.

## Working with a cube

`cube_slice()` selects positions or exact/nearest coordinate values;
`cube_crop()` selects closed coordinate ranges. Neither operation interpolates.
`cube_extract()` returns cells as long or wide tables for points, profiles, time
series, or Cartesian selections. `cube_transect()` extracts ordered horizontal
or vertical paths and is experimental in 0.1.0.

`cube_mask()` applies polygon cell-centre coverage while preserving the 5D
shape. `stock_mask()` supports the established stock-oriented mask workflow.
Polygon operations require the optional `sf` package.

`clim_month()` and `clim_day()` create climatologies; `anom_diff()` and
`anom_z()` create absolute and standardized anomalies. Grid primitives include
`cube_cell_area()`, `cube_layer_thickness()`, `cube_cell_volume()`, and the
experimental `cube_polygon_weights()`.

## Minimal example

``` r
library(oceancube)

lon <- c(-80, -79)
lat <- c(-12, -11)
depth <- c(5, 15)
time <- as.Date(c("2020-01-01", "2020-02-01"))
values <- array(seq_len(2 * 2 * 2 * 2), dim = c(2, 2, 2, 2, 1))

x <- ocean_cube(
  lon = lon, lat = lat, depth = depth, time = time,
  data = values, vars = "temperature", units = "degC",
  source = "example"
)

cube_slice(x, longitude = -79, latitude = -11, by = "value")
cube_crop(x, longitude = c(-80, -79), latitude = c(-12, -11))
cube_extract(
  x, longitude = -79, latitude = -11, depth = 5,
  time = time[1], variable = "temperature", by = "value"
)
```

## Package boundary

`oceancube` owns data-cube representation, local reading, selection,
extraction, masking, geometry, and geometric weights. It does not calculate 2D
or 3D spatial indicators or perform indicator-level inference; those belong to
`spatind`. See the
[formal package boundary](inst/architecture/oceancube-spatind-boundary.md).

## OceanCube Handbook

The navigable [OceanCube Handbook](https://qselmer.github.io/oceancube/handbook/)
explains the five-dimensional contract, backends, all 27 public functions,
checked workflows, the `spatind` boundary, troubleshooting, and the project's
Git release policy. Its executable sources live in [`handbook/`](handbook/).

## Limitations and roadmap

Version 0.1.0 supports local, rectilinear data. NetCDF is read-only. Writing,
OPeNDAP, THREDDS, Zarr, curvilinear or unstructured grids, regridding,
interpolation, cloud stores, parallel reading, and chunk pipelines are outside
this release. Antimeridian handling is deliberately limited and errors when a
geometry cannot be treated safely.

## License and citation

`oceancube` is released under the MIT License. Use `citation("oceancube")` for
the citation generated from installed package metadata. Repository citation
metadata is also available in `CITATION.cff`.

## Security

Do not store Copernicus credentials in scripts, package files, repositories, or
examples. Use local authentication through the official Copernicus Marine
tooling or environment variables.
