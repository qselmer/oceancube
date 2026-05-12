---
output: github_document
---

# oceancube

`oceancube` is an R package for building reproducible oceanographic data cubes from NetCDF files.
It is designed for marine ecology, fisheries, stock-oriented environmental diagnostics, climatologies,
anomalies, vertical layers, signal-to-noise metrics, and annual indicators.

## Installation

```r
# Development version
# remotes::install_github("qselmer/oceancube")
```

## Minimal workflow

```r
library(oceancube)

lon <- seq(-82, -80, length.out = 3)
lat <- seq(-12, -10, length.out = 4)
depth <- c(0, 10, 20)
time <- seq(as.Date("2000-01-01"), by = "month", length.out = 24)
data <- array(rnorm(3 * 4 * 3 * 24 * 1), dim = c(3, 4, 3, 24, 1))

cube <- ocean_cube(
  lon = lon,
  lat = lat,
  depth = depth,
  time = time,
  data = data,
  vars = "thetao"
)

layer <- layer_mean(cube, depth = c(0, 10, 20))
clim <- clim_month(layer)
anom <- anom_diff(layer, clim)
ind <- annual_index(anom)
```

## Development status

Early development prototype. Not ready for CRAN.

## Security

Do not store Copernicus credentials inside scripts, package files, GitHub repositories, or examples.
Use local authentication through the official Copernicus Marine tooling or environment variables.
