# OCEANCUBE 0.3.0-D1A evidence

D1A defines the global ocean-visualization inventory and renderer-neutral
architecture. It starts from C-EXIT `f221b8b121719a187c373ee0a8d5582e5c46aa0d`
and deliberately changes no runtime source, tests, exports, signatures,
dependencies, or package version.

The evidence set audits all five public `viz.*` functions from repository truth,
registers the literature, institutional systems, R packages, and V01-V09 design
exemplars, inventories more than fifty non-trivial concepts across ten families,
evaluates renderers with 1-5 scores and hard blockers, and freezes map, palette,
specialized-ocean, animation, 3-D, API, gallery, scale, and phase boundaries.

The architecture chooses `ggplot2` for static 2-D; `ggiraph` for preferred
optional interactive 2-D; `gganimate` for optional ggplot animation; `rgl` for
primary optional R-native scientific 3-D; `plotly` for secondary web/3-D; and
`rayshader` for optional bathymetry/cinematic rendering. `cmocean` is the
canonical oceanographic palette reference, `patchwork` the preferred
composition engine, and `vdiffr` the third testing layer. No dependency is
added by D1A.

Run from the repository root:

```r
source("dev/visualization/d1a/reference-validator.R")
source("dev/visualization/d1a/inventory-validator.R")
```

The distributed architecture is
`inst/architecture/oceancube-visualization-v1.md`. All `dev/` evidence and the
gallery scaffold are intentionally excluded from the source package. D1B is not
started and requires remote D1A certification.
