# Governed Phase-D visualization gallery

This repository-only gallery defines the certification surface for Phase-D
visualization implementations. D1A created the governance scaffold; D1B adds
five deterministic, network-free baseline graphics for the existing public
API without adding a visualization capability or intentional appearance
change.

Future layout:

```text
dev/gallery/visualization/
├── README.md
├── manifest.csv
├── scripts/
├── static/
├── interactive/
├── animation/
└── 3d/
```

Every implemented capability must have at least one deterministic, network-free
example built from governed fixtures or a small simulated cube. The manifest
must identify the scientific input, renderer, script, output and human reviewer.
A capability is not certified until the gallery artifact has been visually
inspected. Automated checks have three separate layers: scientific-data tests,
return-object contract tests, and visual regression tests. A screenshot or
`vdiffr` snapshot never substitutes for numerical scientific assertions.

Static publication examples should use PNG and, when appropriate, SVG or PDF;
interactive views use bounded HTML; animation uses a short GIF, compressed MP4,
or representative frames; 3-D uses a deterministic screenshot and optionally a
bounded HTML scene. Record dimensions, DPI, fonts, scale/legend behavior,
missingness, source semantics, projection, and accessibility observations.

Soft repository limits per artifact are 2 MiB for PNG/SVG/PDF, 5 MiB for HTML,
5 MiB for GIF, and 10 MiB for MP4. An exception needs explicit review. Prefer
short previews, a representative frame, compressed video, and generated-on-
demand full galleries over committing large binaries. External tiles, imagery,
coastlines, or bathymetry must never be fetched during gallery rendering.

The D1B scripts load an installed `oceancube` package, construct the governed
`d1b-gallery-deterministic-v1` cube with public APIs, call only public `viz.*`
functions, and save 1800 x 1200 PNG output at 300 dpi. Their manifest status is
`GENERATED_BASELINE_PENDING_MAINTAINER`. Pending review does not block the D1B
internal refactor, but a maintainer must review these baselines before D2 makes
any intentional appearance change.

## D1B baseline

### Map

![D1B map baseline](static/d1b-map.png)

### Profile

![D1B profile baseline](static/d1b-profile.png)

### Section

![D1B section baseline](static/d1b-section.png)

### Transect

![D1B transect baseline](static/d1b-transect.png)

### Time series

![D1B time-series baseline](static/d1b-timeseries.png)

## D2A Hovmöller — COMPLETE / CERTIFIED

These figures were generated from the installed public `viz.hovmoller()` API
and deterministic simulated data. Maintainer `qselmer` visually approved these
exact governed artifacts on 2026-09-06. A grey tile is a stored centre whose value is `NA`; a
blank region has no stored centre and therefore no tile or boundary. Tile
extent is a display-only footprint derived from stored-centre spacing when
authoritative bounds are unavailable. No value is interpolated, averaged,
smoothed, or filled.

### Hovmöller time-depth

![D2A Hovmöller time-depth candidate](static/d2a-hovmoller-time-depth.png)

### Hovmöller time-longitude

![D2A Hovmöller time-longitude candidate](static/d2a-hovmoller-time-longitude.png)

### Hovmöller time-latitude

![D2A Hovmöller time-latitude candidate](static/d2a-hovmoller-time-latitude.png)

The named review is recorded in
`../../visualization/d2a/d2a-human-visual-review.csv` and the completed
checklist. Certification is bounded to these exact hashes and the D2A contract.

## D2B core maps — GENERATED_PENDING_MAINTAINER

These candidates use the installed public API and the exact first GODAS time
and shallowest depth. Source longitude is unchanged in prepared data and is
explicitly wrapped only for display. Contours are renderer-only geometry; grey
cells remain stored `NA`. `FILLED_CONTOUR` is deferred and therefore has no
candidate. The two compositions combine already-created public plots through
patchwork and do not introduce `viz.compose()`.

![D2B pottmp field](static/d2b-map-field-pottmp.png)

![D2B salt field](static/d2b-map-field-salt.png)

![D2B pottmp contours](static/d2b-map-contour-pottmp.png)

![D2B pottmp field and contours](static/d2b-map-field-contour-pottmp.png)

![D2B synthetic diverging map](static/d2b-map-diverging-synthetic.png)

![D2B map and profile composition](static/d2b-compose-map-profile.png)

![D2B map and time-series composition](static/d2b-compose-map-timeseries.png)

Reviewer is intentionally blank. These exact artifacts require named
maintainer inspection before D2B may be marked complete/certified or DEC-045
may be allocated.
