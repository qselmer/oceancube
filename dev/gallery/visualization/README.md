# Governed Phase-D visualization gallery

This repository-only scaffold defines the certification surface for future
visualization implementations. D1A intentionally generates no graphic.

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
