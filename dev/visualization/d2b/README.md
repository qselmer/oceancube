# D2B core map certification evidence

Status: **COMPLETE / CERTIFIED LOCALLY**. Maintainer `qselmer` approved the
exact seven governed D2B artifacts on 2026-09-07. DEC-045 freezes the bounded
runtime, scale, longitude, support, missingness and composition contracts.

D2B extends `viz.map()` additively with explicit field, contour and combined
field/contour styles, deterministic sequential/diverging scales, and
display-only longitude wrapping. It adds no export or dependency. Scientific
values, coordinates, provenance and QA remain in `MAP_LAYER` schema 1.0.0;
contour paths exist only as `DISPLAY_CONTOUR_GEOMETRY` in renderer state.

`FILLED_CONTOUR` is reserved but deliberately deferred because continuous band
scales plus irregular/missing support need a stronger contract. A request fails
explicitly rather than fabricating support. Cyclic defaults, categorical maps,
projection expansion, bathymetry, T-S, curtain, interaction, animation, 3-D and
vector-current graphics remain outside D2B.

The gallery script accepts an isolated installed library, uses only exported
oceancube functions for scientific panels, reads the committed GODAS benchmark
offline, and composes already-created public ggplots with patchwork. The two
revised compositions mark the exact source-grid location used by the paired
profile or time series; the display marker is derived from that authoritative
selection without another source read. Grey means stored `NA` / missing source
field support, not inferred land. A contour-only rendering shows isoline
geometry but does not by itself encode the complete valid-support mask.

The original seven hashes remain recorded as `PRE_REVIEW_BASELINE` in
`d2b-gallery-hash-history.csv`. Changed images are additionally recorded as
`POST_REVIEW_FIX_CANDIDATE`; unchanged images retain their original exact
hashes. Named approval of the final seven-file set is recorded in
`d2b-human-visual-review.csv`; no future regeneration inherits that approval.
