# D2B core map technical evidence

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
offline, and composes already-created public ggplots with patchwork. Its outputs
remain `GENERATED_PENDING_MAINTAINER`; no technical check is a visual approval.
