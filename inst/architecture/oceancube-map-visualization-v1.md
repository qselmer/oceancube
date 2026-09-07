# oceancube map visualization contract v1

Status: **D2B COMPLETE / CERTIFIED LOCALLY**. Maintainer `qselmer` approved the
exact seven governed artifacts on 2026-09-07. DEC-045 freezes this bounded map
and scientific-scale contract; remote certification remains separate.

## Scientific product and prepared state

A map is an exact variable/time/depth selection represented as `MAP_LAYER` in
`oceancube_viz_data` schema 1.0.0. Its scientific table, coordinates, units,
selection, provenance and QA are fixed before rendering. Style is a renderer
hint, not a new scientific product or prepared kind.

`FIELD` directly displays stored cells. `CONTOUR` and `FIELD_CONTOUR` may derive
isoline paths only after an explicit style request. Those paths are classified
`DISPLAY_CONTOUR_GEOMETRY`; they never replace observations, enter prepared
data, alter provenance, or claim an interpolated scientific field. D2B performs
no regridding, smoothing, gap filling, spatial/temporal averaging, or hidden
scientific interpolation.

`FILLED_CONTOUR` is architecturally recognized but runtime-deferred. D2B cannot
yet prove continuous-band scale and missing-support truthfulness for every
supported grid, so the reserved request errors explicitly.

## Missingness and support geometry

Stored finite values receive scientific colour. Stored `NA` remains `NA` with
explicit renderer behaviour and is never equated with zero, minimum, land, or
absence. Contour construction receives the same NA mask and cannot write values
into gaps. Deterministic NA-hole tests inspect the resulting path pieces.

D2B certifies `STORED_CENTRES`. A minimum-positive-spacing rectangle is a
`DISPLAY_ONLY` footprint derived from centres; it is neither source nor CF cell
bounds. `EXPLICIT_CELL_BOUNDS` map runtime remains deferred.

## Longitude and coastlines

`SOURCE` is the compatibility default. `NEG180_180` and `ZERO_360` create a
renderer-only copy of longitudes; source coordinates, matching, provenance and
QA remain unchanged. A wrap producing an internal jump larger than 180 degrees
fails explicitly because D2B does not split dateline geometry. Supplied
data-frame coastlines follow the same checked wrapping. Non-source display of
`sf` coastlines is deferred rather than transformed implicitly. No coastline,
tile, basemap, bathymetry or network acquisition occurs.

## Scales and palettes

Certified classes are `UNSPECIFIED_CONTINUOUS`, `SEQUENTIAL` and `DIVERGING`.
The first preserves the historical ggplot2 continuous scale exactly.
Sequential uses ggplot2's deterministic viridis option D. Diverging uses the
base-R HCL `Blue-Red 3` endpoints and requires a finite caller-supplied centre
strictly inside the effective range; zero is never assumed. Limits remain
display-only. Cmocean is reference-only, and optional package availability
cannot change palette resolution. Cyclic runtime defaults and categorical maps
are deferred. Rainbow, jet and spectrum are never defaults.

## Deterministic contour levels

User levels must be finite and strictly increasing. Otherwise explicit contour
styles apply `pretty(finite_display_range, n = 7)`, retain interior levels, and
fall back to seven equally spaced interior levels only if needed. Device size
does not participate. The base-R `contourLines()` engine uses stored horizontal
centres; ggplot2 only draws its renderer-only paths.

## Projection and composition

D2B retains geographic `coord_equal` semantics and adds no projection engine.
All public `viz.map()` results remain modifiable ggplots. Map+profile and
map+timeseries are governed development patterns composed with patchwork after
each child public plot exists. There is no `viz.compose()` export and no merged
scientific observation or composition-time source read.

When a child plot is a point-specific extraction, a governed map composition
must visibly mark the exact spatial selection used by that child. The marker is
a renderer annotation derived from the already-known selection metadata; it
does not select again or read the source. Stored `NA` in an ocean field means
missing source-field support and must not be relabelled as land without an
independently certified land mask. A contour-only rendering communicates
isoline geometry, but does not necessarily communicate the complete valid-
support mask.

## Evidence and governance

Synthetic fixtures certify exact styles, explicit centre errors, missingness,
dateline failure, serialization and backend parity. The canonical GODAS 2024
benchmark supplies real pottmp and salt surface maps with its exact binary and
scientific hashes. Gallery candidates are generated offline from an installed
package. Maintainer `qselmer` approved the exact seven final hashes on
2026-09-07; the manifest and
`dev/visualization/d2b/d2b-human-visual-review.csv` freeze that bounded artifact
set. Regeneration creates a new candidate and does not inherit approval.
