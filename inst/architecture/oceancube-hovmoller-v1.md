# oceancube Hovmöller visualization contract v1

Status: D2A COMPLETE / CERTIFIED. Maintainer `qselmer` approved the three exact
governed gallery artifacts on 2026-09-06. DEC-043 records this bounded visual
contract; remote publication still requires separate authorization.

## Scientific definition and scope

`viz.hovmoller()` visualizes one stored scientific variable on two declared
axes: time and exactly one of longitude, latitude, or depth. Time is always the
horizontal axis and increases from left to right. The selected coordinate is
the vertical axis. Time-by-distance is deferred to the future governed
transect/curtain work because it needs explicit path and support semantics.

The supported public forms are therefore:

- time by longitude;
- time by latitude;
- time by depth.

The function visualizes selected values only. It never calculates an anomaly,
climatology, trend, density, mixed-layer diagnostic, or other scientific
product.

## Public contract

The D2A signature is:

```r
viz.hovmoller(
  x, variable,
  axis = c("longitude", "latitude", "depth"),
  longitude = NULL, latitude = NULL, depth = NULL,
  time_from = NULL, time_to = NULL,
  match = c("exact", "nearest"),
  tolerance = NULL, limits = NULL, na.rm = FALSE,
  reverse_depth = TRUE, title = NULL, subtitle = NULL, caption = NULL
)
```

The return value is a modifiable `ggplot`. The public function validates the
request, prepares an internal renderer-neutral object, validates that object,
and renders it. It does not retain the source cube.

## Bounded axes and selectors

`axis` accepts only `longitude`, `latitude`, or `depth`. Its corresponding
selector must be `NULL`, because that full coordinate is the displayed axis.
Every other non-time dimension must be either singleton or selected explicitly
with a scalar selector. Omitting a remaining non-singleton dimension is an
error.

Exact and nearest selection are delegated to `cube_extract()`. Nearest
selection uses the existing tolerance contract. There is no private fallback
to the first cell and no implicit nearest selection. Inclusive `time_from` and
`time_to` bounds use the existing cube time-selection rules. A valid view must
retain at least two time values and at least two finite displayed-coordinate
values.

## Absolute no-hidden-reduction rule

Preparation never averages, sums, integrates, collapses, smooths, fills,
interpolates, regrids, regularizes, or silently selects an omitted dimension.
A singleton non-display dimension may be dropped because it contains exactly
one scientific value. Any omitted non-singleton dimension raises a governed
error. Prepared values equal the selected stored cube values; display limits
cannot change those values.

## Time semantics

Stored `Date` and `POSIXct` values retain their class, ordering, and inclusive
selection interval. Irregular time positions remain irregular. Preparation
does not sort malformed data for display, resample time, or invent timestamps.
The existing cube invariants remain authoritative.

The current ggplot adapter cannot safely render a supported non-base calendar
without potentially implying Gregorian dates. Such input is rejected with the
governed calendar-operation condition. No coercion or invented Gregorian date
is permitted.

## Depth semantics

Scientific depth remains finite metric depth, positive downward. For a depth
Hovmöller, `reverse_depth = TRUE` changes only the renderer's y scale so the
surface appears at the top. It never negates, rewrites, reorders, or otherwise
changes the prepared scientific depth values.

## Support geometry

Hovmöller support geometry has the bounded vocabulary
`EXPLICIT_CELL_BOUNDS` and `STORED_CENTRES`. `EXPLICIT_CELL_BOUNDS` means that
authoritative current metadata provide actual bounds for both plotted axes and
that the renderer may use those exact bounds as scientific cell extent.
`STORED_CENTRES` means that the scientific product provides coordinate centres
but not authoritative bounds for the complete plotted representation.

D2A implements and certifies only `STORED_CENTRES`. Although current CF
metadata can expose bounds for some individual coordinates, the runtime does
not safely expose authoritative usable bounds for both Hovmöller axes through
the bounded preparation path. `EXPLICIT_CELL_BOUNDS` is therefore
architecturally defined but its runtime path is `DEFERRED_NOT_CERTIFIED_D2A`.
No partial or speculative bounds path is used.

For `STORED_CENTRES`, the renderer uses a deterministic point-centred display
footprint. Prepared metadata declare `semantics = DISPLAY_ONLY`,
`source = DERIVED_FROM_STORED_CENTRES`, and
`method = MINIMUM_POSITIVE_CENTRE_SPACING`. The footprint width and height are
the minimum positive separation of the stored centres on their respective
axes. They are renderer dimensions only: they do not alter coordinates or
values, are not source bounds, never enter CF metadata, and imply neither
interpolation nor certified cell support.

The adapter uses `geom_tile()` with the declared display footprint and a
restrained white boundary. A stored centre with a finite value is a coloured
tile; a stored centre with `NA` is a light-grey tile with the same boundary; a
region with no stored centre has neither tile nor boundary and remains the
light plot background. This makes stored missingness visibly distinct from
absence of a stored centre without filling support.

Irregular time, longitude, latitude, or depth positions remain at their actual
coordinates. A separation larger than the minimum stored spacing remains a
visible blank gap. The adapter does not use `geom_raster()`, midpoint-derived
scientific bounds, half-spacing CF bounds, or regular-grid assumptions.

## Missingness

Missing selected values remain `NA` throughout selection, preparation,
serialization, and rendering. The default adapter uses neutral `grey85` for a
stored missing centre and a subtle white tile boundary against the light
background. `na.rm` controls graphical warning/removal behavior only; it does
not impute, bridge, fill, smooth, or replace values with zero.

## Renderer-neutral prepared state

`.viz_prepare_hovmoller()` returns an `oceancube_viz_data` object with schema
version `1.0.0` and kind `HOVMOLLER`. Its long table contains exactly three
columns: `time`, the declared coordinate, and `value`. Roles explicitly bind
`x`, `y`, `time`, `value`, and the selected coordinate; unused roles are
`NULL`.

The state retains bounded variable metadata, authoritative units when present,
time range, fixed coordinates, depth semantics, backend identity, source
semantics, provenance, QA, support, geometry, scale metadata, and serializable
renderer hints. It contains no ggplot, graphics device, NetCDF handle,
connection, external pointer, full cube payload, or private filesystem path.

## Read and source lifecycle

Memory and NetCDF inputs share the same public selection contract. A
NetCDF-backed request performs one bounded scientific preparation read through
`cube_extract()`. Rendering performs zero NetCDF payload reads. Once prepared,
the source file may become unavailable and the object still validates and
renders because all required bounded values and metadata are in memory.

The prepared data size is bounded by the requested two-dimensional view, not by
the full source cube. Equivalent memory and NetCDF inputs produce equivalent
scientific values, roles, geometry, and scale semantics apart from legitimate
backend provenance identity.

## Scale and palette contract

The internal scale vocabulary is `SEQUENTIAL`, `DIVERGING`, `CYCLIC`,
`CATEGORICAL`, and `UNSPECIFIED_CONTINUOUS`. Classification never comes from a
variable name. Without authoritative product or descriptor semantics, D2A uses
`UNSPECIFIED_CONTINUOUS`.

Scientific scale class and colour palette are separate internal metadata. A
`DIVERGING` scale requires an explicit, scientifically meaningful centre; zero
is never assumed. The D2A default is ggplot2's native viridis continuous scale,
option D. It is deterministic, headless-safe, perceptually ordered, suitable
for common colour-vision deficiencies, and independent of optional packages.
`cmocean` remains a registered scientific reference but is not added as an
Import or Suggest in D2A. Optional-package availability cannot change the
default rendering.

Legend text uses the authoritative variable long name or requested stored name
and units when available; it never title-cases identifiers or manufactures
units. Axis labels likewise use the stored coordinate name and authoritative
units. The display-only formatter maps exactly `degC` to `°C`; stored metadata
remain `degC`, no conversion occurs, and all unknown unit strings are displayed
unchanged. Titles, subtitles, and captions are conservative user-controlled
renderer hints. The public default subtitle remains blank; detailed support
language belongs to the governed gallery and documentation.

## Serialization, provenance, and QA

Prepared Hovmöller objects round-trip through `saveRDS()`/`readRDS()` and
`serialize()`/`unserialize()`, then validate and render without the source.
Selection provenance and QA are preserved as metadata and are not converted
into aesthetics or rewritten by the renderer. Changing display limits,
palette hints, titles, or depth reversal does not change scientific data,
coordinates, provenance, QA, or support.

## Governed gallery and certification gate

The repository gallery contains deterministic, network-free, simulated
time-depth, time-longitude, and time-latitude examples generated from the
installed public API. They exercise temporal, horizontal, and vertical
gradients, irregular spacing, and an intentional missing-data patch without
making physical claims.

Every D2A manifest entry remains `GENERATED_PENDING_MAINTAINER` with a blank
reviewer. The named maintainer must inspect scientific plausibility relative to
the fixture, axis and depth orientation, time order, visible missingness,
legend and units, perceptual ordering, accessibility, clipping, overlap,
publication readability, whitespace, titles, and overall scientific
presentation. Technical validation cannot substitute for this review.

## Current limitations and D2B extension points

D2A is static 2-D only. It does not implement time-by-distance, T-S diagrams,
curtains, composition, stripes, spirals, interaction, animation, 3-D, vectors,
remote context acquisition, or scientific transformations. Palette and scale
classification remain internal. Existing visualization signatures and default
appearance remain unchanged.

After named review of the exact governed PNG files, D2A-REVIEW may resolve any
visual findings, allocate DEC-043 if approved, certify the capability, and
consider push authorization. D2B may extend existing APIs only additively and
only while preserving this no-hidden-science and renderer-neutral contract.
