# oceancube visualization architecture v1

Decision: **DEC-041 — APPROVED — RENDERER-NEUTRAL OCEAN VISUALIZATION
ARCHITECTURE**.

Status: D1A local architecture contract. It contains no renderer implementation,
new export, dependency, numerical algorithm, or scientific transformation.

## Purpose and scope

Phase D turns already-defined ocean science into inspectable and communicable
views without changing that science. The v1 boundary separates four concerns:

1. scientific products and their provenance;
2. renderer-neutral prepared visualization data or scenes;
3. renderer adapters for static, interactive, animated, and 3-D modes;
4. governed outputs with numerical, object, visual-regression, and human review.

D1A inventories the space and chooses architecture. D1B will contract the
prepared representations. D1A does not implement Hovmöller, T-S, curtain,
communication views, 3-D, animation, vector fields, or regridding.

## Formal taxonomy

`GENERIC` means that the representation follows declared data geometry and does
not interpret ocean physics. `OCEAN_SPECIALIZED` means that the representation
depends on ocean semantics such as positive-down depth, hydrographic state
space, isopycnals, mixed-layer or transition diagnostics, bathymetry, stations,
or cruise geometry. `COMMUNICATION` means that the geometry primarily compresses
or narrates an already-computed scientific product. A view can be both
analytical and communicative, but its declared purpose controls annotations,
context, accessibility companions, and review.

The global inventory groups concepts into generic fields, temporal/change,
ocean-specialized, multivariate/QC, communication, 3-D, animation, future
vector/flow, uncertainty/ensemble, and future large-data families. Visual style
aliases do not each justify an export.

## Current public visualization API

The public prefix is frozen as `viz.`. There is no compatibility defect that
justifies migration to `viz_`, `plot_`, `gg_`, or another namespace.

The five existing functions are retained and may be extended only additively:

- `viz.map(x, variable, time, depth, limits, na.rm, coastline, title,
  subtitle, caption)` selects one stored layer and returns a `ggplot` raster or
  tile with an optional user-supplied coastline.
- `viz.profile(x, variable, longitude, latitude, time, depth, limits, na.rm,
  reverse_depth, points, title, subtitle, caption)` selects one exact stored
  profile with at least two depths.
- `viz.section(x, variable, section, time, longitude, latitude, depth, limits,
  na.rm, reverse_depth, title, subtitle, caption)` selects one longitude-depth
  or latitude-depth plane.
- `viz.transect(x, path, variable, time, depth, lon_col, lat_col, id_col,
  match, tolerance, mode, distance, limits, na.rm, reverse_depth, points,
  title, subtitle, caption)` delegates ordered path matching to
  `cube_transect()` and displays a section or horizontal line.
- `viz.timeseries(x, variable, longitude, latitude, depth, time_from, time_to,
  match, tolerance, limits, na.rm, points, title, subtitle, caption)` displays
  an unaggregated stored series at one cell and depth.

All currently return modifiable `ggplot` objects, use the existing `ggplot2`
Import, preserve selection provenance and QA, and use bounded NetCDF reads.
Depth reversal changes only the display scale. Current limits are static-only
output, default continuous colours without semantic palette classes, minimal
cartography, no interaction/animation/3-D/composition contract, no explicit
raw-versus-field visual class, and no supplied scientific-diagnostic overlays.

## Science before rendering

The renderer **must not decide scientific meaning**. Before an adapter runs,
oceancube must know variable, units, coordinates, depth convention, time and
calendar semantics, value semantics, missingness, sampling support,
source-versus-derived status, projection, palette class, and scientific limits.
Switching renderer must preserve values, masks, support, provenance, and QA.

No future `viz.*` function may silently interpolate, regrid, smooth, fill gaps,
aggregate time or depth, calculate climatology or anomaly, fit trend, calculate
MLD, detect thermocline/halocline/pycnocline/oxycline, calculate density or N2,
derive currents, or choose a scientific threshold. A later API that truly
computes science must be named and certified as a scientific operation, not
hidden in a renderer.

## Candidate renderer-neutral prepared data

D1B should contract an internal `oceancube_viz_data` object with at least:

```text
kind, data, variables, units, coordinates, time, depth,
value_semantics, source_semantics, geometry, projection,
scale, limits, palette_class, support, provenance, QA, renderer_hints
```

`source_semantics` must include a bounded `rendered_from` vocabulary:

```text
RAW_POINTS | GRIDDED_FIELD | MODEL_FIELD | DERIVED_FIELD
```

The class should validate coordinate/value alignment, missingness, support,
limits, scale class, and provenance. It is initially internal; public
inspection can be reconsidered only with D1B evidence. Renderer hints are
non-scientific preferences and cannot override semantics.

## Candidate 3-D scene

D1B should separately contract an internal `oceancube_viz_scene` with:

```text
scene_kind, vertices, faces, coordinates, values, units,
x_semantics, y_semantics, z_semantics, vertical_exaggeration,
mask, decimation, camera_defaults, lighting_hints, provenance, QA
```

Triangulation, isosurface extraction, support masks, and decimation are
preparation operations whose method and effects must be recorded. Camera and
lighting are renderer hints. Vertical exaggeration changes display geometry,
not certified depth. Scene objects are initially internal and adapters may
return `rgl`, `htmlwidget`, or animation objects that remain user-modifiable.

## Raw observations and fields

Following Ocean Data View practice, raw station/track/profile observations are
not equivalent to a gridded, interpolated, model, or derived field. Points must
remain visibly discrete unless an explicit upstream gridding operation creates
a new product with method, support, uncertainty, provenance, and QA. A field
legend and caption must disclose its `rendered_from` class. Raw-versus-gridded
comparison is a governed multipanel recipe, not implicit conversion.

## Time, climatology, anomaly, trend, and events

Raw series visualization does not aggregate or deduplicate. A climatological
cycle consumes `cube_climatology()` output. An anomaly view consumes an
`ocean_anom` produced by `cube_anomaly()` and retains baseline/reference-period
metadata. A trend view consumes `cube_trend()` output. A renderer never invents
a baseline, standardization, trend model, interval, change point, or extreme
event. Supplied lines, ribbons, event locations, and labels identify their
upstream method.

The recommended change-summary figure is a `viz.compose()` candidate/recipe:
anomaly series plus supplied trend, trend map, and anomaly map. It is not a
separate scientific calculation or necessarily a dedicated `viz.change()`
export.

## Vertical and ocean-specialized contracts

Scientific depth remains certified metric depth positive downward. Vertical
plots normally show surface at top and depth increasing downward through an
axis transformation only.

Hovmöller is a first-class `viz.hovmoller()` candidate for time by longitude,
latitude, distance, or depth. Input must already be reduced to exactly those
dimensions; omitted axes are never silently averaged.

`viz.ts()` is a first-class candidate. It distinguishes Practical Salinity with
in-situ temperature from Absolute Salinity with Conservative Temperature.
Isopycnals, depth/time/station colour, and water-mass annotations are supplied
prepared layers. TEOS-10 and density calculations never occur in visualization;
SA-CT uses certified C9 state when needed.

`viz.curtain()` is a first-class distance-by-depth candidate and may later have
a geographic 3-D adapter. Station positions, track distance, bathymetry, gaps,
sampling support, and provenance remain explicit.

Bathymetry is both scientific context and 3-D scene geometry. D3 should first
test whether it is a `viz.map()`/`viz.surface3d()` style; a separate
`viz.bathymetry()` export needs evidence. It is always explicitly supplied with
source, resolution, vertical datum, sign, units, mask, and license.

Mixed-layer, thermocline, halocline, pycnocline, oxycline, oxygen-boundary, and
N2 overlays consume certified Phase-C products. Candidate versus boundary,
support gap, localization span, and statistical uncertainty remain distinct.

## Map and projection contract

The core map stack is owned by oceancube through `ggplot2` plus explicit `sf`
data/scales. `ggOceanMaps` is an adapter/reference rather than the authority for
the public contract. `ggspatial`, `metR`, and label/composition packages are
optional extensions. Rendering never downloads coastline, bathymetry, basemap,
satellite imagery, EEZ, or web tiles. Acquisition is a separate auditable step.

Analysis basemaps prioritize simple offline land/coastline and governed
bathymetry. Satellite imagery and web tiles are communication context with
license, attribution, cache, and network provenance; Google/ESRI/Mapbox tiles
are not scientific dependencies.

Plate Carree/geographic, Mercator, Lambert conformal, equal-area, polar
stereographic, and orthographic projections are inventoried. Source and target
CRS must be explicit. Equal-area should be used for scientific area comparison;
Mercator distortion is disclosed; polar views use explicit hemisphere; an
orthographic globe is a real projection, not a decorative sphere texture.

## Palette and accessibility policy

Palettes are selected by scientific scale class: sequential, diverging, cyclic,
or categorical. A diverging centre is never aesthetic inference; zero is used
only when the product semantics define zero as meaningful. Direction/phase
uses a cyclic scale with explicit origin and wrap. QC/water-mass/regime classes
use categorical encodings without false order.

`cmocean` is the canonical oceanographic palette reference, with
`viridisLite`/`scico` general alternatives and `colorspace` construction and
audit tools. This does not add a D1A dependency. Palettes require perceptual,
colour-vision-deficiency, greyscale, print, dark/light background, and legend
review. Rainbow/jet is never a default; an explicit legacy request must be
labelled non-default.

## Renderer decisions

The evaluated hypothesis is approved with bounded qualifications:

- `ggplot2` remains the canonical static 2-D grammar and existing core Import.
- `ggiraph` is preferred over `ggplotly()` for primary 2-D interaction because
  it preserves the ggplot grammar and targeted tooltip/click/selection layers.
  It remains optional due to compiled/libpng and HTML constraints.
- `plotly` is a secondary interactive 2-D and web/3-D adapter. Its breadth is
  valuable, but ggplot conversion fidelity, dependency cost, and renderer-
  specific scene behavior preclude core authority.
- `gganimate` is the primary ggplot animation grammar. `gifski`/`av` are
  optional output renderers; availability, codecs, frame count, and common
  scale are explicit.
- `rgl` is still the best primary R-native scientific 3-D candidate in 2026:
  it has active releases, mesh/scene primitives, OpenGL, WebGL, and multiple
  exports. It remains a Suggests-level adapter because compiled OpenGL/system
  and headless constraints are hard blockers to a core Import.
- `rayshader` is the optional bathymetry/terrain/cinematic renderer. Its
  rayverse dependency and terrain focus make it unsuitable as the general
  scene authority; `rayrender` is a narrower path-traced adapter.
- `threejs` is a narrow globe/3-D reference; `mapdeck` is deferred for core use
  because token/tile coupling weakens offline reproducibility.
- ParaView, pyParaOcean, VAPOR, PyVista, and VTK inform scene, scale, filters,
  remote/headless, and large-data boundaries but are not package dependencies.

Hard blockers override weighted scores. No renderer dependency is added by
D1A; D2-D5 must prove cross-platform and headless behavior before Suggests or
Imports changes.

## Modes, return objects, and export

Supported architectural modes are `STATIC`, `INTERACTIVE`, `ANIMATED`,
`3D_INTERACTIVE`, and `3D_ANIMATED`; no view promises every mode. Static 2-D
returns a modifiable `ggplot`. Interaction may return an `htmlwidget`; 3-D may
return a renderer scene/widget; animation may return an `oceancube_animation`
candidate holding prepared frames, renderer metadata, and outputs. Prepared
classes remain separable from renderer objects.

Export inventory: PNG/TIFF for raster publication, SVG/PDF for vector
publication, JPEG for lossy communication imagery, HTML for interaction, and
GIF/MP4/frame sequences for animation. Publication review covers 300/600 DPI,
journal dimensions, embedded fonts, colour profile, transparent background,
consistent physical units, and legend placement.

## Composition and API minimization

`patchwork` is the preferred compositor because its ggplot-native grammar,
guide collection, and annotations fit map+zoom, map+profile, map+series,
trend+anomaly, section+profile, and multivariable layouts. `cowplot` and
`gridExtra` are capable references/fallbacks. A public `viz.compose()` is only
approved if a later thin wrapper adds stable provenance, shared-scale, or
gallery guarantees beyond returning ordinary patchwork objects.

The target API stays small: extend the existing five; prioritize
`viz.hovmoller()`, `viz.ts()`, and `viz.curtain()`; evaluate limited
`viz.compose()`, `viz.animate()`, `viz.surface3d()`, and isosurface contracts;
and admit communication candidates only with governed use cases. Raster,
contour, station, bathymetry, diagnostics, and distribution variants are
normally styles or overlays rather than exports.

## Communication views

`viz.stripes()` and `viz.spiral()` are communication-extension candidates for
D4. Both consume already-computed explicit one-dimensional products and retain
baseline metadata. `viz.helix()` is deferred to D5 until scene and accessibility
evidence exists. `viz.globe()` is optional D: first a static true orthographic
projection, then bounded interactive/animated adapters. Every communication
view keeps a numerical or conventional 2-D companion and cannot replace an
analytical diagnostic.

## Visual testing and gallery gate

Certification has three automated layers: (1) scientific data tests on prepared
values, coordinates, support, provenance, and QA; (2) object-contract tests on
classes and renderer metadata; and (3) visual regression tests such as
`vdiffr`. Renderer parity is scientific-data parity, not pixel identity.

A new `viz.*` capability cannot be certified until a deterministic governed
gallery output has been generated and inspected by a named human reviewer.
The repository-only manifest records function, style, renderer, data, variable,
purpose, script, output, dimensions, mode, status, reviewer, and notes. Binary
limits favor compact representative artifacts and on-demand full rendering.

## Performance and large-data boundary

D can certify small/medium surfaces, slices, curtains, and bounded isosurfaces
only after explicit memory estimates, masks, decimation, and headless tests.
Full-volume rendering, large voxels, huge 4-D interaction, and large isosurface
sequences are deferred to 0.5 lazy/chunk/storage/remote-rendering architecture.
No renderer may materialize an unlimited cube or imply that browser/WebGL
capacity removes I/O and memory constraints.

## Phase-E vector boundary

Phase D may describe renderer support for arrows, streamlines, trajectories,
roses, hodographs, progressive vector diagrams, tidal ellipses, 3-D vectors,
and flow seeding. Component locations, grid staggering, alignment,
speed/direction conventions, integration, seeding, and vector calculus belong
to Phase E. No vector implementation is authorized by DEC-041.

## Reference governance

Every external source materially used for architecture, renderer, palette, API,
scientific presentation, or implementation must be registered under
`dev/references/visualization/`. Citations cannot survive only in chat or notes.
The repository stores citation metadata and links, not copied PDFs, article
figures, screenshots, tiles, or unlicensed imagery. User exemplars V01-V09 are
design references, never scientific specifications.

## Future sequence

1. D1B: renderer-neutral data/scene contracts and existing-viz refactor plan.
2. D2: core 2-D, Hovmöller, map styles, and composition.
3. D3: T-S/SA-CT, curtain, bathymetry, and supplied ocean diagnostics.
4. D4: interactive, animation, and bounded communication extensions.
5. D5: small/medium 3-D surfaces, slices, curtains, isosurfaces, and helix.
6. D-EXIT: cross-mode scientific, object, visual, gallery, and human-review
   certification.

Phase D is in progress after D1A; D1B is not started. Phase E and 0.5 remain
outside the authorized scope.
