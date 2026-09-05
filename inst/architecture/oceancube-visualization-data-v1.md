# oceancube renderer-neutral visualization data v1

Status: D1B internal implementation contract. Schema name
`oceancube_viz_data`; schema version `1.0.0`.

## Purpose

The visualization boundary is `ocean_cube` -> certified scientific selection ->
plain prepared data -> renderer adapter. Preparation owns the selected values and
their meaning. Rendering owns only graphical representation. Existing
`viz.map()`, `viz.profile()`, `viz.section()`, `viz.transect()`, and
`viz.timeseries()` remain public, keep their exact signatures, and still return
modifiable `ggplot` objects.

## Schema and invariants

Every object has the required fields `schema_name`, `schema_version`, `kind`,
`data`, `roles`, `variables`, `coordinates`, `selection`, `time`, `depth`,
`source_semantics`, `geometry`, `projection`, `scale`, `support`, `provenance`,
`qa`, and `renderer_hints`. It is a plain named R list with class
`oceancube_viz_data` and contains no NetCDF handle, external pointer, connection,
graphics device, ggplot object, htmlwidget, OpenGL object, or live source cube.
Only the bounded selected table and serializable metadata needed by the view are
retained.

The validator rejects missing fields, non-v1 schemas, unsupported kinds,
non-data-frame data, role references to absent columns, coordinate/value length
mismatch, invalid time or depth metadata, invented scale or source classes,
invalid projection state, malformed provenance or QA, and live or
renderer-specific state.

## Supported D1B kinds

Runtime preparation supports exactly `MAP_LAYER`, `PROFILE`, `SECTION`,
`TRANSECT_SECTION`, `TRANSECT_LINE`, and `TIMESERIES`. Hovmöller, T-S, curtain,
3-D scenes, isosurfaces, animation, interaction, composition, semantic palettes,
and vector fields are not executable D1B kinds.

## Roles, variables, and coordinates

The complete role map is `x`, `y`, `value`, `group`, `time`, `depth`,
`longitude`, `latitude`, and `distance`. Every role is present in the map;
unused roles are `NULL`, and used roles name an existing prepared-data column.
Renderers do not guess column names.

Variable metadata records the requested name and only authoritative available
units, standard name, long name, value semantics, and dataset descriptor
identity. Missing CF metadata remains missing. Coordinate records name their
data column, units when available, stored order, finite range, semantics, and
row-alignment count. Fixed coordinates that are not plot columns remain in the
selection or geometry record.

## Time and depth

Time values keep their `Date` or `POSIXct` class, calendar attribute when
available, stored order, selected interval, and represented range. Preparation
does not aggregate, deduplicate, calculate climatology or anomaly, establish a
baseline, or fit a trend.

Scientific depth values remain the certified physical values, positive down.
`display_reverse` is separate display intent. Reversing the display never
negates, rewrites, or reorders the scientific depth values beyond ordering that
the starting public implementation already performed.

## Source semantics

`source_semantics$rendered_from` is bounded to `RAW_POINTS`, `GRIDDED_FIELD`,
`MODEL_FIELD`, or `DERIVED_FIELD`. A certified derived object class may resolve
`DERIVED_FIELD`. Ordinary `ocean_cube` metadata does not currently distinguish
gridded observations, model output, and other fields authoritatively, so those
objects use `NA_character_` with `classification_status = "UNRESOLVED"`.
Column names and regular grid geometry are never used as source-class
heuristics. Current cube-only visualization functions do not manufacture
`RAW_POINTS` semantics.

## Geometry, projection, and scale

Geometry records explicit role columns and kind-specific state: regular versus
irregular map/section support, fixed section coordinate, requested versus
matched transect distance, and fixed time-series location. Projection is a
metadata contract only. D1B performs no CRS transformation; maps report
`UNKNOWN` unless current authoritative metadata supplies a CRS, and
non-geographic views report `NOT_APPLICABLE`.

The D1B scale classification is `UNSPECIFIED_CONTINUOUS`. Sequential,
diverging, cyclic, and categorical semantics are not inferred from variable
names. Semantic palette resolution remains D2 work. User limits are stored as
requested display-scale limits and never clip or alter prepared scientific
values; the existing squish behavior is applied by the ggplot adapter.

## Support, provenance, and QA

Support records selected row count, remaining missingness, backend identity,
selection status, and applicable transect or nearest-match diagnostics.
Preparation retains the exact selection/transect Provenance V1 and QA objects.
It does not add or rewrite public lineage, and QA is never transformed into an
aesthetic. Prepared state contains no private path beyond any pre-existing
governed source identity permitted by Provenance V1.

## Renderer hints

Hints are serializable, non-scientific state: title, subtitle, caption, points,
coastline context, depth-display reversal, labels, and display limits. Changing
a hint cannot change prepared data, coordinates, variable metadata, support,
provenance, QA, or source classification. A supplied coastline is renderer
context only; it is neither downloaded nor used to mutate the field.

## Serialization and backend independence

Objects round-trip through `saveRDS()`/`readRDS()` and
`serialize()`/`unserialize()`. Equivalent memory and NetCDF selections yield
semantically equivalent prepared state apart from legitimate backend provenance
and QA identity. After a NetCDF selection finishes, rendering uses only the
prepared object and remains possible if a controlled temporary source copy is
removed.

## Prepare/render boundary and ggplot adapter

The internal preparers `.viz_prepare_map()`, `.viz_prepare_profile()`,
`.viz_prepare_section()`, `.viz_prepare_transect()`, and
`.viz_prepare_timeseries()` delegate selection solely to `cube_extract()` or
`cube_transect()`. They do not duplicate nearest-cell matching, time/depth
selection, path matching, distance calculation, or NetCDF subsetting.

`.viz_render_ggplot()` dispatches on the six supported kinds. It accepts only a
validated prepared object, performs zero source or NetCDF reads, and recreates
the starting layers, scales, labels, coordinates, point options, coastline
behavior, depth display, and `oceancube_*` plot attributes. The five public
functions are thin validate/prepare/render orchestrators through their bounded
preparers. No renderer dependency was added; ggplot2 remains the only current
renderer.

## Existing visualization parity and NetCDF lifecycle

D1B parity is certified against an isolated installed package built from
`079b353a552ee941d3132baa3e376c552fa71cc1`, not copied private source. Exact
scientific plot data, built ggplot data, labels, layer and geom classes, scales,
coordinates, warnings, errors, and public attributes are compared. Exact values
use zero tolerance; only already-certified floating-point transect distance may
use its existing representation. NetCDF lifecycle remains one bounded
preparation read followed by zero renderer reads, with no retained cube payload.

## Scene contract deferral

The future `oceancube_viz_scene` schema is frozen architecturally with
`schema_name`, `schema_version`, `scene_kind`, `vertices`, `faces`,
`coordinates`, `values`, `units`, `x_semantics`, `y_semantics`, `z_semantics`,
`vertical_exaggeration`, `mask`, `decimation`, `camera_defaults`,
`lighting_hints`, `provenance`, and `qa`. Scientific geometry is distinct from
camera, lighting, and exaggeration hints. No runtime scene class, rgl, plotly,
rayshader, mesh, isosurface, or 3-D implementation exists in D1B; runtime work is
deferred to D5.

## Gallery baseline and D2 extension points

The repository-only gallery contains five deterministic, network-free images
generated through installed public APIs. They are baseline outputs with no
intentional appearance change and remain pending maintainer visual review.
Pending review does not block this internal refactor, but review is mandatory
before D2 intentionally changes appearance.

D2 may extend the prepared-data schema additively for semantic palettes, map
styles, Hovmöller, and governed 2-D composition. It must preserve v1 scientific
state, source truthfulness, the no-hidden-science rule, existing signatures,
and the prepare/render boundary.
