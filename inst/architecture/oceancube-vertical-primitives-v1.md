# oceancube vertical primitives v1

## Scope

This contract governs vertical support used by `cube_layer_thickness()`,
`cube_cell_volume()`, and `layer_mean()`. It certifies only finite,
one-dimensional dimensional metric ocean depth in metres or kilometres with
explicit valid bounds. Pressure, height, dimensionless and parametric axes,
pressure-to-depth conversion, formula evaluation, interpolation, integration,
and static-field support are outside this contract.

## Explicit-bounds principle

Certified vertical science never infers physical support from coordinate
centres. A vertical-support object records centres, paired lower/upper bounds,
native thickness, unit and metre scale, positive direction, storage order,
bounds source, contiguity, geometry status, support basis, and certification
status. CF bounds have priority when the current CF descriptor declares the
supported metric-depth subset. A caller-supplied geometry argument remains a
user-declared explicit override for the geometry APIs. Manually constructed
cubes enter the explicit algorithm only when bounds, a supported unit, and an
`up`/`down` direction are all declared.

The legacy support object retains the exact historical `.depth_edges()` and
`.depth_weights()` calculation. It is intentionally available for backward
compatibility, emits no routine warning, carries no output bounds, and is never
C-series certified. A CF depth descriptor that claims runtime-supported metric
depth but has missing or malformed bounds errors; it cannot silently fall back.

## Overlap and coverage

For source cell `[L_i, U_i]` and requested layer `[a, b]`, the weight is
`max(0, min(U_i, b) - max(L_i, a))`. This clips partial cells and naturally
combines multiple nonuniform cells. Geometric coverage is the length of the
union of all clipped source intervals divided by `b - a`; it is distinct from
the availability of scientific values.

A scale-aware tolerance of
`8 * sqrt(.Machine$double.eps) * max(1, abs(coordinates and bounds))` is used
for containment, overlap, contiguity, and coverage classification. Full
coverage succeeds and is snapped to one. Partial coverage, including an
internal vertical gap, raises `oceancube_vertical_partial_coverage` before a
payload read. Zero coverage is snapped to zero and returns an all-missing
layer. Gaps are never filled. Bounds may be non-contiguous but may not overlap;
storage may be ascending or descending and is never reordered for data access.

Metres and kilometres are converted explicitly. No other physical conversion
is implied. Requested layer centres are arithmetic midpoints in the input
depth-coordinate unit. Certified output bounds are the exact requested pairs
with unit and positive-direction attributes.

## Operation semantics

`cube_layer_thickness()` consumes the shared explicit support and reports
positive widths in native, metre, or kilometre units without reading scientific
payload. `cube_cell_volume()` multiplies those metre thicknesses by geodesic
s2 cell areas and converts to cubic kilometres only on request.

On the certified `layer_mean()` path, overlap weights and coverage are resolved
before one indexed read of the union of contributing depth cells. Finite values
are renormalized over their available positive weights; an all-missing profile
remains missing. This data-missingness policy never turns incomplete geometry
into complete coverage. On the legacy path, numerical behavior—including its
historical descending-axis behavior—is unchanged and uncertified.

Provenance V1 keeps its schema and method identifier. Resolved parameters record
support and weight bases, requested ranges, source bounds, bounds source,
coverage fractions and policy, units, contributing source cells and centres,
and certification status. For certified CF results, immutable `cf$source`
metadata is unchanged; `cf$current$vertical` truthfully describes the derived
metric centres and exact requested bounds while overall current semantic status
remains derivation-pending for other meaning-changing metadata.

Future vertical integration, gradients, interpolation, parametric evaluation,
and named diagnostics require separate contracts, missingness rules, scientific
validation, and public-API review.
