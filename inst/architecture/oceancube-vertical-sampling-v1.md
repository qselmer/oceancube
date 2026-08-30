# oceancube vertical sampling v1

Decision: **DEC-032 — APPROVED / IMPLEMENTED C3**.

## Scope and API boundary

`depth_sample(x, depth, method = c("auto", "cell", "linear"))` reconstructs
values only along the certified metric ocean-depth axis. It differs from
`cube_slice()`, which selects exact/nearest stored coordinates, and
`cube_extract()`, which returns stored cells as tables. C3 adds no nearest,
horizontal, temporal, multidimensional, higher-order, parametric, pressure or
height interpolation and never extrapolates.

## Value semantics and automatic resolution

The C2 classifier is the single semantic authority. Under `auto`, exact CF
vertical cell means resolve to `cell` and vertical point values resolve to
`linear`, independently per variable. Cell sum, other, ambiguous and unresolved
semantics fail. Explicit `cell` requires every variable to be a cell mean;
explicit `linear` requires every variable to be a point value. Mixed automatic
plans are valid and preserve the method used for each variable.

## Cell-mean sampling

Cell sampling requires DEC-029 metric depth and DEC-030 valid explicit bounds;
centre-inferred support is prohibited. A target contained by exactly one source
interval returns that cell mean under the `piecewise_constant_cell_mean`
reconstruction. Shared interior boundaries are ambiguous and error. Outer
boundaries belonging to one cell are accepted. Targets in explicit gaps or
outside total support error. A non-finite containing-cell value returns `NA`.

## Linear point interpolation

Point coordinates must be finite, unique and strictly monotonic in either
storage direction. An effective exact match returns the stored value with
`EXACT_POINT` status. Otherwise adjacent physical depths `z1 < z* < z2` use
`w = (z* - z1)/(z2-z1)` and `X(z*) = X(z1) + w[X(z2)-X(z1)]`. Both bracket
values must be finite; otherwise the result is `NA`. Valid explicit bounds that
establish a gap between the bracket points prohibit interpolation. Without
bounds, adjacent point coordinates define the certified baseline bracket.

## Units, tolerance and ordering

Targets use the source coordinate unit. The bounded B7 metre/kilometre subset
is supported without changing field units. Planning uses the common
`.vertical_geometry_tolerance()` for exact points, containment, boundaries and
gap tests; it is too small to bridge real gaps. Source storage may increase or
decrease, while requested targets must be unique and strictly increasing and
become the output order.

## Output geometry and downstream safety

The output is a memory cube shaped lon × lat × target-depth × time × variable.
Depth coordinates equal the request. Source cell bounds are never attached.
Current vertical geometry is `GEOMETRY_NO_BOUNDS` with `BOUNDS_MISSING`, so
`cube_layer_thickness()`, `cube_cell_volume()` and `layer_integral()` cannot
mistake sampled points for physical cells. The legacy bounded `layer_mean()`
path also cannot promote the result to certified geometry.

## CF metadata and provenance

Immutable `cf$source` is preserved. Current state adds the plain-R,
serializable `oceancube_vertical_sampling` 1.0.0 descriptor with requested
depths/unit, requested and per-variable resolved methods, input/output value
semantics, no-extrapolation, gap, boundary, missingness, output-support and
certification fields. Current `standard_name` and `cell_methods` derivation
remain conservative and pending. A stale C2 `vertical_reduction` descriptor is
cleared.

Provenance V1 remains unchanged. The `depth_sample` step records targets,
source coordinates/order, per-variable semantics and methods, per-target cell
or bracket indices, source bounds where relevant, linear weights, policies,
the union read index, output coordinate and shape. One indexed read loads the
union of required depth cells/points after the whole geometry and semantic plan
passes.

## Future boundary

Spline, PCHIP, derivatives, gradients, mixed-cube regridding, density
coordinates, TEOS-10, profile diagnostics and gap-distance controls require a
later independently approved contract.
