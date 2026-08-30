# oceancube vertical reduction v1

Decision: **DEC-031 — APPROVED / IMPLEMENTED C2**.

## Scope and dependency

This contract governs overlap-weighted vertical means and metric vertical
integrals. Both operations use one `.vertical_reduce()` engine and inherit the
C1 explicit-bounds support and bin-resolution machinery. Certified science
depends on DEC-029 metric ocean depth and DEC-030 explicit support; it does not
infer certified bounds from centres.

## Mean versus integral

For overlap lengths `w_i`, `layer_mean()` computes
`sum(w_i x_i) / sum(w_i)` over finite contributors. Its missing-value policy
renormalizes available positive overlap and its legacy centre-derived path is
unchanged and uncertified.

For certified cell means, `layer_integral()` computes `sum(x_i w_i^m)`, where
`w_i^m` is exact overlap converted to metres. Any non-finite positive-overlap
contributor makes the result missing. It does not renormalize.

## Vertical cell-value semantics

The bounded classifier reads preserved source `cell_methods`, resolves only the
exact current CF vertical source-axis identifier (or its exact basename), and
classifies each selected variable as `VERTICAL_CELL_MEAN`,
`VERTICAL_CELL_SUM`, `VERTICAL_POINT`, `VERTICAL_CELL_OTHER`,
`VERTICAL_CELL_METHOD_AMBIGUOUS`, or `VERTICAL_SEMANTICS_UNRESOLVED`. Missing a
vertical method has CF default point semantics. Only `VERTICAL_CELL_MEAN` is
eligible. Mixed inputs fail atomically and name every incompatible variable.
Substring matches, full CF grammar interpretation, extensive sums, point
interpolation, pressure/height conversion, dimensionless/parametric evaluation,
and provider-specific repair are outside the contract.

## Cell support and approximation

Overlap is `max(0, min(U_i,b)-max(L_i,a))`. Partial-cell contributions use the
explicit `piecewise_constant_cell_mean` assumption: geometry is exact but the
within-cell field representation is an approximation. Full union coverage is
required; partial support and gaps error before a scientific payload read, and
zero coverage returns missing without a read. No gap filling, interpolation,
surface extrapolation, or bottom extrapolation occurs.

## Units and CF meaning

The integration measure is canonical metre length. Metre and kilometre depth
coordinates therefore produce equivalent results. Output units are a bounded
symbolic expression: `<source unit> m`, with dimensionless `1` represented as
`m`. Expressions are `SYMBOLIC_UNNORMALIZED_UNVALIDATED`; no algebra,
simplification, dimensional validation, or unit conversion of the field is
claimed. Missing source units are rejected.

Immutable `cf$source` retains source `standard_name`, units and `cell_methods`.
The current view records an `oceancube_vertical_reduction` descriptor with
input semantics, support, method, measure, assumptions, coverage, missingness,
unit and certification status. Current `standard_name` and CF cell-method
derivation remain pending. In particular, CF `depth: sum` describes a source
cell method and is never fabricated as a substitute for metric integration.
Derived depth centres are requested-bound midpoints and output bounds are the
exact requested intervals.

## Provenance, deferred I/O and column terminology

Provenance V1 is unchanged. `layer_integral` uses method
`oceancube:vertical_metric_integral` and records requested bins, source bounds,
source-unit and metre overlaps, coverage, contributing indices, value
semantics, assumptions, missing policy, units and separate geometric/value
certification. Deferred cubes read the union of contributing depth indices once
after geometry and semantics pass.

“Column integration” means integration only across a fully covered requested
vertical interval of a certified input. WOA23’s gapped standard-depth sampling
does not imply a supported full water column. Inventory language must not claim
general 3-D, area, density, heat-content, salt-content, interpolation, named
diagnostic, static-field, multifile, remote or Zarr support. Those remain future
contract boundaries.
