# oceancube vertical gradient v1

Decision: **DEC-033 — APPROVED / IMPLEMENTED C4**.

## Scope

`depth_gradient(x, method = c("auto", "point", "cell"))` is the first bounded
vertical derivative primitive. It computes only signed adjacent-level
first-order secants. It does not detect thermoclines, oxyclines, haloclines,
mixed layers, extrema, curvature, or density-based stratification.

## Canonical derivative coordinate

The internal metric-depth primitive resolves the current B7 vertical
descriptor, source unit, positive direction, scale and order. Source
coordinates are converted once to canonical physical ocean depth `D` in
metres, positive downward. For every storage-adjacent pair,

```text
G[i] = (X[i+1] - X[i]) / (D[i+1] - D[i])
```

This signed denominator makes the result invariant to increasing/decreasing
storage and to metre/kilometre encoding. Output locations are arithmetic
source-unit midpoints and preserve source-pair order. Irregular spacing uses
the actual absolute `spacing_m` per pair. There are no endpoint derivatives.

## Point and cell-mean semantics

Original and certified C3-derived point values produce adjacent point secants.
Original CF cell means and certified C1 overlap-weighted layer means produce
adjacent representative-cell-mean secants, not exact continuous derivatives.
C3 piecewise-constant cell reconstructions, C2 vertical integrals, unsupported
cell methods, manual cubes without metadata, and gradient outputs are rejected.
Current sampling/reduction descriptors take precedence over stale source
`cell_methods`.

## Spacing and support gaps

Valid explicit bounds classify every pair as `CONTIGUOUS_SUPPORT` or
`GAPPED_SUPPORT`; point profiles without bounds use
`POINT_SUPPORT_UNBOUNDED`. `support_gap_m` and `spacing_m` are distinct.
Gapped secants are finite slopes between representative endpoints, not local
observations throughout the gap. No values are interpolated or inserted.
Overlapping or malformed explicit bounds are errors.

## Units and missingness

Each variable requires a present unit. Output units are symbolic per metre;
dimensionless `1` becomes `m-1`, while other strings become
`<source-unit> m-1` with no general UDUNITS claim. A non-finite endpoint makes
only pairs using it `NA`; no other level is searched and missing levels are not
bridged.

## Output, CF and provenance

The output is a memory cube with `n-1` midpoint levels. Midpoints are point
locations with `GEOMETRY_NO_BOUNDS` and `BOUNDS_MISSING`; thickness, volume and
vertical integration therefore reject them. Immutable `cf$source` is retained.
Current state uses the serializable `oceancube_vertical_gradient` 1.0.0
descriptor and clears stale sampling/reduction descriptors. No derivative
`standard_name` or `cell_methods` claim is invented.

Provenance V1 is unchanged. The operation records source/canonical depths,
pair indices, midpoint coordinates, spacing, support relation and gap,
semantics, methods, units, equation, missingness policy, output shape and the
single complete-profile indexed read.

## Future boundary

Feature and transition-layer detection requires a later contract that is
spacing-aware and gap-aware. Smoothing, higher-order differences, second
derivatives, MLD, TEOS-10, density, pressure conversion and parametric
coordinates remain excluded.
