# Mixed-layer depth v1

Decision: **DEC-037 — APPROVED / IMPLEMENTED C8**.

## Certified scope

C8 certifies only the direct point-profile temperature-threshold method exposed
as `mixed_layer_depth(x, method = "temperature_threshold", variable = NULL,
reference_depth_m = 10, threshold = 0.2, support = c("local", "all"))`.
Temperature identity is established only by immutable preserved source CF
`standard_name`: `sea_water_temperature`,
`sea_water_potential_temperature`, or
`sea_water_conservative_temperature`. Variable names and `long_name` are not
semantic evidence. Compatible bounded Kelvin/Celsius units are interval-
equivalent for the departure magnitude; this is not a general thermodynamic
conversion engine.

The source must retain supported `DEPTH_LENGTH` semantics and direct
`VERTICAL_POINT` temperature values. C1 means, C2 integrals, C3 samples, C4
gradients, and all cell-mean source profiles are rejected. In particular, WOA23
`t_an` is a valid negative regression because it is a vertical cell mean; its
cell centres are never reinterpreted as point observations.

## Reference and first crossing

Depth is converted to canonical physical metres, positive downward, before any
decision. The requested reference is a finite non-negative scalar. An observed
point at the reference is used within the governed scale-aware coordinate
tolerance. Otherwise the reference temperature is linearly interpolated only
between two adjacent finite point observations with locally permitted support:

`Tref = T1 + (reference_z - z1) / (z2 - z1) * (T2 - T1)`.

No extrapolation, missing-value bridge, or interpolation through an explicit
gap is allowed. The corresponding statuses are
`REFERENCE_EXACT_POINT`, `REFERENCE_INTERPOLATED_POINT`,
`REFERENCE_GAPPED_BRACKET`, `REFERENCE_DEPTH_OUTSIDE_PROFILE`, and
`REFERENCE_TEMPERATURE_UNRESOLVED`.

The search then proceeds in physical depth order strictly deeper than the
reference. For every usable point, `D(z) = abs(T(z) - Tref)`. The first point
for which `D(z) >= threshold` wins; the strongest gradient, deepest crossing,
largest departure, and later recrossings are irrelevant. Exact departure within
tolerance yields `MLD_EXACT_THRESHOLD_POINT`. A monotonic, finite, locally
supported adjacent bracket is interpolated by:

`fraction = (threshold - D1) / (D2 - D1)`

`z_mld = z1 + fraction * (z2 - z1)`.

The fraction must remain in `[0, 1]` within numerical tolerance. Its sign at
the deeper threshold side records `COOLER_WITH_DEPTH` or
`WARMER_WITH_DEPTH`, so temperature inversions remain valid absolute-departure
crossings.

## Gaps, missingness, and open bottom

Interpolation never crosses missing values, explicit support gaps, or
non-adjacent source levels. Local support stops at the first gap with
`MLD_UNRESOLVED_BEFORE_SUPPORT_GAP`. All-support inspection may preserve an
apparent gapped threshold bracket, but returns
`GAPPED_MLD_THRESHOLD_BRACKET` and no exact depth. A non-finite temperature
before resolution returns `MLD_UNRESOLVED_INCOMPLETE_PATH`; missing values
deeper than an already selected crossing are immaterial. A complete supported
path with no crossing returns `MLD_OPEN_AT_PROFILE_BOTTOM`, meaning the MLD is
deeper than the observed extent—not equal to the deepest observation.

Equivalent increasing/decreasing storage and metre/kilometre encodings produce
the same reference temperature, status, direction, and `mld_depth_m`.

## Output, provenance, and QA

The base data-frame result contains one row per longitude, latitude, and time.
It retains the temperature identity and basis, requested/resolved reference,
threshold, source-unit and canonical MLD depth, crossing direction, geometric
bracket, support relation/gap, path counts, status, and certification status.
Brackets are profile-resolution evidence, not confidence intervals.

Provenance V1 appends `operation = mixed_layer_depth` with the method,
temperature semantics, reference and crossing evidence, threshold, physical
search direction, first-crossing rule, support/missingness policies, status
counts, and output rows. `oceancube_qa$mixed_layer_depth` counts exact and
interpolated resolutions, open bottoms, reference/gap/missing failures, and
temperature inversions.

The scientific default follows de Boyer Montegut et al. (2004), who evaluated
a 10 m reference and 0.2 degree-Celsius temperature-departure criterion on
individual profiles: <https://doi.org/10.1029/2004JC002378>. Both values remain
configurable. Thermoclines are gradient candidates; MLD is a first reference-
departure threshold. Neither substitutes for the other.

Density-threshold, gradient, hybrid/Holte-Talley, pycnocline, and stratification
methods are explicitly deferred.
