# Oxygen profile diagnostics v1

## Scope and semantic gate

C7 certifies branch-aware oxycline candidates and explicit oxygen-threshold
zone boundaries for direct source-profile cubes. Oxygen identity comes only
from preserved CF `standard_name` plus a compatible bounded unit. The frozen
vocabulary was checked against the CF Standard Name Table current during C7
(2026-09-01); runtime remains offline. Supported identities are
`moles_of_oxygen_per_unit_mass_in_sea_water`,
`mole_concentration_of_dissolved_molecular_oxygen_in_sea_water`, and
`mass_concentration_of_oxygen_in_sea_water`. Saturation, AOU, variable names,
and `long_name` are not identity evidence.

The distinct amount-per-mass, amount-per-volume, and mass-per-volume families
allow exact multiplicative scaling only within a family. Cross-family
conversion is rejected because it would require density, molecular mass, or
standard-state assumptions. Initial C7 does not support mL/L.

## Depth, profile core, and branches

All decisions use canonical physical depth in metres, positive downward,
independent of storage order. A complete profile has a finite value at every
selected depth. Its core is the scale-tolerant global oxygen minimum. A unique
minimum has its observed depth; a contiguous equal-minimum plateau has shallow
and deep members and a midpoint representative; disjoint tied minima are
ambiguous. Incomplete and flat profiles do not receive a certified core.
Physical cell bounds remain evidence and are never replaced by core bounds.

The upper branch consists only of C4 gradient pairs at or shallower than the
core's shallow edge; its oxycline candidate is the C5 strongest eligible
negative oxygen gradient. The lower branch starts at the core's deep edge and
uses the strongest eligible positive gradient. This follows the physical
profile convention that oxygen decreases toward a low-oxygen core from above
and increases below it. Local support excludes explicit gaps; all support can
rank a gapped pair but labels it. Ties and missingness retain C5's conservative
statuses. No oxycline width is inferred.

## Explicit threshold zones

`oxygen_boundary()` requires a finite non-negative threshold. There is no
universal OMZ/ODZ threshold: concentration criteria vary among studies, and
both profile shape and concentration matter. The reported zone is the
threshold-qualified connected component containing the resolved core. Other
low-oxygen components are not silently merged.

For point values, exact threshold points are retained and a crossing may be
linearly localized only between valid contiguous adjacent points using
`z = z1 + (T-O1)/(O2-O1) * (z2-z1)`. Cell means yield bracket evidence, never
centre interpolation. Gapped brackets remain inexact, and open profile edges
are not extrapolated. Exact observed thickness is returned only when both
point boundaries are exact or locally interpolated; cell-mean and open-edge
zones have no exact thickness.

## Provenance, QA, and future work

Both diagnostics append Provenance V1 records with the resolved oxygen
identity, quantity family, unit scaling, core policy, support, branch or
threshold evidence, and bounded payload-read diagnostics. QA counts complete,
ambiguous, flat, gapped, open-edge, bracket, and exact/interpolated outcomes.
Nonportable local source paths are reduced to basenames.

Future work may expand oxygen vocabularies, quantify uncertainty, or assess
community-specific ODZ criteria, but must not weaken the semantic, unit,
support, or no-extrapolation gates. MLD, density, TEOS-10, smoothing, second
derivatives, prominence, saturation and AOU remain outside C7.

Scientific motivation is consistent with oxygen-profile literature that
distinguishes an oxygen-decreasing upper oxycline, an oxygen-increasing lower
oxycline, and study-dependent concentration boundaries rather than one global
threshold. CF metadata supplies the machine-readable quantity identity; it
does not itself define an ecosystem-specific OMZ or ODZ.
