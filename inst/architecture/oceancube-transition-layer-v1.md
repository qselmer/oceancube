# OceanCube transition-layer architecture v1

## Scope

C6 is the first variable-aware interpretation layer above certified C4
gradients and C5 generic features. It supports exactly unthresholded
thermocline-gradient and halocline-gradient candidates. It does not establish
that a physically strong layer exists.

## Generic feature versus interpreted diagnostic

`depth_feature()` owns ranking, effective-zero tolerance, ties, missingness,
completeness, local/all support and gap policy. `transition_layer()` never
reimplements those rules. It supplies a diagnostic-specific polarity and adds
only physical-variable interpretation and truthful diagnostic labels.

## Source identity and variable resolution

Each current variable must resolve uniquely to `metadata$cf$source` by exact
path or unique basename. Eligibility is determined only by the preserved
source `standard_name`; variable name, `long_name`, comment, title, units, and
cell methods never substitute for missing semantic identity. `variable = NULL`
selects exactly one eligible variable and rejects zero or multiple candidates.
An explicit variable cannot override semantic incompatibility.

## Supported standard names and units

Temperature accepts `sea_water_temperature`,
`sea_water_potential_temperature`, and
`sea_water_conservative_temperature`. Bounded compatible units are K, kelvin,
degree_Celsius/degrees_Celsius and degree_C/degrees_C spellings, including the
governed WOA `degrees_celsius`. Offsets cancel in vertical differences, but no
general temperature conversion framework is introduced.

Salinity accepts `sea_water_salinity`, practical, absolute, reference, Cox,
and Knudsen salinity. Generic/practical/Cox/Knudsen accept the bounded
dimensionless forms `1`, `1e-3`, `1e-03`, and `0.001`; absolute/reference
accept bounded `g kg-1` equivalents. No conversion or cross-basis strength
comparison is performed. Preformed salinity is excluded.

## Operational definitions

With canonical ocean depth positive downward, thermocline uses C5
`polarity = "negative"`: the strongest finite eligible negative temperature
gradient. A positive temperature inversion is not an absolute-gradient
fallback. Halocline uses `polarity = "absolute"` and retains whether salinity
increases or decreases with depth. Diagnostic strength is the existing C5
gradient magnitude in the existing gradient unit.

No physical threshold, smoothing, secondary peak, prominence, top, bottom,
width, thickness, interpolation, curvature, or second derivative is added.

## Support and candidate language

Local support admits C5 contiguous and point brackets and excludes explicit
gaps. All-support may select a gapped secant only as a gapped diagnostic
candidate. Incomplete gapped profiles carry both limitations. C6 never
strengthens upstream certification. `localization_half_span_m = spacing_m/2`
is vertical resolution, not statistical uncertainty.

## Input modes and I/O

`SOURCE_PROFILE` requires current metric depth and no current sampling,
reduction, or gradient. It selects exactly one variable before computing C4;
a deferred NetCDF input therefore reads that scientific variable once across
the selected profile extent. `CERTIFIED_GRADIENT` requires a current C4
descriptor whose variable semantic source is the original current/source CF
classifier. It performs no second gradient and no NetCDF payload read.
Gradients derived from C1 layer means or C3 interpolation, and direct C1/C2/C3
products, are excluded from initial diagnostic certification.

## Output, provenance, and QA

The base data frame has one row per longitude, latitude, and time for one
selected variable. It retains useful C5 fields, renames C5 status fields to
`feature_status` and `feature_certification_status`, and records diagnostic,
definition, source standard name, semantic family, quantity basis, input mode,
gradient semantics/method/direction, strength, unit, `threshold_applied =
FALSE`, diagnostic status, and diagnostic certification.

Provenance V1 appends `transition_layer` without a schema change or erased
history. The transition record includes variable identity, unit recognition,
polarity, support, input mode, computed/supplied gradient route, result counts,
and bounded I/O. QA preserves C5 QA and adds the corresponding transition
summary. No canonical C6 record stores a filesystem path, username, or host.

## Deferred oxycline and future C7

Gradient sign alone does not prove whether an oxygen feature is above or below
an oxygen minimum or OMZ core. Upper/lower oxycline therefore requires an
independent C7 contract covering oxygen identity and units, minima/plateaus,
profile branches, concentration/isoline alternatives, missingness, thresholds,
and Peruvian OMZ applicability. MLD and density/TEOS-10 remain separate.
