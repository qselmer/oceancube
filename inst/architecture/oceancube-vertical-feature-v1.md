# oceancube vertical feature detection v1

Decision: **DEC-034 — APPROVED / IMPLEMENTED C5**.

## Scope and gradient-input requirement

`depth_feature(x, polarity, support)` consumes only a current certified
`oceancube_vertical_gradient` 1.0.0 descriptor and its actual C4 scientific
payload. Raw cubes, C1 reductions, C2 integrals and C3 sampling outputs are not
gradient inputs and are rejected. C5 never calls `depth_gradient()` implicitly
and never recalculates a secant. The required C4 result and supported
selections are memory backed, so C5 performs zero NetCDF scientific payload
reads.

## Generic candidate concept and polarity

The primitive identifies at most one strongest observed gradient candidate per
longitude, latitude, time and variable. `absolute` ranks `abs(G)` while
retaining signed `G`; `positive` requires `G` above tolerance; `negative`
requires `G` below negative tolerance. Variable names do not determine
polarity. Candidate does not mean thermocline, oxycline, halocline, pycnocline,
mixed-layer depth or another interpreted transition.

One feature tolerance is computed per profile:

```text
8 * sqrt(.Machine$double.eps) * max(1, abs(finite eligible gradients))
```

It governs effective zero and score equality. Tied maxima remain
`AMBIGUOUS_TIE`; storage order is not a physical tie breaker. An absolute
profile with no gradient above tolerance is `FLAT_PROFILE`.

## Local and all support

C5 consumes C4 support relations directly. `support = "local"` admits
`CONTIGUOUS_SUPPORT` and `POINT_SUPPORT_UNBOUNDED` while excluding
`GAPPED_SUPPORT`. `support = "all"` admits all three, but a gapped winner is
only `GAPPED_SECANT_CANDIDATE`, never a locally resolved transition. No
interpolation, filling, smoothing or continuous-support claim is introduced.

## Feature location and bracket diagnostics

Feature depth is the winning C4 midpoint in source coordinate units.
`feature_depth_m` is the midpoint of the C4 source pair in canonical physical
ocean depth, metres positive downward. C4 source-pair indices provide source
and canonical bracket depths. `spacing_m`, `support_relation` and
`support_gap_m` are copied from C4. `localization_half_span_m = spacing_m / 2`
is a vertical resolution scale, explicitly not statistical uncertainty.

## Missing gradients and completeness

For every profile, `n_support_eligible` counts positions allowed by the support
policy and `n_finite_gradient` counts finite values among them. Their ratio is
`gradient_completeness` when support exists. No finite values produce
`NO_FINITE_GRADIENT`; no local positions produce `NO_LOCAL_SUPPORT`. A unique
winner from an incomplete profile is labelled
`OBSERVED_CANDIDATE_INCOMPLETE_PROFILE` and cannot claim the guaranteed global
maximum. Ties remain ambiguous even when incomplete.

## Candidate status and certification

Complete unique winners distinguish local contiguous, local point-bracket and
gapped-secant candidates. No-feature states distinguish flat profiles,
polarity mismatch, missing gradients, absent local support and ambiguity.
Certification is strongest only for complete profiles and remains explicit
about gapped support.

## Output table

The base data frame has one row per longitude, latitude, time and variable,
ordered longitude fastest, then latitude, time and variable. It retains
coordinate classes and contains per-row source/gradient units, polarity,
support policy, feature midpoint, canonical midpoint, source bracket, signed
gradient and magnitude, spacing, gap, localization scale, completeness,
candidate/tie counts, tolerance, status and certification status. No new S3
class or package-wide metadata system is introduced.

## Provenance and QA

Provenance V1 appends `operation = depth_feature` with the C4 descriptor
version, policies, tolerance/ranking rules, profile dimensions, bounded
selection diagnostics, status counts and output size. QA records bounded
status totals, resolved/incomplete/gapped counts, zero payload reads, memory
sizes and a large-output flag. New fields contain no local paths or host data.

## Future boundary

Variable eligibility, expected polarity, physical-unit requirements and
diagnostic strength belong to later thermocline/oxycline/halocline contracts.
MLD requires a separate choice among temperature, density, gradient or hybrid
definitions and may require an independently governed TEOS-10 architecture.
C5 adds no threshold, smoothing, prominence, width, second derivative,
density, pressure conversion or parametric-coordinate support.
