# Density diagnostics architecture v1

Decision: **DEC-039 — APPROVED / IMPLEMENTED C10**.

## Certified density basis

Density diagnostics accept only the current oceancube_thermodynamic_state
descriptor v1.0.0 with certification CERTIFIED_C9_TEOS10_STATE. The exact
variable is sea_water_potential_density (kg m-3) referenced to exactly 0 dbar.
Raw density, in-situ density, name-only identity, other reference pressures,
and equation-of-state fallbacks are rejected.

## Density-threshold mixed layer

The density method preserves the mixed_layer_depth() public signature. An
omitted threshold selects 0.03 kg m-3; an explicit threshold is used exactly.
The reference default is 10 physical metres positive down. The crossing is the
first deeper positive departure rho(z) - rho(reference) at the threshold;
absolute departure is forbidden. Negative inversions are allowed and recorded.
Missing values and support gaps are never interpolated through. No crossing
returns MLD_OPEN_AT_PROFILE_BOTTOM with missing depth. The original temperature
method and output remain unchanged.

## Pycnocline candidate

transition_layer(diagnostic = "pycnocline") selects certified C9 potential
density, calls depth_gradient(), then depth_feature(polarity = "positive").
C4 controls geometry and gaps; C5 controls ranking, tolerance, ties,
missingness, and localization. PYCNOCLINE_GRADIENT_CANDIDATE is an operational
unthresholded candidate, not a universal complete pycnocline.

Primary basis: de Boyer Montégut et al. (2004), JGR Oceans 109, C12003,
DOI 10.1029/2004JC002378.
