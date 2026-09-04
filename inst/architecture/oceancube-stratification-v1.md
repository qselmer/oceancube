# TEOS-10 stratification architecture v1

Decision: **DEC-039 — APPROVED / IMPLEMENTED C10**.

stratification(metric = "N2") accepts only a certified C9 state containing
exact Absolute Salinity, Conservative Temperature, and sea pressure. It uses
only gsw::gsw_Nsquared(SA, CT, p, latitude). No EOS implementation, fallback,
smoothing, interpolation, or conversion to N is provided. Signed N2, including
negative values, is preserved.

Profiles are ordered internally by canonical physical depth positive down.
Pressure must increase strictly between adjacent complete states. Missing
states split finite runs. Local support also splits at GAPPED_SUPPORT; all
support may calculate the pair while retaining its gap classification.

N source depths produce N-1 arithmetic source-coordinate depth midpoints and
canonical metric midpoint metadata. The distinct, location-dependent GSW
p_mid is retained as a full second variable. Output is exactly
buoyancy_frequency_squared with CF standard name
square_of_brunt_vaisala_frequency_in_sea_water and unit s-2, followed by
sea_water_pressure_midpoint with standard name sea_water_pressure and unit
dbar.

The descriptor is oceancube_stratification v1.0.0, method
TEOS10_GSW_NSQUARED, certification CERTIFIED_C10_TEOS10_N2. Geometry is
GEOMETRY_NO_BOUNDS / BOUNDS_MISSING. Provenance V1 and bounded QA retain C9
identity, GSW version/function, support, runs, pressure rule, signed pair
counts, and zero NetCDF payload reads.

Primary authorities: TEOS-10 GSW-R gsw_Nsquared documentation and the CF
Standard Name Table.
