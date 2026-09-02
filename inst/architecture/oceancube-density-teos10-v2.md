# TEOS-10 thermodynamic-state architecture v2

Decision: **DEC-038 — APPROVED / IMPLEMENTED C9**.

## Dependency decision

C9 uses the reviewed R package `gsw` as an optional runtime dependency in
`Suggests`, never `Imports`, `Depends`, or `LinkingTo`. The certified minimum
is 1.2-0. Runtime calls fail deterministically when `gsw` is absent or older;
they never install software, access the network, use EOS-80/UNESCO, call
Python, or use vendored GSW-C.

The 2026-09-02 audit found CRAN `gsw` 1.2-0, published 2024-08-19, licensed
`GPL (>= 2) | file LICENSE`, requiring R >= 3.5.0 and native compilation.
CRAN supplied current Windows and macOS binaries and reported `OK` checks for
Linux, Windows, macOS arm64, and macOS x86_64. A Windows R 4.5.1 isolated
binary installation and load passed. The package wraps GSW-C release
3.06-16-0, commit `657216dd4f5ea079b5f0e021a4163e2d26893371`, dated
2022-10-11, and identifies SCOR/IAPSO Working Group 127 as the institutional
algorithm source.

Required APIs are present: `gsw_p_from_z()`, `gsw_SA_from_SP()`,
`gsw_CT_from_t()`, `gsw_CT_from_pt()`, `gsw_rho()`, `gsw_sigma0()`, and
`gsw_infunnel()`. Official package vectors for pressure, SA, CT, rho, sigma0,
and CT-from-pt matched with maximum absolute error `4.55e-13`, below the
cross-platform absolute tolerance `1.5e-8` published by GSW-R for its
reference comparisons.

Primary sources:

- <https://cran.r-project.org/package=gsw>
- <https://github.com/TEOS-10/GSW-R>
- <https://github.com/TEOS-10/GSW-C>
- <https://www.teos-10.org/pubs/TEOS-10_Manual.pdf>

## Certified input subset

`thermodynamic_state()` consumes only a direct source-profile `ocean_cube`
with metric `DEPTH_LENGTH` and exact `VERTICAL_POINT` semantics for every
selected state variable. WOA23 `depth: mean` temperature and salinity remain a
scientific rejection. Surface-only OISST, oxygen-only profiles, reductions,
integrals, sampled profiles, gradients, tables, oxygen outputs, and C8 MLD
tables cannot establish a paired source state.

Only preserved source `standard_name` authorizes a variable. Names,
`long_name`, title, comment, and description never substitute for it.

Supported salinity identities:

- `sea_water_practical_salinity`: PSS-78 SP, exact unit `1`, certified source
  interval `[2, 42]`, converted with `gsw_SA_from_SP(SP,p,lon,lat)`;
- `sea_water_absolute_salinity`: SA, certified interval `[0, 42] g kg-1`, with
  identity `g kg-1`/`g/kg` or exact `kg kg-1` to `g kg-1` scaling by 1000.

Finite negative salinity is rejected before GSW. Generic, reference,
preformed, Cox, and Knudsen salinity standard names are not assigned a scale.

Supported temperature identities:

- `sea_water_temperature`: in-situ ITS-90 temperature, converted with
  `gsw_CT_from_t(SA,t,p)`;
- `sea_water_potential_temperature`: sea-level-reference pt0, converted with
  `gsw_CT_from_pt(SA,pt0)`;
- `sea_water_conservative_temperature`: CT, used directly.

Kelvin is converted as the absolute value `degC = K - 273.15`; governed
Celsius declarations are identity conversions. Temperature bases are never
treated as synonyms.

## Six thermodynamic paths

The certified paths are SA+CT, SP+CT, SA+in-situ t, SP+in-situ t,
SA+pt0, and SP+pt0. Direct SA or CT is not routed through an unnecessary
inverse conversion. Density-to-SA/CT and arbitrary GSW dispatch are excluded.

## Pressure and position

`pressure = NULL` means only `DERIVED_FROM_DEPTH_AND_LATITUDE`. Canonical
physical depth is finite, non-negative metres, positive downward; GSW height
is therefore `z = -canonical_depth_m`. Latitude is finite decimal degrees
north in `[-90,90]`. Longitudes accepted by the cube contract are normalized
deterministically to `[0,360)` degrees east for GSW while output coordinates
retain their exact source encoding.

Explicit pressure requires an exact source variable with `standard_name =
sea_water_pressure`, point semantics, and aligned cube cells. `dbar` is
identity and `Pa` scales by `1e-4`. The result is sea pressure, not absolute
pressure. No automatic search occurs when `pressure = NULL`.

All requested source variables are resolved before `cube_slice()` performs
one bounded multi-variable selection/read. Derived pressure reads only
coordinates and causes no extra scientific NetCDF payload read.

## State, reference pressure, and funnel

The output order is fixed:

1. `absolute_salinity` (`sea_water_absolute_salinity`, `g kg-1`)
2. `conservative_temperature` (`sea_water_conservative_temperature`,
   `degree_Celsius`)
3. `sea_water_pressure` (`sea_water_pressure`, `dbar`)
4. `sea_water_density` (`sea_water_density`, `kg m-3`)
5. `sea_water_potential_density` (`sea_water_potential_density`, `kg m-3`)

In-situ density is `gsw_rho(SA,CT,p)`. Full potential density is
`gsw_rho(SA,CT,reference_pressure_dbar)`; the reference pressure is preserved
explicitly. At zero dbar, runtime and certification cross-check
`rho_ref - 1000` against `gsw_sigma0(SA,CT)`.

Every complete finite `(SA,CT,p)` source state must pass `gsw_infunnel()`.
One failure aborts the operation with `oceancube_teos10_outside_funnel` and a
count; mixed certified/uncertified output is not returned. The funnel gate is
applied at the supplied source pressure, exactly matching the C9 contract.

## Missingness and nonlinear semantics

Missing source values remain missing without imputation. Derived pressure may
remain finite where T/S is missing. Direct SA and CT retain their dependency
independence; conversions requiring SA or source pressure become missing when
those prerequisites are missing. Both densities require a complete source
state. Unrelated coordinates and cells are unchanged.

TEOS-10 state is evaluated from the supplied representative point values.
C9 does not claim that density evaluated from mean T/S equals mean density and
does not accept vertical cell means.

## Metadata, provenance, and QA

`metadata$cf$source` remains immutable. Current derived metadata uses
`oceancube_thermodynamic_state` 1.0.0 and records source identities, bases,
unit conversions, pressure origin/sign, reference pressure, GSW version and C
provenance, functions, funnel policy, source cell-method evidence, output
identities, and certification. Stale reduction/sampling/gradient descriptors
are cleared; valid axes, vertical geometry, chronology, and source evidence
remain.

Provenance V1 appends exactly `operation = thermodynamic_state` with method
`oceancube:teos10_thermodynamic_state`. QA is scalar/bounded and records path,
counts, funnel results, payload reads, source variables, allocation estimate,
reference pressure, sigma0 error, and runtime `gsw` version. No path, username,
host, or duplicated data-sized mask is stored.

## Downstream gate

DEC-038 certifies the state engine only. Density-threshold MLD, pycnocline,
density gradient, N2/buoyancy frequency, Turner angle, density ratio, dynamic
height, geostrophy, spiciness, sound speed, cabbeling, and thermobaricity remain
deferred to C10 or later review.
