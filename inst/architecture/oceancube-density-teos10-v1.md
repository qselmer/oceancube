# Density and TEOS-10 architecture v1

Decision: **DEC-037 — ARCHITECTURE APPROVED / RUNTIME DEFERRED C8**.

## Boundary

C8 performs no density, potential-density, pressure, pycnocline,
stratification, buoyancy-frequency, or density-threshold MLD calculation. It
adds no `gsw` or other TEOS-10 dependency. This document freezes the semantic
questions C9 must adjudicate before any thermodynamic runtime exists.

The future engine should preferentially follow the official TEOS-10 state
variables—Absolute Salinity (`SA`), Conservative Temperature (`CT`), and sea
pressure (`p`)—rather than an ad-hoc equation of state. TEOS-10 is the
internationally adopted thermodynamic standard and explicitly distinguishes
Absolute from Practical Salinity. Primary specification:
<https://www.teos-10.org/pubs/TEOS-10_Manual.pdf>.

## Separately validated future input paths

Every path requires exact preserved CF identity, compatible units, aligned
coordinates, finite-domain checks, and an explicit conversion/provenance chain:

1. **SA + CT + p already available.** Validate Absolute Salinity,
   Conservative Temperature, sea pressure, units, grid alignment, and TEOS-10
   funnel/domain constraints before density evaluation.
2. **Practical Salinity + in-situ temperature + pressure.** Convert Practical
   Salinity to Absolute Salinity using the reviewed TEOS-10 path and required
   longitude/latitude/pressure; convert in-situ temperature to the requested
   thermodynamic temperature basis explicitly.
3. **Practical Salinity + potential temperature + pressure.** Validate the
   potential-temperature reference pressure, then perform explicit TEOS-10
   transformations to SA/CT or an independently justified exact route.
4. **Existing certified density variable.** Validate the exact density or
   potential-density `standard_name`, units, reference pressure, value
   semantics, and provenance. Never treat distinct density bases as
   interchangeable.

No path may silently assert `SP = SA`, in-situ temperature = potential
temperature = CT, depth = pressure, or in-situ density = potential density.

## Pressure and geographic position

Pressure must come either from a semantically valid source pressure coordinate
or from a separately certified TEOS-10 depth-to-pressure conversion. The latter
requires canonical physical depth and latitude. Transforming Practical to
Absolute Salinity can additionally require longitude, latitude, and pressure.
Future provenance therefore preserves position, pressure and its origin,
salinity basis and conversion, temperature basis and conversion, algorithm and
library versions, funnel/domain results, and numerical status.

C9 must decide sea versus absolute pressure units and sign, latitude/longitude
validity, missing-position behavior, reference pressure, and the exact
functions used. C8 implements none of these conversions.

## Density products and downstream gates

The engine must distinguish in-situ density from potential density and attach
its reference pressure. Only after numerical parity, platform CI, metadata,
provenance, and semantic validation are certified may downstream work consider:

- a density-threshold MLD using the first potential-density departure from a
  10 m reference (classical candidate magnitude 0.03 kg m-3);
- a pycnocline based on a certified density gradient; or
- stratification and `N2`, with gravity, pressure/depth, and staggering
  conventions explicitly governed.

Hybrid/Holte-Talley profile algorithms remain a separate future methodology.
None of these downstream diagnostics is implemented merely because a density
value becomes available.

## Dependency decision required in C9

Before adoption, C9 independently reviews package availability, license,
public API stability, TEOS-10/GSW version, numerical parity against official
reference values, supported platforms, CI behavior, installation burden, and
whether the dependency is required, suggested, vendored, or otherwise isolated.
No dependency-field change is authorized by DEC-037/C8.
