# A4R coast-distance reproducibility evidence

This directory contains bounded, deterministic, offline evidence for the
0.3.0-A4R decision. No external coastline or provider data was downloaded.

## External method audit

The comparison ran on 2026-08-26 with `sf` 1.1.1, `s2` 1.1.9, PROJ 9.7.1,
GEOS 3.14.1, and GDAL 3.12.1. The installed `sf::st_distance()` reference
documentation and runtime agree: geographic coordinates use spherical S2
geometry when `sf_use_s2(TRUE)` and ellipsoidal distance through
`st_geod_distance()`/GeographicLib (part of PROJ) when `sf_use_s2(FALSE)`.
These versions are certification evidence only and are deliberately absent
from semantic V1 provenance.

## Method comparison

`a4r-method-comparison.csv` covers equatorial and Peru-like latitudes, zero,
short, medium, and long distances, point, LINESTRING, POLYGON, and multiple
coast features. Among the seven non-zero cases, absolute S2-versus-ellipsoidal
differences were:

- minimum: 0.749708533 m;
- median: 274.856953052 m;
- maximum: 4,760.474656637 m;
- p95: 3,497.242133044 m.

Relative differences were:

- minimum: 0.0000613836482 (0.006138%);
- median: 0.0012619498434 (0.126195%);
- maximum: 0.0025441301603 (0.254413%);
- p95: 0.0021594761909 (0.215948%).

The largest absolute difference occurs in the synthetic 1,871 km ellipsoidal
case. No case showed pathological behavior, sign disagreement, or failure at
zero distance. The methods are measurably different and are not claimed to be
equivalent. For the current purpose—shortest geographic distance from a cube
cell centre to accepted coast geometry—controlled S2 is scientifically
defensible, matches the modern `sf` default, remains consistent with existing
oceancube horizontal geometry, and removes caller-state dependence.

## Canonical A4R contract

`coast_dist()` now executes its distance calculation through the existing
`.with_s2_geometry()` helper. The helper saves the caller's state, enables S2,
and restores the prior state on success and error. Identical inputs therefore
produce identical `dc` matrices and semantic provenance whether the caller
starts with S2 enabled or disabled. The normal pre-A4R S2=TRUE calculation is
binary-identical because its distance call and historical nautical-mile factor
are unchanged.

The canonical scientific method is
`oceancube:s2_coast_distance`, version `1`. Resolved semantic metadata records
S2, a spherical Earth model, shortest geographic distance, EPSG:4326, metres
as the input distance unit, and nautical miles as output. Runtime library
versions and the caller's prior S2 state are not semantic provenance.

LINESTRING and MULTILINESTRING inputs measure shortest distance to their line
geometry. POLYGON and MULTIPOLYGON retain existing `sf` semantics: distance is
to the full polygon geometry, so points inside or on its boundary return zero.
The possible user ambiguity between distance to a polygon and distance only to
its boundary is recorded as non-blocking S3 finding `A4R-001`; A4R does not
change accepted geometry types or public arguments.

`a4r-runtime-results.csv` records the executable implementation, regression,
build, check, and invariant results used for final certification.

The final local suite contains 61 files and 603 cases: 4,903 expectations pass
in 557.2 seconds with zero failures, errors, warnings, or skips. The clean-source
fallback tarball is 2,867,526 bytes with SHA-256
`21B6539858794C25F146BD8836E334D52C72512659CC21CF87DCDA564787A6B9`.
`R CMD check --no-manual` finishes with `Status: OK` and zero errors, warnings,
or notes. The fallback is required only because direct `R CMD build .`
reproduces the known `.git/refs/codex/turn-diffs` copy defect.
