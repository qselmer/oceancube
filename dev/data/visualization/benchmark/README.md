# oceancube visualization benchmark v1

`oceancube-viz-benchmark-v1.nc` is the canonical real-ocean physical benchmark
for Phase-D visualization integration. It is a 431,246-byte NetCDF-4 file with
lossless deflate level 9. It is repository-only evidence under `dev/` and is
excluded from the package tarball.

It contains exact NOAA/NCEP GODAS monthly source values for 2024:

- `pottmp`: potential temperature in K;
- `salt`: salinity in kg/kg;
- 12 native longitudes from 276.5E to 287.5E (83.5W to 72.5W);
- 51 native latitudes from about 19.834S to 3.167S;
- 18 native levels from 5 m to 459 m, positive down; and
- 12 exact monthly source timestamps from 2024-01-01 to 2024-12-01.

The file is not a provider framework, acquisition API, regridding product,
derived scientific product, or package test fixture. The real benchmark
supplements but does not replace deterministic synthetic fixtures.

## Source and selection

Provider: NOAA Physical Sciences Laboratory / NCEP. Product: NCEP Global Ocean
Data Assimilation System (GODAS). Product documentation and use statement:
<https://psl.noaa.gov/data/gridded/data.godas.html>.

The builder opens the official annual `pottmp.2024.nc` and `salt.2024.nc`
OPeNDAP endpoints and reads only the regional/time/depth-enclosing hyperslabs.
It retains every native longitude and latitude inside the requested bounds, all
12 source times, and the explicitly listed 18 source levels. There is no
horizontal source-grid subsampling (`lon stride = 1`, `lat stride = 1`). The
source `level` coordinate is named `depth` in the canonical container; its
values, units, positive direction, and axis metadata are unchanged.

GODAS writes the time-unit origin as `00:00:0.0`. The benchmark uses the
lexically equivalent, standards-parseable `00:00:00`, preserves the original
string in `oceancube_source_time_units`, and leaves all numeric time values
unchanged. No calendar attribute is invented; oceancube resolves the absent
calendar to standard on read.

No regridding, interpolation, spatial or temporal averaging, smoothing,
filling, climatology, anomaly, or derived scientific variable is performed.
The exact documented GODAS missing sentinel is mapped to NetCDF `_FillValue`
on write and decodes to missing values without filling.

## Redistribution and attribution

The GODAS product page states **Usage Restrictions: None** and asks users to
acknowledge NOAA PSL. NOAA-created digital media are generally public-domain
unless specifically annotated otherwise. Redistribution status is therefore
`PERMITTED_WITH_ATTRIBUTION`.

Suggested acknowledgment: NCEP Global Ocean Data Assimilation System (GODAS)
data provided by NOAA PSL, Boulder, Colorado, USA; this repository file is a
derived regional subset accessed 2026-09-06 and is not an official NOAA file.

## Rebuild and validate

From the repository root, with R packages `ncdf4` and `openssl` available:

```sh
Rscript dev/visualization/vizdata/build-oceancube-viz-benchmark-v1.R
Rscript dev/visualization/vizdata/validate-oceancube-viz-benchmark-v1.R
```

The build needs credential-free network access; validation is fully offline.
No test or package check contacts NOAA. The builder downloads no complete
annual archive and retains no source cache.

File SHA-256 and the deterministic scientific-content SHA-256 are recorded in
`oceancube-viz-benchmark-v1-manifest.csv`. Per-variable and coordinate metadata,
200 exact parity samples, and artifact checksums are in the adjacent CSV files.
Any future change to product, year, domain, variables, coordinates,
subselection, or numeric content requires a new benchmark version or an
explicit governed replacement decision.
