# A3a governance and A3b real-data fixture execution

Status: **three maintainer-approved fixtures derived, checksummed, attributed,
and covered by focused offline tests**.

This directory preserves the A3a source/legal review separately from the A1
historical `../fixture-manifest.csv`. `proposed-fixtures.csv` remains the
unchanged historical proposal. A3a official source documentation was accessed
on `2026-08-20`; that phase inspected only metadata pages, product documentation
and metadata-scale OPeNDAP headers and downloaded no provider binary.

A3b executed the maintainer's exact product approval on `2026-08-21`.
`executed-fixtures.csv` and `executed-contract-matrix.csv` record actual files,
checksums, sizes, attributes, current compatibility and future classifications.
The final distribution manifest and attribution README are committed beside the
fixtures under `tests/testthat/fixtures/real-data/`.

## Recommendation

The maintainer approved and A3b executed exactly:

1. **Surface/time:** NOAA/NCEI OISST AVHRR-only Final v2.1 (`v02r01`), four
   final days from 2020-01-01 through 2020-01-04.
2. **Bathymetry:** NOAA/NCEI ETOPO 2022 bedrock elevation, official 60
   arc-second NetCDF, with no resampling.
3. **Vertical ocean:** NOAA/NCEI WOA23 full-release annual 1-degree all-decades
   (1955-2022) temperature and salinity, NCEI Accession 0270533 v3.3.

NASA/JPL MUR v4.1 is the SST alternate because its 0.01-degree detail does not
add enough minimum-suite contract coverage to offset required Earthdata
authentication. GEBCO_2025 is the bathymetry alternate: its public-domain terms,
CF-1.6 NetCDF and versioned DOI are strong, but ETOPO provides an official
lower-resolution product and explicit dataset-level SPDX CC0 evidence.

The existing Copernicus Marine Peru RDS remains maintainer-only because its
exact source/license/derivation chain is absent and redistribution is
`UNKNOWN`.

## A3b execution result

The three deterministic outputs are:

| Role | File | Bytes | Current status |
|---|---|---:|---|
| Surface/time | `noaa-oisst21-surface-time-fv1.nc` | 64,088 | `CURRENT-PASS` with public `depth_name="zlev"` mapping |
| Bathymetry | `noaa-etopo2022-bathymetry-fv1.nc` | 1,211,653 | `CURRENT-EXPECTED-LIMITATION`: static source has no time |
| Vertical | `noaa-woa23-vertical-fv1.nc` | 57,343 | `CURRENT-EXPECTED-LIMITATION`: annual raw climatology support conflicts with declared coverage; generic UDUNITS months are supported |

Combined size is 1,333,084 bytes. ETOPO is above the preferred 1 MB target but
below the reportable 2 MB bound; retaining provider Float32 values avoids an
unapproved quantization solely for size. Every source response and final
fixture is SHA-256 recorded. A second run of all three scripts produced the
same final SHA-256 byte for byte.

OISST uses four exact final granules and retains packed short storage. ETOPO
uses the official NCEI Grid Extract service on the native 60 arc-second grid,
not the large global download, and retains elevation sign. WOA uses bounded
OPeNDAP reads because NCSS changes provider depth centres; the fixture retains
the exact 0/10/20/50/100/200 m coordinates, bounds, native climatological time,
and the exact files' absence of a calendar attribute.

## Common Peru selection

The exact proposed selection bounds are:

```text
longitude: 84°W through 75°W
latitude:  18°S through 6°S
```

In signed degrees this is `lon [-84, -75]`, `lat [-18, -6]`. OISST must retain
its native 0-360 axis, so its request is `lon [276, 285]`, `lat [-18, -6]`;
the fixture must record the resulting provider grid centres rather than
rewriting them to -180..180. The footprint crosses the Peruvian coast, includes
ocean plus land/coastal missingness, spans nontrivial shelf/slope bathymetry and
contains upper-ocean WOA profiles while remaining bounded.

OISST uses four daily final files. WOA retains its actual annual all-decades
climatological time coordinate/bounds and the exact standard depths `0, 10, 20,
50, 100, 200 m` (`positive=down`). These levels are handling evidence, not
prescribed diagnostic thresholds.

## Candidate ranking

Scores use fourteen 0-3 criteria (maximum 42) from
`source-evaluation.csv`. A total never overrides a legal blocker.

| Rank | Candidate | Score | Recommendation |
|---:|---|---:|---|
| 1 | OISST v2.1 | 40 | `APPROVE-FOR-A3b` surface/time |
| 1 | ETOPO 2022 | 40 | `APPROVE-FOR-A3b` bathymetry |
| 1 | WOA23 | 40 | `APPROVE-FOR-A3b` vertical |
| 4 | GEBCO_2025 | 39 | `ALTERNATE` bathymetry |
| 5 | MUR SST v4.1 | 33 | `ALTERNATE` surface/time |
| 6 | local Copernicus case | 25 | `MAINTAINER-ONLY`; legal blocker |

## CF and encoding evidence

| Attribute | OISST v2.1 | ETOPO 2022 | WOA23 |
|---|---|---|---|
| `Conventions` | `CF-1.6, ACDD-1.3` | file-level value to verify in A3b | file `CF-1.6`; collection specification also records ACDD-1.3 |
| coordinates | `lon`, `lat`, `zlev`, `time` | longitude/latitude grid; exact names to verify | `lon`, `lat`, `depth`, `time`; axes X/Y/Z/T |
| main variables | `sst`, `anom`, `err`, `ice` | elevation; exact file name to verify | `t_an`, `s_an`; associated `*_mn`, `*_dd`, `*_sd`, `*_se`, `*_oa`, `*_gp`, `*_sdo`, `*_sea` fields exist |
| `standard_name` / `long_name` | no `standard_name` in inspected header; descriptive `long_name` on every variable | verify exact 60″ file | `sea_water_temperature` / objectively analyzed mean; `sea_water_salinity` or practical-salinity equivalent to confirm from exact WOA23 salinity header |
| units | Celsius, Celsius, Celsius, `%`; `zlev` metres | elevation metres | `t_an` degrees_celsius; `s_an` exact WOA23 units to verify; depth metres |
| vertical | `zlev=0`, `positive=down` | elevation relative to reference surface; ocean values negative | 102 standard depths, positive down |
| longitude | 0.125..359.875 by 0.25° | -180..180 global product | -180..180, 1° proposal |
| fill/missing | data variables `_FillValue=-999` | verify exact 60″ file | float analyses `_FillValue=9.96921E36`; count fields use `-32767` |
| packing | Int16, `scale_factor=0.01`, `add_offset=0` | preserve official encoding | preserve exact encoding if packed |
| bounds | not exposed in inspected daily header | verify exact file | `lat_bnds`, `lon_bnds`, `depth_bnds`, `climatology_bounds` |
| `calendar` | not present in inspected header; units are days since 1978-01-01 12:00:00 | not applicable | no explicit calendar in inspected file page; time units months since 1955-01-01 and `climatology_bounds` |
| `cell_methods` | not present in inspected header | not applicable | `area: mean depth: mean time: mean within years time: mean over years` on analyses |
| `grid_mapping` | not present in inspected header | EPSG:4326 horizontal and EPSG:3855 vertical metadata at collection level | `crs`; latitude_longitude; EPSG:4326 |

OISST header evidence came from the official NCEI OPeNDAP `.dds` and `.das`
for `oisst-avhrr-v02r01.20250501.nc`; this was a metadata-scale text request.
The WOA THREDDS query form confirms 360 lon, 180 lat, one time, 102 depths,
coordinate/depth/climatology bounds, `t_an`, CF cell methods, `crs`, open public
access and a required acknowledgment. The official salinity catalog confirms
`woa23_decav_s00_01.nc`; `s_an` follows the official WOA variable convention.
Fields still marked for A3b verification are not guessed in
`proposed-fixtures.csv`.

## Official evidence

- OISST product and archive:
  <https://www.ncei.noaa.gov/products/optimum-interpolation-sst>
- OISST DOI metadata:
  <https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc%3AC01606>
- OISST CDR use agreement:
  <https://www.ncei.noaa.gov/pub/data/sds/cdr/CDRs/Sea_Surface_Temperature_Optimum_Interpolation/UseAgreement_01B-09.pdf>
- ETOPO product and resolution options:
  <https://www.ncei.noaa.gov/products/etopo-global-relief-model>
- ETOPO metadata, citation and explicit SPDX CC0-1.0:
  <https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ngdc.mgg.dem%3Aetopo_2022>
- ETOPO official Grid Extract service used for the bounded 60 arc-second
  provider-side response:
  <https://www.ncei.noaa.gov/maps/grid-extract/>
- WOA23 data application:
  <https://www.ncei.noaa.gov/access/world-ocean-atlas-2023/>
- WOA23 metadata, accession, CF/ACDD and citations:
  <https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc%3ANCEI-WOA23>
- WOA23 annual 1-degree temperature OPeNDAP metadata:
  <https://www.ncei.noaa.gov/thredds-ocean/dodsC/woa23/DATA/temperature/netcdf/decav/1.00/woa23_decav_t00_01.nc.html>
- WOA23 annual 1-degree salinity catalog:
  <https://www.ncei.noaa.gov/thredds-ocean/catalog/woa23/DATA/salinity/netcdf/decav/1.00/catalog.html>
- NCEI internal NOAA-data CC0 component:
  <https://data.noaa.gov/docucomp/xmlComponent/show/690447>
- MUR v4.1 product/DOI and authenticated access:
  <https://podaac.jpl.nasa.gov/dataset/MUR-JPL-L4-GLOB-v4.1>
- NASA Earthdata use/citation policy:
  <https://www.earthdata.nasa.gov/engage/open-data-services-software/data-use-policy>
- GEBCO_2025 product, NetCDF/CF and DOI:
  <https://www.gebco.net/data-products-gridded-bathymetry-data/gebco2025-grid>
- GEBCO redistribution and attribution terms:
  <https://www.gebco.net/data-products/gridded-bathymetry/terms-of-use>

## Files

- `fixture-policy.md`: legal, attribution, checksum, size, network,
  immutability and derivation rules.
- `source-evaluation.csv`: common scoring matrix and source decisions.
- `proposed-fixtures.csv`: required future manifest schema populated without
  invented checksums or retrieval facts.
- `contract-matrix.csv`: specialized fixture-by-contract allocation.
- `executed-fixtures.csv`: A3b actual files, attributes, checksums, sizes,
  compatibility and byte-identical regeneration evidence.
- `executed-contract-matrix.csv`: A3a assignments reconciled as
  `EXECUTED-PASS`, `EXECUTED-EXPECTED-LIMITATION`, or `FUTURE`.

`A1-005` is partially closed because governance now exists but Copernicus legal
status remains unknown. A successful A3b certification closes `A1-010` because
all three approved real-data fixtures are legally auditable, committed, local,
and tested in offline CI. It approves `DEC-024` for this governed source set.
This phase does not complete 0.3.0-A or satisfy Gate B.
