# NOAA/NCEI real-data test fixtures

These three files are small, derived test subsets. They are not complete or
original provider products. Ordinary oceancube tests read them locally and do
not contact a provider, require credentials, invoke Python, or depend on the
maintainer artifact directory. The exact derivations are in
`data-raw/fixtures/` and the machine-readable provenance is in
`fixture-manifest.csv`.

## OISST surface/time

`noaa-oisst21-surface-time-fv1.nc` combines exact coordinate subsets from four
NOAA/NCEI OISST AVHRR-only **final** v2.1/v02r01 granules dated 2020-01-01
through 2020-01-04. It retains native 0–360 longitude, singleton `zlev`, four
packed Int16 variables (`sst`, `anom`, `err`, `ice`), `_FillValue = -999`,
`scale_factor = 0.01`, and provider CF/ACDD attributes.

- Product DOI: https://doi.org/10.25921/RE9P-PT57
- Product metadata ID: `gov.noaa.ncdc:C01606`
- Terms: NOAA climate-data-record open-data use agreement; non-proprietary and
  no use restrictions, with source attribution and citation retained.
- Attribution: cite the OISST v2.1 DOI, NOAA/NCEI, the subset and access date,
  and state that this is a derived fixture.
- Derivation: `data-raw/fixtures/derive-oisst21.R`
- SHA-256: `8ae631afcafa6a5855a92c90b1d6098f3cefd2f534ea93ae47023f91db66d072`
- Size: 64,088 bytes

## ETOPO bed elevation

`noaa-etopo2022-bathymetry-fv1.nc` is a provider-side Grid Extract response for
ETOPO 2022 v1 60 arc-second bedrock elevation, encoded as deterministic
NetCDF. The request matches the native 60″ grid and uses nearest-neighbour
export. The semantic variable remains `z` elevation in metres; values are not
renamed to depth and are not sign-flipped. Negative ocean elevations are
intentional. This product is not suitable for navigation.

- Product DOI: https://doi.org/10.25921/fd45-gt74
- Product metadata ID: `gov.noaa.ngdc.mgg.dem:etopo_2022`
- License: CC0-1.0 (SPDX CC0-1.0 in official NCEI dataset metadata).
- Attribution: cite NOAA/NCEI ETOPO 2022 and its DOI, identify the subset and
  access date, and state that this is a derived fixture.
- Derivation: `data-raw/fixtures/derive-etopo2022.R`
- SHA-256: `7b67bf4291015afc47a5218db25a6254eaa60d55c7afa1517bee5ff853cafddc`
- Size: 1,211,653 bytes. This deliberately retains Float32 source values; it is
  above the preferred 1 MB target but below the approved reportable 2 MB bound.

## WOA23 vertical ocean

`noaa-woa23-vertical-fv1.nc` combines exact OPeNDAP coordinate subsets of the
WOA23 annual 1-degree all-decades temperature and salinity files from NCEI
Accession 0270533 v3.3. It retains `t_an`, `s_an`, six exact provider depths,
positive-down orientation, coordinate/depth bounds, `cell_methods`, the native
`months since 1955-01-01 00:00:00` time value, and `climatology_bounds`. The
provider files have no `calendar` attribute; this fixture does not invent one.

- Dataset DOI: https://doi.org/10.25921/va26-hv25
- Temperature volume DOI: https://doi.org/10.25923/54bh-1613
- Salinity volume DOI: https://doi.org/10.25923/70qt-9574
- Exact-file terms: openly available to the public; acknowledgment is required.
  The files refer to an `acknowledgment` attribute that is absent in both exact
  files, so this fixture retains both complete volume references and requires
  the three DOI citations above. NCEI's internal-data component records
  CC0-1.0.
- Derivation: `data-raw/fixtures/derive-woa23.R`
- SHA-256: `70fddb97edcda4e8fb8a4ddbb3428823f5f6a47deba51ff3624659f660fb8456`
- Size: 57,343 bytes

The combined fixture size is 1,333,084 bytes. All final and feasible constrained
source SHA-256 values, exact URLs, coordinate selections, classifications, and
known current limitations are recorded in `fixture-manifest.csv`. SHA-256 is
not recomputed during default tests because oceancube does not add a runtime or
test dependency solely for hashing; Git, the manifest, derivation checks, and
the byte-identical second-run certification retain immutable integrity evidence.

Copernicus data are not present here. The historical Copernicus artifact remains
maintainer-only, credentialed, non-CI, and uncommitted while redistribution is
unknown.
