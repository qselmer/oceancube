# Real-data fixture policy

This policy governs real provider data used by oceancube. A3a selects sources
and defines controls; it does not authorize a binary fixture. A3b may derive a
fixture only after the maintainer approves the exact product and this policy is
satisfied.

## Hard legal gate

Public accessibility is not redistribution permission. Every repository
fixture must have first-party evidence for one of these values:

- `YES`: redistribution is expressly permitted without an attribution
  condition.
- `YES-WITH-ATTRIBUTION`: redistribution is expressly permitted and the
  recorded attribution must accompany the fixture.
- `NO`: redistribution is expressly prohibited.
- `UNKNOWN`: evidence is missing, ambiguous or not tied to the exact product.

Only `YES` and `YES-WITH-ATTRIBUTION` can proceed to a committed fixture.
`UNKNOWN` is a hard blocker regardless of a source's scientific score. The
existing Copernicus RDS therefore remains maintainer-only.

## Source and fixture terminology

- **Source product**: the provider's versioned original product.
- **Source subset**: an exact coordinate, time, depth and variable selection
  from the product, preferably produced by NCSS, OPeNDAP or a provider subset
  service.
- **Derived fixture**: the small repository object produced deterministically
  from one or more source subsets. It is never described as an untouched
  provider original.

The fixture must retain real coordinates, names, units, relevant CF attributes,
missingness, packing and realistic dimension order. Conversion to a synthetic
array is not a real-data fixture.

## Authentication and network

Authentication is classified as `NONE`, `OPTIONAL` or `REQUIRED`. Credentials
must never be stored in the repository, fixtures, logs or CI. Authentication
does not by itself prohibit a fixture, but `REQUIRED` weakens reproducibility
and normally routes provider access to a maintainer workflow.

Package tests and CI use no provider network. Approved small fixtures are
committed and read locally. Network access is allowed only in a manual
maintainer regeneration workflow.

## Attribution and provenance

Attribution is recorded redundantly in the manifest, the real-data README and,
where NetCDF permits, global attributes of the derived fixture. It contains:

- provider, product name and provider version;
- stable product ID and DOI;
- license/terms identifier and URL;
- required citation and acknowledgment;
- provider documentation access date;
- actual retrieval date;
- explicit `derived subset for oceancube testing` statement;
- exact spatial, time, depth and variable selections;
- all processing steps and source/output checksums.

The required documentation access date for A3a is `2026-08-20`.

## Checksums and immutability

Every committed fixture has a SHA-256 for the final file. Each source granule or
provider subset also has a SHA-256 when the response can be retained or streamed
deterministically. A filename or product version is not a checksum.

Once used by a contract test, a fixture is immutable. A provider revision,
selection change, metadata repair or derivation change creates a new fixture
version and checksum plus an explicit test migration. It must not silently
overwrite the old fixture.

Provider and fixture versions are independent. Names follow:

```text
<provider>-<product>-<role>-fv<fixture-version>.nc
```

Proposed first names are:

```text
noaa-oisst21-surface-time-fv1.nc
noaa-etopo2022-bathymetry-fv1.nc
noaa-woa23-vertical-fv1.nc
```

## Size policy

Size classes refer to final files on disk:

| Class | Size |
|---|---:|
| `MICRO` | `< 100 KB` |
| `TINY` | `100 KB` through `< 500 KB` |
| `SMALL` | `500 KB` through `2 MB` |
| `MAINTAINER` | `> 2 MB` |

Committed CI fixtures should each be at most 1 MB and together at most 3 MB
where feasible. Scientific structure and contract-relevant metadata must not be
discarded merely to reach a smaller class. Any fixture over 1 MB needs a written
exception; anything over 2 MB is maintainer-only by default.

## Derivation script contract

A3b must place deterministic scripts under `data-raw/fixtures/` unless a
repository convention review selects `dev/hardening/fixtures/scripts/`. Each
script must:

1. identify provider, exact product/version, DOI, stable ID and source URL;
2. record retrieval date and each source checksum when feasible;
3. specify coordinate convention, bounds, variables, dates and depths;
4. perform only documented exact subsets, merges or resampling;
5. preserve relevant CF/ACDD attributes, bounds, fill values and packing;
6. label the output as a derived oceancube fixture;
7. write the final SHA-256 and byte size into the manifest;
8. fail rather than silently use a different provider version or endpoint.

Provider-side exact subsetting is preferred, followed by an exact local
coordinate subset, then a documented deterministic aggregate. Custom
interpolation requires a separate justification. The ETOPO proposal uses the
official 60 arc-second source and no resampling.

## Scientific scope

The three proposed roles are orthogonal:

- OISST: surface, real daily time, singleton vertical axis, native 0-360
  longitude, packing, error/ice variables and missingness.
- ETOPO: static elevation, coast/terrain, units and sign. Negative elevation is
  not silently renamed to positive-down ocean depth.
- WOA23: real temperature/salinity at multiple positive-down standard depths,
  profiles, sections and climatological metadata.

WOA validates data handling and future Gate B/vertical infrastructure. It is
not reference truth for thermocline, mixed-layer-depth, pycnocline or other
scientific diagnostic methods; those require later methodological datasets and
reviews.

## Approval and release controls

Before a fixture can become CI-eligible, the maintainer must approve the exact
source row, the derivation must be reviewed, license/attribution must be carried
into the output, checksums and sizes must be populated, and focused offline
contract tests must pass. `DEC-024` remains open until those product-specific
decisions and A3b evidence exist.
