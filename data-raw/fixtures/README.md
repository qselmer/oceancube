# Governed real-data fixture derivation

These maintainer scripts regenerate the four NOAA/NCEI real-data fixtures
approved for oceancube 0.3.0-A3b. They are intentionally separate from the
ordinary test suite: regeneration uses provider network access, while tests use
only the committed files under `tests/testthat/fixtures/real-data/`.

Run each script from the repository root with Rscript:

```text
Rscript --vanilla data-raw/fixtures/derive-oisst21.R
Rscript --vanilla data-raw/fixtures/derive-etopo2022.R
Rscript --vanilla data-raw/fixtures/derive-woa23.R
Rscript --vanilla data-raw/fixtures/derive-woa23-monthly.R
```

An alternate output may be supplied as `--output=<path>`. This is used by the
second-run reproducibility check. Provider downloads, DAP responses, temporary
files, and generated derivation evidence are written below
`data-raw/fixtures/cache/`, which is ignored by Git.

Every script pins exact URLs, checks source identity and SHA-256, preserves the
approved native coordinate semantics, writes deterministic NetCDF, prints the
fixture SHA-256 and byte size, and emits an ignored one-row derivation record.
The committed attribution and final checksums live beside the fixtures in
`tests/testthat/fixtures/real-data/`.

Required maintainer-only tooling is R, `ncdf4`, and `openssl`; the ETOPO script
also uses `terra` to read the provider's official Grid Extract GeoTIFF response.
These are derivation tools, not oceancube runtime dependencies.

Copernicus data are deliberately excluded. The historical Copernicus case
remains maintainer-only, credentialed, non-CI, and uncommitted while exact
redistribution evidence is unknown.
