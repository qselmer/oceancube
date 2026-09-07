# D-VIZDATA source and redistribution evidence

Checked: 2026-09-06

## Selected source

- Provider: NOAA Physical Sciences Laboratory (PSL), sourced from NOAA/NCEP.
- Product: NCEP Global Ocean Data Assimilation System (GODAS).
- Authoritative product page: <https://psl.noaa.gov/data/gridded/data.godas.html>
- Authoritative machine catalogue: <https://psl.noaa.gov/thredds/catalog/Datasets/godas/catalog.html>
- NOAA digital-media policy: <https://sos.noaa.gov/copyright/>
- License determination: NOAA public-domain data; the GODAS page states
  **Usage Restrictions: None** and asks users to acknowledge NOAA PSL.
- Redistribution conclusion: **PERMITTED_WITH_ATTRIBUTION** for this exact,
  unmodified-value regional subset. The repository identifies it as a derived
  subset, retains the provider/product identity, and gives the requested NOAA
  PSL acknowledgment. It does not present the subset as an official NOAA file
  or imply NOAA endorsement.

The annual OPeNDAP endpoints are public and credential-free. Public access was
not treated as permission by itself: the conclusion rests on the explicit
product use statement together with NOAA's policy that NOAA-created digital
media are generally not copyrighted unless annotated otherwise.

## Alternatives checked

- NOAA/NCEI World Ocean Atlas 2023: authoritative and reusable with required
  acknowledgment, but its twelve monthly temperature/salinity fields are
  climatological composites rather than one recent complete physical year.
- Copernicus Marine `GLOBAL_MULTIYEAR_PHY_001_030`: the Copernicus Marine
  licence documents redistribution with prescribed credit, but reproducible
  subset access requires account state. It was not selected because the
  credential-free GODAS alternative meets the scientific contract.

No license text is copied here beyond the minimum phrase needed to record the
GODAS use restriction. Full terms remain at the authoritative links.
