# B6 — CF climatological time and WOA23 adjudication

## Decisions

`DEC-B6-1` is **APPROVED / IMPLEMENTED**. CF/UDUNITS month and year units are
elapsed durations: one year is exactly `365.242198781` days and one month is
one twelfth of that. The decoder uses the internal identities
`udunits_month` and `udunits_year`; it never advances the civil calendar by a
month or year.

`DEC-B6-2` is **APPROVED WITH A CONSERVATIVE RUNTIME GATE**. A generic CF
climatology is runtime-supported only when `time:climatology` resolves to a
finite `n x 2` support envelope, every selected variable carries the bounded
`time: ... within years ... time: ... over years` pattern, representative
coordinates lie within their envelopes, and any parseable declared coverage
does not conflict with the decoded envelope. The versioned descriptor lives at
`x$metadata$cf$current$chronology`; no parallel cube field or public API was
added.

The descriptor distinguishes
`CLIMATOLOGY_STRUCTURALLY_VALID`,
`CLIMATOLOGY_SEMANTICALLY_INTERPRETABLE`, and
`CLIMATOLOGY_RUNTIME_SUPPORTED`. Bounds are the earliest beginning and latest
end of potentially discontinuous climatological support, not ordinary
contiguous cell bounds. Representative time remains usable for ordering and
selection. Ordinary temporal aggregation, climatology, anomaly, trend and
month conversion are guarded because the field is already a temporal
statistic. Provenance V1 remains unchanged; its inability to encode this state
is a known gap and CF current metadata is authoritative.

## Governed WOA fixture

The governed file is NetCDF-4 with dimensions `nbounds=2`, `lon=9`, `lat=12`,
`depth=6`, `time=1`; CF-1.6; Float32 raw time `4614`; units `months since
1955-01-01 00:00:00`; no calendar attribute (therefore CF `standard`);
`time:climatology=climatology_bounds`; Float32 bounds `4212,5028`; and exact
cell methods `area: mean depth: mean time: mean within years time: mean over
years`. Variables use `coordinates="time lat lon depth"`. Depth is
`0,10,20,50,100,200 m`, positive down, with provider bounds retained. The CRS
is latitude/longitude EPSG:4326. All global attributes, source URLs, two source
response hashes, access date, attribution, spatial limits, `time_coverage_start
= 1955-01-01`, `time_coverage_duration = P68Y`, and fixture governance fields
were read again from the binary.

The exact local fixture SHA-256 is
`70fddb97edcda4e8fb8a4ddbb3428823f5f6a47deba51ff3624659f660fb8456`.
Its deterministic derivation asserts official raw time `4614`, bounds
`4212,5028`, units, absent calendar, coordinate identity, and exact constrained
NCEI response hashes before writing. Thus `4614` is the raw unscaled official
annual value, not a decoded value, packing artifact, fixture transformation, or
transcription error.

Literal normative decoding yields representative
`2339-07-02T15:00:37.263854Z` and support
`2306-01-01T00:16:57.112121Z` through
`2373-12-31T11:33:03.390258Z`. Civil month advancement would instead yield
2339-07-01 and is not CF/UDUNITS semantics. Subtracting the first bound would
make the values look like 1955–2023, but neither CF metadata nor NCEI metadata
authorizes that correction.

Official sibling comparisons reinforce the classification: winter `t13` uses
`397.5` with bounds `0,807`, and January `t01` uses `396.5` with bounds
`0,805`; both are coherent with the declared historical period under literal
elapsed durations. Annual `t00` uniquely carries the far-future shifted values.
The most evidence-bounded classification is therefore an official annual
encoding inconsistency of unknown cause. Core does not branch on WOA, filename,
dataset id, or provider and does not repair it.

Consequently `read_nc()` and `cube_open()` reject the governed WOA fixture with
a deterministic coverage-conflict error; `cube_collect()` is not applicable.
Its climatological role is known, but its numeric support cannot safely become
a current cube. `A3B-003` is **RECLASSIFIED** from one unsupported-unit gap into
closed generic duration-unit support, closed generic climatology architecture,
and an open official-WOA annual encoding remediation. The vertical fixture
cannot yet be represented safely. Gate B remains **UNSATISFIED**; B7 is not
authorized.

## Normative and comparison evidence

CF 1.13 sections 4.4 and 7.4 are normative. The official NCEI landing page,
product selector, catalog and OPeNDAP metadata are authoritative for WOA23.
CFtime 1.7.3 was used only as a comparison oracle; its calendar-month-like
rendering is not used as normative evidence. NCEI file endpoints were
temporarily unavailable during live byte retrieval, so exact official identity
is supported by the governed derivation assertions and official metadata pages;
an archival full-file mirror was used only to compare annual, seasonal and
monthly raw fields and is not authoritative.

See `b6-fixture-source-audit.csv`, `b6-woa-interpretation-options.csv`,
`b6-official-product-audit.csv`, `b6-runtime-results.csv`,
`b6-regression-matrix.csv`, and `b6-sources.csv`.
