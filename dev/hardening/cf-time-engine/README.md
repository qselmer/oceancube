# OCEANCUBE 0.3.0-B5 — CF time engine evidence

Status: local runtime implementation and repository-wide package certification
complete.

Decision DEC-023 is **APPROVED — HYBRID CALENDAR-AWARE TIME MODEL; CORE
RUNTIME IMPLEMENTED/CERTIFIED** for the bounded subset documented here.

## Authoritative class contract

`oceancube_cf_time` is an internal compact numeric vector. The numeric payload
is elapsed seconds on an ordinal defined by the canonical calendar. Attributes
are plain R and record:

- `schema_name = oceancube_cf_time`;
- `schema_version = 1.0.0`;
- canonical and raw calendar identity;
- calendar family and `chronology_kind = historical`;
- `precision = double_seconds`;
- normalized fixed source unit, raw units, raw origin, and a plain normalized
  origin descriptor.

No environment, external pointer, connection, R6 object, random identifier,
wall-clock creation time, locale-owned state, or CFtime object enters canonical
state. `serialize()` and `saveRDS()` preserve it identically. The supported
year envelope is 0001–9999 and the declared sub-second tolerance within that
envelope is `1e-4` seconds.

Subsetting, replacement, repetition, concatenation, `min`, `max`, `range`,
formatting and compatible comparisons preserve or respect the class contract.
Formatting is ISO-like and always includes `[calendar]`. Arithmetic is
unsupported. Cross-calendar comparison and concatenation error. Package table
producers retain the class through construction, row subsetting and the tested
same-schema `rbind()` path; arbitrary external table coercions are not claimed.

## Decoder and calendar scope

One decoder serves eager `read_nc()` and deferred `cube_open()`. It accepts only
seconds, minutes, hours, or days since a calendar-valid origin. Origins support
date-only or sub-day values, `Z`, `+/-HH:MM`, and `+/-HHMM`; offsets are
normalized on the declared calendar. Negative and fractional coordinate
offsets are supported. Missing, non-finite, duplicate, non-increasing,
out-of-envelope, invalid-date, invalid reform-gap, and unsupported declarations
error deterministically.

Implemented calendars are:

- modern `standard`/`gregorian` and `proleptic_gregorian` values as UTC
  `POSIXct` whenever exactly base-representable;
- mixed pre-reform `standard` as `oceancube_cf_time`, with 1582-10-04 followed
  by 1582-10-15 and the ten reform-gap labels invalid;
- `julian`, including 1900-02-29;
- `365_day`/`noleap`;
- `366_day`/`all_leap`;
- `360_day`, including 2001-02-30.

Months since, years since, `none`, UTC, TAI, custom calendars, leap seconds,
unbounded years, WOA-specific interpretation, and climatological-bin semantics
remain unsupported.

## Integration and safety

Validation, inspection, eager/deferred ingestion, collect, exact/nearest slice,
closed-range crop, extract, transect and non-temporal time-carrying operations
retain canonical calendar identity. Same-calendar nearest distance is measured
in elapsed seconds and temporal ties choose the earlier coordinate. Provenance
V1 remains schema version 1.0.0 and records `class = oceancube_cf_time`, a NULL
timezone, canonical calendar, count, and calendar-aware endpoints without
duplicating the axis.

Tier-2 `cube_aggregate_time()`, `cube_climatology()`, `cube_anomaly()`,
`cube_trend()`, `to_month()`, compatibility climatology wrappers, and
`viz.timeseries()` reject explicitly instead of entering Gregorian code.

## Boundaries and open work

OISST remains a supported modern-time regression case. ETOPO remains rejected
because B5 does not add static fields (`A3B-002` OPEN). WOA23 remains rejected
because `months since`, climatology bounds, cell methods and provider intent do
not uniquely define ordinary timestamps (`A3B-003` OPEN). Gate B remains
UNSATISFIED. No export, public signature, dependency, version, provider,
remote/multifile/Zarr, static-field, vertical-science, UTC/TAI, or generic
month/year-unit feature is added.

CFtime 1.7.3 was used only from the isolated development library as an optional
oracle. Date components agree for the five probed supported calendars. Its
display omits midnight while oceancube deliberately emits time and calendar
identity; this is a formatting-policy difference, not a chronology discrepancy.

The final source-tree suite ran under the Windows UTF-8 locale and completed
66 test files, 646 `test_that()` cases, and 5,293 dynamic expectations in
663.440 seconds: zero failures, errors, test warnings, or skips. The explicit
locale prevents the established Windows NetCDF Unicode-path probe from being
misrepresented by the host's invalid startup `C.UTF-8` request.

A clean-snapshot source build produced `oceancube_0.2.0.9000.tar.gz`; `R CMD
check --no-manual` completed with `Status: OK`. The exact tarball was then
installed into an isolated library and exercised through public APIs only:
39 exports, modern standard time as UTC `POSIXct`, eager/deferred/collect,
exact slice, closed-range crop, 360/365/366-day and Julian calendars, and the
conservative WOA unsupported-unit rejection all passed.

## Evidence index

- `b5-calendar-results.csv`: calendar and edge-case behavior.
- `b5-decoder-results.csv`: unit/origin/validation behavior.
- `b5-selector-results.csv`: exact, nearest, range and table behavior.
- `b5-oracle-parity.csv`: isolated CFtime 1.7.3 date-component comparison.
- `b5-real-fixture-results.csv`: governed offline fixture disposition.
- `b5-operation-safety.csv`: supported carrying versus guarded analytics.
- `b5-regression-matrix.csv`: compatibility and release constraints.

The single recommended next subphase after B5 is **0.3.0-B6 — CF
CLIMATOLOGICAL TIME AND WOA23 ADJUDICATION**. B6 is not executed here.
