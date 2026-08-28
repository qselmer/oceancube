# oceancube CF time and calendar architecture V1

Status: approved architecture; runtime implementation pending B5

Decision: DEC-023 — HYBRID CALENDAR-AWARE TIME MODEL

Normative reference: CF Metadata Conventions 1.13

## Scope and terminology

This document defines how oceancube will represent and reason about CF time
without changing the 0.2.0.9000 runtime. A *base civil coordinate* is an exact
current `Date` or UTC `POSIXct`. A *calendar-aware coordinate* is a future
oceancube-owned plain-R state for dates that base R cannot represent faithfully.
The *source declaration* is the raw CF units/calendar/origin and related
attributes in `x$metadata$cf`. *Chronology kind* distinguishes historical,
climatological and perpetual coordinates.

## Governing principles

1. Never convert a supported calendar merely to fit R.
2. Preserve existing Date/POSIXct behavior for every current exact case.
3. Own canonical scientific state as plain R; external objects are transient.
4. Keep coordinate values, CF declaration and provenance separate.
5. Comparisons and transformations require compatible calendar semantics.
6. Preserve/defer rather than invent dates or provider intent.

## Calendar taxonomy

TIER-1-CURRENT contains the existing exact post-reform `standard`/`gregorian`
and base-representable `proleptic_gregorian` subset. TIER-2-CALENDAR-AWARE
contains Julian, 365-day, 366-day, 360-day and non-base-representable civil
values. TIER-3-PRESERVE initially contains `none`, `utc`, `tai`, and explicitly
defined calendars. Unknown declarations are preserve-only.

Raw aliases are never rewritten in source metadata. Interpreted identity maps
`gregorian` to `standard`, `noleap` to `365_day`, and `all_leap` to `366_day`.
`standard` retains the Julian/Gregorian discontinuity; it is not globally
proleptic Gregorian. UTC and TAI are distinct time scales, not timezone labels.

## Canonical representation

Existing exact coordinates remain Date/POSIXct. The future calendar-aware state
has a versioned plain schema containing calendar identity/family, chronology
kind, exact or bounded-precision ordinal/sub-day values, source unit/origin
references, precision, custom definition when applicable, and validity status.
It contains no environment, external pointer, connection, R6 object, wall-clock
field, random ID or locale-dependent text.

The coordinate array exists once as `x$time`. Raw CF declarations remain under
`x$metadata$cf`. Provenance records transformations and compact descriptors;
neither metadata nor provenance duplicates the full coordinate array.

## Date/POSIXct compatibility

Manual and decoded inputs that are exactly representable continue to expose the
same Date/POSIXct classes. POSIXct remains UTC. Daily CF decoding continues to
produce POSIXct under the current contract until a separately reviewed runtime
change. Calendar-aware state is selected only when base representation would be
false, out of range, year-number-incompatible, leap-second-incompatible, or
insufficiently precise.

## Ordering and comparison

Within one canonical calendar/definition and chronology kind, ordering uses the
calendar ordinal and sub-day component. Equality is exact under the recorded
precision contract. Duplicate detection and monotonicity use the same key.
Cross-calendar equality or ordering is unavailable and raises an incompatible-
calendar error unless a future explicit conversion defines the mapping.

## Selection

Exact selection accepts compatible canonical values, calendar-valid strings,
component lists, exact source offsets, and base values when compatible. Nearest
and closed ranges use the same-calendar elapsed metric; ties select the earlier
stored coordinate. Strings are parsed by calendar, so `2001-02-30` is valid for
360_day and invalid for Gregorian calendars.

## Grouping, climatology and elapsed time

Year, month, day and season keys come from calendar components. Season mappings
are explicit. Climatological coordinates carry `chronology_kind =
climatological`; their bounds and cell methods define recurring statistical
intervals and their midpoint does not uniquely identify a bin.

Anomaly alignment requires matching canonical calendar, chronology and group
definition. Trend operates on an explicit elapsed metric. Idealized calendars
use their exact days/year; UTC requires leap-aware SI seconds; TAI uses atomic
seconds. No global 365.25-day assumption is introduced for new calendars.

## Special calendars

`none` is perpetual and rejects ordinary annual/monthly/seasonal grouping and
chronological analysis absent explicit experiment semantics. UTC remains
preserve-only until a versioned IERS leap-second table, validity horizon and
`23:59:60` representation are implemented. TAI remains preserve-only until its
continuous scale and explicit UTC conversion are implemented. Custom calendars
preserve validated month lengths and optional four-year leap rule; future
native arithmetic is definition-driven.

## Units, origin and precision

Calendar identity is independent of the reference-datetime UTC offset. Raw
origin text/offset are preserved and interpreted deterministically. Fixed
seconds/minutes/hours/days and prefixed seconds are duration units. UDUNITS
month/year duration, calendar month/year, and climatological bins are distinct.
Negative and fractional offsets are valid. Precision/tolerance follows source
representation and must reject values beyond exact supported bounds.

## Serialization, tables, printing and plotting

Plain state must survive serialize/unserialize and saveRDS/readRDS identically.
Tables retain Date/POSIXct for base cases; calendar-aware rows expose
round-trippable value/components plus calendar, chronology and schema metadata.
Printing includes calendar identity. Plotting uses an ordinal/category adapter
with formatted labels or rejects explicitly; it never applies a false Gregorian
axis.

## External arithmetic engine

Base R remains the TIER-1 engine. Native oceancube code owns canonical identity,
validation, comparison keys, simple ordinal operations and custom definitions.
CFtime is the preferred optional transient adapter/oracle where supported:
plain state reconstructs an engine object, performs an operation, converts the
result back to plain state, then discards the object. Engine name/version is a
software diagnostic, not scientific coordinate identity. PCICt is not selected
because limited calendar coverage and remapping behavior conflict with this
architecture.

## WOA23 policy

WOA raw `months since` values, absent calendar, climatology link/bounds and cell
methods are preserved. Literal UDUNITS duration, calendar-month advancement and
climatological-bin interpretations are not interchangeable. Because the file
does not uniquely authorize an ordinary timestamp, oceancube refuses production
decoding until B5 plus provider evidence defines the mapping. `A3B-003` remains
open despite the architectural decision.

## Provenance relationship

A future provenance revision minimally adds representation kind, chronology
kind, canonical calendar/custom definition identity, precision and—only for
UTC arithmetic—a leap-table identifier. Raw declarations remain CF metadata;
engine versions remain software diagnostics. Provenance V1 is unchanged by B4.

## Unsupported cases and evolution

B4 adds no runtime calendar. B5 implements the approved representation in
stages and must preserve all current public behavior. UTC, TAI, `none`, custom
calendars, out-of-range values, unbounded precision and ambiguous provider
encodings may remain preserve-only until separately certified. Schema evolution
is versioned and must define migration plus semantic equality.
