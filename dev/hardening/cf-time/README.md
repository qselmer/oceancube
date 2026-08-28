# OCEANCUBE 0.3.0-B4 — CF time and calendar decision evidence

## Result

`DEC-023` is **APPROVED — HYBRID CALENDAR-AWARE TIME MODEL**. Existing values
which oceancube can represent exactly continue to use `Date` or UTC `POSIXct`.
Future non-base-representable CF time uses an oceancube-owned, plain-R,
calendar-aware state. CFtime may be reconstructed transiently as an arithmetic
adapter; an R6 object is never canonical cube state.

B4 is architecture and evidence only. It changes no runtime file, test,
signature, export, dependency, decoder, scientific result, or current temporal
behavior. Calendar runtime implementation belongs to B5.

## Why B4 precedes static fields

WOA23 is the governed vertical/depth-bearing fixture and is blocked by temporal
interpretation. Reliable time representation is therefore a nearer dependency
of Gate B and vertical science than ETOPO static-time support. `A3B-002` remains
open; B4 does not add a fabricated time coordinate to ETOPO.

## Normative authority

The normative reference is [CF Metadata Conventions
1.13](https://cfconventions.org/Data/cf-conventions/cf-conventions-1.13/cf-conventions.html),
especially sections 4.4, 4.4.2–4.4.6, 7.4 and Appendix M. Package behavior is
probe evidence only. CF requires a reference datetime, makes the interpretation
depend jointly on units and calendar, defines the standard-calendar reform gap,
defines idealized calendars, gives `none` separate semantics, and introduces
leap-second-aware `utc` plus continuous `tai`. CF recommends against time units
of `year` and `month`; they are durations, not calendar bins.

## Current oceancube contract

The current cube time axis is a non-empty one-dimensional `Date` or `POSIXct`
vector, complete, finite, unique and strictly increasing. Character input is
accepted only as ISO dates or datetimes with explicit `Z`/numeric offsets.
`POSIXct` is normalized to UTC. Date preserves civil-day resolution; POSIXct
preserves sub-day and fractional-second instants subject to R double precision.
`temporal_extent`, inspection, printing, provenance, selection, crop,
aggregation, climatology, anomaly and trend all assume base temporal classes.
The exact consumer audit is in `b4-current-time-consumers.csv`.

`.decode_cf_time()` accepts finite numeric offsets with exactly seconds,
minutes, hours or days since a parseable reference datetime. An absent calendar
defaults to `standard`; accepted calendars are `standard`, `gregorian` and
`proleptic_gregorian`. Output is UTC POSIXct. `standard`/`gregorian` values
before 1582-10-15 are rejected. Julian, idealized, `none`, `utc`, `tai`, custom,
and month/year-unit inputs are rejected. B4 does not change any of this.

## Representation decision

Weights were declared per criterion in `b4-representation-options.csv` before
selection. Weighted totals are A Date/POSIXct-only 321, B CFtime-canonical 261,
C native plain state 357, D hybrid 410, and E raw offsets plus descriptor 250.
The hybrid wins because it combines fidelity, base compatibility, stable plain
serialization and future backend independence.

- A fails non-Gregorian fidelity.
- B has strong calendar arithmetic but breaks current classes, couples the
  canonical cube to R6/package lifetime, and serializes poorly as identity.
- C is scientifically strong but needlessly disrupts current exact base cases
  and demands all arithmetic at once.
- D retains current behavior and adds plain calendar-aware state only when
  required.
- E preserves source values but is insufficient by itself for deterministic
  grouping, comparison, selectors, tables and trends.

## Canonical plain state contract

The future non-base representation is conceptually
`oceancube_cf_time_state`, a plain list/vector schema owned by oceancube. It
contains schema identity/version, canonical and raw calendar identity,
calendar family, normalized unit and raw unit, parsed origin plus raw origin and
offset, authoritative numeric/ordinal coordinate values, sub-day precision,
chronology kind (`historical`, `climatological`, or `perpetual`), optional
custom-calendar definition, and validity/status. Derived components may be
cached only if they are verified projections of the authoritative coordinate.

The full source declaration remains in `x$metadata$cf`; the time coordinate is
authoritative in `x$time`; transformation history remains provenance. Complete
coordinate arrays are not copied into CF metadata or provenance.

The canonical value domain uses a calendar ordinal plus sub-day component (or
an exactly reconstructable source offset) rather than a fake Gregorian epoch.
Whole-day ordinals must stay inside exact-double integer range; finer precision
records source scale/tolerance. Out-of-range or lossy values are preserved and
rejected operationally rather than rounded silently.

## Calendar policy

- `standard`: base R remains valid only for the already-supported post-reform
  subset. Mixed Julian/Gregorian dates use calendar-aware state. Dates
  1582-10-05 through 1582-10-14 are invalid.
- `gregorian`: preserve raw spelling; canonical interpreted identity is
  `standard`, following its deprecated-alias status.
- `proleptic_gregorian`: use base types only when range, year numbering and
  precision are exact; year zero, negative/out-of-range years or precision
  overflow use calendar-aware state.
- `julian`: calendar-aware only; no storage conversion to Gregorian.
- `noleap`/`365_day`: canonical `365_day`; no Feb 29.
- `all_leap`/`366_day`: canonical `366_day`; Feb 29 every year.
- `360_day`: twelve 30-day months; `2001-02-30` is valid and was preserved.
- `none`: perpetual/no annual cycle. Preserve, allow only explicitly meaningful
  raw-offset ordering, and reject ordinary year/month/season climatology,
  anomaly and trend until experiment semantics are supplied.
- `utc`: initial preserve-only tier. Operational support requires a versioned
  authoritative IERS leap-second table, rejection before 1972 and beyond the
  table/current instant, second-based arithmetic, and exact `23:59:60` support.
- `tai`: initial preserve-only tier. Future order/interval uses continuous
  atomic seconds from its valid domain; UTC conversion is explicit and
  leap-table-dependent.
- custom: preserve `month_lengths`, `leap_year`, `leap_month` and a canonical
  definition identity. Initial runtime is preserve-only; a future native
  definition-driven engine is required.

Support tiers are: TIER-1-CURRENT for exact existing base behavior;
TIER-2-CALENDAR-AWARE for Julian/365/366/360 and non-base-representable civil
cases; TIER-3-PRESERVE for `none`, `utc`, `tai`, and custom calendars until
their explicit operational contracts are implemented; unknown declarations
are preserve-only.

## Operations contract

Ordering/equality is deterministic within the same canonical calendar,
calendar definition and chronology kind using normalized ordinal/sub-day
values. Cross-calendar comparison is unavailable and errors unless a future
explicit conversion operation supplies scientifically defined semantics.

Exact selectors accept a compatible canonical object, calendar-valid string,
component list, exact numeric offset, or Date/POSIXct when representable.
Nearest and closed-range selectors require the same chronology and use its
elapsed metric; nearest ties choose the earlier stored coordinate. Thus a
360-day selector may validly be `2001-02-30` without passing through `as.Date()`.

Year/month/day/season grouping derives calendar components, never Gregorian
format strings. Seasons require an explicit month-to-season definition.
Climatological time is first-class and is identified by chronology kind plus
the CF `climatology` link, bounds and cell methods; its coordinate midpoint
alone does not define a bin. Anomaly alignment requires identical calendar and
group definitions.

Trend uses an explicit elapsed metric. Fixed SI seconds/days are safe for
non-leap-second calendars; fixed 360/365/366 calendars can expose their exact
year duration. `standard`/Gregorian year remains a duration choice, not a
universal calendar year. UTC uses leap-aware SI intervals; TAI uses continuous
atomic seconds. The current 31556952-second public `year` behavior is preserved
until B5 separately changes a calendar-aware path.

Tables must be round-trippable: base cases retain Date/POSIXct; calendar-aware
cases expose components/value plus explicit calendar, chronology and schema
metadata, never fake Gregorian timestamps. Printing includes the calendar,
for example `2001-02-30 [360_day]`. Plots use a formatted ordinal/categorical
axis adapter or reject explicitly; no base date scale is applied falsely.

## Time units and precision

Seconds, minutes, hours and days are fixed elapsed units; prefixed seconds may
be supported after precision tests. Negative and fractional offsets are valid.
Calendar and reference-datetime offset are separate. Raw origin text and offset
are preserved; interpreted instants normalize deterministically without using
machine timezone or locale.

UDUNITS month is a fixed `year/12` duration and UDUNITS year is a fixed duration;
neither means “advance a calendar month/year.” Calendar months are component
bins, and climatological months are recurring statistical intervals described
by climatology bounds and cell methods. These concepts never alias implicitly.

## WOA23 evidence and A3B-003

The governed fixture has one raw time value `4614`, units `months since
1955-01-01 00:00:00`, no calendar (therefore CF default `standard`),
`climatology=climatology_bounds`, bounds `4212, 5028`, and cell methods `time:
mean within years time: mean over years`. Literal CFtime/UDUNITS duration
rendering gives 2339-07-01 with bounds 2306-01-01 to 2374-01-01. Treating 4614
as calendar months is not normatively implied. The metadata clearly identifies
a climatological statistic, but does not uniquely justify an ordinary timestamp
or provider-specific month reinterpretation.

Selected policy: preserve raw values/units/bounds/cell methods and refuse
ordinary chronological decoding until B5 plus provider evidence defines a safe
climatological mapping. `A3B-003` remains **OPEN — architecture resolved,
runtime implementation/provider ambiguity pending**; it is not runtime-closed.

## Engine evidence

`CFtime` 1.7.3 was installed only in an isolated artifact library. It correctly
constructed and formatted the required standard, proleptic, Julian, idealized,
none, UTC and TAI cases; it handled negative/fractional offsets, the reform
transition, a known UTC leap second, and deterministic reconstruction. It does
not accept the probed explicit custom calendar specification. Direct R6 object
serialization was not structurally identical, while the oceancube plain state
round-tripped identically and reconstructed the same engine timestamps.

Engine weighted totals are base R 140, CFtime 206, native oceancube 210, PCICt
102. Native arithmetic owns validation, identity, custom definitions and simple
ordinal operations. CFtime is the preferred optional/reconstructable adapter
for supported complex arithmetic and an independent oracle. Base R remains the
TIER-1 engine. PCICt is rejected because its narrow calendar coverage and
cross-calendar remapping conflict with the no-coercion rule.

Engine name/version belongs in diagnostics/software provenance, not scientific
coordinate identity. UTC results additionally require a leap-table identifier.

## Prototype results

`b4-prototype.R` produced `B4_PROTOTYPE: PASS` with:

- 360-day `2001-02-30` preserved exactly;
- noleap `2000-02-28 -> 2000-03-01`;
- all-leap `2001-02-29` present;
- Julian 1900 leap day present without Gregorian evaluation;
- standard `1582-10-04 -> 1582-10-15` transition;
- negative and fractional offsets;
- UTC `2016-12-31T23:59:60` probe;
- continuous TAI ordering;
- exact plain-state serialize/unserialize and engine reconstruction;
- explicit custom 30-day month rollover in plain native prototype.

## Provenance and determinism

Provenance V1 currently records source/current class, timezone, calendar, count,
start and end. A future compatible schema revision needs representation kind,
chronology kind, canonical calendar/custom-definition identity and precision.
It must not duplicate the time array or raw CF tree. B4 records these gaps but
does not modify V1.

Canonical output is independent of system timezone, locale, wall clock and
random identifiers. Plain state survives serialize/unserialize and
saveRDS/readRDS exactly. External engine objects are reconstructed, used, and
discarded.

## Decision and staging

- `DEC-023`: APPROVED — HYBRID CALENDAR-AWARE TIME MODEL.
- `A1-002`: PARTIALLY-CLOSED; representation architecture is closed, runtime
  calendar/static/decoder work remains.
- `A3B-001`: CLOSED.
- `A3B-002`: OPEN.
- `A3B-003`: OPEN; architecture resolved, runtime/provider adjudication pending.
- `DEC-015`: APPROVED / ACTIVE.
- `0.3.0-A`: COMPLETE/CERTIFIED.
- `0.3.0-B`: IN PROGRESS; B4 complete after certification.
- Gate B: UNSATISFIED.

The single next subphase is **0.3.0-B5 — CF TIME AND CALENDAR ENGINE
IMPLEMENTATION**. B4 does not execute it.

## Package certification

The unchanged package suite completed with 65 files, 636 cases and 5,196
expectations in 899.000 seconds: zero failures, errors, warnings or skips. The
Windows certification process explicitly used
`English_United States.utf8`; an initial inherited invalid `C.UTF-8` locale was
reproduced as an environment-only Unicode-path failure and the affected test
passed in isolation and in the complete deterministic rerun.

Direct `R CMD build .` reproduced the known Codex `.git/refs/codex/turn-diffs`
copy defect. The contract-authorized clean-snapshot fallback built
`oceancube_0.2.0.9000.tar.gz`. `R CMD check --no-manual` on that tarball ended
`Status: OK` with zero errors, warnings and notes. Restricted-network index
lookup messages were environmental and the dependency phase itself ended OK.

## Evidence index

- `b4-current-time-consumers.csv`: runtime assumption audit.
- `b4-calendar-matrix.csv`: calendar taxonomy and disposition.
- `b4-representation-options.csv`: weighted representation scorecard.
- `b4-engine-options.csv`: separate arithmetic-engine scorecard.
- `b4-operation-calendar-matrix.csv`: selection/grouping/analysis/table policy.
- `b4-woa-time-analysis.csv`: four-way WOA interpretation evidence.
- `b4-cftime-probe.csv`: exact CFtime 1.7.3 results and limitations.
- `b4-provenance-time-gap.csv`: future provenance requirements.
- `b4-prototype.R`: executable plain-state/reconstruction prototype.
