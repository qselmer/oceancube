# A4 Provenance V1 architecture, implementation, and global certification

## Result

A4a selects **C — hybrid**: a flat ordered primary history plus a flat registry
of secondary lineages referenced by deterministic local operation/entity IDs.
The canonical design is
`inst/architecture/oceancube-provenance-v1-design.md`. This directory contains
the machine-readable audit, candidate comparison, field contract, operation
mapping, and legacy migration design used to reach that choice.

A4a changed no package runtime. A4b1 added the internal V1 engine and focused
tests. A4b2 migrated the linear core cube lifecycle. A4b3a migrated the
temporal and multi-input engines plus their delegating compatibility wrappers.
A4b3b migrates extraction, transect, polygon weights, layer mean, coast
distance, and stock crop. A4-EXIT certifies the resulting contract globally.
Every identified runtime producer emits V1 and `A1-003` is closed.

## Current audit

Source inspection found more provenance producers than the original nine-class
A1 matrix. Current outputs use at least four incompatible families:

1. recursive `parent + operation-key` cube records;
2. `.make_provenance()` environment/date/function wrappers;
3. standalone table attributes with copied `source_provenance` or no parent;
4. internal NetCDF descriptors containing normalized absolute paths and file or
   construction timestamps.

The OISST governed fixture was used only for a read-only measurement pipeline:

```text
read_nc → cube_crop → cube_aggregate_time → cube_climatology
        → cube_anomaly → cube_trend
```

| Stage | Parent depth | Serialized provenance bytes | List nodes |
|---|---:|---:|---:|
| read_nc | 0 | 1,838 | 5 |
| crop | 1 | 5,413 | 13 |
| aggregate | 2 | 6,851 | 16 |
| climatology | 3 | 9,055 | 20 |
| anomaly | 4 | 17,599 | 40 |
| trend | 5 | 19,453 | 43 |

Anomaly almost doubles provenance size because it embeds both source and
climatology parent trees. The final record is 10.6 times the serialized size of
the ingestion record. The observed pattern supports bounded flat history plus
one-copy secondary lineage registration; it does not justify a full runtime
entity/activity graph.

## Special findings

- Wall-clock fields include `.make_provenance()$date`, `sliced_utc`,
  `cropped_utc`, `extracted_utc`, `collected_utc`, `masked_utc`,
  `constructed_utc`, and file modification time. They are useful for debugging
  but nondeterministic and excluded from semantic equivalence.
- `.make_provenance()$system` includes host/user information. Canonical V1 must
  not retain usernames, home paths, hostnames, tokens, signed URLs, or secrets.
- Absolute/normalized paths appear in deferred NetCDF construction and read
  metrics. They are optional non-portable locators, never source identity.
- `netcdf_read`, physical reads, amplification, valid-count and coverage arrays,
  cell-fit counts, residual diagnostics, memory, and elapsed time belong in QA.
- Current time lineage is valuable but mixes original decoding and current
  output semantics. V1 separates `time$source` from `time$current` and records
  transformations in history.

## External references

W3C PROV is adopted conceptually: source/derived objects are Entities,
operations are Activities, software is an Agent-like responsibility record,
and input-output links are Derivations. RDF, PROV-O, and formal serialization
are rejected for runtime V1.

CF `history` is treated as a future compact textual export from V1, not as V1
itself. CF variable/grid metadata remains outside operation provenance.

RO-Crate is classified as a future optional packaging/export format for
datasets, scripts, fixtures, workflows, and citations. It is not a dependency
or per-operation representation.

## Files

- `current-shape-audit.csv` — every source-discovered producer family and risk;
- `schema-candidates.csv` — candidates A/B/C/other and selection evidence;
- `field-contract-v1.csv` — normative field-level contract;
- `operation-mapping.csv` — current operation records to V1;
- `migration-matrix.csv` — legacy fields and deterministic destinations.
- `a4b1-migration-results.csv` — executed normalization/privacy disposition;
- `a4b1-field-coverage.csv` — executable coverage of the 75-field contract;
- `a4b1-growth-performance.csv` — serialized growth and helper timing smoke.
- `a4b2-runtime-results.csv` — linear producer, compatibility, real-data,
  serialization, suite, API, and dependency certification;
- `a4b2-growth.csv` — 1/3/5/10-operation flat-history serialization evidence.
- `a4b3a-runtime-results.csv` — temporal, multi-input, wrapper, time-kind,
  parity, serialization, privacy, OISST, API, and dependency certification;
- `a4b3a-growth.csv` — representative V1 temporal/multi-input growth against
  the 19,453-byte A4a legacy reference.
- `a4b3b-starting-producer-inventory.csv` — exact six-producer starting state;
- `a4b3b-final-producer-scan.csv` — final legacy-pattern classification;
- `a4b3b-runtime-results.csv` — table, geometry, remaining-cube, parity,
  serialization, privacy, suite, API, dependency, build, and check evidence.
- `a4-exit-producer-scan.csv` — exhaustive final classification of requested
  runtime patterns, compatibility remnants, parsers, timestamps, and aliases;
- `a4-exit-output-matrix.csv` — canonical provenance location and behavior for
  all seven runtime output families;
- `a4-exit-qa-boundary.csv` — operation-by-operation diagnostic placement and
  semantic-exclusion audit;
- `a4-exit-open-findings.csv` — A4 and 0.3.0-A blocking classification;
- `a4-exit-certification.csv` — consolidated global certification evidence.

## Status

`A1-003` is `CLOSED`: all identified runtime producers emit the common V1
schema and A4-EXIT certifies migration, secondary lineages, serialization,
semantic determinism, privacy, real-data lineage, and cross-output access.
`DEC-019` remains APPROVED and is now IMPLEMENTED/CERTIFIED by executable
A4b1/A4b2/A4b3a/A4b3b/A4-EXIT evidence.
The polygon implementation explicitly enables and restores s2. A4R now gives
coast distance the same state-control guarantee: initial TRUE/FALSE states
produce identical output and semantic provenance, the caller state is restored
on success and error, and the operation truthfully records
`oceancube:s2_coast_distance` version 1. `A4B3B-001` is CLOSED. 0.3.0-A and Gate
B remain incomplete; A5 is next.
