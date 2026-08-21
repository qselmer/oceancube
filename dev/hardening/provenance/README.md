# A4a architecture and A4b1 core-engine evidence

## Result

A4a selects **C — hybrid**: a flat ordered primary history plus a flat registry
of secondary lineages referenced by deterministic local operation/entity IDs.
The canonical design is
`inst/architecture/oceancube-provenance-v1-design.md`. This directory contains
the machine-readable audit, candidate comparison, field contract, operation
mapping, and legacy migration design used to reach that choice.

A4a changed no package runtime. A4b1 now adds the internal V1 engine and focused
tests while leaving every runtime producer on its legacy shape. A4b2 is the
next producer-by-producer integration phase.

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

## Status

`A1-003` remains `PARTIALLY-CLOSED`: the design, core engine, legacy
normalization and executable tests now exist, but runtime producer rollout is
still pending A4b2/A4b3. `DEC-019` remains APPROVED and is backed by the exact
V1 design plus executable A4b1 evidence. 0.3.0-A and Gate B remain incomplete.
