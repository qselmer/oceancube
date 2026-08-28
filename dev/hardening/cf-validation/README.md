# OCEANCUBE 0.3.0-B3 — CF supported-subset interpretation and validation

Status: **runtime implementation and full local package certification
complete; remote CI pending exact-SHA push authorization**.

Normative references are CF Metadata Conventions 1.13 and its Conformance
Requirements and Recommendations. The only public claim authorized by this
work is: **CF-aware; supports a documented subset of CF 1.13**. A source
summary `PASS` is not full CF conformance or a complete validator result.

## Implemented architecture

The B2 immutable `cf$source` tree is scanned once, then paths and simple links
are resolved, roles are classified, the B3 supported subset is interpreted and
validated, deterministic diagnostics are built, and only then is the current
cube resolved. The internal subset definition and validator are version
`1.0.0`, reference `CF-1.13`, and use validation scope
`oceancube_supported_subset`.

Diagnostics separate status (`PASS`, `FAIL`, `DEFERRED`, `NOT_APPLICABLE`),
severity (`ERROR`, `WARNING`, `INFO`, `DEFERRED`), and rule kind
(`REQUIREMENT`, `RECOMMENDATION`, `OCEANCUBE-SAFETY`). They remain plain-R CF
metadata at `x$metadata$cf$diagnostics`; they are not Provenance V1 operations
or `x$qa`. Source diagnostics are nonblocking by default. Existing required
current-axis and canonical-cube safety failures remain blocking.

## Coverage boundary

B3 structurally validates path/dimension identity, coordinate classification,
shared axis evidence, simple `coordinates`, `ancillary_variables`, `bounds`,
`climatology`, `cell_measures`, `grid_mapping`, and `formula_terms` links,
bounded flag and missing/range metadata, and a simple `cell_methods` clause.
It explicitly defers bounds/climatology payload checks, unit and standard-name
tables, mapping-name tables, complex cell-method grammar, formula semantics,
extended grid mapping, flag application, and scientific payload validation.
No validator path reads variable arrays.

## Fixture evidence

The deterministic valid temporary fixture has 101 source rule results: 69
pass, 0 fail, and 32 deferred. Thirteen focused invalid fixtures each produce
exactly one expected stable ERROR rule. No binary fixture is committed.

OISST reports source subset PASS (39 checked, 30 pass, 9 deferred) and remains
numerically exact across eager and collected paths without a `zlev` override.
ETOPO reports PASS (19 checked, 14 pass, 5 deferred) and `NOT_DECLARED`, but
still cannot construct a temporal cube. WOA reports PASS (89 checked, 64 pass,
1 nonblocking recommendation failure, 24 deferred), preserves climatology and
the complete complex `cell_methods`
text, and still rejects `months since` decoding.

Eager and deferred source trees, supported-subset contracts/summaries, and
source diagnostics are identical for the same file and selection.
`cube_collect()` preserves metadata exactly without revalidation. Selection
updates current interpretation; aggregation and other meaning-changing
operations keep the source result while current semantics remain
`DERIVATION_PENDING`.

## Governance

- `A1-002`: PARTIALLY-CLOSED; decoder convergence remains.
- `A3B-001`: CLOSED.
- `A3B-002`: OPEN; B3 scan support is not a static cube contract.
- `A3B-003`: OPEN; B3 does not decode duration-month coordinates.
- `DEC-015`: APPROVED — HYBRID ACTIVE SUPPORTED-SUBSET ENGINE.
- `DEC-023`: OPEN.
- Gate B: UNSATISFIED; vertical science remains unauthorized.
- API: 39 exports; no new export, signature, dependency, version, NAMESPACE,
  scientific-result, time, calendar, or static-field change.

## Local package certification

- full suite: 65 files, 636 cases, 5,196 expectations, 819.870 seconds;
  0 failures, 0 errors, 0 warnings, 0 skips;
- direct `R CMD build .` reproduced the known
  `.git/refs/codex/turn-diffs` copy defect; the approved clean-snapshot fallback
  built `oceancube_0.2.0.9000.tar.gz` successfully;
- `R CMD check --no-manual`: `Status: OK`, 0 errors, 0 warnings, 0 notes;
- isolated installed-package smoke: PASS at version `0.2.0.9000` with 39
  exports, OISST eager/deferred source diagnostic parity, exact collect
  metadata, and exact numerical parity;
- GitHub Actions remains unrun until the bounded B3 commit exists and its exact
  SHA is authorized for a non-force push to `origin/dev-0.3.0`.

## Evidence index

- `b3-supported-subset.csv`: authoritative preservation/interpretation/
  validation/defer boundary.
- `b3-rule-matrix.csv`: stable rules, CF sections, authority and blocking.
- `b3-real-fixture-results.csv`: OISST/ETOPO/WOA source results.
- `b3-invalid-fixture-results.csv`: focused failure cases.
- `b3-eager-deferred-parity.csv`: shared-reader semantics and collect boundary.
- `b3-diagnostic-contract.csv`: canonical diagnostic fields and vocabularies.
- `b3-regression-matrix.csv`: protected package contracts.

## Next boundary

The single recommended next subphase is **0.3.0-B4 — CF TIME AND CALENDAR
REPRESENTATION DECISION**. WOA/climatology and all later temporal operations
need one safe calendar model before runtime expansion; the static optional-time
contract can follow without forcing an unresolved temporal representation into
the canonical cube. B4 is not implemented here.
