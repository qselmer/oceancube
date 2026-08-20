# oceancube — canonical post-0.2.0 roadmap

This directory is the design gate for oceancube development after the stable
0.2.0 release and through the provisional 1.0.0 scientific contract. It is an
architecture and provenance record, not an implementation plan that authorizes
code changes by itself.

## Status

- Canonical roadmap revision: v1
- Stable release: oceancube 0.2.0
- Stable tag: `v0.2.0`
- Public API baseline: 38 exports
- Runtime changes in this roadmap phase: none

The historical `docs/roadmap/oceancube-v0.2.0-scope.Rmd` and the audit,
decision, relocation, and auxdata directories remain immutable evidence of how
0.2.0 was planned and reconciled. They are not the current source of future API
commitments. In particular, their approximately 114 proposed functions must not
be interpreted as 114 future exports.

## Authoritative files

- `oceancube-roadmap-v1.Rmd` is the human-readable architecture, staged
  roadmap, dependency graph, gates, boundary, and technical-debt register.
- `roadmap-provenance.csv` is the machine-readable source of roadmap work
  packages and their provenance.
- `roadmap-decisions.csv` separates approved architecture from open, deferred,
  and superseded decisions.

Proposals become implementation work only after their prerequisites and the
relevant roadmap gate have been satisfied. A row marked `PROPOSED` is not a
public API promise. Candidate function names remain provisional until a
separate API and methodological review approves them.

## Change control

Future roadmap changes should update the provenance matrix first, reference an
existing or new decision ID when maintainer judgment is required, preserve
unique IDs, and keep proposals distinct from approved decisions. Runtime
contracts continue to live in source, tests, and roxygen documentation; this
area records why later changes may be authorized.
