# OCEANCUBE 0.3.0-D2A technical evidence

D2A implements `viz.hovmoller()` over the D1B renderer-neutral data boundary.
The public capability supports stored time by longitude, latitude, or depth;
all other non-singleton dimensions require explicit selectors and no hidden
reduction is permitted. The internal scale vocabulary and deterministic
ggplot2-native viridis palette foundation are recorded here without changing
the five D1B visual defaults or adding a dependency.

The three gallery candidates were generated from an installed package with
public APIs and no network access. Their automated scientific, object,
serialization, backend, read-bound, and regression checks are evidence of
technical validity only. Named maintainer visual review is still required.

The normative bounded architecture is
`inst/architecture/oceancube-hovmoller-v1.md`.

Final status: **COMPLETE / CERTIFIED**. Maintainer `qselmer` approved the three
exact governed artifacts on 2026-09-06; the decision is bounded to the D2A
contract and those hashes.

The support-geometry correction is recorded in
`d2a-support-geometry.csv`, `d2a-display-footprint.csv`,
`d2a-na-vs-absence.csv`, `d2a-unit-display.csv`, and
`d2a-pre-post-review.csv`. The original image hashes remain as
`PRE_REVIEW_BASELINE`; revised hashes are separate
`POST_REVIEW_FIX_CANDIDATE` evidence.

DEC-043 certifies the bounded Hovmöller 2-D scientific visualization contract.
DEC-044 remains unallocated.
