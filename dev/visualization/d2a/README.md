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

Status: **IMPLEMENTED / TECHNICALLY VALIDATED; VISUAL REVIEW PENDING**.

No DEC-043 is created by D2A. DEC-043 remains the next available decision for
the later human visual-review and final-certification step.
