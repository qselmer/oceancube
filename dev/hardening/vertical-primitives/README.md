# OCEANCUBE 0.3.0-C1 — vertical primitives

C1 implements DEC-030 without changing the public API, signatures,
dependencies, NAMESPACE, or version. One internal support engine serves
`cube_layer_thickness()`, `cube_cell_volume()`, and `layer_mean()`.

The certified path is limited to explicit valid m/km ocean-depth bounds. It
uses exact interval clipping and union coverage, requires full coverage, rejects
partial coverage and gaps before payload reads, and returns missing for zero
coverage. Storage order is preserved. CF source metadata is immutable; current
derived bounds and provenance V1 are truthful.

The historical centre-derived mean remains numerically unchanged, silent, and
uncertified. Accordingly B7-001 is PARTIALLY-CLOSED. Gate B remains SATISFIED
only for the DEC-029 subset; pressure/height conversion, parametric evaluation,
integration, interpolation, static fields, and named diagnostics remain absent.

The CSV files record the supported subset, analytic results, provider evidence,
regressions, findings, and certification outcome. The architecture contract is
`inst/architecture/oceancube-vertical-primitives-v1.md`.
