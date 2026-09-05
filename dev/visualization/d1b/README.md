# OCEANCUBE 0.3.0-D1B evidence

D1B implements the internal renderer-neutral `oceancube_viz_data` v1.0.0
contract and routes the five existing public `viz.*` functions through bounded
prepare/render stages. It adds no export, dependency, renderer, visualization
capability, scientific transformation, or version change.

The parity oracle is an isolated installed package produced from the clean
starting SHA `079b353a552ee941d3132baa3e376c552fa71cc1` with `git archive` and
`R CMD INSTALL`. `parity-worker.R` is executed in separate R processes against
that library and the installed D1B candidate. `compare-parity.R` compares exact
public signatures, plot data, ggplot build data, labels, layers, mappings,
scales, coordinates, public `oceancube_*` attributes, representative errors,
and warnings. Private source copying is not used as an oracle.

The prepared-data validator, serialization, backend-independence, source-file
removal, display/science separation, and zero-renderer-I/O proofs are package
tests. NetCDF read counts use existing extraction QA. Gallery scripts use only
installed public APIs and deterministic memory data; the five PNGs are governed
baselines pending maintainer visual review. Review is required before D2 makes
an intentional appearance change, but is not a blocker for this internal
refactor.

`oceancube_viz_scene` is only an architectural schema in D1B. No runtime scene
class or 3-D renderer is implemented. D2 and later visualization capabilities
remain outside this evidence set.

The final evidence also records exact runtime/API differences, regression
gates, gallery dimensions and SHA256 values, clean-source build and installed
package checks, and the local certification state. Gallery images remain
generated baselines pending maintainer review; D1B makes no intentional
appearance change.
