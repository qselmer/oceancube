# Auxdata retention decision for oceancube 0.2.0

This review covers the two tracked legacy workspaces in `auxdata/`. It records
their identity and enough structural metadata to unblock the 0.2.0 functional
work without exposing their contents or claiming that they are reproducible.

## Decision

Both files are **retain-temporarily**. Neither file has an active reference in
package code, tests, documentation, configuration, workflows, or generation
scripts. The only references found are historical inventory and relocation
records under `docs/roadmap/`.

The absence of active consumers is not sufficient for safe removal. No source
dataset, generation script, owner, license, or approved external archive was
identified for either file. Consequently, reproducibility and the absence of
unique information cannot be demonstrated. The files remain tracked and are
not renamed.

## File review

### `auxdata/PER_ADM0.RData`

- Contains one object, `PER_ADM0`, with class `SpatialPolygonsDataFrame` and a
  serialized in-memory size of 1,648,000 bytes.
- Its name and class indicate a legacy administrative-boundary dataset for
  Peru. This is potentially useful spatial information.
- No active code, test, documentation, configuration, workflow, or generator
  reference was found.
- Provenance, source date, owner, license, and regeneration procedure are
  unknown.
- Risk: deletion could discard a unique or legally constrained boundary
  dataset. Decision: **retain-temporarily**.

### `auxdata/layer_sp.Rdata`

- Contains one object, `layer_sp`, with class `data.frame` and a serialized
  in-memory size of 1,360 bytes.
- Its apparent purpose is a small legacy spatial-layer descriptor, but the
  structural inventory alone does not establish its semantics.
- No active code, test, documentation, configuration, workflow, or generator
  reference was found.
- Provenance, source date, owner, license, and regeneration procedure are
  unknown.
- Risk: despite its small size, uniqueness and reproducibility are not proven.
  Decision: **retain-temporarily**.

## Why this does not block 0.2.0

The two files are not part of any detected runtime, test, build, check, or
documentation path. Retaining them preserves all information while allowing
the public validation and inspection API to proceed. A later data-governance
review should identify their source and license, designate an owner, and either
create a reproducible canonical source or approve a checksum-verified archive
before reconsidering removal.
