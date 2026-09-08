# oceancube 0.3.0 repository cleanup plan

Inventory date: 2026-09-07

This plan records the bounded physical-cleanup decision before deletion. No file
was deleted while producing the inventory.

## Verified Git baseline

- `HEAD` and `origin/dev-0.3.0`: `c4bf153f72b93c9a3b6c8fb26d655ed704f93e32`
- `origin/main`: `40bf4b16755ffb48c08eaf22e0678ac7cf683040`
- `origin/main...origin/dev-0.3.0`: `0 behind / 44 ahead`
- working tree at inventory start: clean
- remote branches: `main`, `dev-0.3.0`
- extra local-only branch: `backup/c-exit-blocked-4c491`

The backup branch is not part of the physical-file cleanup. Its retention or
removal must be adjudicated only after `main` has been fast-forwarded, pushed,
and certified.

## Physical cleanup decision

Delete only generated or obsolete targets resolved inside the repository:

- ignored `artifacts/` (3.008 GB; no tracked files);
- ignored `data-raw/fixtures/cache/` (42.47 MB; provider-download cache);
- ignored `oceancube.Rcheck/`;
- ignored source tarballs in the repository root;
- ignored `.RData`, `.Rhistory`, and `.Rproj.user/` state;
- ignored generated documentation and Quarto caches while preserving every
  tracked source under `docs/roadmap/` and `handbook/`;
- tracked legacy `auxdata/` only in a reviewed semantic commit.

`auxdata/PER_ADM0.RData` contains an unused legacy `SpatialPolygonsDataFrame` and
`auxdata/layer_sp.Rdata` contains an unused data frame. Both entered in the
initial package-structure commit, have no consumers, are excluded by
`.Rbuildignore`, and lack documented third-party provenance. Removing them
reduces repository weight and release licensing risk without changing the
package or tests.

## Retention decision

- `R/`, `man/`, `tests/`, `vignettes/`, and currently `inst/` remain package
  content.
- tracked fixture derivation scripts under `data-raw/` remain repository-only.
- tracked `dev/`, `docs/roadmap/`, and handbook sources remain repository-only
  scientific, certification, provenance, and publication evidence.
- the governed visualization benchmark and exact approved gallery PNGs remain
  tracked repository evidence; this cleanup does not regenerate or alter them.

## Safety gate

Before deletion, resolve every target to an absolute path under
`D:\.packages\oceancube`, reconfirm its tracked/ignored state, and preserve all
tracked files except the separately reviewed `auxdata/` removal. After cleanup,
run `git status --short`, recompute repository size, and verify that `R/`, tests,
API, version, and governed evidence are unchanged.

## Execution result

Physical cleanup completed on 2026-09-07 after the safety gate passed:

- checkout size excluding `.git`: approximately 3,085 MB before and 13.04 MB
  after cleanup;
- remaining checkout files excluding `.git`: 897;
- `artifacts/`, `data-raw/fixtures/cache/`, `oceancube.Rcheck/`, `.Rproj.user/`,
  `handbook/.quarto/`, local R state, root tarballs, and ignored generated site
  files: removed;
- tracked removals: exactly `auxdata/PER_ADM0.RData` and
  `auxdata/layer_sp.Rdata`;
- package load: PASS;
- public exports: 49, unchanged;
- version: `0.2.0.9000`, unchanged;
- governed visualization evidence and tracked offline test fixtures: retained.

Generated files removed by this step are not recoverable from the working tree,
but are reproducible from versioned sources. The two tracked `auxdata/` objects
remain recoverable from Git history.
