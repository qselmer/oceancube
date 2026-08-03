# oceancube 0.2.0 — Cleanup decision

Static, non-destructive decision review of the 0.1.0 codebase on dev-0.2.0.

## Count reconciliation

- Previous source-defined functions: 182
- Verified top-level source functions: 182
- Previous internal functions: 155
- Verified internal top-level functions: 155
- Named local nested functions, excluded: 3
- Imported namespace functions: 0
- Registered S3 methods: 4

## Current exports

- Total: 27
- retain: 21
- retain-and-expand: 3
- rename-with-alias: 3
- deprecate-later: 0
- internalize: 0
- review: 0

The four previous review exports are resolved as retain: annual_index, coast_dist, crop_stock and stock_mask.

## Release tiers

- core-0.2.0: 42
- post-core-0.2.x: 62
- future: 10

## Internal functions

- retain-internal: 155
- move-later: 0
- merge-candidate: 0
- rename-internal-later: 0
- review: 0
- remove-candidate: 0

## Files

- Previous audit review rows: 1375
- retain: 178
- relocate-later: 142
- merge-later: 61
- ignore: 2073
- review: 89
- remove-candidate: 8

No deletion is approved. Every remove-candidate requires a new safety check and explicit human approval.

## Human decisions pending

- Exact ignore and build-exclusion rules for Commit 1.
- Exact paths and destinations for any relocation or remove-candidate action.
- Topic-level handbook-to-vignette parity.
- Start of warnings and any removal version for compatibility aliases.
- Scientific semantics for calendar, season, missingness, units, depth and uncertainty.
- Optional backend dependencies and operational-data storage.

## Outputs

- 01-source-function-verification.csv
- 02-count-reconciliation.csv
- 03-current-export-decisions.csv
- 04-public-review-resolution.csv
- 05-deprecation-plan.csv
- 06-api-release-tiers.csv
- 07-internal-function-decisions.csv
- 08-file-cleanup-decisions.csv
- 09-documentation-policy.Rmd
- 10-module-migration-plan.csv
- 11-cleanup-execution-plan.Rmd
- oceancube-0.2.0-cleanup-decision.Rmd
- generate-decisions.R

## Next recommended phase

Execute only the first approved cleanup commit: align .gitignore and .Rbuildignore using an explicit, human-approved subset of the file decision table.
