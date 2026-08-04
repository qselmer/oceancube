# oceancube 0.2.0 — Artifact relocation proposal

Non-destructive, reproducible review prepared from the clean ignore-rules baseline.

## File state

- Files examined: 2503
- Tracked files: 221
- Untracked files: 0
- Ignored files: 2282
- Missing historical inventory paths: 62

## Relocation review

- Relocate-later candidates reviewed: 142
- Verified relocation candidates: 0
- Leave-local decisions: 140
- Merge-later decisions: 0
- Retain decisions: 0
- Review decisions: 2
- Safe B01 files: 0

## Removal review

- Remove candidates reviewed: 8
- Retain: 0
- Archive: 0
- Merge-later: 0
- Review: 0
- Remove-after-approval recommendations: 8

No movement or deletion is approved in this phase.

## Reference analysis

- Candidates with non-roadmap references: 0
- Reference rows requiring future updates: 0
- Dynamic-reference risk targets: 150

## Human approvals

- Candidate-level approvals required: 10
- Approve provenance, ownership, license, checksum and archive retention for the two tracked auxdata RData files.
- Approve or reject exact removal of eight reproducible untracked caches after a fresh reference scan.

## Outputs

- 01-file-state.csv
- 02-relocation-candidates.csv
- 03-remove-candidate-review.csv
- 04-reference-map.csv
- 05-operational-data-policy.Rmd
- 06-relocation-batches.csv
- 07-path-mapping.csv
- 08-ignore-build-compatibility.csv
- 09-relocation-execution-plan.Rmd
- oceancube-0.2.0-relocation-proposal.Rmd
- generate-relocation-proposal.R

## Recommended next action

Approve or reject B06-remove-candidate-review. B01 is intentionally empty because no tracked relocation is currently safe.
