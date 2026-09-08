# oceancube 0.3.0 Git object cleanup audit

## Result

**PASS**

Standard Git maintenance removed obsolete, backed-up object history without
changing any reachable commit, branch, remote-tracking reference, tag, tracked
package file, public export, or package version.

## Size comparison

| Component | Before | After | Recovered |
|---|---:|---:|---:|
| Repository total | 738.34 MB | 36.06 MB | 702.28 MB |
| `.git` | 725.25 MB | 22.95 MB | 702.30 MB |
| Working tree | 13.10 MB | 13.10 MB | 0.00 MB |

The small difference between repository and `.git` recovery is rounding plus
the addition of this repository-only evidence. Before cleanup, reflog-only
objects occupied 697.27 MB and fully unreachable objects occupied 1.58 MB.
Packing the reachable object database accounts for the additional reduction.

## Pre-cleanup object database

```text
count: 3032
size: 725.08 MiB
in-pack: 0
packs: 0
size-pack: 0 bytes
prune-packable: 0
garbage: 0
size-garbage: 0 bytes
```

The dominant obsolete object was blob
`87ee090165157eb5dc81f1a9fa645b6831d06e48`, with an uncompressed size of
1,604,344,656 bytes and a loose-object disk size of 731,108,686 bytes. It was
present only in an abandoned May 2026 history retained by the `HEAD` reflog.

## Reflog-only commit inventory

| SHA | Commit date | Subject | Reachable from branch/tag | Reflog only | Contains large blob |
|---|---|---|---|---|---|
| `190487ac7f535396fa91c8ad9ef047a9fe804ed6` | 2026-09-05T23:44:39-05:00 | feat: add renderer-neutral hovmoller visualization | no | yes | no |
| `262770e48e7ee4148024413d919f1ed41f4fe884` | 2026-05-11T09:01:56-05:00 | Initial OceanCube package structure | no | yes | no; removes it |
| `3cb62f5716341782ba74d42bae9c086b9d7dca12` | 2026-05-10T12:28:02-05:00 | clim_day | no | yes | yes |
| `c782ee6f12f1b671824d4be5697b36a82660ec41` | 2026-05-10T12:12:08-05:00 | Initial commit | no | yes | yes |
| `da4eb79e3c066b8b4897ea4d2059981fb0a921cf` | 2026-09-05T23:12:21-05:00 | feat: add renderer-neutral hovmoller visualization | no | yes | no |

All five commits were tested independently with `git merge-base
--is-ancestor` against `HEAD`, `main`, `dev-0.3.0`, `origin/main`,
`origin/dev-0.3.0`, `v0.1.0^{commit}`, and `v0.2.0^{commit}`. All 35 checks
returned false.

## External backup

```text
Path:
D:\.packages\.oceancube-backup\oceancube-reflog-cleanup-20260908.bundle

Size:
742164093 bytes (707.783 MB)

SHA-256:
db4c1d4594b1eb0c8f2f7c383d6545a8101bfda029d47db2c3ac8b0d75e47457

git bundle verify:
PASS - complete history, SHA-1 object format
```

Temporary refs under `refs/backup/reflog-cleanup/` were created for all five
reflog-only commits. The verified bundle listed all five refs. Only after the
bundle passed verification and hashing were those temporary refs deleted.
No temporary backup ref remains in the repository.

## Reflog action

Dry run and execution used the same deliberately narrow policy:

```text
git reflog expire --dry-run --verbose \
  --expire=never \
  --expire-unreachable="2026-06-01T00:00:00-05:00" \
  HEAD

git reflog expire --verbose \
  --expire=never \
  --expire-unreachable="2026-06-01T00:00:00-05:00" \
  HEAD
```

This targeted only unreachable `HEAD` reflog entries older than June 2026. The
execution pruned five reflog records: the abandoned initial commit, the
abandoned `clim_day` commit, two rename records for that history, and the
abandoned commit that removed the large NetCDF. All reachable-history entries
were retained. The newer September amend predecessors remain available through
their reflogs and are also included in the bundle.

## Garbage collection

The only collection command was:

```text
git gc
```

Neither `--aggressive` nor `--prune=now` was used. After standard collection,
the obsolete large blob no longer exists in the object database.

Post-cleanup object database:

```text
count: 0
size: 0 bytes
in-pack: 2820
packs: 2
size-pack: 22.78 MiB
prune-packable: 0
garbage: 0
size-garbage: 0 bytes
```

## Reference integrity

| Reference | Before | After |
|---|---|---|
| `HEAD` | `a85180b203dff8755423b78fdf3e5e68a0c87cda` | identical |
| `dev-0.3.0` | `a85180b203dff8755423b78fdf3e5e68a0c87cda` | identical |
| `main` | `40bf4b16755ffb48c08eaf22e0678ac7cf683040` | identical |
| `origin/dev-0.3.0` | `c4bf153f72b93c9a3b6c8fb26d655ed704f93e32` | identical |
| `origin/main` | `40bf4b16755ffb48c08eaf22e0678ac7cf683040` | identical |
| `v0.1.0^{commit}` | `93d2a79b11a6ae7622443ae068e6e2a2709c9324` | identical |
| `v0.2.0^{commit}` | `d83008066ba3b1f3ea8df3e7ca3001d472f20308` | identical |
| `backup/c-exit-blocked-4c491` | `4c4914531932db74c1e8d7c85f5fc457cc6d67da` | identical |

The complete `git show-ref` listing was identical before and after collection.
The branch remained nine commits ahead and zero behind
`origin/dev-0.3.0` before creation of the evidence commit.

## Object integrity

`git fsck --full` exited successfully. It reported no corrupt or missing
objects and no dangling commits. It listed 18 dangling blobs and 14 dangling
trees retained by normal GC policy; these are non-fatal diagnostics and occupy
only the compact 22.78 MiB packed object database.

## Package revalidation

| Contract | Before | After |
|---|---:|---:|
| Public exports | 49 | 49 |
| Version | 0.2.0.9000 | 0.2.0.9000 |
| Tarball | `oceancube_0.2.0.9000.tar.gz` | same |
| Tarball size | 1,796,072 bytes | 1,796,078 bytes |
| Tar entries | 295 | 295 |
| Extracted files | 283 | 283 |
| Repository-only entries | 0 | 0 |

The six-byte archive-size difference is generated metadata. File-by-file
SHA-256 comparison of both extracted source packages found one raw difference:
the automatic `Packaged:` timestamp in `DESCRIPTION`. After normalizing only
that generated field, all 283 extracted files were byte-identical. Both
tarballs contained `DESCRIPTION`, `NAMESPACE`, `R/`, `man/`, `tests/`,
`vignettes/`, `inst/`, `LICENSE`, and `README.md`, and excluded `.git`,
`.github`, `artifacts`, `data-raw`, `dev`, `docs`, `handbook`, `auxdata`,
`oceancube.Rcheck`, project files, and local caches.

## Certification

- Reachable history: unchanged
- Branches and tags: unchanged
- Remote-tracking references: unchanged
- Tracked package source: unchanged
- Public API: unchanged at 49 exports
- Package version: unchanged at 0.2.0.9000
- External backup: verified
- Git integrity: PASS
- Source-package integrity: PASS
- Final classification: **PASS**
