# OCEANCUBE 0.3.0-B7 — CF vertical semantics and Gate B

B7 implements the versioned current vertical descriptor documented in
`inst/architecture/oceancube-cf-vertical-v1.md`. It reuses the B2 scanner, link
registry, and shared axis resolver; no second resolver or source-metadata tree
was added.

The certified runtime subset is dimensional metric ocean depth with finite
strictly monotonic coordinates and explicit valid CF bounds. Geometry supports
the existing metre/kilometre conversion only. Height, pressure, generic
dimensionless, and recognized Appendix-D parametric axes are preserved and
classified but cannot drive metric thickness or volume. Formula terms receive
metadata-only structural interpretation and are never evaluated.

The positive real fixture is the January (`t01`) WOA23 1-degree all-decades
temperature/salinity product. The exact official full files are pinned by
SHA-256. NOAA states that the data are openly available to the public and asks
for acknowledgment; the fixture retains the volume references and requires the
dataset and temperature/salinity DOI citations. The deterministic script creates
a 9 x 12 x 6 x 1 subset without temporal or vertical reinterpretation. A second
run was byte-identical.

The fixture preserves raw time 396.5 months since 1955-01-01 and climatology
bounds 0,805; the B5/B6 engine interprets it safely. Depth is
0,10,20,50,100,200 m, positive down, with provider bounds and non-contiguous
coverage. `read_nc()`, `cube_open()`, `cube_collect()`, thickness, and volume
pass with eager/deferred semantic parity. Geometry accesses coordinate/bounds
metadata only and does not read `t_an` or `s_an`.

The annual WOA `t00` fixture remains a required negative regression and
`A3B-003` remains OPEN-RECLASSIFIED. ETOPO remains metadata-scannable but its
static no-time form remains rejected (`A3B-002` OPEN). OISST remains a valid
explicit singleton Z axis but has no bounds and therefore cannot define layer
thickness. `A3B-001` remains CLOSED.

`layer_mean()` was audited but not numerically changed. Finding B7-001 records
that its inferred centre edges are legacy behavior and not certified for the
C-series physical vertical contract. Profile, section, and transect
visualizations display stored source-coordinate values; their plotting reversal
is presentation only.

Gate B is SATISFIED for the narrow certified subset. Static fields are not a
vertical-science prerequisite, and the safely rejected annual WOA product does
not block a separate positive governed product. This does not mark every
0.3.0-B interoperability item complete.

Final local certification executed 68 test files, 662 cases, and 5,406
expectations in 925.64 seconds with zero failures, errors, warnings, or skips.
The direct build hit the known Codex `.git/refs/codex/turn-diffs` copy defect;
the contracted clean-snapshot fallback built
`oceancube_0.2.0.9000.tar.gz` with SHA-256
`c905cb1d177fdbfbaad1487b31531f3c4aa76c88881532ed19d26470bc365803`.
`R CMD check --no-manual` returned Status OK with 0 errors, 0 warnings, and
0 notes. The isolated installed-package public smoke passed all required
positive and negative real-data cases with 39 exports.
