# oceancube CF vertical semantics v1

Status: approved and implemented for 0.3.0-B7. Normative reference: CF Metadata
Conventions 1.13, especially sections 4.3, 7.1, 7.1.4, and Appendix D.

## Scope and authority

The authority order is CF 1.13, the source NetCDF declarations, this bounded
oceancube contract, and only then external implementation behavior. The engine
is CF-aware and implements a documented subset; it is not a complete UDUNITS or
CF conformance engine.

The B2 scanner remains the sole source-metadata authority. It preserves
attributes and relationships under `metadata$cf$source`. The shared B2 axis
resolver chooses the vertical dimension governing the selected variables.
Only after that resolution does B7 construct
`metadata$cf$current$vertical`. Source metadata are never rewritten.

## Versioned current descriptor

The plain-R descriptor has schema `oceancube_cf_vertical` version `1.0.0` and
lives at `x$metadata$cf$current$vertical`. It records source axis and coordinate
values, semantic kind, runtime and geometry statuses, raw and normalized units,
raw and canonical positive direction, standard name, axis attribute, coordinate
storage order, CF bounds target and values, formula-term status, metric scale,
contiguity, surface status, and typed diagnostics. It contains no path, backend
handle, scientific payload, timestamp, host name, or external pointer.

`cf$source` states what the file declared. `cf$current$vertical` states what the
current selected cube can safely interpret and execute.

## Taxonomy

`DEPTH_LENGTH` is a dimensional physical depth. The executable subset requires
a bounded metre/kilometre spelling, a known positive direction, finite 1-D
coordinates, and strict monotonicity without duplicates. `standard_name=depth`
is strong semantic evidence. A standard-name/positive contradiction is recorded
as a CF recommendation warning and independently blocks oceancube geometry.

`HEIGHT_LENGTH` is a dimensional height above a reference. It is preserved and
interpretable but is never silently negated or converted to ocean depth.

`PRESSURE` recognizes a bounded list of pressure spellings: Pa, hPa, kPa, bar,
mbar/millibar, dbar/decibar, and atm. Missing `positive` defaults to down for
recognized pressure units. Pressure differences are not geometric metres and
cannot drive thickness or volume. No pressure-to-depth model is implemented.

`PARAMETRIC` recognizes the CF Appendix-D standard names implemented in the
classifier. Formula-term links are structurally checked through the B2 link
registry, including the presence and term-key consistency of boundary
`formula_terms` when a parametric coordinate has bounds. Formulae are not
evaluated and physical coordinates are not synthesized.

`DIMENSIONLESS_GENERIC` preserves dimensionless levels without treating them as
physical depth. `SURFACE_SINGLETON` represents selected variables with no
explicit vertical axis. It is distinct from an explicit singleton coordinate,
such as OISST `zlev=0 m positive=down`. `UNKNOWN_VERTICAL` preserves resolved Z
axes whose physical family cannot safely be classified.

## Units, direction, and coordinate order

The executable length subset is m/metre/meter variants and km/kilometre/
kilometer variants. Other recognizable length spellings remain unsupported;
unknown units are preserved without a UDUNITS claim. `positive` is interpreted
case-insensitively as `up` or `down`. Physical positive direction is independent
of source storage order, which is recorded as increasing, decreasing, singleton,
duplicate, or nonmonotonic. Scientific arrays are never reordered.

`x$depth` remains the source-compatible selection coordinate. For supported
physical calculations, the descriptor supplies the positive convention and
`scale_to_m`; it does not replace or duplicate a normalized public coordinate.
The existing `x$depth_extent` remains the extent of coordinate centres, not the
physical layer-bound extent. A physical extent belongs to a future C-series
contract.

## Bounds and metric geometry

CF `bounds` relationships come from the B2 source link registry. The small
coordinate/bounds arrays may be read during opening; scientific variables are
not read. The normalized current representation is one preserved pair per
selected depth.

Metric geometry requires finite numeric `n_depth x 2` bounds, each centre inside
its pair, positive interval widths, and no overlap. Bound order is preserved;
runtime thickness uses the positive distance between endpoints. Explicit m/km
differences are converted using the bounded conversion already supported by
oceancube. Missing or incompatible units block geometry. Contiguity is recorded
separately and is not required for individual layer thickness.

`cube_layer_thickness()` consumes certified CF bounds automatically and never
infers them from centres. `cube_cell_volume()` is certified only for
rectilinear geodesic horizontal area multiplied by metric geometric thickness.
Height, pressure, dimensionless, and parametric axes are rejected even if a
caller supplies numeric bounds.

## Propagation and boundaries

Depth selection keeps `cf$source` immutable and subsets the current coordinate
and bound pairs in selected order. `cube_collect()` preserves the descriptor
exactly. Meaning-changing transforms set vertical runtime and geometry statuses
to `DERIVATION_PENDING`; they are not automatically recertified.

Provenance V1 is unchanged. CF semantics remain in metadata, not `x$qa`.
`layer_mean()` retains its legacy centre-edge numerical behavior and is not
Gate-B certified. `viz.profile()`, `viz.section()`, and `viz.transect()` display
stored source depth values and optionally reverse their plotting scale; they do
not normalize vertical semantics and are not physical vertical transforms.

## Gate-B subset and exclusions

Gate B authorizes dimensional metric ocean depth on a rectilinear cube with
certified CF semantics and explicit vertical bounds for physical geometry,
paired with ordinary or safely supported climatological time. It does not
authorize height-to-depth or pressure-to-depth conversion, parametric formula
evaluation, static-field ingest, vertical interpolation/integration, named
vertical diagnostics, or legacy vertical-reduction certification.

The governed WOA23 January fixture supplies positive evidence. Its six sampled
layers are intentionally non-contiguous and have thicknesses 2.5, 5, 5, 5, 15,
and 25 m. The annual WOA23 `t00` temporal inconsistency remains a safely rejected
external-product finding and does not invalidate this vertical subset.

The next phase is `0.3.0-C1 — VERTICAL PRIMITIVES AND EXPLICIT-BOUNDS
HARDENING`, which must separately define first-class bounds, clipping,
coverage, and the future of legacy centre-edge reduction.
