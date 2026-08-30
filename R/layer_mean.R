#' Average an ocean cube over vertical layers
#'
#' @param x An `<ocean_cube>` object with at least two depth levels.
#' @param depth Numeric vector. Either `c(zmin, zmax)` for one layer or a sequence
#'   of breaks, e.g. `c(0, 10, 25, 50, 100)`.
#'
#' @return An `<ocean_cube>` with the depth dimension replaced by vertical layers.
#'
#' @details For certified CF metric-depth cubes, weights are exact overlaps
#'   between requested intervals and explicit cell bounds. Every requested
#'   interval must have full geometric coverage; partial coverage (including
#'   internal gaps) errors before payload data are read, while zero coverage
#'   returns an all-missing layer. Missing scientific values are renormalized
#'   over finite contributing cells and do not change geometric coverage.
#'   Geometric certification and value-semantic certification are separate:
#'   only variables recognized from preserved CF `cell_methods` as exact
#'   vertical cell means receive combined certification. Other value semantics
#'   retain the backward-compatible numerical operation but are explicitly
#'   uncertified.
#'
#'   Cubes without certified metric bounds retain the historical
#'   centre-derived `.depth_edges()` / `.depth_weights()` calculation for
#'   backward compatibility. That path is explicitly uncertified. Certified
#'   results carry requested output bounds and a derived current CF vertical
#'   descriptor; the immutable CF source record is preserved.
#' @export
layer_mean <- function(x, depth) {
  .vertical_reduce(x, depth, method = "mean")
}
