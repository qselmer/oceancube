#' Integrate CF vertical cell means over metric depth
#'
#' @param x An `<ocean_cube>` with a certified CF metric-depth axis, explicit
#'   valid bounds, and variables declared as vertical cell means.
#' @param depth Numeric vector. Either `c(zmin, zmax)` for one layer or a
#'   sequence of increasing layer breaks.
#'
#' @return An in-memory `<ocean_cube>` whose depth dimension contains the
#'   requested vertical integrals. Output units are represented symbolically as
#'   the source unit multiplied by metres.
#'
#' @details Integration uses exact metric overlaps with explicit CF depth-cell
#'   bounds and treats each source cell mean as piecewise constant within its
#'   cell. Full geometric coverage is required; partial coverage errors before
#'   payload values are read and zero coverage returns missing output. Missing
#'   values are strict: any non-finite value with positive overlap makes that
#'   output integral missing. The operation does not synthesize a CF
#'   `depth: sum` claim or derive a new `standard_name`.
#' @examples
#' \dontrun{
#' # For a CF NetCDF variable declared with `depth: mean` and explicit bounds:
#' x <- read_nc("ocean-column.nc", vars = "temperature")
#' heat_content_proxy <- layer_integral(x, depth = c(0, 10, 50, 100))
#' }
#' @export
layer_integral <- function(x, depth) {
  .vertical_reduce(x, depth, method = "integral")
}
