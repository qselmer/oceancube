% Generated manually from R/depth_gradient.R for the bounded C4 contract.
\name{depth_gradient}
\alias{depth_gradient}
\title{Compute certified vertical secant gradients}
\usage{
depth_gradient(x, method = c("auto", "point", "cell"))
}
\arguments{
\item{x}{An \code{<ocean_cube>} with a certified multi-level metric-depth axis
and supported current vertical value semantics.}

\item{method}{Gradient method. \code{"auto"} resolves every variable from
current semantics. \code{"point"} requires point values and \code{"cell"}
requires certified vertical cell means.}
}
\value{
A memory-backed \code{<ocean_cube>} with one fewer depth level, symbolic
per-metre units, midpoint depth coordinates, and no physical layer bounds.
}
\description{
Compute signed adjacent-level first-order secant gradients with respect to
canonical physical ocean depth in metres, positive downward.
}
\details{
For adjacent levels, the result is \code{(X[i+1] - X[i]) /
(D[i+1] - D[i])}, where \code{D} is positive-down depth in metres. The same
transparent secant equation is used for point values and cell means while
their semantic classes remain distinct. Output locations are arithmetic
midpoints in the source coordinate unit.

Irregular spacing and increasing or decreasing storage order are supported.
Explicit support gaps are diagnosed and recorded but never filled. Missing or
non-finite endpoints make only their adjacent pairs missing; no missing level
is bridged. No smoothing, higher-order stencil, endpoint derivative,
interpolation, or extrapolation is performed.

Gradient midpoint coordinates have no certified layer bounds and cannot be
used automatically for thickness, volume, or vertical integration.
}
\examples{
\dontrun{
# Offline point profile: 0, 10, 20 m and values 1, 21, 41.
point <- read_nc("synthetic-point-profile.nc", vars = "temperature")
depth_gradient(point, method = "point")

# Offline certified cell means over explicit metric bounds.
cells <- read_nc("synthetic-cell-mean-profile.nc", vars = "temperature")
depth_gradient(cells, method = "cell")
}
}
