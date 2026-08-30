% Generated manually from R/depth_sample.R for the bounded C3 contract.
\name{depth_sample}
\alias{depth_sample}
\title{Sample a cube at physical metric depths}
\usage{
depth_sample(x, depth, method = c("auto", "cell", "linear"))
}
\arguments{
\item{x}{An \code{<ocean_cube>} with a certified CF metric-depth coordinate.}

\item{depth}{Non-empty, finite, strictly increasing numeric target depths,
interpreted in the source depth-coordinate units.}

\item{method}{Sampling method. \code{"auto"} uses source CF value semantics:
vertical cell means use cell containment and vertical point values use local
linear interpolation. Explicit \code{"cell"} and \code{"linear"} require all
selected variables to satisfy the corresponding contract.}
}
\value{
A memory-backed \code{<ocean_cube>} whose depth coordinates are exactly
\code{depth}. Scientific variable units are unchanged. The output has no
physical depth-cell bounds.
}
\description{
Reconstruct scientifically bounded values at requested physical depths using
either explicit source-cell containment or local linear interpolation of point
values.
}
\details{
Unlike \code{cube_slice()}, this function does not select a nearest stored
coordinate. Unlike \code{cube_extract()}, it does not return existing stored
cells as a table.

CF vertical cell means require valid explicit metric bounds and use a
piecewise-constant-cell-mean reconstruction. Shared interior boundaries,
explicit gaps, and outside-support targets are errors. Vertical point values
use exact source values when matched within the common vertical tolerance;
otherwise a transparent two-point linear interpolation is used. Missing or
non-finite required values return \code{NA}. No imputation, nearest fallback,
gap bridging, or extrapolation is performed.

Only certified dimensional ocean depth in metres or kilometres is supported.
Sampled outputs deliberately have no certified layer bounds and therefore do
not automatically qualify for thickness, volume, or vertical integration.
}
\examples{
\dontrun{
# Offline synthetic CF point profile: 0, 10, 20 m and values 0, 20, 40.
point <- read_nc("synthetic-point-profile.nc", vars = "temperature")
depth_sample(point, c(5, 12.5), method = "linear")

# Offline synthetic cell means with explicit 0-10, 10-20, 20-30 m bounds.
cells <- read_nc("synthetic-cell-mean-profile.nc", vars = "temperature")
depth_sample(cells, c(2, 12, 28), method = "cell")
}
}
