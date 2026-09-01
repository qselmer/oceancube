% Generated manually for the bounded C7 contract.
\name{oxygen_boundary}
\alias{oxygen_boundary}
\title{Resolve explicit oxygen-threshold zone boundaries}
\usage{
oxygen_boundary(
  x,
  threshold,
  threshold_unit = NULL,
  variable = NULL,
  support = c("local", "all")
)
}
\arguments{
\item{x}{A direct source-profile \code{<ocean_cube>} with one eligible
preserved CF oxygen concentration variable.}
\item{threshold}{Required finite non-negative numeric scalar.}
\item{threshold_unit}{Optional bounded oxygen unit from the source physical
quantity family. \code{NULL} means the exact source unit.}
\item{variable}{Optional exact eligible current oxygen variable name.}
\item{support}{Connectivity policy. \code{"local"} breaks components at
explicit gaps; \code{"all"} retains gapped evidence without an exact crossing.}
}
\value{
A base \code{data.frame} with one row per longitude, latitude, and time,
describing the core-containing threshold component, boundary evidence,
Provenance V1, and bounded QA.
}
\description{
Resolve an explicit oxygen-threshold zone and its observed boundaries without
introducing a universal OMZ or ODZ threshold.
}
\details{
Oxygen identity comes only from supported preserved CF \code{standard_name}
and compatible units. Point crossings may be exact or locally linearly
interpolated inside contiguous brackets. Cell means and gapped brackets return
bracket-only evidence. Open edges are not extrapolated; exact thickness is
reported only for two exact or locally interpolated point boundaries.
}
\examples{
\dontrun{
oxygen_boundary(cube, 20, "umol kg-1")
}
}
