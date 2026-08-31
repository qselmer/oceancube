% Generated manually from R/depth_feature.R for the bounded C5 contract.
\name{depth_feature}
\alias{depth_feature}
\title{Detect the strongest certified vertical-gradient candidate}
\usage{
depth_feature(
  x,
  polarity = c("absolute", "positive", "negative"),
  support = c("local", "all")
)
}
\arguments{
\item{x}{A memory-backed \code{<ocean_cube>} produced by
\code{depth_gradient()}, or a supported selection of one, with a current
certified C4 vertical-gradient descriptor.}

\item{polarity}{Candidate direction. \code{"absolute"} ranks gradient
magnitude, \code{"positive"} ranks positive gradients, and \code{"negative"}
ranks negative gradients.}

\item{support}{Support policy. \code{"local"} excludes explicitly gapped
secants. \code{"all"} also ranks them but preserves a gapped-candidate label.}
}
\value{
A base \code{data.frame} with one row per longitude, latitude, time, and
variable, ordered with longitude fastest. Each row contains either one
resolved candidate and its C4 diagnostics or a conservative status explaining
why no candidate was certified.
}
\description{
Reduce a current certified C4 vertical-gradient cube to the strongest generic
gradient candidate per profile and variable without recalculating gradients.
}
\details{
Absolute, positive, and negative polarity use one scale-aware tolerance for
effective zero and ties. Tied strongest candidates remain ambiguous. Missing
eligible gradients reduce \code{gradient_completeness}; a returned candidate
from an incomplete profile is explicitly only the strongest observed
candidate.

Local support admits contiguous cell support and point brackets but excludes
\code{GAPPED_SUPPORT}. An explicit \code{support = "all"} request may select a
gapped secant only as \code{GAPPED_SECANT_CANDIDATE}. Gaps are never filled.

Feature depth is the C4 midpoint. Canonical depth is positive-down metres, and
\code{localization_half_span_m = spacing_m / 2} is a vertical resolution
scale, not statistical uncertainty. The result is a generic candidate, not a
thermocline, oxycline, halocline, mixed-layer depth, or other physics-aware
interpretation. No smoothing, threshold, prominence, feature width, or second
derivative is applied.
}
\examples{
\dontrun{
# Offline synthetic examples; `cube` must have certified vertical metadata.
gradient <- depth_gradient(cube)
depth_feature(gradient, polarity = "absolute")
depth_feature(gradient, polarity = "negative")

# Default local support excludes explicitly gapped secants.
depth_feature(gradient, support = "local")
}
}
