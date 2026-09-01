% Generated manually from R/transition_layer.R for the bounded C6 contract.
\name{transition_layer}
\alias{transition_layer}
\title{Diagnose a variable-aware vertical transition-layer candidate}
\usage{
transition_layer(
  x,
  diagnostic,
  variable = NULL,
  support = c("local", "all")
)
}
\arguments{
\item{x}{A direct certified source-profile \code{<ocean_cube>} or an already
computed certified C4 vertical-gradient \code{<ocean_cube>}. C1, C2, and C3
derived profiles are outside the initial certified subset.}

\item{diagnostic}{Required diagnostic name: \code{"thermocline"},
\code{"halocline"}, \code{"upper_oxycline"}, or
\code{"lower_oxycline"}.}

\item{variable}{Optional exact current variable name. \code{NULL} selects the
sole variable whose preserved source CF \code{standard_name} is eligible;
zero or multiple eligible variables are errors.}

\item{support}{C5 support policy. \code{"local"} excludes explicitly gapped
secants; \code{"all"} may return a truthfully labelled gapped candidate.}
}
\value{
A base \code{data.frame} with one row per longitude, latitude, and time. C5
feature diagnostics are retained and augmented with physical-variable
identity, diagnostic definition, basis, direction, strength, input mode,
diagnostic status, Provenance V1, and bounded QA.
}
\description{
Interpret a generic C5 gradient candidate as an unthresholded thermocline,
halocline, or branch-aware oxycline candidate only when preserved source CF
identity and units authorize that interpretation.
}
\details{
Physical eligibility is determined only from immutable preserved source CF
\code{standard_name} and a bounded compatible-unit contract. Variable names
and \code{long_name} are never semantic evidence. Thermocline selects the
strongest eligible negative temperature gradient with depth. Halocline selects
the strongest eligible absolute salinity gradient and preserves its sign.

Direct source profiles compose \code{depth_gradient()} and
\code{depth_feature()}. Existing certified gradients are not differentiated a
second time. Gaps, ties, missingness, completeness, source brackets, spacing,
support gaps, and certification limits remain authoritative. Oxygen diagnostics
first resolve a complete-profile minimum/core, then rank negative gradients on
the upper branch or positive gradients on the lower branch. No threshold,
smoothing, prominence, width, curvature, density, mixed-layer depth, or
pycnocline is calculated. \code{localization_half_span_m} is a vertical
resolution scale, not statistical uncertainty.
}
\examples{
\dontrun{
transition_layer(cube, diagnostic = "thermocline")
transition_layer(cube, diagnostic = "halocline", support = "all")
transition_layer(cube, diagnostic = "upper_oxycline")
transition_layer(cube, diagnostic = "lower_oxycline")

gradient <- depth_gradient(cube)
transition_layer(gradient, diagnostic = "thermocline")
}
}
