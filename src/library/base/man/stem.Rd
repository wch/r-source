\name{stem}
\title{Stem-and-Leaf Plots}
\usage{
stem(x, scale = 1, width = 80, atom = 1e-08)
}
\alias{stem}
\description{
  \code{stem} produces a stem-and-leaf plot of the values in \code{x}.
  The parameter \code{scale} can be used to expand the scale of the
  plot.  A value of \code{scale=2} will cause the plot to be roughly
  twice as long as the default.
}
\examples{
data(islands)
stem(islands)
stem(log10(islands))
}
\keyword{univar}
\keyword{distribution}
