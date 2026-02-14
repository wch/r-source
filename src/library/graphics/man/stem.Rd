% File src/library/graphics/man/stem.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{stem}
\title{Stem-and-Leaf Plots}
\usage{
stem(x, scale = 1, width = 80, atom = 1e-08)
}
\alias{stem}
\description{
  \code{stem} produces a stem-and-leaf plot of the values in \code{x}.
  The parameter \code{scale} can be used to expand the scale of the
  plot.  A value of \code{scale = 2} will cause the plot to be roughly
  twice as long as the default.
}
\arguments{
  \item{x}{a numeric vector.}
  \item{scale}{This controls the plot length.}
  \item{width}{The desired width of plot.}
  \item{atom}{a tolerance.}
}
\details{
  Infinite and missing values in \code{x} are discarded.
}
\value{
  Invisible \code{NULL}.
}
\references{
  \bibshow{R:Becker+Chambers+Wilks:1988}
}
\examples{
stem(islands)
stem(log10(islands))
}
\keyword{univar}
\keyword{distribution}
