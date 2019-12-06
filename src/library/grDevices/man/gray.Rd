% File src/library/grDevices/man/gray.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{gray}
\alias{gray}
\alias{grey}
\title{Gray Level Specification}
\description{
  Create a vector of colors from a vector of gray levels.
}
\usage{
gray(level, alpha)
grey(level, alpha)
}
\arguments{
  \item{level}{a vector of desired gray levels between \code{0} and
    \code{1}; zero indicates \code{"black"} and one indicates
    \code{"white"}.}
  \item{alpha}{the opacity, if specified.}
}
\details{
  The values returned by \code{gray} can be used with a \code{col=}
  specification in graphics functions or in \code{\link{par}}.

  \code{grey} is an alias for \code{gray}.
}
\value{
  A vector of colors of the same length as \code{level}.
}
\seealso{
  \code{\link{rainbow}},
  \code{\link{hsv}},
  \code{\link{hcl}},
  \code{\link{rgb}}.
}
\examples{
gray(0:8 / 8)
}
\keyword{color}
