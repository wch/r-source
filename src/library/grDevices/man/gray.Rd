\name{gray}
\alias{gray}
\alias{grey}
\title{Gray Level Specification}
\description{
  Create a vector of colors from a vector of gray levels.
}
\usage{
gray(level)
grey(level)
}
\arguments{
  \item{level}{a vector of desired gray levels between \code{0} and
    \code{1}; zero indicates \code{"black"} and one indicates
    \code{"white"}.}
}
\details{
  The values returned by \code{gray} can be used with a \code{col=}
  specification in graphics functions or in \code{\link{par}}.

  \code{grey} is an alias for \code{gray}.
}
\value{
  A vector of \dQuote{colors} of the same length as \code{level}.
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
