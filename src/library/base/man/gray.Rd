\name{gray}
\title{Gray Level Specification}
\usage{
gray(level)
grey(level)
}
\alias{gray}
\alias{grey}
\arguments{
  \item{level}{a vector of desired gray levels between \code{0} and \code{1};
  zero indicates \code{"black"} and one indicates \code{"white"}}
}
\value{
  A vector of ``colors'' of the same length as \code{level}.
}
\details{
  The values returned by \code{gray} can be used with a
  \code{col=} specification in graphics functions or in
  \code{\link{par}}.

  \code{grey} is an alias for \code{gray}.
}
\seealso{
  \code{\link{rainbow}}, \code{\link{hsv}}, \code{\link{rgb}}.
}
\examples{
gray(0:8 / 8)
}
\keyword{color}
