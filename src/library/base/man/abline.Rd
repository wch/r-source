\name{abline}
\title{Add a Straight Line to a Plot}
\usage{
abline(a, b, \dots)
abline(h=, \dots)
abline(v=, \dots)
abline(coef=, \dots)
abline(reg=, \dots)
}
\alias{abline}
\description{
  This function adds one or more straight lines through the current plot.
  The first form above specifies the line in intercept/slope form
  (alternatively \code{a} can be specified on its own and is taken to
  contain the slope and intercept in vector form).
  The \code{h=} and \code{v=} forms draw horizontal and vertical lines
  at the specified coordinates.
  The \code{coef} form specifies the line by a vector containing the
  slope and intercept.
  \code{reg} is a regression object which contains \code{reg$coef}.  If
  it is of length 1 then the value is taken to be the slope of a line
  through the origin, otherwise, the first 2 values are taken to be the
  slope and intercept.

  The graphical parameters \code{col} and \code{lty} can be specified as
  arguments to \code{abline}; see \code{par} for details.
}
\seealso{
  \code{\link{lines}} and \code{\link{segments}} for connected and
  arbitrary lines given by their \emph{endpoints}.
  \code{\link{par}}.
}
\examples{
data(cars)
z <- lm(dist ~ speed, data = cars)
plot(cars)
abline(z)
}
\keyword{aplot}
