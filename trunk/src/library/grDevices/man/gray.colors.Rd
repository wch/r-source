% File src/library/grDevices/man/gray.colors.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{gray.colors}
\alias{gray.colors}
\alias{grey.colors}
\title{Gray Color Palette}
\description{
  Create a vector of \code{n} gamma-corrected gray colors.
}
\usage{
gray.colors(n, start = 0.3, end = 0.9, gamma = 2.2, alpha, rev = FALSE)
grey.colors(n, start = 0.3, end = 0.9, gamma = 2.2, alpha, rev = FALSE)
}
\arguments{
  \item{n}{the number of gray colors (\eqn{\ge 1}) to be in the
    palette.}
  \item{start}{starting gray level in the palette (should be between
    \code{0} and \code{1} where zero indicates \code{"black"} and one
    indicates \code{"white"}).}
  \item{end}{ending gray level in the palette.}
  \item{gamma}{the gamma correction.}
  \item{alpha}{the opacity, if specified.}
  \item{rev}{logical indicating whether the ordering of the colors should
    be reversed.}
}
\details{
  The function \code{gray.colors} chooses a series of \code{n}
  gamma-corrected gray levels between \code{start} and \code{end}:
  \code{seq(start^gamma, end^gamma, length = n)^(1/gamma)}.
  The returned palette contains the corresponding gray colors.
  This palette is used in \code{\link{barplot.default}}.

  \code{grey.colors} is an alias for \code{gray.colors}.
}
\value{
  A vector of \code{n} gray colors.
}
\seealso{
  \code{\link{gray}},
  \code{\link{rainbow}},
  \code{\link{palette}}.
}
\examples{
require(graphics)

pie(rep(1, 12), col = gray.colors(12))
barplot(1:12, col = gray.colors(12))
}
\keyword{color}
