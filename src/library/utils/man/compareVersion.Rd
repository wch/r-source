% File src/library/utils/man/compareVersion.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{compareVersion}
\alias{compareVersion}
\title{Compare Two Package Version Numbers}
\description{
  Compare two package version numbers to see which is later.
}
\usage{
compareVersion(a, b)
}
\arguments{
  \item{a, b}{Character strings representing package version numbers.}
}
\value{
  \code{0} if the numbers are equal, \code{-1} if \code{b} is later
  and \code{1} if \code{a} is later (analogous to the C function
  \code{strcmp}).
}
\details{
  \R package version numbers are of the form \code{x.y-z} for integers
  \code{x}, \code{y} and \code{z}, with components after \code{x}
  optionally missing (in which case the version number is older than
  those with the components present).
}
\seealso{
  \code{\link{package_version}},
  \code{\link{library}}, \code{\link{packageStatus}}.
}
\examples{
compareVersion("1.0", "1.0-1")
compareVersion("7.2-0","7.1-12")
}
\keyword{utilities}
