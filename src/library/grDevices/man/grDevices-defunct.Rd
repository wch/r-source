% File src/library/grDevices/man/grDevices-defunct.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grDevices-defunct}
\alias{postscriptFont}
\title{Defunct Functions in Package \pkg{grDevices}}
\description{
  The functions or variables listed here are no longer part of \R as
  they are not needed (any more).
}
\usage{
# Defunct in R 2.4.0
postscriptFont(family, metrics, encoding = "default")
}
\details{
  The function \code{postscriptFont} has been replaced by
  \code{\link{Type1Font}}, which is just a change of name.
}
\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}
