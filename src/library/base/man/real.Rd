% File src/library/base/man/real.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{real}
\title{Real Vectors}
\alias{real}
\alias{as.real}
\alias{is.real}
\usage{
real(length = 0)
as.real(x, \dots)
is.real(x)
}
\description{
  These functions are the same as their \code{\link{double}} equivalents
  and are provided for backwards compatibility only.
}
\arguments{
  \item{length}{A non-negative integer specifying the desired length.
    Double values will be coerced to integer:
    supplying an argument of length other than one is an error.}
  \item{x}{object to be coerced or tested.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  \code{as.real} is a generic function, but S3 methods must be
  written for \code{\link{as.double}}.  
}
\keyword{classes}
