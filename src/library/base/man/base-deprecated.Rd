% File src/library/base/man/base-deprecated.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{base-deprecated}
\alias{base-deprecated}
\alias{real}
\alias{as.real}
\alias{is.real}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Package \pkg{base}}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
\usage{
real(length = 0)
as.real(x, \dots)
is.real(x)
}
\arguments{
  \item{length}{A non-negative integer specifying the desired length.
    Double values will be coerced to integer:
    supplying an argument of length other than one is an error.}
  \item{x}{object to be coerced or tested.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  These functions are the same as their \code{\link{double}} equivalents
  and are provided for backwards compatibility only.
  
  \code{as.real} is a generic function, but S3 methods must be
  written for \code{\link{as.double}}.  
  
%  The original help page for these functions is often
%  available at \code{help("oldName-deprecated")} (note the quotes).
  Functions in packages other than the base package are listed in
  \code{help("pkg-deprecated")}, where \samp{pkg} is replaced by the
  name of the package.
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{internal}
\keyword{misc}
