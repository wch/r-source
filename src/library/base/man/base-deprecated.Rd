% File src/library/base/man/base-deprecated.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{base-deprecated}
\alias{base-deprecated}
\alias{Sys.putenv}
\alias{gammaCody}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Base package}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
\usage{
Sys.putenv(\dots)

gammaCody(x)
}
\arguments{
  \item{\dots}{named arguments with values coercible to a character string.}
  \item{x}{numeric, \eqn{\ge 0}{>= 0}.}
}
\details{
  The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).
  Functions in packages other than the base package are listed in
  \code{help("pkg-deprecated")}.

  \code{Sys.putenv} is a deprecated synonym for
  \code{\link{Sys.setenv}}.
  
  \code{gammaCody} is the \eqn{(\Gamma)} function from the Specfun
  package and was at one time used in the Bessel code.  It may be
  somewhat faster but less precise and/or robust than \R's standard
  \code{\link{gamma}}.  It was here for experimental purposes.
}
\seealso{
  \code{\link{Deprecated}}, \code{\link{base-defunct}}
}
\keyword{misc}
