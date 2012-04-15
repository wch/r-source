% File src/library/utils/man/utils-deprecated.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{utils-deprecated}
%------ NOTE:  ../R/utils-deprecated.R   must be synchronized with this!
\alias{utils-deprecated}
\alias{CRAN.packages}
\title{Deprecated Functions in Package \pkg{utils}}
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as of the next release.
}
\usage{
CRAN.packages(CRAN = getOption("repos"), method,
              contriburl = contrib.url(CRAN))

}
\arguments{
  \item{CRAN}{character, an earlier way to specify a repository.}
  \item{method}{Download method, see \code{\link{download.file}}.}
  \item{contriburl}{URL(s) of the contrib section of the
    repositories. Use this argument only if your CRAN mirror is
    incomplete, e.g., because you burned only the \file{contrib} section on a
    CD.  Overrides argument \code{repos}.
  }  
}
\seealso{
  \code{\link{Deprecated}}, \code{\link{Defunct}}
}
\keyword{misc}
