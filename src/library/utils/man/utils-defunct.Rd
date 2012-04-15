% File src/library/utils/man/utils-defunct.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{utils-defunct}
%------ NOTE:  ../R/utils-defunct.R   must be synchronized with this!
\alias{utils-defunct}
\alias{package.contents}
\alias{zip.file.extract}
#ifdef windows
\alias{zip.unpack}
#endif

\title{Defunct Functions in Package \pkg{utils}}
\description{
  The functions or variables listed here are no longer part of \R as
  they are not needed (any more).
}
\usage{
package.contents(pkg, lib.loc = NULL)

zip.file.extract(file, zipname = "R.zip",
                 unzip = getOption("unzip"), dir = tempdir())
#ifdef windows
zip.unpack(zipname, dest)
#endif
}
\seealso{
  \code{\link{utils-deprecated}},
  \code{\link{Deprecated}}, \code{\link{Defunct}}
}
\keyword{internal}
