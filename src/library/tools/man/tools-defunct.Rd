% File src/library/tools/man/tools-defunct.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2022 R Core Team
% Distributed under GPL 2 or later

\name{tools-defunct}
\title{Defunct Objects in Package \pkg{tools}}
% NOTE: ../R/tools-defunct.R must be synchronized with this!
\alias{tools-defunct}
%------ PLEASE: put \alias{.} here for *each* (and keep them) !
\alias{Rd_parse}
\alias{readNEWS}
\alias{checkNEWS}
\alias{package.dependencies}
\alias{getDepList}
\alias{pkgDepends}
\alias{installFoundDepends}
\alias{vignetteDepends}

\description{
  The functions or variables listed here are no longer part of \R as
  they are not needed (any more).
}
\usage{
# Defunct in R 2.10.0, removed in R 3.0.0
Rd_parse(file, text = NULL)

# Defunct in R 3.1.0,  removed in R 4.1.0
readNEWS(file = file.path(R.home(), "NEWS"), trace = FALSE,
         chop = c("first", "1", "par1", "keepAll"))
checkNEWS(file = file.path(R.home(), "NEWS"))

# Defunct in R 4.1.0
package.dependencies(x, check = FALSE,
                     depLevel = c("Depends", "Imports", "Suggests"))
getDepList(depMtrx, instPkgs, recursive = TRUE, local = TRUE,
           reduce = TRUE, lib.loc = NULL)
pkgDepends(pkg, recursive = TRUE, local = TRUE, reduce = TRUE,
           lib.loc = NULL)
installFoundDepends(depPkgList, ...)
vignetteDepends(vignette, recursive = TRUE, reduce = TRUE,
                local = TRUE, lib.loc = NULL)
}
\seealso{
  \code{\link{tools-deprecated}},
  \code{\link{Deprecated}},
  \code{\link{Defunct}}
}
\keyword{internal}
