% File src/library/tools/man/tools-defunct.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{tools-defunct}
% NOTE: ../R/tools-defunct.R must be synchronized with this!
\alias{tools-defunct}
\alias{Rd_parse}
\alias{readNEWS}
\alias{checkNEWS}

\title{Defunct Objects in Package \pkg{tools}}
\description{
  The functions or variables listed here are no longer part of \R as
  they are not needed (any more).
}
\usage{
# Defunct in R 2.10.0, removed in R 3.0.0
Rd_parse(file, text = NULL)
# Defunct in R 3.1.0
readNEWS(file = file.path(R.home(), "NEWS"), trace = FALSE,
         chop = c("first", "1", "par1", "keepAll"))
checkNEWS(file = file.path(R.home(), "NEWS"))
}
\seealso{
  \code{\link{tools-deprecated}},
  \code{\link{Deprecated}},
  \code{\link{Defunct}}
}
\keyword{internal}
