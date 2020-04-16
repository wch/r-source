% File src/library/base/man/base-deprecated.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{base-deprecated}
\alias{base-deprecated}
\alias{unix.time}% in 2016, for R 3.4.0 (moved here only for R 4.0.0)
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Package \pkg{base}}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
\usage{
unix.time(\dots)  ### -- use  system.time(...)  instead
}
\arguments{
  \item{\dots}{(arguments)}
}
\details{
  Functions in standard packages other than the base package are listed in
  \code{help("pkg-deprecated")}, where \samp{pkg} is replaced by the
  name of the package.
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{internal}
\keyword{misc}
