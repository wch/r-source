% File src/library/stats/man/stats-deprecated.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{stats-deprecated}
\alias{stats-deprecated}
\alias{clearNames}
%------ NOTE:  ../R/stats-deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Stats package}
%------ PLEASE: one \alias{.} for EACH ! (+ one \usage{} & \arguments{} for all)
\usage{
clearNames(object)
}
\arguments{
 \item{object}{ an object that may have a \code{names} attribute }
}
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
\details{
  %-- Either:
  % There are currently no deprecated functions in this package.
  %-- Or:
  %% explain *why* it's deprecated, and \code{\link{..}} to new
  \code{clearNames} had been introduced at about the same time as
  \code{\link{unname}}, but is less general and has been used rarely.
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{misc}
