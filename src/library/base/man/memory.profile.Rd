% File src/library/base/man/memory.profile.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{memory.profile}
\alias{memory.profile}
\title{Profile the Usage of Cons Cells}
\description{
    Lists the usage of the cons cells by \code{SEXPREC} type.
}
\usage{
memory.profile()
}
\details{
    The current types and their uses are listed in the include file
    \file{Rinternals.h}.
}
% There will be blanks in the list corresponding
% to types that are no longer in use (types 11 and 12 at the time of
% writing).
% Also \code{FUNSXP} is not included.
% Nonsense: that is a mode, not a type.
\value{
  A vector of counts, named by the types.  See \code{\link{typeof}} for
  an explanation of types.
}

\seealso{
  \code{\link{gc}} for the overall usage of cons cells.
  \code{\link{Rprofmem}} and \code{\link{tracemem}} allow memory profiling
  of specific code or objects, but need to be enabled at compile time.
}

\examples{\donttest{
memory.profile()
}}
\keyword{utilities}
