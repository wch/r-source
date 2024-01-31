% File src/library/base/man/base-deprecated.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{base-deprecated}
\encoding{UTF-8}
\alias{base-deprecated}
%------ Eventually move entries to ---->> ./base-defunct.Rd
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Package \pkg{base}}
%------ PLEASE: put \alias{.} here for EACH !
\alias{is.R}
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.

%% for example 'Spotfire® Enterprise Runtime for R' aka TERR or 'pqR'
  \code{is.R} tests if it is not running under historical dialects of S
  (including S-PLUS), but cannot exclude other (including future)
  programs claiming compatibility with \R.  Its historical definition
  was
\preformatted{
  exists("version") && !is.null(vl <- version$language) && vl == "R"
}
  which always evaluates to \code{TRUE} in \R.
}
\usage{
is.R()
}

\details{
  Deprecated
  functions in standard packages other than the base package are listed in
  \code{help("pkg-deprecated")}, where \samp{pkg} is replaced by the
  name of the package.
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{internal}
\keyword{misc}
