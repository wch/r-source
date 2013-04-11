% File src/library/base/man/base-deprecated.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{base-deprecated}
\alias{base-deprecated}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Package \pkg{base}}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
%% \usage{
%% .find.package(\dots)
%% .path.package(\dots)
%% }
%% \arguments{
%%    \item{\dots}{}
%% }
\details{
  The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).
  Functions in standard packages other than the base package are listed in
  \code{help("pkg-deprecated")}, where \samp{pkg} is replaced by the
  name of the package.
}
\seealso{
  \code{\link{Deprecated}}
}
\keyword{internal}
\keyword{misc}
