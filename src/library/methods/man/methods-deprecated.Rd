% File src/library/methods/man/methods-deprecated.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{methods-deprecated}
\alias{methods-deprecated}
\alias{trySilent}
%----- NOTE:  ../R/methods-deprecated.R   must be synchronized with this!
\title{Deprecated Functions in Methods package}
%----- PLEASE: put \alias{.} here for EACH ! --- Do not remove this file, even when ``empty''
%
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as the next release.
}
\usage{
trySilent(expr)
}
\arguments{
  \item{expr}{an \R expression to try.}
}
\details{
  The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).
  Functions in packages other than the methods package are listed in
  \code{help("pkg-deprecated")}.

  \code{trySilent(ex)} is a deprecated equivalent of \code{\link{try}(e,
    silent=TRUE)}; in \R programming it is typically more efficient and
    flexible to work with something like \code{\link{tryCatch}(ex,
      error = function(e) e)} instead. % possibly also 'function(e) NULL'
}
\seealso{
  \code{\link{Deprecated}}, \code{\link{methods-defunct}}
}
\keyword{misc}
