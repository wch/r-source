% File src/library/base/man/Last.value.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{Last.value}
\alias{.Last.value}
\title{Value of Last Evaluated Expression}
\usage{
.Last.value
}
\description{
  The value of the internal evaluation of a top-level \R expression
  is always assigned to \code{.Last.value} (in \code{package:base})
  before further processing (e.g., printing).
}
\details{
  The value of a top-level assignment \emph{is} put in \code{.Last.value},
  unlike S.

  Do not assign to \code{.Last.value} in the workspace, because this
  will always mask the object of the same name in \code{package:base}.
}
\seealso{\code{\link{eval}}
}
\examples{
## These will not work correctly from example(),
## but they will in make check or if pasted in,
## as example() does not run them at the top level
gamma(1:15)          # think of some intensive calculation...
fac14 <- .Last.value # keep them

library("splines") # returns invisibly
.Last.value    # shows what library(.) above returned
\dontshow{detach("package:splines")}
}
\keyword{programming}
