% File src/library/base/man/invisible.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{invisible}
\alias{invisible}
\title{Change the Print Mode to Invisible}
\description{
  Return a (temporarily) invisible copy of an object.
}
\usage{invisible(x)}
\arguments{
  \item{x}{an arbitrary \R object.}
}
\details{
  This function can be useful when it is desired to have functions
  return values which can be assigned, but which do not print when they
  are not assigned.

  This is a \link{primitive} function.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{withVisible}},
  \code{\link{return}},
  \code{\link{function}}.
}
\examples{
# These functions both return their argument
f1 <- function(x) x
f2 <- function(x) invisible(x)
f1(1)  # prints
f2(1)  # does not
}
\keyword{programming}
