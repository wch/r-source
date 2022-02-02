% File src/library/base/man/callCC.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{callCC}
\title{Call With Current Continuation}
\usage{
callCC(fun)
}
\alias{callCC}
\arguments{
  \item{fun}{function of one argument, the exit procedure.}
}
\description{
A downward-only version of Scheme's call with current continuation.
}
\details{
  \code{callCC} provides a non-local exit mechanism that can be useful
  for early termination of a computation.  \code{callCC} calls
  \code{fun} with one argument, an \emph{exit function}.  The exit
  function takes a single argument, the intended return value.  If the
  body of \code{fun} calls the exit function then the call to
  \code{callCC} immediately returns, with the value supplied to the exit
  function as the value returned by \code{callCC}.
}
\author{Luke Tierney}

\examples{
# The following all return the value 1
callCC(function(k) 1)
callCC(function(k) k(1))
callCC(function(k) {k(1); 2})
callCC(function(k) repeat k(1))
}
\keyword{programming}
