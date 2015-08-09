% File src/library/base/man/pos.to.env.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{pos.to.env}
\alias{pos.to.env}
\title{Convert Positions in the Search Path to Environments}
\description{
  Returns the environment at a specified position in the search path.
}
\usage{pos.to.env(x)}
\arguments{
  \item{x}{an integer between \code{1} and \code{length(search())}, the length
    of the search path, or \code{-1}.}
}
\details{
  Several \R functions for manipulating objects in environments (such as
  \code{\link{get}} and \code{\link{ls}}) allow specifying environments
  via corresponding positions in the search path.  \code{pos.to.env} is
  a convenience function for programmers which converts these positions
  to corresponding environments; users will typically have no need for
  it.  It is \link{primitive}.

  \code{-1} is interpreted as the environment the function is called
  from.

  This is a \link{primitive} function.
}
\examples{
pos.to.env(1) # R_GlobalEnv
# the next returns the base environment
pos.to.env(length(search()))
}
\keyword{utilities}
