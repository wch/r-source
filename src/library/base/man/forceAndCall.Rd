% File src/library/base/man/force.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{forceAndCall}
\alias{forceAndCall}
\title{Call a function with Some Arguments Forced}
\description{
  Call a function with a specified number of leading arguments forced
  before the call if the function is a closure.
}
\usage{
forceAndCall(n, FUN, ...)
}
\arguments{
  \item{n}{number of leading arguments to force.}
  \item{FUN}{function to call.}
  \item{\dots}{arguments to \code{FUN}.}
}
\details{
  \code{forceAndCall} calls the function \code{FUN} with arguments
  specified in \code{\dots}. If the value of \code{FUN} is a closure
  then the first \code{n} arguments to the function are evaulated
  (i.e. their delayed evaluation promises are forced) before executing
  the function body. If the value of \code{FUN} is a primitive then
  the call \code{FUN(\dots)} is evaluated in the usual way.

  \code{forceAndCall} is intended to held defining higher order
  functions line \code{apply} to behave more rasonably when the result
  returned by the function applied is a closure that captured its
  arguments.
  
}
\keyword{programming}
