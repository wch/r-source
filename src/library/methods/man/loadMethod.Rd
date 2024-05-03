% File src/library/methods/man/MethodsList.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{loadMethod}
\title{Load an S4 Method}
%
\alias{loadMethod}
\alias{loadMethod-methods}
\alias{loadMethod,ANY-method}
\alias{loadMethod,MethodDefinition-method}
\alias{loadMethod,MethodWithNext-method}
\description{
  Internal generic function with methods; only for internal use of methods initialization.
}
\usage{
loadMethod(method, fname, envir)
}
\details{
      Called, if necessary, just before a call to \code{method} is
      dispatched in the frame \code{envir}.  The function exists so that
      methods can be defined for special classes of objects.  Usually
      the point is to assign or modify information in the frame
      environment to be used evaluation.  For example, the standard
      class \code{MethodDefinition} has a method that stores the target
      and defined signatures in the environment.  Class
      \code{MethodWithNext} has a method taking account of the
      mechanism for storing the method to be used in a call to
      \code{\link{callNextMethod}}.

      Any methods defined for \code{loadMethod} must return the function
      definition to be used for this call; typically, this is just the
      \code{method} argument.
}
\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\keyword{internal}

