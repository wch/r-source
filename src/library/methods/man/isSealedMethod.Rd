% File src/library/methods/man/isSealedMethod.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{isSealedMethod}
\alias{isSealedMethod}
\alias{isSealedClass}
\title{ Check for a Sealed Method or Class }
\description{
  These functions check for either a method or a class that has been
  \emph{sealed} when it was defined, and which therefore cannot be
  re-defined.
}
\usage{
isSealedMethod(f, signature, fdef, where)
isSealedClass(Class, where)
}
\arguments{
  \item{f}{ The quoted name of the generic function. }
  \item{signature}{ The class names in the method's signature, as
  they would be supplied to \code{\link{setMethod}}. }
  \item{fdef}{ Optional, and usually omitted:  the generic function
      definition for \code{f}. }
  \item{Class}{The quoted name of the class.}

  \item{where}{where to search for the method or class definition.  By
    default, searches from the top environment of the call to
    \code{isSealedMethod} or \code{isSealedClass}, typically the
    global environment or the namespace of a package containing a call
    to one of the functions.}
}
\details{
  In the \R implementation of classes and methods, it is possible to
  seal the definition of either a class or a method.  The basic
  classes (numeric and other types of vectors, matrix and array data)
  are sealed.  So also are the methods for the primitive functions on
  those data types.  The effect is that programmers cannot re-define
  the meaning of these basic data types and computations.  More
  precisely, for primitive functions that depend on only one data
  argument, methods cannot be specified for basic classes.  For
  functions (such as the arithmetic operators) that depend on two
  arguments, methods can be specified if \emph{one} of those arguments
  is a basic class, but not if both are.

  Programmers can seal other class and method definitions by using the
  \code{sealed} argument to \code{\link{setClass}} or \code{\link{setMethod}}.
}
\value{
  The functions return \code{FALSE} if the method or class is not
  sealed (including the case that it is not defined); \code{TRUE} if
  it is.
}
\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\examples{
## these are both TRUE
isSealedMethod("+", c("numeric", "character"))
isSealedClass("matrix")

setClass("track",
            representation(x="numeric", y="numeric"))
## but this is FALSE
isSealedClass("track")
## and so is this
isSealedClass("A Name for an undefined Class")
## and so are these, because only one of the two arguments is basic
isSealedMethod("+", c("track", "numeric"))
isSealedMethod("+", c("numeric", "track"))

\dontshow{
removeClass("track")
}
}
\keyword{programming}
\keyword{classes}
\keyword{classes}
\keyword{methods}
