\name{isSealedMethod}
\alias{isSealedMethod}
\alias{isSealedClass}
\title{ Check for a Sealed Method or Class }
\description{
  These functions check for either a method or a class that has been
  \dQuote{sealed} when it was defined, and which therefore cannot be
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
  In the R implementation of classes and methods, it is possible to
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
  The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes and methods in the book
  \emph{Programming with Data} (John M. Chambers, Springer, 1998), in
  particular sections 1.6, 2.7, 2.8, and chapters 7 and 8.

  While the programming interface for the \pkg{methods} package follows
  the reference, the R software is an original implementation, so
  details in the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details see \code{?\link{Methods}}
  and the links from that documentation.
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
