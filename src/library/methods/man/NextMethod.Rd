\name{callNextMethod}
\alias{callNextMethod}
\title{Call an Inherited Method}
\description{
  A call to \code{callNextMethod} can only appear inside a method
  definition.  It then results in a call to the first inherited method
  after the current method, with the arguments to the current method
  passed down to the next method.  The value of that method call is the
  value of \code{callNextMethod}.
}
\usage{
callNextMethod(...)
}
\arguments{
  \item{\dots}{
If included, the call to the next methods uses these as its arguments
(but note that the dispatch is as described below.)  The
recommendation for most applications is to use \code{callNextMethod}
with no explicit arguments.
    }
}
\details{
  The definition of the first inherited method is that is the method
  which \emph{would} have been called if the current method did not
  exist.
  This is more-or-less literally what happens: The current method is
  deleted from a copy of the methods for the current generic, and
  \code{\link{selectMethod}} is called to find the next method (the
  result is cached in a special object, so the search only typically
  happens once per session per combination of argument classes).

  It is also legal, and often useful, for the method called by
  \code{callNextMethod} to itself have a call to \code{callNextMethod}.  This
  works the same way, except that now two methods are deleted before
  selecting the next method, and so on for further nested calls to
  \code{callNextMethod}.

  The statement that the method is called with the current arguments is
  more precisely as follows.  Arguments that were missing in the current
  call are still missing (remember that \code{"missing"} is a valid
  class in a method signature).  For a formal argument, say \code{x}, that
  appears in the original call, there is a corresponding argument in the
  next method call equivalent to ``\code{x = x}''.  In effect, this
  means that the next method sees the same actual arguments, but
  arguments are evaluated only once.
}
\value{
  The value returned by the selected method.
}
\references{
  The R package \code{methods} implements, with a few exceptions, the
  programming interface for classes
  and methods in the book \emph{Programming with Data} (John
  M. Chambers, Springer, 1998), in particular sections 1.6, 2.7, 2.8,
  and chapters 7 and 8.

  While the programming interface for the methods package follows the reference,
  the R software is an original implementation, so details in
  the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details and ongoing development, see the web page 
  \url{http://developer.r-project.org/methodsPackage.html} and the
  pointers from that page.
}
\seealso{\link{Methods} for the general behavior of method dispatch}

\examples{

## some class definitions with simple inheritance
setClass("B0" , representation(b0 = "numeric"))

setClass("B1", "B0")

setClass("B2", representation("B1", b2 = "logical"))

## and a rather silly function to illustrate callNextMethod

f <- function(x) class(x)

setMethod("f", "B0", function(x) c(x@b0, callNextMethod()))

setMethod("f", "B2", function(x) c(x@b2, callNextMethod()))

b2 <- new("B2", b2 = FALSE, b0 = 10)

b1 <- new("B1", b0 = 2)

f(b2)

f(b1)

}
\keyword{programming}
\keyword{classes}
\keyword{methods}
